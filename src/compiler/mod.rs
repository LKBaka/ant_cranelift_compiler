pub mod arc;
pub mod compile_state_impl;
mod constants;
mod convert_type;
mod imm;

pub mod compiler_impl;
pub mod handler;
pub mod table;

use std::cell::RefCell;
use std::env::{current_dir, current_exe};
use std::path::PathBuf;
use std::{collections::HashMap, fs, path::Path, rc::Rc, sync::Arc};

use cranelift_codegen::{
    isa::TargetIsa,
    settings::{self, Configurable},
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::FuncId;
use cranelift_object::ObjectModule;

use crate::compiler::table::SymbolTable;

use crate::args::ARG;

// 编译器结构体
pub struct Compiler {
    module: ObjectModule,

    builder_ctx: FunctionBuilderContext,
    context: cranelift_codegen::Context,

    function_map: HashMap<String, cranelift_module::FuncId>,
    data_map: HashMap<String, cranelift_module::DataId>,

    target_isa: Arc<dyn TargetIsa>,

    table: Rc<RefCell<SymbolTable>>,

    arc_alloc: FuncId,
    arc_retain: FuncId,
    arc_release: FuncId,
}

pub struct CompilerState<'a> {
    pub builder: FunctionBuilder<'a>,
    pub target_isa: Arc<dyn TargetIsa>,
    pub module: &'a mut ObjectModule,
    pub table: Rc<RefCell<SymbolTable>>,
    pub function_map: &'a mut HashMap<String, cranelift_module::FuncId>,
    pub data_map: &'a mut HashMap<String, cranelift_module::DataId>,

    pub arc_alloc: FuncId,
    pub arc_retain: FuncId,
    pub arc_release: FuncId,
}

// 创建目标 ISA 的辅助函数
pub fn create_target_isa() -> Arc<dyn TargetIsa> {
    let mut flag_builder = settings::builder();
    flag_builder.set("opt_level", "speed").unwrap();

    let isa_builder = cranelift_native::builder().unwrap();
    isa_builder
        .finish(settings::Flags::new(flag_builder))
        .unwrap()
}

/// 将对象代码编译为可执行文件
///
/// output_path: 目录 + 文件名 + 后缀  
pub fn compile_to_executable(
    object_code: &[u8],
    output_path: &Path,
) -> Result<(), Box<dyn std::error::Error>> {
    use tempfile;

    // 创建临时对象文件
    let temp_dir = tempfile::tempdir()?;

    // 目录 + 文件名 + .o
    let object_file_path = temp_dir.path().join("output.o");

    // 写入对象代码到临时文件
    fs::write(&object_file_path, object_code)?;

    use cc;

    #[cfg(target_os = "windows")]
    let target = "x86_64-pc-windows-gnu";

    #[cfg(target_os = "linux")]
    let target = "x86_64-unknown-linux-none";

    // 使用 cc crate 链接为 lib
    let mut builder_binding = cc::Build::new();

    let builder = builder_binding
        .object(&object_file_path)
        .opt_level(2)
        .target(target)
        .out_dir(output_path.parent().unwrap_or(Path::new(""))) // 输出目录
        .host("CONSOLE");

    // 去掉后缀 compile 时会自动添加
    builder.try_compile(output_path.file_stem().unwrap().to_str().unwrap())?;

    // 清除临时文件
    fs::remove_file(object_file_path)?;

    let compiler = builder.get_compiler(); // 返回 `cc::Tool`

    let lib_name = format!(
        "lib{}.a",
        output_path.file_stem().unwrap().to_str().unwrap()
    );
    let lib_path = output_path.parent().unwrap().join(&lib_name);

    let compiler_dir = if std::env::var("CARGO").is_ok() {
        current_dir().map_or(".".to_string(), |it| it.display().to_string())
    } else {
        current_exe().map_or(".".to_string(), |it| {
            it.to_path_buf()
                .parent()
                .map_or(".".to_string(), |it| it.display().to_string())
        })
    };
    let mut command = compiler.to_command();
    command
        .arg("-o")
        .arg(output_path)
        .arg(&lib_path)
        .arg("-L")
        .arg(format!("{}/include", compiler_dir,))
        .arg("-l")
        .arg("arc");

    // 添加需要链接的库
    if let Some(it) = unsafe { (*&raw const ARG).clone() } {
        for path in &it.link_with {
            command.arg("-L").arg(
                PathBuf::from(path)
                    .parent()
                    .map_or("./".to_string(), |it| it.to_string_lossy().to_string()),
            );
        }

        for lib in it.link_with {
            if lib.trim().is_empty() {
                continue;
            }
            let lib_name = PathBuf::from(&lib).file_stem().map_or_else(
                || Err(format!("lib {lib} file stem not found")),
                |it| {
                    Ok({
                        let s = it.to_string_lossy().to_string();

                        if s.starts_with("lib") {
                            s.chars()
                                .enumerate()
                                .filter(|(i, _)| *i > 2usize)
                                .map(|it| it.1.to_string())
                                .collect::<Vec<String>>()
                                .join("")
                        } else {
                            s
                        }
                    })
                },
            )?;
            command.arg(format!("-l{lib_name}"));
        }
    }

    // 静态链接
    command.arg("-static");

    // Windows 显式链 msvcrt
    #[cfg(target_os = "windows")]
    command.arg("-lmsvcrt").status().expect("link failed");

    // Linux 显式链 libc
    #[cfg(target_os = "linux")]
    command.arg("-lc").status().expect("link failed");

    // 清除 lib 文件
    fs::remove_file(lib_path)?;

    Ok(())
}

pub fn get_platform_width() -> usize {
    #[cfg(target_pointer_width = "64")]
    return 64;

    #[cfg(target_pointer_width = "32")]
    return 32;

    #[cfg(target_pointer_width = "16")]
    return 16;
}
