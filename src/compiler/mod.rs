pub mod arc;
pub mod compile_state_impl;
pub mod compiler_impl;
pub mod handler;
pub mod table;

mod constants;
mod convert_type;
mod imm;


use std::cell::RefCell;
use std::env::{current_dir, current_exe};
use std::path::PathBuf;
use std::sync::Mutex;
use std::{collections::HashMap, fs, path::Path, rc::Rc, sync::Arc};

use ant_type_checker::table::TypeTable;
use cranelift_codegen::{
    isa::TargetIsa,
    settings::{self, Configurable},
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::FuncId;
use cranelift_object::ObjectModule;

use crate::compiler::table::SymbolTable;

use crate::args::read_arg;

// 编译器结构体
pub struct Compiler {
    module: ObjectModule,

    builder_ctx: FunctionBuilderContext,
    context: cranelift_codegen::Context,

    function_map: HashMap<String, cranelift_module::FuncId>,
    data_map: HashMap<String, cranelift_module::DataId>,

    target_isa: Arc<dyn TargetIsa>,

    table: Rc<RefCell<SymbolTable>>,
    type_table: Arc<Mutex<TypeTable>>,

    arc_alloc: FuncId,
    arc_retain: FuncId,
    arc_release: FuncId,
}

pub struct GlobalState<'a> {
    pub target_isa: Arc<dyn TargetIsa>,
    pub module: &'a mut ObjectModule,
    pub function_map: &'a mut HashMap<String, cranelift_module::FuncId>,
    pub data_map: &'a mut HashMap<String, cranelift_module::DataId>,
    
    pub table: Rc<RefCell<SymbolTable>>,
    pub type_table: Arc<Mutex<TypeTable>>,

    pub arc_alloc: FuncId,
    pub arc_retain: FuncId,
    pub arc_release: FuncId,
}

pub struct FunctionState<'a> {
    pub builder: FunctionBuilder<'a>,
    pub target_isa: Arc<dyn TargetIsa>,
    pub module: &'a mut ObjectModule,
    pub function_map: &'a mut HashMap<String, cranelift_module::FuncId>,
    pub data_map: &'a mut HashMap<String, cranelift_module::DataId>,
    
    pub table: Rc<RefCell<SymbolTable>>,
    pub type_table: Arc<Mutex<TypeTable>>,

    pub arc_alloc: FuncId,
    pub arc_retain: FuncId,
    pub arc_release: FuncId,
}

#[allow(unused)]
pub trait CompileState {
    fn get_target_isa(&self) -> Arc<dyn TargetIsa>;
    fn get_module(&mut self) -> &mut ObjectModule;
    fn get_function_map(&mut self) -> &mut HashMap<String, cranelift_module::FuncId>;
    fn get_data_map(&mut self) -> &mut HashMap<String, cranelift_module::DataId>;
    
    fn get_table(&self) -> Rc<RefCell<SymbolTable>>;
    fn get_type_table(&self) -> Arc<Mutex<TypeTable>>;

    fn get_arc_alloc(&self) -> FuncId;
    fn get_arc_retain(&self) -> FuncId;
    fn get_arc_release(&self) -> FuncId;
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
    use cc;
    use tempfile;

    // 临时 .o
    let temp_dir = tempfile::tempdir()?;
    let object_file_path = temp_dir.path().join("output.o");
    fs::write(&object_file_path, object_code)?;

    // -------- target triple --------
    #[cfg(target_os = "windows")]
    let target = "x86_64-pc-windows-gnu";

    #[cfg(target_os = "linux")]
    let target = "x86_64-unknown-linux-gnu";

    #[cfg(target_os = "macos")]
    let target = if cfg!(target_arch = "aarch64") {
        "aarch64-apple-darwin"
    } else {
        "x86_64-apple-darwin"
    };

    // -------- 先用 cc 生成 libxxx.a --------
    let mut build = cc::Build::new();
    build
        .object(&object_file_path)
        .opt_level(2)
        .target(target)
        .host("CONSOLE")
        .out_dir(output_path.parent().unwrap_or(Path::new("")));

    build.try_compile(output_path.file_stem().unwrap().to_str().unwrap())?;

    let compiler = build.get_compiler();

    let lib_name = format!(
        "lib{}.a",
        output_path.file_stem().unwrap().to_str().unwrap()
    );
    let lib_path = output_path.parent().unwrap().join(&lib_name);

    // -------- 最终链接 --------
    let compiler_dir = if std::env::var("CARGO").is_ok() {
        current_dir().map_or(".".into(), |p| p.display().to_string())
    } else {
        current_exe().map_or(".".into(), |p| {
            p.parent().map_or(".".into(), |it| it.display().to_string())
        })
    };

    let mut command = compiler.to_command();
    command
        .arg("-o")
        .arg(output_path)
        .arg(&lib_path)
        .arg("-L")
        .arg(format!("{}/include", compiler_dir))
        .arg("-l")
        .arg("arc");

    // 用户额外链接库
    if let Some(it) = read_arg() {
        for path in &it.link_with {
            command.arg("-L").arg(
                PathBuf::from(path)
                    .parent()
                    .map_or("./".to_string(), |p| p.to_string_lossy().to_string()),
            );
        }

        for lib in it.link_with {
            if lib.trim().is_empty() {
                continue;
            }

            let stem = PathBuf::from(&lib)
                .file_stem()
                .ok_or("invalid lib name")?
                .to_string_lossy()
                .to_string();

            let name = stem.strip_prefix("lib").unwrap_or(&stem);
            command.arg(format!("-l{name}"));
        }
    }

    // Linux: 静态 + libc + noexecstack
    #[cfg(target_os = "linux")]
    {
        command.arg("-static").arg("-lc").arg("-Wl,-z,noexecstack");
    }

    // Windows
    #[cfg(target_os = "windows")]
    {
        command.arg("-static").arg("-lmsvcrt");
    }

    // macOS: 不要 static / 不要 -lc（clang 自动处理）

    command.status().expect("link failed");

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

impl CompileState for GlobalState<'_> {
    fn get_target_isa(&self) -> Arc<dyn TargetIsa> {
        self.target_isa.clone()
    }

    fn get_module(&mut self) -> &mut ObjectModule {
        self.module
    }

    fn get_function_map(&mut self) -> &mut HashMap<String, cranelift_module::FuncId> {
        self.function_map
    }

    fn get_data_map(&mut self) -> &mut HashMap<String, cranelift_module::DataId> {
        self.data_map
    }

    fn get_table(&self) -> Rc<RefCell<SymbolTable>> {
        self.table.clone()
    }

    fn get_type_table(&self) -> Arc<Mutex<TypeTable>> {
        self.type_table.clone()
    }

    fn get_arc_alloc(&self) -> FuncId {
        self.arc_alloc
    }

    fn get_arc_retain(&self) -> FuncId {
        self.arc_retain
    }

    fn get_arc_release(&self) -> FuncId {
        self.arc_release
    }
}

impl CompileState for FunctionState<'_> {
    fn get_target_isa(&self) -> Arc<dyn TargetIsa> {
        self.target_isa.clone()
    }

    fn get_module(&mut self) -> &mut ObjectModule {
        self.module
    }

    fn get_function_map(&mut self) -> &mut HashMap<String, cranelift_module::FuncId> {
        self.function_map
    }

    fn get_data_map(&mut self) -> &mut HashMap<String, cranelift_module::DataId> {
        self.data_map
    }

    fn get_table(&self) -> Rc<RefCell<SymbolTable>> {
        self.table.clone()
    }

    fn get_type_table(&self) -> Arc<Mutex<TypeTable>> {
        self.type_table.clone()
    }

    fn get_arc_alloc(&self) -> FuncId {
        self.arc_alloc
    }

    fn get_arc_retain(&self) -> FuncId {
        self.arc_retain
    }

    fn get_arc_release(&self) -> FuncId {
        self.arc_release
    }
}
