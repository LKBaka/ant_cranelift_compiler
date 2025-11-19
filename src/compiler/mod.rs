mod convert_type;
mod imm;
pub mod table;

use std::{collections::HashMap, fs, path::Path, rc::Rc, sync::Arc};

use ant_type_checker::typed_ast::GetType;
use cranelift::codegen::ir::InstBuilder;
use cranelift::prelude::{AbiParam, Signature, Value, types};
use cranelift_codegen::{
    ir::{Function, UserFuncName},
    isa::{CallConv, TargetIsa},
    settings::{self, Configurable},
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{Linkage, Module, default_libcall_names};
use cranelift_object::object::{Object, ObjectSymbol};
use cranelift_object::{ObjectBuilder, ObjectModule};

use ant_type_checker::typed_ast::{
    typed_expr::TypedExpression, typed_node::TypedNode, typed_stmt::TypedStatement,
};

use crate::compiler::{
    convert_type::convert_type_to_cranelift_type, imm::int_value_to_imm, table::SymbolTable,
};

// 编译器结构体
pub struct Compiler<'table> {
    module: ObjectModule,

    builder_ctx: FunctionBuilderContext,
    context: cranelift_codegen::Context,

    function_map: HashMap<String, cranelift_module::FuncId>,
    data_map: HashMap<String, cranelift_module::DataId>,

    target_isa: Arc<dyn TargetIsa>,

    table: &'table mut SymbolTable,
}

pub struct CompilerState<'a> {
    pub builder: FunctionBuilder<'a>,
    pub module: &'a mut ObjectModule,
    pub table: &'a mut SymbolTable,
}

impl<'table> Compiler<'table> {
    pub fn new(
        target_isa: Arc<dyn TargetIsa>,
        file: Rc<str>,
        table: &'table mut SymbolTable,
    ) -> Compiler<'table> {
        // 创建 ObjectModule
        let builder =
            ObjectBuilder::new(target_isa.clone(), file.as_bytes(), default_libcall_names())
                .expect("Failed to create ObjectBuilder");

        let module = ObjectModule::new(builder);

        Self {
            module,
            builder_ctx: FunctionBuilderContext::new(),
            context: cranelift_codegen::Context::new(),
            function_map: HashMap::new(),
            data_map: HashMap::new(),
            target_isa,
            table,
        }
    }

    fn compile_stmt(state: &mut CompilerState, stmt: &TypedStatement) -> Result<Value, String> {
        match stmt {
            TypedStatement::ExpressionStatement(expr) => Self::compile_expr(state, expr),
            TypedStatement::Let {
                name, value, ty, ..
            } => {
                let val = Self::compile_expr(state, value)?;

                let symbol = state.table.define(&name.value);

                let cranelift_ty = convert_type_to_cranelift_type(ty);
                state
                    .builder
                    .declare_var(Variable::from_u32(symbol.index as u32), cranelift_ty);
                state
                    .builder
                    .def_var(Variable::from_u32(symbol.index as u32), val);

                Ok(val)
            }
            _ => todo!("impl function 'compile_stmt'"),
        }
    }

    fn compile_expr(state: &mut CompilerState, expr: &TypedExpression) -> Result<Value, String> {
        match expr {
            TypedExpression::Int { value, ty, .. } => Ok(state
                .builder
                .ins()
                .iconst(convert_type_to_cranelift_type(ty), int_value_to_imm(value))),

            TypedExpression::Ident(it, _) => {
                if let Some(var) = state.table.get(&it.value) {
                    Ok(state.builder.use_var(Variable::from_u32(var.index as u32)))
                } else {
                    Err(format!("undefined variable: {}", it.value))
                }
            }

            TypedExpression::Function {
                name,
                params,
                block: block_ast,
                ret_ty,
                ty,
                ..
            } => {
                let mut converted_params = vec![];

                for param in params {
                    converted_params.push(AbiParam::new(convert_type_to_cranelift_type(
                        &param.get_type(),
                    )));
                }

                let mut ctx = state.module.make_context();
                ctx.func.signature.params.append(&mut converted_params);
                ctx.func
                    .signature
                    .returns
                    .push(AbiParam::new(convert_type_to_cranelift_type(
                        &block_ast.get_type(),
                    )));

                if let Some(name) = name.as_ref() {
                    let name = &name.value;

                    let func_id = match state.module.declare_function(
                        &name,
                        Linkage::Export,
                        &ctx.func.signature,
                    ) {
                        Ok(it) => it,
                        Err(it) => Err(it.to_string())?,
                    };

                    let mut bcx = FunctionBuilderContext::new();
                    let mut bx = FunctionBuilder::new(&mut ctx.func, &mut bcx);
                    let block = bx.create_block();
                    bx.append_block_params_for_function_params(block);
                    bx.switch_to_block(block);
                    
                    for param in params {
                        if let TypedExpression::TypeHint(name, _, ty) = &**param {
                            let symbol = state.table.define(&name.value);

                        let cranelift_ty = convert_type_to_cranelift_type(ty);
                        state
                            .builder
                            .declare_var(Variable::from_u32(symbol.index as u32), cranelift_ty);
                        }
                    }

                    let result = Self::compile_stmt(state, block_ast)?;

                    bx.ins().return_(&[result]);
                    bx.finalize();

                    state.module.define_function(func_id, &mut ctx);
                    state.module.clear_context(&mut ctx);
                }

                todo!()
            }

            TypedExpression::If {
                condition,
                consequence,
                else_block,
                ..
            } => {
                let then_block = state.builder.create_block();
                let end_block = state.builder.create_block();

                state.builder.append_block_param(
                    end_block,
                    convert_type_to_cranelift_type(&consequence.get_type()),
                );

                let else_block_label = match else_block {
                    Some(_) => Some(state.builder.create_block()),
                    None => None,
                };

                let cond_val = Self::compile_expr(state, &condition)?;
                state.builder.ins().brif(
                    cond_val,
                    then_block,
                    &[],
                    if let Some(it) = else_block_label {
                        it
                    } else {
                        end_block
                    },
                    &[],
                );

                state.builder.switch_to_block(then_block);
                let val = Self::compile_expr(state, &consequence)?;
                state.builder.ins().jump(end_block, &[val]);
                state.builder.seal_block(then_block);

                if let Some(else_block_label) = else_block_label {
                    state.builder.switch_to_block(else_block_label);
                    let else_val = Self::compile_expr(state, else_block.as_ref().unwrap())?;
                    state.builder.ins().jump(end_block, &[else_val]);
                    state.builder.seal_block(else_block_label);
                }

                state.builder.switch_to_block(end_block);
                state.builder.seal_block(end_block);
                let end_val = state.builder.block_params(end_block)[0];

                Ok(end_val)
            }

            TypedExpression::Block(it, _) => {
                let mut ret_val = state.builder.ins().iconst(types::I64, 0);

                for stmt in it {
                    ret_val = Self::compile_stmt(state, &stmt)?;
                }

                Ok(ret_val)
            }

            _ => todo!("impl function 'compile_expr'"),
        }
    }

    pub fn compile_program(mut self, program: TypedNode) -> Result<Vec<u8>, String> {
        let statements = match program {
            TypedNode::Program { statements, .. } => statements,
        };

        let mut sig = Signature::new(CallConv::SystemV);
        sig.returns.push(AbiParam::new(types::I64));

        let func_id = self
            .module
            .declare_function("main", Linkage::Export, &sig)
            .map_err(|e| format!("declare main failed: {}", e))?;

        self.context.func = Function::with_name_signature(UserFuncName::user(0, 0), sig);
        {
            let mut builder_ctx = FunctionBuilderContext::new();
            let mut builder = FunctionBuilder::new(&mut self.context.func, &mut builder_ctx);

            let entry = builder.create_block();
            builder.append_block_params_for_function_params(entry);
            builder.switch_to_block(entry);
            builder.seal_block(entry);

            let mut ret_val = builder.ins().iconst(types::I64, 0);

            let mut state = CompilerState {
                builder,
                module: &mut self.module,
                table: self.table,
            };

            for stmt in statements {
                ret_val = Self::compile_stmt(&mut state, &stmt)?;
            }

            state.builder.ins().return_(&[ret_val]);
            state.builder.finalize();
        }

        self.module
            .define_function(func_id, &mut self.context)
            .map_err(|e| format!("define main failed: {}", e))?;
        self.context.clear();

        let obj = self.module.finish();
        Ok(obj.emit().unwrap().to_vec())
    }
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

#[cfg(test)]
mod tests {
    use std::path::Path;

    use ant_lexer::Lexer;
    use ant_parser::Parser;

    use ant_type_checker::{TypeChecker, table::TypeTable};

    use crate::compiler::{Compiler, compile_to_executable, create_target_isa, table::SymbolTable};

    #[test]
    fn simple_program() {
        let file: std::rc::Rc<str> = "__simple_program__".into();

        // 创建目标 ISA
        let target_isa = create_target_isa();

        // 创建编译器实例
        let mut table = SymbolTable::new();

        let compiler = Compiler::new(target_isa, "__simple_program__".into(), &mut table);

        // 解析ast
        let tokens = (&mut Lexer::new(
            "if 0i64 {42i64} else if 1i64 {42i64} else {0i64}".into(),
            file.clone(),
        ))
            .get_tokens();

        let node = (&mut Parser::new(tokens)).parse_program().unwrap();

        let typed_node = (&mut TypeChecker::new(&mut TypeTable::new()))
            .check_node(node)
            .unwrap();

        // 编译程序
        match compiler.compile_program(typed_node) {
            Ok(object_code) => {
                println!(
                    "Compilation successful! Object code size: {} bytes",
                    object_code.len()
                );

                // 编译到可执行文件
                compile_to_executable(&object_code, Path::new("test_program.exe")).unwrap();
            }
            Err(e) => {
                eprintln!("Compilation failed: {}", e);
            }
        }
    }
}

/// 将对象代码编译为可执行文件
pub fn compile_to_executable(
    object_code: &[u8],
    output_path: &Path,
) -> Result<(), Box<dyn std::error::Error>> {
    use tempfile;

    // 创建临时对象文件
    let temp_dir = tempfile::tempdir()?;
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
        .out_dir(output_path.parent().unwrap_or(Path::new("")))
        .host("CONSOLE");

    builder.try_compile(output_path.file_stem().unwrap().to_str().unwrap())?;

    // 清除临时文件
    fs::remove_file(object_file_path)?;

    let compiler = builder.get_compiler(); // 返回 `cc::Tool`

    let lib_name = format!(
        "lib{}.a",
        output_path.file_stem().unwrap().to_str().unwrap()
    );

    compiler
        .to_command()
        .arg("-o")
        .arg(output_path.to_str().unwrap()) // 输出文件名
        .arg(&lib_name) // 刚才 Cranelift 生成的 .o
        .status()
        .expect("link failed");

    // 清除 lib 文件
    fs::remove_file(lib_name)?;

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
