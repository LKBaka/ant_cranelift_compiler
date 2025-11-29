mod convert_type;
pub mod handler;
mod imm;
pub mod table;

use std::cell::RefCell;
use std::{collections::HashMap, fs, path::Path, rc::Rc, sync::Arc};

use ant_type_checker::ty::Ty;
use ant_type_checker::typed_ast::GetType;
use cranelift::codegen::ir::InstBuilder;
use cranelift::prelude::{AbiParam, Signature, Value, types};
use cranelift_codegen::ir::FuncRef;
use cranelift_codegen::{
    ir::{Function, UserFuncName},
    isa::{CallConv, TargetIsa},
    settings::{self, Configurable},
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{Linkage, Module, default_libcall_names};
use cranelift_object::{ObjectBuilder, ObjectModule};

use ant_type_checker::typed_ast::{
    typed_expr::TypedExpression, typed_node::TypedNode, typed_stmt::TypedStatement,
};

use crate::compiler::handler::compile_infix::compile_infix;
use crate::compiler::imm::platform_width_to_int_type;
use crate::compiler::{
    convert_type::convert_type_to_cranelift_type, imm::int_value_to_imm, table::SymbolTable,
};

// 编译器结构体
pub struct Compiler {
    module: ObjectModule,

    builder_ctx: FunctionBuilderContext,
    context: cranelift_codegen::Context,

    function_map: HashMap<String, cranelift_module::FuncId>,
    data_map: HashMap<String, cranelift_module::DataId>,

    target_isa: Arc<dyn TargetIsa>,

    table: Rc<RefCell<SymbolTable>>,
}

pub struct CompilerState<'a> {
    pub builder: FunctionBuilder<'a>,
    pub module: &'a mut ObjectModule,
    pub table: Rc<RefCell<SymbolTable>>,
    pub function_map: &'a mut HashMap<String, cranelift_module::FuncId>,
    pub data_map: &'a mut HashMap<String, cranelift_module::DataId>,
}

impl Compiler {
    pub fn new(
        target_isa: Arc<dyn TargetIsa>,
        file: Rc<str>,
        table: Rc<RefCell<SymbolTable>>,
    ) -> Compiler {
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

                let symbol = state.table.borrow_mut().define(&name.value);

                let cranelift_ty = convert_type_to_cranelift_type(ty);
                state
                    .builder
                    .declare_var(Variable::from_u32(symbol.index as u32), cranelift_ty);
                state
                    .builder
                    .def_var(Variable::from_u32(symbol.index as u32), val);

                return Ok(state.builder.ins().iconst(types::I64, 0)); // unit
            }

            TypedStatement::Block { statements: it, .. } => {
                let mut ret_val = state.builder.ins().iconst(types::I64, 0);

                for stmt in it {
                    ret_val = Self::compile_stmt(state, &stmt)?;
                }

                Ok(ret_val)
            }

            TypedStatement::While {
                condition, block, ..
            } => {
                let head = state.builder.create_block(); // while 头
                let body = state.builder.create_block(); // 循环体
                let exit = state.builder.create_block(); // 退出

                state.builder.ins().jump(head, &[]);

                state.builder.switch_to_block(head);
                let condition_val = Self::compile_expr(state, condition)?;
                state
                    .builder
                    .ins()
                    .brif(condition_val, body, &[], exit, &[]);

                state.builder.switch_to_block(body);
                let _body_val = Self::compile_stmt(state, &block.as_ref())?;
                state.builder.ins().jump(head, &[]);

                state.builder.seal_block(body);
                state.builder.seal_block(head);

                state.builder.switch_to_block(exit);
                state.builder.seal_block(exit);

                // Return a default value for the while loop expression
                Ok(state.builder.ins().iconst(types::I64, 0))
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

            TypedExpression::Bool { value, ty, .. } => Ok(state
                .builder
                .ins()
                .iconst(convert_type_to_cranelift_type(ty), *value as i64)),

            TypedExpression::Ident(it, _) => {
                if let Some(var) = state.table.borrow().get(&it.value) {
                    Ok(state.builder.use_var(Variable::from_u32(var.index as u32)))
                } else {
                    Err(format!("undefined variable: {}", it.value))
                }
            }

            TypedExpression::StrLiteral { value, .. } => {
                let content = value.to_string() + "\0";

                let data_id = *state.data_map.entry(content.clone()).or_insert_with(|| {
                    let name = format!("str_{}", content.len());
                    let id = state
                        .module
                        .declare_data(&name, Linkage::Local, true, false)
                        .unwrap();
                    let mut desc = cranelift_module::DataDescription::new();

                    // 使用 Init::Bytes
                    desc.init = cranelift_module::Init::Bytes {
                        contents: content.into_bytes().into_boxed_slice(),
                    };
                    state.module.define_data(id, &desc).unwrap();
                    id
                });

                let gv = state
                    .module
                    .declare_data_in_func(data_id, &mut state.builder.func);
                Ok(state
                    .builder
                    .ins()
                    .global_value(platform_width_to_int_type(), gv))
            }

            TypedExpression::Assign { left, right, .. } => {
                let TypedExpression::Ident(ident, _) = &**left else {
                    return Err("assign target must be ident".into());
                };

                let new_val = Self::compile_expr(state, &right)?;

                let var_symbol = state
                    .table
                    .borrow()
                    .get(&ident.value)
                    .ok_or_else(|| format!("undefined variable `{}`", ident.value))?;

                state
                    .builder
                    .def_var(Variable::from_u32(var_symbol.index as u32), new_val);

                Ok(state.builder.ins().iconst(types::I64, 0)) // unit
            }

            TypedExpression::Function {
                name,
                params,
                block: block_ast,
                ..
            } => {
                let mut converted_params = vec![];

                for param in params {
                    converted_params.push(AbiParam::new(convert_type_to_cranelift_type(
                        &param.get_type(),
                    )));
                }

                let mut ctx = state.module.make_context();
                ctx.func.signature = Signature::new(CallConv::SystemV);
                ctx.func.signature.params.append(&mut converted_params);

                if block_ast.get_type() != Ty::Unit {
                    ctx.func
                        .signature
                        .returns
                        .push(AbiParam::new(convert_type_to_cranelift_type(
                            &block_ast.get_type(),
                        )));
                }

                if let Some(name) = name.as_ref() {
                    let name = &name.value;

                    // 1. 首先声明函数
                    let func_id = match state.module.declare_function(
                        &name,
                        Linkage::Export,
                        &ctx.func.signature,
                    ) {
                        Ok(it) => it,
                        Err(it) => Err(it.to_string())?,
                    };

                    // 2. 立即将函数ID注册到function_map中
                    state.function_map.insert(name.to_string(), func_id);

                    // 3. 在编译函数体之前就获取FuncRef
                    let func_ref = state
                        .module
                        .declare_func_in_func(func_id, &mut state.builder.func);

                    let ref_val = state
                        .builder
                        .ins()
                        .func_addr(platform_width_to_int_type(), func_ref);

                    // 4. 定义外部作用域的符号
                    let func_symbol = state.table.borrow_mut().define(&name);
                    state.builder.declare_var(
                        Variable::from_u32(func_symbol.index as u32),
                        platform_width_to_int_type(),
                    );
                    state.builder.def_var(
                        Variable::from_u32(func_symbol.index as u32),
                        ref_val.clone(),
                    );

                    // 5. 创建新的编译上下文
                    let mut func_builder_ctx = FunctionBuilderContext::new();
                    let mut func_builder =
                        FunctionBuilder::new(&mut ctx.func, &mut func_builder_ctx);

                    let entry_block = func_builder.create_block();
                    func_builder.append_block_params_for_function_params(entry_block);
                    func_builder.switch_to_block(entry_block);
                    func_builder.seal_block(entry_block);

                    // 6. 创建函数内部的符号表
                    let func_symbol_table =
                        Rc::new(RefCell::new(SymbolTable::from_outer(state.table.clone())));

                    // 7. 在函数内部符号表中也定义这个函数 - 修正这里！
                    let inner_func_symbol = func_symbol_table.borrow_mut().define(&name);
                    func_builder.declare_var(
                        Variable::from_u32(inner_func_symbol.index as u32),
                        platform_width_to_int_type(),
                    );

                    // 在函数内部重新创建FuncRef和对应的Value
                    let inner_func_ref = state
                        .module
                        .declare_func_in_func(func_id, &mut func_builder.func);
                    let inner_ref_val = func_builder
                        .ins()
                        .func_addr(platform_width_to_int_type(), inner_func_ref);
                    func_builder.def_var(
                        Variable::from_u32(inner_func_symbol.index as u32),
                        inner_ref_val, // 使用内部创建的Value
                    );

                    // 8. 声明参数变量
                    for (i, param) in params.iter().enumerate() {
                        if let TypedExpression::TypeHint(param_name, _, ty) = &**param {
                            let symbol = func_symbol_table.borrow_mut().define(&param_name.value);
                            let cranelift_ty = convert_type_to_cranelift_type(ty);

                            func_builder
                                .declare_var(Variable::from_u32(symbol.index as u32), cranelift_ty);

                            let param_value = func_builder.block_params(entry_block)[i];
                            func_builder
                                .def_var(Variable::from_u32(symbol.index as u32), param_value);
                        }
                    }

                    // 9. 编译函数体
                    let mut func_state = CompilerState {
                        builder: func_builder,
                        module: state.module,
                        table: func_symbol_table,
                        function_map: state.function_map,
                        data_map: state.data_map,
                    };

                    let result = Self::compile_stmt(&mut func_state, block_ast)?;

                    if block_ast.get_type() != Ty::Unit {
                        func_state.builder.ins().return_(&[result]);
                    } else {
                        func_state.builder.ins().return_(&[]);
                    }

                    func_state.builder.finalize();

                    state
                        .module
                        .define_function(func_id, &mut ctx)
                        .map_or_else(|err| Err(err.to_string()), |_| Ok(()))?;
                    state.module.clear_context(&mut ctx);

                    return Ok(ref_val);
                }

                todo!()
            }

            TypedExpression::Call {
                func,
                args,
                func_ty,
                ..
            } => {
                let (params_type, ret_ty) = match func_ty {
                    Ty::Function {
                        params_type,
                        ret_type,
                    } => (params_type, ret_type),
                    _ => unreachable!(),
                };

                let direct_func: Option<FuncRef> = if let TypedExpression::Ident(ident, _) = &**func
                {
                    state
                        .function_map
                        .get(&ident.value.to_string())
                        .copied()
                        .map(|fid| {
                            state
                                .module
                                .declare_func_in_func(fid, &mut state.builder.func)
                        })
                } else {
                    None
                };

                let func_val = Self::compile_expr(state, &func)?;

                // 编译所有参数
                let mut arg_values = Vec::new();

                for arg in args {
                    let arg_val = Self::compile_expr(state, arg)?;
                    arg_values.push(arg_val);
                }

                if let Some(fref) = direct_func {
                    // 直接 call
                    let call = state.builder.ins().call(fref, &arg_values);
                    return Ok(state
                        .builder
                        .inst_results(call)
                        .first()
                        .copied()
                        .unwrap_or_else(|| {
                            state.builder.ins().iconst(platform_width_to_int_type(), 0)
                        }));
                }

                // 创建函数签名
                let mut sig = Signature::new(CallConv::SystemV);

                for param_ty in params_type {
                    sig.params
                        .push(AbiParam::new(convert_type_to_cranelift_type(param_ty)));
                }

                if **ret_ty != Ty::Unit {
                    sig.returns
                        .push(AbiParam::new(convert_type_to_cranelift_type(ret_ty)));
                }

                // 导入签名
                let sig_ref = state.builder.import_signature(sig);

                // 生成间接调用指令
                let call_inst = state
                    .builder
                    .ins()
                    .call_indirect(sig_ref, func_val, &arg_values);

                let results = state.builder.inst_results(call_inst);
                if results.is_empty() {
                    Ok(state.builder.ins().iconst(platform_width_to_int_type(), 0))
                } else {
                    Ok(results[0])
                }
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

            TypedExpression::Infix {
                op, left, right, ..
            } => compile_infix(state, op.clone(), left, right),
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
                function_map: &mut self.function_map,
                data_map: &mut self.data_map,
            };

            #[cfg(windows)]
            let call_conv = CallConv::WindowsFastcall;

            #[cfg(target_os = "linux")]
            let call_conv = CallConv::SystemV;

            // 初始化 __cputs
            {
                // 构造 __cputs 签名:  i32 __cputs(i8*, ...)
                let mut puts_sig = Signature::new(call_conv);
                puts_sig
                    .params
                    .push(AbiParam::new(platform_width_to_int_type())); // 第一个参数 format: i8*
                puts_sig.returns.push(AbiParam::new(types::I32));

                let printf_id = state
                    .module
                    .declare_function("puts", Linkage::Import, &puts_sig)
                    .map_err(|e| format!("declare puts failed: {}", e))?;

                // 放进 function_map，方便后面 call
                state.function_map.insert("__cputs".into(), printf_id);

                // 登记进符号表后，立刻 declare
                let func_symbol = state.table.borrow_mut().define("__cputs");
                state.builder.declare_var(
                    Variable::from_u32(func_symbol.index as u32),
                    platform_width_to_int_type(), // 函数指针类型
                );
            }

            for stmt in statements {
                ret_val = Self::compile_stmt(&mut state, &stmt)?;
            }

            state.builder.ins().return_(&[ret_val]);

            #[cfg(debug_assertions)]
            {
                let func_ref = &state.builder.func;
                eprintln!("=== before finalize:\n{}", {
                    let mut s = String::new();
                    cranelift::codegen::write_function(&mut s, func_ref).unwrap();
                    s
                });
            }

            state.builder.finalize();
        }

        {
            match cranelift_codegen::verify_function(&self.context.func, &*self.target_isa) {
                Ok(_) => {}
                Err(errors) => {
                    let mut msg = String::new();
                    for e in errors.0.iter() {
                        use std::fmt::Write;
                        writeln!(msg, "verifier: {}", e).unwrap();
                    }
                    return Err(format!("verifier errors:\n{}", msg));
                }
            }
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
    use std::{cell::RefCell, path::Path, rc::Rc};

    use ant_lexer::Lexer;
    use ant_parser::Parser;

    use ant_type_checker::{
        TypeChecker,
        table::TypeTable,
        ty::{IntTy, Ty},
    };

    use crate::compiler::{Compiler, compile_to_executable, create_target_isa, table::SymbolTable};

    #[test]
    fn simple_program() {
        let file: std::rc::Rc<str> = "__simple_program__".into();

        // 创建目标 ISA
        let target_isa = create_target_isa();

        // 创建编译器实例
        let table = SymbolTable::new();

        let compiler = Compiler::new(
            target_isa,
            "__simple_program__".into(),
            Rc::new(RefCell::new(table)),
        );

        // 解析ast
        let tokens = (&mut Lexer::new(
            // "if 0i64 {42i64} else if 1i64 {42i64} else {0i64}".into(),
            // "1i64 + 2i64 * 3i64 - 4i64".into(),
            "__cputs(\"hello, world!\n\"); 0i64".into(),
            file.clone(),
        ))
            .get_tokens();

        let node = (&mut Parser::new(tokens)).parse_program().unwrap();

        let typed_node = (&mut TypeChecker::new(Rc::new(RefCell::new({
            let mut table = TypeTable::new();
            table.define_var(
                "__cputs",
                Ty::Function {
                    params_type: vec![Ty::Str],
                    ret_type: Box::new(Ty::IntTy(IntTy::I32)),
                },
            );

            table
        }))))
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
                panic!("Compilation failed: {}", e);
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

    let mut command_binding = compiler.to_command();

    let command = command_binding
        .arg("-o")
        .arg(output_path.to_str().unwrap()) // 输出文件名
        .arg(output_path.parent().unwrap().join(&lib_name)); // 刚才 Cranelift 生成的 .o

    // Windows 显式链 msvcrt
    #[cfg(target_os = "windows")]
    command.arg("-lmsvcrt").status().expect("link failed");

    // Linux 显式链 libc
    #[cfg(target_os = "linux")]
    command.arg("-lc").status().expect("link failed");

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
