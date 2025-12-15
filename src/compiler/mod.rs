mod convert_type;
pub mod handler;
mod imm;
pub mod table;

use std::cell::RefCell;
use std::path::PathBuf;
use std::{collections::HashMap, fs, path::Path, rc::Rc, sync::Arc};

use ant_type_checker::ty::Ty;
use ant_type_checker::typed_ast::GetType;
use cranelift::codegen::ir::InstBuilder;
use cranelift::prelude::{
    AbiParam, MemFlags, Signature, StackSlotData, StackSlotKind, Value, types,
};
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
use crate::compiler::table::{StructLayout, SymbolTy};
use crate::compiler::{
    convert_type::convert_type_to_cranelift_type, imm::int_value_to_imm, table::SymbolTable,
};

use crate::args::ARG;

#[cfg(windows)]
const CALL_CONV: CallConv = CallConv::WindowsFastcall;

#[cfg(target_os = "linux")]
const CALL_CONV: CallConv = CallConv::SystemV;

#[cfg(target_os = "macos")]
const CALL_CONV: CallConv = CallConv::AppleAarch64;

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
    pub target_isa: Arc<dyn TargetIsa>,
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

    /// 在编译阶段计算 struct 布局（目标平台相关）
    fn compile_struct_layout(
        state: &mut CompilerState,
        fields: &[(Rc<str>, Ty)],
    ) -> Result<StructLayout, String> {
        let pointer_width = state.target_isa.pointer_bytes() as u32;

        let mut offsets = Vec::with_capacity(fields.len());
        let mut current_offset = 0u32;
        let mut max_align = 1u32;

        for (_, ty) in fields {
            let field_align = Self::get_type_align(state, ty, pointer_width)?;
            let field_size = Self::get_type_size(state, ty, pointer_width)?;

            // 对齐当前偏移
            if current_offset % field_align != 0 {
                current_offset += field_align - (current_offset % field_align);
            }

            offsets.push(current_offset);
            current_offset += field_size;
            max_align = max_align.max(field_align);
        }

        // 对齐总大小
        let size = if current_offset % max_align != 0 {
            current_offset + max_align - (current_offset % max_align)
        } else {
            current_offset
        };

        Ok(StructLayout {
            fields: fields.to_vec(),
            offsets,
            size,
            align: max_align,
        })
    }

    fn get_type_size(
        state: &mut CompilerState,
        ty: &Ty,
        pointer_width: u32,
    ) -> Result<u32, String> {
        match ty {
            Ty::IntTy(it) => Ok(it.get_bytes_size() as u32),
            Ty::Bool => Ok(1),
            Ty::Str => Ok(pointer_width),
            Ty::Struct { name, .. } => {
                let SymbolTy::Struct(layout) = state.table.borrow().get(name).map_or_else(
                    || Err(format!("undefine struct: {name}")),
                    |it| Ok(it.symbol_ty),
                )?
                else {
                    Err(format!("not a struct: {name}"))?
                };

                Ok(layout.align)
            }
            _ => todo!(),
        }
    }

    fn get_type_align(
        state: &mut CompilerState,
        ty: &Ty,
        pointer_width: u32,
    ) -> Result<u32, String> {
        match ty {
            Ty::IntTy(it) => Ok(it.get_bytes_size() as u32),
            Ty::Bool => Ok(1),
            Ty::Str => Ok(pointer_width),
            Ty::Struct { name, .. } => {
                let SymbolTy::Struct(layout) = state.table.borrow().get(name).map_or_else(
                    || Err(format!("undefine struct: {name}")),
                    |it| Ok(it.symbol_ty),
                )?
                else {
                    Err(format!("not a struct: {name}"))?
                };

                Ok(layout.align)
            }
            _ => todo!(),
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

                // unit
                Ok(state.builder.ins().iconst(types::I64, 0))
            }

            TypedStatement::Struct { ty, .. } => {
                // 从 Type 中提取字段定义
                let Ty::Struct { name, fields } = ty else {
                    return Err(format!("not a struct"));
                };

                let layout = Self::compile_struct_layout(
                    state,
                    &fields
                        .iter()
                        .map(|(name, val_ty)| (name.clone(), val_ty.clone()))
                        .collect::<Vec<(Rc<str>, Ty)>>(),
                )?;

                state.table.borrow_mut().define_struct(name, layout);

                // unit
                Ok(state.builder.ins().iconst(types::I64, 0))
            }

            TypedStatement::Extern {
                abi,
                extern_func_name,
                alias,
                ty,
                ..
            } => {
                // 检查 abi (目前只支持c)
                if abi.value.as_ref() != "C" {
                    return Err(format!("unsupported abi: {}", abi.value));
                }

                let Ty::Function {
                    params_type,
                    ret_type,
                    ..
                } = ty
                else {
                    return Err(format!("not a function: {ty}"));
                };

                let mut cranelift_params = params_type
                    .iter()
                    .map(|it| AbiParam::new(convert_type_to_cranelift_type(it)))
                    .collect::<Vec<_>>();

                // 构造签名
                let mut extern_func_sig = Signature::new(CALL_CONV);

                extern_func_sig.params.append(&mut cranelift_params);

                extern_func_sig
                    .returns
                    .push(AbiParam::new(convert_type_to_cranelift_type(&ret_type)));

                let extern_func_id = state
                    .module
                    .declare_function(&extern_func_name.value, Linkage::Import, &extern_func_sig)
                    .map_err(|e| format!("declare {extern_func_name} failed: {}", e))?;

                // 放进 function_map，方便后面 call
                state
                    .function_map
                    .insert(alias.value.to_string(), extern_func_id);

                // 登记进符号表后，立刻 declare
                let func_symbol = state.table.borrow_mut().define(&alias.value);
                state.builder.declare_var(
                    Variable::from_u32(func_symbol.index as u32),
                    platform_width_to_int_type(), // 函数指针类型
                );

                let func_ref = state
                    .module
                    .declare_func_in_func(extern_func_id, &mut state.builder.func);

                let func_addr_val = state
                    .builder
                    .ins()
                    .func_addr(platform_width_to_int_type(), func_ref);

                state
                    .builder
                    .def_var(Variable::from_u32(func_symbol.index as u32), func_addr_val);

                // unit
                Ok(state.builder.ins().iconst(platform_width_to_int_type(), 0))
            }

            stmt => todo!("impl function 'compile_stmt' {stmt}"),
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

            TypedExpression::FieldAccess(obj, field, _) => {
                // 编译对象表达式
                let obj_ptr = Self::compile_expr(state, &obj)?;

                // 获取对象类型，确保是 struct
                let obj_ty = obj.get_type();
                let Ty::Struct { name, .. } = &obj_ty else {
                    return Err("field access on non-struct type".into());
                };

                // 从符号表获取结构体布局
                let SymbolTy::Struct(layout) = state
                    .table
                    .borrow()
                    .get(name)
                    .ok_or_else(|| format!("undefined struct: {}", name))?
                    .symbol_ty
                else {
                    Err(format!("not a struct type"))?
                };

                // 查找字段索引
                let field_idx = layout
                    .fields
                    .iter()
                    .position(|(n, _)| n == &field.value)
                    .ok_or_else(|| format!("field '{}' not found in struct '{}'", field, name))?; // 类型检查已保证存在，这里只是安全断言

                let offset = layout.offsets[field_idx];

                // 计算字段地址
                let field_ptr = if offset == 0 {
                    obj_ptr
                } else {
                    state.builder.ins().iadd_imm(obj_ptr, offset as i64)
                };

                // 加载字段值
                let field_ty = &layout.fields[field_idx].1;
                let cranelift_ty = convert_type_to_cranelift_type(field_ty);
                Ok(state
                    .builder
                    .ins()
                    .load(cranelift_ty, MemFlags::new(), field_ptr, 0))
            }

            TypedExpression::BuildStruct(struct_name, fields, _) => {
                let SymbolTy::Struct(layout) =
                    state.table.borrow().get(&struct_name.value).map_or_else(
                        || Err(format!("undefined struct: {struct_name}")),
                        |it| Ok(it.symbol_ty),
                    )?
                else {
                    Err(format!("not a struct: {struct_name}"))?
                };

                // 分配内存
                let stack_slot = state.builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: layout.size,
                    align_shift: layout.align.trailing_zeros() as u8,
                });
                let struct_ptr =
                    state
                        .builder
                        .ins()
                        .stack_addr(platform_width_to_int_type(), stack_slot, 0);

                // 构造结构体
                for (field_name, field_expr) in fields {
                    let field_idx = layout
                        .fields
                        .iter()
                        .position(|(n, _)| n == &field_name.value)
                        .unwrap(); // 类型检查保证存在

                    let offset = layout.offsets[field_idx];
                    let field_ptr = if offset == 0 {
                        struct_ptr
                    } else {
                        state.builder.ins().iadd_imm(struct_ptr, offset as i64)
                    };

                    // 编译字段值
                    let field_val = Self::compile_expr(state, field_expr)?;
                    state
                        .builder
                        .ins()
                        .store(MemFlags::new(), field_val, field_ptr, 0);
                }

                Ok(struct_ptr)
            }

            TypedExpression::Assign { left, right, .. } => {
                let TypedExpression::Ident(ident, _) = &**left else {
                    return Err("assign target must be ident".into());
                };

                if left.get_type() != right.get_type() {
                    return Err(format!(
                        "expected: {}, got: {}",
                        left.get_type(),
                        right.get_type()
                    ));
                }

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
                        target_isa: state.target_isa.clone(),
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
                let (params_type, ret_ty, va_arg) = match func_ty {
                    Ty::Function {
                        params_type,
                        ret_type,
                        is_variadic,
                    } => (params_type, ret_type, is_variadic),
                    _ => unreachable!(),
                };

                let func_id = if let TypedExpression::Ident(ident, _) = &**func {
                    state.function_map.get(&ident.value.to_string()).copied()
                } else {
                    None
                };

                let direct_func: Option<FuncRef> = func_id.map(|fid| {
                    state
                        .module
                        .declare_func_in_func(fid, &mut state.builder.func)
                });

                let func_val = Self::compile_expr(state, &func)?;

                // 编译所有参数
                let mut arg_values = Vec::new();

                for arg in args {
                    let arg_val = Self::compile_expr(state, arg)?;
                    arg_values.push(arg_val);
                }

                if let Some(fref) = direct_func
                    && !*va_arg
                {
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
                let mut sig = Signature::new(CALL_CONV);

                if *va_arg {
                    for arg in args {
                        sig.params
                            .push(AbiParam::new(convert_type_to_cranelift_type(
                                &arg.get_type(),
                            )));
                    }
                } else {
                    for param_ty in params_type {
                        sig.params
                            .push(AbiParam::new(convert_type_to_cranelift_type(param_ty)));
                    }
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
                target_isa: self.target_isa.clone(),
                module: &mut self.module,
                table: self.table,
                function_map: &mut self.function_map,
                data_map: &mut self.data_map,
            };

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

    use ant_type_checker::{TypeChecker, table::TypeTable};

    use crate::{
        compiler::{Compiler, compile_to_executable, create_target_isa, table::SymbolTable},
        monomorphizer::Monomorphizer,
    };

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
            r#"
            extern "C" func printf(s: str, ...) -> i64;

            func f<T>(val: T) -> T {
                val
            }

            printf("%s\n", f("something here"));
            "#
            .into(),
            file.clone(),
        ))
            .get_tokens();

        let node = (&mut Parser::new(tokens)).parse_program().unwrap();

        let mut typed_node =
            (&mut TypeChecker::new(Rc::new(RefCell::new(TypeTable::new().init()))))
                .check_node(node)
                .unwrap();

        (&mut Monomorphizer::new())
            .monomorphize(&mut typed_node)
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
/// object_code: &[u8]
/// output_path: &Path
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

    let mut command = compiler.to_command();
    command.arg("-o").arg(output_path).arg(&lib_path);

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
            let lib_name = PathBuf::from(lib).file_stem().map_or_else(
                || Err(String::from("lib {lib} file stem not found")),
                |it| Ok({
                    let s = it.to_string_lossy().to_string();

                    if s.starts_with("lib") {
                        s
                            .chars()
                            .enumerate()
                            .filter(|(i, _)| *i > 2usize)
                            .map(|it| it.1.to_string())
                            .collect::<Vec<String>>()
                            .join("")
                    } else {
                        s
                    }
                }),
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
