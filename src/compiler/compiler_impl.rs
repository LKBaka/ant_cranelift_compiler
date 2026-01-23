use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    sync::{Arc, Mutex, atomic::{AtomicUsize, Ordering}},
};

use cranelift::prelude::{AbiParam, InstBuilder, MemFlags, Signature, Value, types};
use cranelift_codegen::{
    ir::{FuncRef, Function, UserFuncName},
    isa::TargetIsa,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{Linkage, Module, default_libcall_names};
use cranelift_object::{ObjectBuilder, ObjectModule};

use ant_ast::node::GetToken;
use ant_token::{token::Token, token_type::TokenType};
use ant_type_checker::{
    table::TypeTable,
    ty::{IntTy, Ty},
    typed_ast::{
        GetType, typed_expr::TypedExpression, typed_expressions::ident::Ident,
        typed_node::TypedNode, typed_stmt::TypedStatement,
    },
};

use crate::{
    args::read_arg,
    compiler::{
        CompileState, Compiler, FunctionState, GlobalState,
        constants::CALL_CONV,
        convert_type::convert_type_to_cranelift_type,
        handler::compile_infix::compile_infix,
        imm::{int_value_to_imm, platform_width_to_int_type},
        table::{StructLayout, SymbolScope, SymbolTable, SymbolTy},
    },
    traits::{LiteralExprToConst, NeedGc, ToLeBytes},
};

pub static STR_COUNTER: AtomicUsize = AtomicUsize::new(1);

impl Compiler {
    pub fn new(
        target_isa: Arc<dyn TargetIsa>,
        file: Arc<str>,
        table: Rc<RefCell<SymbolTable>>,
        type_table: Arc<Mutex<TypeTable>>,
    ) -> Compiler {
        // 创建 ObjectModule
        let builder =
            ObjectBuilder::new(target_isa.clone(), file.as_bytes(), default_libcall_names())
                .expect("Failed to create ObjectBuilder");

        let mut module = ObjectModule::new(builder);

        let ptr_ty = platform_width_to_int_type();

        // void* __obj_alloc(size_t)
        let mut alloc_sig = Signature::new(CALL_CONV);
        alloc_sig.params.push(AbiParam::new(ptr_ty));
        alloc_sig.returns.push(AbiParam::new(ptr_ty));

        let arc_alloc = module
            .declare_function("__obj_alloc", Linkage::Import, &alloc_sig)
            .map_err(|e| e.to_string())
            .expect("cannot declare function '__obj_alloc'");

        // void __obj_retain(void*)
        let mut retain_sig = Signature::new(CALL_CONV);
        retain_sig.params.push(AbiParam::new(ptr_ty));

        let arc_retain = module
            .declare_function("__obj_retain", Linkage::Import, &retain_sig)
            .map_err(|e| e.to_string())
            .expect("cannot declare function '__obj_retain'");

        // void __obj_release(void*)
        let mut release_sig = Signature::new(CALL_CONV);
        release_sig.params.push(AbiParam::new(ptr_ty));

        let arc_release = module
            .declare_function("__obj_release", Linkage::Import, &release_sig)
            .map_err(|e| e.to_string())
            .expect("cannot declare function '__obj_release'");

        Self {
            module,
            builder_ctx: FunctionBuilderContext::new(),
            context: cranelift_codegen::Context::new(),
            function_map: HashMap::new(),
            data_map: HashMap::new(),
            target_isa,

            table,
            type_table,

            arc_alloc,
            arc_release,
            arc_retain,
        }
    }

    /// 在编译阶段计算 struct 布局（目标平台相关）
    fn compile_struct_layout(
        state: &impl CompileState,
        name: &Arc<str>,
        fields: &[(Arc<str>, Ty)],
    ) -> Result<StructLayout, String> {
        let pointer_width = state.get_target_isa().pointer_bytes() as u32;

        let mut new_fields: Vec<(Arc<str>, Ty)> = Vec::with_capacity(fields.len() + 1);

        // 判断是否需要插入 __ref_count__ 字段
        if fields.is_empty() || fields[0].0.as_ref() != "__ref_count__" {
            new_fields.push((Arc::from("__ref_count__"), Ty::IntTy(IntTy::USize)));
        }

        new_fields.extend_from_slice(&fields);

        let mut offsets = Vec::with_capacity(new_fields.len());
        let mut current_offset = 0u32;
        let mut max_align = 1u32;

        for (_, ty) in &new_fields {
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
            name: name.clone(),
            fields: new_fields,
            offsets,
            size,
            align: max_align,
        })
    }

    fn get_type_size(
        state: &impl CompileState,
        ty: &Ty,
        pointer_width: u32,
    ) -> Result<u32, String> {
        match ty {
            Ty::IntTy(it) => Ok(it.get_bytes_size() as u32),
            Ty::Bool => Ok(1),
            Ty::Str => Ok(pointer_width),
            Ty::Struct { name, .. } => {
                let SymbolTy::Struct(layout) =
                    state.get_table().borrow_mut().get(name).map_or_else(
                        || Err(format!("undefine struct: {name}")),
                        |it| Ok(it.symbol_ty),
                    )?
                else {
                    Err(format!("not a struct: {name}"))?
                };

                Ok(layout.size)
            }
            _ => todo!(),
        }
    }

    fn get_type_align(
        state: &impl CompileState,
        ty: &Ty,
        pointer_width: u32,
    ) -> Result<u32, String> {
        match ty {
            Ty::IntTy(it) => Ok(it.get_bytes_size() as u32),
            Ty::Bool => Ok(1),
            Ty::Str => Ok(pointer_width),
            Ty::Struct { name, .. } => {
                let SymbolTy::Struct(layout) =
                    state.get_table().borrow_mut().get(name).map_or_else(
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

    pub fn is_top_level_stmt(stmt: &TypedStatement) -> bool {
        matches!(
            stmt,
            TypedStatement::ExpressionStatement(TypedExpression::Function { .. }) |
            TypedStatement::Const { .. }
        )
    }

    pub fn compile_top_level_stmt(
        state: &mut GlobalState,
        stmt: &TypedStatement,
    ) -> Result<(), String> {
        match stmt {
            TypedStatement::Const {
                name, value, ..
            } => {
                let const_val = value.to_const().map_or_else(
                    || Err(format!("expression `{value}` is not a constant")),
                    |it| Ok(it),
                )?;

                let data_id = state
                    .module
                    .declare_data(&name.value, Linkage::Local, false, false) // Declare as Local
                    .unwrap();

                let mut data_desc = cranelift_module::DataDescription::new();
                data_desc.init = cranelift_module::Init::Bytes {
                    contents: const_val.to_le_bytes().into_boxed_slice(),
                };

                state.data_map.insert(name.value.to_string(), data_id);
                
                state.module.define_data(data_id, &data_desc).unwrap();

                state.table.borrow_mut().define(&name.value);

                Ok(())
            }
            TypedStatement::ExpressionStatement(TypedExpression::Function {
                name,
                params,
                block: block_ast,
                ..
            }) => {
                let mut converted_params = vec![];

                for param in params {
                    converted_params.push(AbiParam::new(convert_type_to_cranelift_type(
                        &param.get_type(),
                    )));
                }

                let mut ctx = state.module.make_context();
                ctx.func.signature = Signature::new(CALL_CONV);
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

                    // 3. 定义外部作用域的符号
                    let _func_symbol = state.table.borrow_mut().define_func(&name);

                    // 4. 创建新的编译上下文
                    let mut func_builder_ctx = FunctionBuilderContext::new();
                    let mut func_builder =
                        FunctionBuilder::new(&mut ctx.func, &mut func_builder_ctx);

                    let entry_block = func_builder.create_block();
                    func_builder.append_block_params_for_function_params(entry_block);
                    func_builder.switch_to_block(entry_block);
                    func_builder.seal_block(entry_block);

                    // 5. 创建函数内部的符号表
                    let func_symbol_table =
                        Rc::new(RefCell::new(SymbolTable::from_outer(state.table.clone())));

                    // 6. 在函数内部符号表中也定义这个函数
                    let inner_func_symbol = func_symbol_table.borrow_mut().define_func(&name);
                    func_builder.declare_var(
                        Variable::from_u32(inner_func_symbol.var_index as u32),
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
                        Variable::from_u32(inner_func_symbol.var_index as u32),
                        inner_ref_val, // 使用内部创建的Value
                    );

                    // 8. 声明参数变量
                    for (i, param) in params.iter().enumerate() {
                        if let TypedExpression::TypeHint(param_name, _, ty) = &**param {
                            let symbol = func_symbol_table.borrow_mut().define(&param_name.value);
                            let cranelift_ty = convert_type_to_cranelift_type(ty);

                            func_builder.declare_var(
                                Variable::from_u32(symbol.var_index as u32),
                                cranelift_ty,
                            );

                            let param_value = func_builder.block_params(entry_block)[i];
                            func_builder
                                .def_var(Variable::from_u32(symbol.var_index as u32), param_value);
                        }
                    }

                    // 9. 编译函数体
                    let mut func_state = FunctionState {
                        builder: func_builder,
                        module: state.module,
                        table: func_symbol_table,
                        type_table: state.type_table.clone(),
                        function_map: state.function_map,
                        data_map: state.data_map,
                        target_isa: state.target_isa.clone(),

                        arc_alloc: state.arc_alloc,
                        arc_release: state.arc_release,
                        arc_retain: state.arc_retain,
                    };

                    let result = Self::compile_expr(&mut func_state, block_ast)?;

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

                    return Ok(());
                }

                todo!()
            }

            stmt => todo!("impl function 'compile_stmt' {stmt}"),
        }
    }

    pub fn compile_stmt(state: &mut FunctionState, stmt: &TypedStatement) -> Result<Value, String> {
        match stmt {
            TypedStatement::ExpressionStatement(expr) => Self::compile_expr(state, expr),
            TypedStatement::Let {
                name, value, ty, ..
            } => {
                let val = Self::compile_expr(state, value)?;

                // ARC: retain 新值
                state.retain_if_needed(val, ty);

                let symbol = state.table.borrow_mut().define(&name.value);
                let cranelift_ty = convert_type_to_cranelift_type(ty);
                state
                    .builder
                    .try_declare_var(Variable::from_u32(symbol.var_index as u32), cranelift_ty)
                    .map_err(|it| format!("failed to declare variable '{}': {it}", symbol.name))?;
                state
                    .builder
                    .def_var(Variable::from_u32(symbol.var_index as u32), val);

                return Ok(state.builder.ins().iconst(types::I64, 0)); // unit
            }

            TypedStatement::Block { statements: it, .. } => {
                let mut ret_val = state.builder.ins().iconst(types::I64, 0);

                for stmt in it {
                    ret_val = Self::compile_stmt(state, &stmt)?;
                }

                let symbols = state.table.borrow().map.clone();

                for (_, sym) in symbols {
                    if sym.is_val && sym.symbol_ty.need_gc() {
                        let var = Variable::from_u32(sym.var_index as u32);
                        let val = state.builder.use_var(var);
                        state.emit_release(val);
                    }
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
                let Ty::Struct { name, fields, .. } = ty else {
                    return Err(format!("not a struct"));
                };

                let layout = Self::compile_struct_layout(
                    state,
                    name,
                    &fields
                        .iter()
                        .map(|(name, val_ty)| (name.clone(), val_ty.clone()))
                        .collect::<Vec<(Arc<str>, Ty)>>(),
                )?;

                state.table.borrow_mut().define_struct_type(name, layout);

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
                    Variable::from_u32(func_symbol.var_index as u32),
                    platform_width_to_int_type(), // 函数指针类型
                );

                let func_ref = state
                    .module
                    .declare_func_in_func(extern_func_id, &mut state.builder.func);

                let func_addr_val = state
                    .builder
                    .ins()
                    .func_addr(platform_width_to_int_type(), func_ref);

                state.builder.def_var(
                    Variable::from_u32(func_symbol.var_index as u32),
                    func_addr_val,
                );

                // unit
                Ok(state.builder.ins().iconst(platform_width_to_int_type(), 0))
            }

            TypedStatement::Return { expr, .. } => {
                let val = Self::compile_expr(state, expr)?;

                state.retain_if_needed(val, &expr.get_type());

                state.builder.ins().return_(&[val]);

                // 这个值永远不会被使用
                Ok(val)
            }

            TypedStatement::Impl {
                impl_, for_, block, ..
            } => {
                if state.table.borrow_mut().get(&impl_.value).is_none() {
                    return Err(format!("cannot find type '{impl_}' in this scope"));
                }

                if let Some(for_) = for_
                    && state.table.borrow_mut().get(&for_.value).is_none()
                {
                    return Err(format!("cannot find type '{for_}' in this scope"));
                }

                let type_name = impl_.value.clone();

                let TypedStatement::Block { statements, .. } = &**block else {
                    unreachable!();
                };

                for stmt in statements {
                    let TypedStatement::ExpressionStatement(expr) = stmt else {
                        continue;
                    };

                    let TypedExpression::Function {
                        name: Some(fn_name),
                        token,
                        params,
                        generics_params,
                        block,
                        ret_ty,
                        ty,
                    } = expr.clone()
                    else {
                        continue;
                    };

                    // mangling
                    let mut new_name_token = fn_name.clone();
                    new_name_token.value = format!("{}__{}", type_name, &fn_name.value).into();

                    Self::compile_expr(
                        state,
                        &TypedExpression::Function {
                            token,
                            name: Some(new_name_token),
                            params,
                            generics_params,
                            block,
                            ret_ty,
                            ty,
                        },
                    )?;
                }

                // unit
                Ok(state.builder.ins().iconst(platform_width_to_int_type(), 0))
            }

            stmt => todo!("impl function 'compile_stmt' {stmt}"),
        }
    }

    pub fn compile_expr(
        state: &mut FunctionState,
        expr: &TypedExpression,
    ) -> Result<Value, String> {
        match expr {
            TypedExpression::Int { value, ty, .. } => Ok(state
                .builder
                .ins()
                .iconst(convert_type_to_cranelift_type(ty), int_value_to_imm(value))),

            TypedExpression::Bool { value, ty, .. } => Ok(state
                .builder
                .ins()
                .iconst(convert_type_to_cranelift_type(ty), *value as i64)),

            TypedExpression::Ident(it, ty) => {
                let sym = state.table.borrow_mut().get(&it.value);
                if let Some(var) = &sym
                    && var.scope == SymbolScope::Local
                {
                    let v = Variable::from_u32(var.var_index as u32);

                    return Ok(state.builder.use_var(v));
                } else if let Some(var) = sym
                    && var.scope == SymbolScope::Global
                {
                    // 获取 DataId
                    let data_id = state
                        .data_map
                        .get(&var.name.to_string())
                        .map(|it| it.clone())
                        .map_or(
                            Err(format!("variable `{}` not in data map", var.name)),
                            |it| Ok(it),
                        )?;

                    let global_var = state
                        .module
                        .declare_data_in_func(data_id, state.builder.func);

                    let val_ptr = state
                        .builder
                        .ins()
                        .global_value(platform_width_to_int_type(), global_var);

                    if *ty == Ty::Str {
                        return Ok(val_ptr)
                    }

                    return Ok(state.builder.ins().load(
                        convert_type_to_cranelift_type(ty),
                        MemFlags::new(),
                        val_ptr,
                        0,
                    ));
                }

                Err(format!("undefined variable: {}", it.value))
            }

            TypedExpression::StrLiteral { value, .. } => {
                let content = value.to_string() + "\0";
                
                // 获取当前是第几个字符串 (从一开始计数)
                let str_count = STR_COUNTER.load(Ordering::Relaxed);
                STR_COUNTER.fetch_add(1, Ordering::Relaxed);

                let data_id = *state.data_map.entry(content.clone()).or_insert_with(|| {
                    let name = format!("str_{}_{str_count}", content.len());
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
                    .borrow_mut()
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

            TypedExpression::BuildStruct(_, struct_name, fields, _) => {
                let SymbolTy::Struct(layout) = state
                    .table
                    .borrow_mut()
                    .get(&struct_name.value)
                    .map_or_else(
                        || Err(format!("undefined struct: {struct_name}")),
                        |it| Ok(it.symbol_ty),
                    )?
                else {
                    Err(format!("not a struct: {struct_name}"))?
                };

                // 堆分配
                let size_val = state
                    .builder
                    .ins()
                    .iconst(platform_width_to_int_type(), layout.size as i64);
                let struct_ptr = state.emit_alloc(size_val);

                // 写字段
                for (field_name, field_expr) in fields {
                    let field_idx = layout
                        .fields
                        .iter()
                        .position(|(n, _)| n == &field_name.value)
                        .unwrap();

                    let offset = layout.offsets[field_idx];
                    let field_ptr = if offset == 0 {
                        struct_ptr
                    } else {
                        state.builder.ins().iadd_imm(struct_ptr, offset as i64)
                    };

                    let field_val = Self::compile_expr(state, field_expr)?;
                    state
                        .builder
                        .ins()
                        .store(MemFlags::new(), field_val, field_ptr, 0);
                }

                // ref_count = 1，由 arc.c 保证
                Ok(struct_ptr)
            }

            TypedExpression::Assign { left, right, .. } => {
                if let TypedExpression::Ident(ident, _) = &**left {
                    if left.get_type() != right.get_type() {
                        return Err(format!(
                            "expected: `{}`, got: `{}`",
                            left.get_type(),
                            right.get_type()
                        ));
                    }

                    let new_val = Self::compile_expr(state, &right)?;

                    let var_symbol = state
                        .table
                        .borrow_mut()
                        .get(&ident.value)
                        .ok_or_else(|| format!("undefined variable `{}`", ident.value))?;

                    if !var_symbol.is_val {
                        return Err(format!("assign to a type: `{}`", ident.value));
                    }

                    let var = Variable::from_u32(var_symbol.var_index as u32);

                    let old_val = state.builder.use_var(var);

                    state.update_ptr(new_val, old_val);

                    state.builder.def_var(var, new_val);

                    return Ok(new_val); // 该值不会被使用
                } else if let TypedExpression::FieldAccess(obj, field, _) = &**left {
                    let new_val = Self::compile_expr(state, &right)?;

                    // 编译对象表达式
                    let obj_ptr = Self::compile_expr(state, &obj)?;

                    // 获取对象类型，确保是 struct
                    let obj_ty = obj.get_type();
                    let Ty::Struct { name, .. } = &obj_ty else {
                        return Err("field set on non-struct type".into());
                    };

                    // 从符号表获取结构体布局
                    let sym = state
                        .table
                        .borrow_mut()
                        .get(name)
                        .ok_or_else(|| format!("undefined struct: `{}`", name))?;

                    if !sym.is_val {
                        return Err(format!("assign to a type: `{}`", sym.name));
                    }

                    let SymbolTy::Struct(layout) = sym.symbol_ty else {
                        Err(format!("not a struct"))?
                    };

                    // 查找字段索引
                    let field_idx = layout
                        .fields
                        .iter()
                        .position(|(n, _)| n == &field.value)
                        .ok_or_else(|| {
                            format!("field `{}` not found in struct `{}`", field, name)
                        })?; // 类型检查已保证存在，这里只是安全断言

                    // 计算字段地址
                    let offset = layout.offsets[field_idx];
                    let field_ptr = if offset == 0 {
                        obj_ptr
                    } else {
                        state.builder.ins().iadd_imm(obj_ptr, offset as i64)
                    };

                    // 先 load 旧值（如果字段需要 GC）
                    let field_ty = &layout.fields[field_idx].1;

                    let old_val = if field_ty.need_gc() {
                        Some(state.builder.ins().load(
                            convert_type_to_cranelift_type(field_ty),
                            MemFlags::new(),
                            field_ptr,
                            0,
                        ))
                    } else {
                        None
                    };

                    // retain 新值
                    state.retain_if_needed(new_val, field_ty);

                    // store 新值
                    state
                        .builder
                        .ins()
                        .store(MemFlags::new(), new_val, field_ptr, 0);

                    // release 旧值
                    if let Some(old) = old_val {
                        state.release_if_needed(old, field_ty);
                    }

                    return Ok(new_val);
                } else {
                    return Err("assign target must be ident or field".into());
                };
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
                ctx.func.signature = Signature::new(CALL_CONV);
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
                    let func_symbol = state.table.borrow_mut().define_func(&name);
                    state.builder.declare_var(
                        Variable::from_u32(func_symbol.var_index as u32),
                        platform_width_to_int_type(),
                    );
                    state.builder.def_var(
                        Variable::from_u32(func_symbol.var_index as u32),
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

                    // 7. 在函数内部符号表中也定义这个函数
                    let inner_func_symbol = func_symbol_table.borrow_mut().define_func(&name);
                    func_builder.declare_var(
                        Variable::from_u32(inner_func_symbol.var_index as u32),
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
                        Variable::from_u32(inner_func_symbol.var_index as u32),
                        inner_ref_val, // 使用内部创建的Value
                    );

                    // 8. 声明参数变量
                    for (i, param) in params.iter().enumerate() {
                        if let TypedExpression::TypeHint(param_name, _, ty) = &**param {
                            let symbol = func_symbol_table.borrow_mut().define(&param_name.value);
                            let cranelift_ty = convert_type_to_cranelift_type(ty);

                            func_builder.declare_var(
                                Variable::from_u32(symbol.var_index as u32),
                                cranelift_ty,
                            );

                            let param_value = func_builder.block_params(entry_block)[i];
                            func_builder
                                .def_var(Variable::from_u32(symbol.var_index as u32), param_value);
                        }
                    }

                    // 9. 编译函数体
                    let mut func_state = FunctionState {
                        builder: func_builder,
                        module: state.module,
                        table: func_symbol_table,
                        type_table: state.type_table.clone(),
                        function_map: state.function_map,
                        data_map: state.data_map,
                        target_isa: state.target_isa.clone(),

                        arc_alloc: state.arc_alloc,
                        arc_release: state.arc_release,
                        arc_retain: state.arc_retain,
                    };

                    let result = Self::compile_expr(&mut func_state, block_ast)?;

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

                if let TypedExpression::FieldAccess(obj, field, _) = &**func
                    && let Ty::Struct { name, fields, .. } = &obj.get_type()
                    && let Some(Ty::Function {
                        params_type,
                        ret_type,
                        ..
                    }) = fields.get(&field.value)
                {
                    // 函数名重命名
                    let func_name = format!("{}__{}", name, field.value);

                    let call_expr = TypedExpression::Call {
                        token: Token::new(
                            "(".into(),
                            TokenType::LParen,
                            func.token().value,
                            func.token().line,
                            func.token().column,
                        ), // 充填伪造 Token
                        func: Box::new(TypedExpression::Ident(
                            Ident {
                                value: func_name.clone().into(),
                                token: Token::new(
                                    func_name.clone().into(),
                                    TokenType::Ident,
                                    func.token().value,
                                    func.token().line,
                                    func.token().column,
                                ),
                            },
                            func_ty.clone(),
                        )),
                        args: args.clone(),
                        func_ty: Ty::Function {
                            params_type: params_type.clone(),
                            ret_type: ret_type.clone(),
                            is_variadic: false,
                        },
                    };

                    return Self::compile_expr(state, &call_expr);
                }

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

                // 编译所有参数
                let mut arg_values = Vec::new();

                if *va_arg {
                    for arg in args {
                        let arg_val = Self::compile_expr(state, arg)?;
                        arg_values.push(arg_val);
                    }
                } else {
                    for (arg, arg_ty) in args.iter().zip(params_type.iter()) {
                        let v = Self::compile_expr(state, arg)?;
                        state.retain_if_needed(v, arg_ty);
                        arg_values.push(v);
                    }
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

                let func_val = Self::compile_expr(state, &func)?;

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
                let result = if results.is_empty() {
                    state.builder.ins().iconst(platform_width_to_int_type(), 0)
                } else {
                    results[0]
                };

                for (val, ty) in arg_values.iter().zip(params_type.iter()) {
                    state.release_if_needed(*val, ty);
                }

                Ok(result)
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
            TypedExpression::Block(_, it, _) => {
                let mut ret_val = state.builder.ins().iconst(types::I64, 0);

                for stmt in it {
                    ret_val = Self::compile_stmt(state, &stmt)?;
                }

                let symbols = state.table.borrow().map.clone();

                for (_, sym) in symbols {
                    if sym.is_val && sym.symbol_ty.need_gc() {
                        let var = Variable::from_u32(sym.var_index as u32);
                        let val = state.builder.use_var(var);
                        state.emit_release(val);
                    }
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

        let mut sig = Signature::new(CALL_CONV);
        sig.returns.push(AbiParam::new(types::I32));

        let is_script_mode = read_arg().map_or(false, |it| it.script_mode);
        if is_script_mode {
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

                let mut ret_val = builder.ins().iconst(types::I32, 0);

                let mut state = FunctionState {
                    builder,
                    target_isa: self.target_isa.clone(),
                    module: &mut self.module,
                    function_map: &mut self.function_map,
                    data_map: &mut self.data_map,

                    table: self.table,
                    type_table: self.type_table,

                    arc_alloc: self.arc_alloc,
                    arc_retain: self.arc_retain,
                    arc_release: self.arc_release,
                };

                for stmt in statements {
                    ret_val = Self::compile_stmt(&mut state, &stmt)?;
                }

                state.builder.ins().return_(&[ret_val]);

                #[cfg(debug_assertions)]
                {
                    let func_ref = &state.builder.func;
                    println!("=== before finalize:\n{}", {
                        let mut s = String::new();
                        cranelift::codegen::write_function(&mut s, func_ref).unwrap();
                        s
                    });
                }

                state.builder.finalize();
            }

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

            self.module
                .define_function(func_id, &mut self.context)
                .map_err(|e| format!("define main failed: {}", e))?;
            self.context.clear();
        } else {
            let mut state = GlobalState {
                target_isa: self.target_isa.clone(),
                module: &mut self.module,
                function_map: &mut self.function_map,
                data_map: &mut self.data_map,

                table: self.table,
                type_table: self.type_table,

                arc_alloc: self.arc_alloc,
                arc_retain: self.arc_retain,
                arc_release: self.arc_release,
            };

            for stmt in statements {
                if !Self::is_top_level_stmt(&stmt) {
                    continue;
                }

                Self::compile_top_level_stmt(&mut state, &stmt)?;
            }
        }

        let obj = self.module.finish();
        Ok(obj.emit().unwrap().to_vec())
    }
}

#[cfg(test)]
mod tests {
    use std::{
        cell::RefCell,
        path::Path,
        rc::Rc,
        sync::{Arc, Mutex},
    };

    use ant_lexer::Lexer;
    use ant_parser::Parser;

    use ant_type_checker::{TypeChecker, table::TypeTable};

    use crate::{
        compiler::{Compiler, compile_to_executable, create_target_isa, table::SymbolTable},
        monomorphizer::Monomorphizer,
    };

    #[test]
    fn simple_program() {
        let file: std::sync::Arc<str> = "__simple_program__".into();

        // 创建目标 ISA
        let target_isa = create_target_isa();

        // 解析ast
        let tokens = (&mut Lexer::new(
            r#"
            func main() -> i32 {
                extern "C" func printf(s: str, ...) -> i32;
            
                struct A {}
                
                impl A {
                    func f() -> i64 {
                        917813i64
                    }
                }
                    
                let o = new A {};
                    
                printf("%lld\n", o.f())
                    
                printf("end\n");
            }
            "#
            .into(),
            file.clone(),
        ))
            .get_tokens();

        let node = (&mut Parser::new(tokens)).parse_program().unwrap();

        let type_table = Arc::new(Mutex::new(TypeTable::new().init()));

        let mut typed_node = (&mut TypeChecker::new(type_table.clone()))
            .check_node(node)
            .unwrap();

        // 创建编译器实例
        let table = SymbolTable::new();

        let compiler = Compiler::new(
            target_isa,
            "__simple_program__".into(),
            Rc::new(RefCell::new(table)),
            type_table.clone(),
        );

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
