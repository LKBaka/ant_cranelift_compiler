use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
};

use cranelift::prelude::{AbiParam, InstBuilder, IntCC, MemFlags, Signature, Value, types};
use cranelift_codegen::{
    ir::{Function, UserFuncName},
    isa::TargetIsa,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{Linkage, Module, default_libcall_names};
use cranelift_object::{ObjectBuilder, ObjectModule};

use ant_type_checker::{
    module::TypedModule,
    ty::{IntTy, Ty, display_ty},
    typed_ast::{
        GetType, typed_expr::TypedExpression, typed_node::TypedNode, typed_stmt::TypedStatement,
    },
};
use indexmap::IndexMap;

use crate::{
    args::read_arg,
    compiler::{
        CompileResult, CompileState, Compiler, FunctionState, GlobalState,
        compile_state_impl::PushGetGeneric,
        constants::CALL_CONV,
        convert_type::convert_type_to_cranelift_type,
        function::{compile_function, make_signature},
        generic::GenericInfo,
        get_platform_width,
        handler::{
            compile_build_struct::{compile_build_struct, instantiate_struct},
            compile_call::compile_call,
            compile_cast::compile_cast,
            compile_infix::compile_infix,
        },
        imm::{int_value_to_imm, platform_width_to_int_type},
        table::{StructLayout, SymbolScope, SymbolTable, SymbolTy},
    },
    traits::{LiteralExprToConst, NeedGc, ToLeBytes},
};

pub static STR_COUNTER: AtomicUsize = AtomicUsize::new(1);

impl<'a> Compiler<'a> {
    pub fn new(
        target_isa: Arc<dyn TargetIsa>,
        file: Arc<str>,
        table: Rc<RefCell<SymbolTable>>,
        typed_module: TypedModule<'a>,
    ) -> Compiler<'a> {
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
            generic_map: HashMap::new(),
            compiled_generic_map: IndexMap::new(),
            target_isa,

            table,
            typed_module,

            arc_alloc,
            arc_release,
            arc_retain,
        }
    }

    /// 在编译阶段计算 struct 布局（目标平台相关）
    pub fn compile_struct_layout<'aa, 'b>(
        state: &impl CompileState<'aa, 'b>,
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

    pub fn get_type_size<'aa, 'b>(
        state: &impl CompileState<'aa, 'b>,
        ty: &Ty,
        pointer_width: u32,
    ) -> Result<u32, String> {
        match ty {
            Ty::IntTy(it) => Ok(it.get_bytes_size() as u32),
            Ty::Bool => Ok(1),
            Ty::Str => Ok(pointer_width),
            Ty::Function { .. } => Ok(pointer_width),
            Ty::Ptr(_) => Ok(pointer_width),
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

            Ty::AppliedGeneric(name, _) => {
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

            _ => todo!(
                "todo ty {}",
                display_ty(ty, state.get_typed_module_ref().tcx_ref())
            ),
        }
    }

    fn get_type_align<'aa, 'b>(
        state: &impl CompileState<'aa, 'b>,
        ty: &Ty,
        pointer_width: u32,
    ) -> Result<u32, String> {
        match ty {
            Ty::IntTy(it) => Ok(it.get_bytes_size() as u32),
            Ty::Bool => Ok(1),
            Ty::Ptr(_) => Ok(pointer_width),
            Ty::Str => Ok(pointer_width),
            Ty::Function { .. } => Ok(pointer_width),
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
            _ => todo!("impl ty: {ty}"),
        }
    }

    pub fn is_top_level_stmt(state: &GlobalState, stmt: &TypedStatement) -> bool {
        if matches!(
            stmt,
            TypedStatement::Const { .. }
                | TypedStatement::Struct { .. }
                | TypedStatement::Extern { .. }
        ) {
            return true;
        } else if let TypedStatement::ExpressionStatement(_, id, _) = stmt {
            return matches!(
                state.typed_module.get_expr(*id).unwrap(),
                TypedExpression::Function { .. }
            );
        }

        false
    }

    pub fn compile_top_level_stmt(
        state: &mut GlobalState,
        stmt: &TypedStatement,
    ) -> Result<(), String> {
        match stmt {
            TypedStatement::Const { name, value, .. } => {
                let value = state.get_expr_ref(*value);

                let const_val = value.to_const().map_or_else(
                    || Err(format!("the expression is not a constant")),
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

            TypedStatement::ExpressionStatement(_, id, _) => {
                if let TypedExpression::Function {
                    name,
                    params,
                    block: block_ast,
                    generics_params: generics,
                    ty: tyid,
                    ..
                } = state.get_expr_ref(*id).clone()
                {
                    let Ty::Function {
                        params_type,
                        ret_type,
                        ..
                    } = state.tcx_ref().get(tyid)
                    else {
                        unreachable!()
                    };

                    if let Some(name) = name.as_ref() {
                        let name = &name.value;

                        // 1. 检查是否为泛型
                        if !generics.is_empty() {
                            let Ty::Function { params_type, .. } =
                                state.tcx_ref().get(tyid).clone()
                            else {
                                unreachable!()
                            };

                            let param_names = params
                                .iter()
                                .map(|it| {
                                    let it = state.get_expr_ref(*it);

                                    let TypedExpression::TypeHint(it, _, _) = it else {
                                        unreachable!()
                                    };
                                    it.value.clone()
                                })
                                .collect::<Vec<Arc<str>>>();

                            let generic_params = param_names
                                .iter()
                                .zip(params_type.clone())
                                .map(|(name, id)| (name, state.tcx_ref().get(id)))
                                .filter(|(_, it)| matches!(it, Ty::Generic(_, _)))
                                .map(|(name, ty)| (name.clone(), ty.to_string().into()))
                                .collect();

                            let all_params = param_names
                                .iter()
                                .zip(params_type)
                                .map(|(name, id)| (name.clone(), id))
                                .collect();

                            state.push_generic(
                                name.to_string(),
                                GenericInfo::Function {
                                    generic: generics
                                        .iter()
                                        .filter(|it| {
                                            matches!(
                                                state.get_expr_ref(**it),
                                                TypedExpression::Ident(..)
                                            )
                                        })
                                        .map(|it| {
                                            let it = state.get_expr_ref(*it);

                                            let TypedExpression::Ident(s, _) = it else {
                                                unreachable!()
                                            };
                                            s.value.clone()
                                        })
                                        .collect::<Vec<Arc<str>>>(),
                                    generic_params,
                                    all_params,
                                    block: Box::new(state.get_expr_cloned(block_ast)),
                                    ret_ty: *ret_type,
                                },
                            );

                            return Ok(());
                        }

                        let signature = make_signature(
                            &params_type
                                .iter()
                                .map(|it| state.tcx_ref().get(*it))
                                .collect::<Vec<&Ty>>(),
                            state.tcx_ref().get(*ret_type),
                        );

                        let mut ctx = state.module.make_context();
                        ctx.func.signature = signature.clone();

                        // 2. 首先声明函数
                        let func_id =
                            match state
                                .module
                                .declare_function(&name, Linkage::Export, &signature)
                            {
                                Ok(it) => it,
                                Err(it) => Err(it.to_string())?,
                            };

                        // 3. 立即将函数ID注册到function_map中
                        state.function_map.insert(name.to_string(), func_id);

                        // 4. 定义外部作用域的符号
                        let _func_symbol = state.table.borrow_mut().define_func(&name);

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

                        // 8. 在函数内部重新创建FuncRef和对应的Value
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

                        // 9. 声明参数变量
                        for (i, param) in params.iter().enumerate() {
                            let param = state.get_expr_ref(*param);

                            if let TypedExpression::TypeHint(param_name, _, ty) = param {
                                let symbol =
                                    func_symbol_table.borrow_mut().define(&param_name.value);
                                let cranelift_ty =
                                    convert_type_to_cranelift_type(state.tcx_ref().get(*ty));

                                func_builder.declare_var(
                                    Variable::from_u32(symbol.var_index as u32),
                                    cranelift_ty,
                                );

                                let param_value = func_builder.block_params(entry_block)[i];
                                func_builder.def_var(
                                    Variable::from_u32(symbol.var_index as u32),
                                    param_value,
                                );
                            }
                        }

                        let block_ty_id = state.get_expr_ref(block_ast).get_type();

                        let block_val_ty = state.tcx().get(block_ty_id).clone();

                        let block = state.get_expr_ref(block_ast).clone();

                        // 10. 编译函数体
                        let mut func_state = FunctionState {
                            builder: func_builder,
                            module: state.module,
                            table: func_symbol_table,
                            typed_module: state.typed_module,
                            function_map: state.function_map,
                            data_map: state.data_map,
                            generic_map: state.generic_map,
                            compiled_generic_map: state.compiled_generic_map,
                            subst: &IndexMap::new(),

                            target_isa: state.target_isa.clone(),

                            arc_alloc: state.arc_alloc,
                            arc_release: state.arc_release,
                            arc_retain: state.arc_retain,

                            terminated: false,
                        };

                        let result = Self::compile_expr(&mut func_state, &block)?;

                        if !func_state.terminated {
                            let symbols = state.table.borrow().map.clone();

                            for (_, sym) in symbols {
                                if sym.is_val && sym.symbol_ty.need_gc() {
                                    let var = Variable::from_u32(sym.var_index as u32);
                                    let val = func_state.builder.use_var(var);
                                    func_state.emit_release(val);
                                }
                            }

                            if block_val_ty != Ty::Unit {
                                func_state.builder.ins().return_(&[result]);
                            } else {
                                func_state.builder.ins().return_(&[]);
                            }
                        }

                        #[cfg(feature = "debug")]
                        {
                            let func_ref = &func_state.builder.func;
                            println!("=== before finalize:\n{}", {
                                let mut s = String::new();
                                cranelift::codegen::write_function(&mut s, func_ref).unwrap();
                                s
                            });
                        }

                        func_state.builder.finalize();

                        match cranelift_codegen::verify_function(&ctx.func, &*state.target_isa) {
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

                        state
                            .module
                            .define_function(func_id, &mut ctx)
                            .map_or_else(|err| Err(err.to_string()), |_| Ok(()))?;
                        state.module.clear_context(&mut ctx);

                        return Ok(());
                    }

                    todo!()
                } else {
                    todo!()
                };
            }

            TypedStatement::Struct {
                ty, name, generics, ..
            } => {
                let name = &name.value;

                // 从 Type 中提取字段定义
                let Ty::Struct { fields, .. } = state.tcx_ref().get(*ty) else {
                    return Err(format!("not a struct"));
                };

                // 检查是否为泛型
                if generics.is_empty() {
                    let layout = Self::compile_struct_layout(
                        state,
                        name,
                        &fields
                            .iter()
                            .map(|(name, val_ty)| {
                                (name.clone(), state.tcx_ref().get(*val_ty).clone())
                            })
                            .collect::<Vec<(Arc<str>, Ty)>>(),
                    )?;

                    state.table.borrow_mut().define_struct_type(name, layout);
                } else {
                    let generic_params = generics
                        .iter()
                        .map(|it| {
                            let it = state.get_expr_ref(*it);

                            let TypedExpression::Ident(it, _) = it else {
                                todo!("todo generic expression")
                            };

                            it.value.clone()
                        })
                        .collect();

                    state.push_generic(
                        name.to_string(),
                        GenericInfo::Struct {
                            generic_params,
                            fields: fields.clone(),
                        },
                    );
                }

                Ok(())
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
                } = state.tcx_ref().get(*ty)
                else {
                    return Err(format!("not a function: {ty}"));
                };

                let mut cranelift_params = params_type
                    .iter()
                    .map(|it| {
                        AbiParam::new(convert_type_to_cranelift_type(state.tcx_ref().get(*it)))
                    })
                    .collect::<Vec<_>>();

                // 构造签名
                let mut extern_func_sig = Signature::new(CALL_CONV);

                extern_func_sig.params.append(&mut cranelift_params);

                extern_func_sig
                    .returns
                    .push(AbiParam::new(convert_type_to_cranelift_type(
                        &state.tcx_ref().get(*ret_type),
                    )));

                let extern_func_id = state
                    .module
                    .declare_function(&extern_func_name.value, Linkage::Import, &extern_func_sig)
                    .map_err(|e| format!("declare {extern_func_name} failed: {}", e))?;

                // 放进 function_map，方便后面 call
                state
                    .function_map
                    .insert(alias.value.to_string(), extern_func_id);

                state.table.borrow_mut().define_func(&alias.value);

                Ok(())
            }

            _ => todo!("impl function 'compile_stmt'"),
        }
    }

    pub fn compile_stmt(state: &mut FunctionState, stmt: &TypedStatement) -> CompileResult<Value> {
        match stmt {
            TypedStatement::ExpressionStatement(_, expr, _) => {
                let expr = state.get_expr_ref(*expr).clone();
                Self::compile_expr(state, &expr)
            }

            TypedStatement::Let {
                name, value, ty, ..
            } => {
                let value = state.get_expr_ref(*value).clone();

                let val = Self::compile_expr(state, &value)?;

                let obj_ty = state.resolve_concrete_ty(*ty, state.subst);

                // ARC: retain 新值
                state.retain_if_needed(val, &obj_ty);

                let symbol = state.table.borrow_mut().define(&name.value);
                let cranelift_ty = convert_type_to_cranelift_type(&obj_ty);
                state
                    .builder
                    .try_declare_var(Variable::from_u32(symbol.var_index as u32), cranelift_ty)
                    .map_err(|it| format!("failed to declare variable '{}': {it}", symbol.name))?;
                state
                    .builder
                    .def_var(Variable::from_u32(symbol.var_index as u32), val);

                // unit
                Ok(state
                    .builder
                    .ins()
                    .iconst(convert_type_to_cranelift_type(&Ty::Unit), 0))
            }

            TypedStatement::Block {
                statements: it, ty, ..
            } => {
                let ty = state.resolve_concrete_ty(*ty, state.subst);

                let mut ret_val = state
                    .builder
                    .ins()
                    .iconst(convert_type_to_cranelift_type(&ty), 0);

                for stmt in it {
                    let stmt = state.get_stmt_ref(*stmt).clone();

                    ret_val = Self::compile_stmt(state, &stmt)?;
                }

                if ty == Ty::Unit {
                    Ok(state.builder.ins().iconst(convert_type_to_cranelift_type(&ty), 0))
                } else {
                    Ok(ret_val)
                }
            }

            TypedStatement::While {
                condition, block, ..
            } => {
                let condition = state.get_expr_ref(*condition).clone();
                let block = state.get_stmt_ref(*block).clone();

                let head = state.builder.create_block(); // while 头
                let body = state.builder.create_block(); // 循环体
                let exit = state.builder.create_block(); // 退出

                state.builder.ins().jump(head, &[]);

                state.builder.switch_to_block(head);
                let condition_val = Self::compile_expr(state, &condition)?;
                state
                    .builder
                    .ins()
                    .brif(condition_val, body, &[], exit, &[]);

                state.builder.switch_to_block(body);
                let _body_val = Self::compile_stmt(state, &block)?;
                state.builder.ins().jump(head, &[]);

                state.builder.seal_block(body);
                state.builder.seal_block(head);

                state.builder.switch_to_block(exit);
                state.builder.seal_block(exit);

                // unit
                Ok(state
                    .builder
                    .ins()
                    .iconst(convert_type_to_cranelift_type(&Ty::Unit), 0))
            }

            TypedStatement::Struct {
                ty, generics, name, ..
            } => {
                let name = &name.value;

                // 从 Type 中提取字段定义
                let Ty::Struct { fields, .. } = state.tcx_ref().get(*ty) else {
                    return Err(format!("not a struct"));
                };

                // 检查是否为泛型
                if generics.is_empty() {
                    let layout = Self::compile_struct_layout(
                        state,
                        name,
                        &fields
                            .iter()
                            .map(|(name, val_ty)| {
                                (name.clone(), state.tcx_ref().get(*val_ty).clone())
                            })
                            .collect::<Vec<(Arc<str>, Ty)>>(),
                    )?;

                    state.table.borrow_mut().define_struct_type(name, layout);
                } else {
                    let generic_params = generics
                        .iter()
                        .map(|it| {
                            let it = state.get_expr_ref(*it);

                            let TypedExpression::Ident(it, _) = it else {
                                todo!("todo generic expression")
                            };

                            it.value.clone()
                        })
                        .collect();

                    state.push_generic(
                        name.to_string(),
                        GenericInfo::Struct {
                            generic_params,
                            fields: fields.clone(),
                        },
                    );
                }

                // unit
                Ok(state
                    .builder
                    .ins()
                    .iconst(convert_type_to_cranelift_type(&Ty::Unit), 0))
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
                } = state.tcx_ref().get(*ty).clone()
                else {
                    return Err(format!("not a function: {ty}"));
                };

                let mut cranelift_params = params_type
                    .iter()
                    .map(|it| {
                        AbiParam::new(convert_type_to_cranelift_type(
                            &state.resolve_concrete_ty(*it, state.subst),
                        ))
                    })
                    .collect::<Vec<_>>();

                // 构造签名
                let mut extern_func_sig = Signature::new(CALL_CONV);

                extern_func_sig.params.append(&mut cranelift_params);

                extern_func_sig
                    .returns
                    .push(AbiParam::new(convert_type_to_cranelift_type(
                        &state.resolve_concrete_ty(ret_type, state.subst),
                    )));

                let extern_func_id = state
                    .module
                    .declare_function(&extern_func_name.value, Linkage::Import, &extern_func_sig)
                    .map_err(|e| format!("declare {extern_func_name} failed: {}", e))?;

                // 放进 function_map，方便后面 call
                state
                    .function_map
                    .insert(alias.value.to_string(), extern_func_id);

                // 登记进符号表后，立刻 declare
                let func_symbol = state.table.borrow_mut().define_func(&alias.value);
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
                Ok(state
                    .builder
                    .ins()
                    .iconst(convert_type_to_cranelift_type(&Ty::Unit), 0))
            }

            TypedStatement::Return { expr, .. } => {
                if let Some(expr) = expr {
                    let expr = state.get_expr_ref(*expr).clone();

                    let val = Self::compile_expr(state, &expr)?;

                    let obj_ty = state.tcx().get(expr.get_type()).clone();

                    state.retain_if_needed(val, &obj_ty);
                }

                let symbols = state.table.borrow().map.clone();

                for (_, sym) in symbols {
                    if sym.is_val && sym.symbol_ty.need_gc() {
                        let var = Variable::from_u32(sym.var_index as u32);
                        let val = state.builder.use_var(var);
                        state.emit_release(val);
                    }
                }

                let val = state
                    .builder
                    .ins()
                    .iconst(convert_type_to_cranelift_type(&Ty::Unit), 0);

                state.builder.ins().return_(&[val]);

                state.terminated = true;

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

                let block = state.get_stmt_ref(*block).clone();

                let TypedStatement::Block { statements, .. } = block else {
                    unreachable!();
                };

                for stmt in statements {
                    let stmt = state.get_stmt_ref(stmt).clone();

                    let TypedStatement::ExpressionStatement(_, expr, _) = stmt else {
                        continue;
                    };

                    let expr = state.get_expr_ref(expr);

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
                Ok(state
                    .builder
                    .ins()
                    .iconst(convert_type_to_cranelift_type(&Ty::Unit), 0))
            }

            _ => todo!("impl function 'compile_stmt'"),
        }
    }

    pub fn compile_expr(state: &mut FunctionState, expr: &TypedExpression) -> CompileResult<Value> {
        match expr {
            TypedExpression::Int { value, ty, .. } => Ok({
                let ty = state.tcx_ref().get(*ty).clone();

                state
                    .builder
                    .ins()
                    .iconst(convert_type_to_cranelift_type(&ty), int_value_to_imm(value))
            }),

            TypedExpression::Bool { value, ty, .. } => Ok({
                let ty = state.tcx_ref().get(*ty).clone();

                state
                    .builder
                    .ins()
                    .iconst(convert_type_to_cranelift_type(&ty), *value as i64)
            }),

            TypedExpression::SizeOf(_, it, ty) => {
                let val_ty = state.get_expr_ref(*it).get_type();
                let val_ty = &state.resolve_concrete_ty(val_ty, state.subst);

                let ty_size =
                    Self::get_type_size(state, val_ty, get_platform_width() as u32)? as i64;

                let ty = state.tcx_ref().get(*ty).clone();

                Ok(state
                    .builder
                    .ins()
                    .iconst(convert_type_to_cranelift_type(&ty), ty_size))
            }

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
                    if let Some(&func_id) = state.function_map.get(&it.value.to_string()) {
                        let func_ref = state
                            .module
                            .declare_func_in_func(func_id, &mut state.builder.func);
                        return Ok(state
                            .builder
                            .ins()
                            .func_addr(platform_width_to_int_type(), func_ref));
                    }

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

                    if state.tcx().get(*ty) == &Ty::Str {
                        return Ok(val_ptr);
                    }

                    let ty = state.resolve_concrete_ty(*ty, state.subst);

                    return Ok(state.builder.ins().load(
                        convert_type_to_cranelift_type(&ty),
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

            TypedExpression::FieldAccess(_, obj, field, _) => {
                let obj = state.get_expr_ref(*obj).clone();

                // 编译对象表达式
                let obj_ptr = Self::compile_expr(state, &obj)?;

                // 获取对象类型，确保是 struct
                let obj_ty = state.resolve_concrete_ty(obj.get_type(), state.subst);

                let name = if let Ty::Struct { name, .. } = &obj_ty {
                    name
                } else if let Ty::AppliedGeneric(it, _) = &obj_ty {
                    it
                } else {
                    return Err(format!(
                        "field access on `{}` type",
                        display_ty(&obj_ty, state.tcx_ref())
                    ));
                };

                let concrete_name = match &obj_ty {
                    Ty::Struct { name, .. } => name.clone(), // 普通结构体，直接用原名
                    Ty::AppliedGeneric(base_name, type_args) => {
                        instantiate_struct(state, base_name, type_args)?
                    }

                    _ => {
                        return Err(format!(
                            "field access on `{}` type",
                            display_ty(&obj_ty, state.tcx_ref())
                        ));
                    }
                };

                // 从符号表获取结构体布局
                let SymbolTy::Struct(layout) = state
                    .table
                    .borrow_mut()
                    .get(&concrete_name)
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
                let fields = fields
                    .iter()
                    .map(|(name, val_id)| (name.clone(), state.get_expr_cloned(*val_id)))
                    .collect();

                compile_build_struct(state, struct_name, &fields)
            }

            TypedExpression::Assign { left, right, .. } => {
                let left = state.get_expr_ref(*left).clone();
                let right = state.get_expr_ref(*right).clone();

                if let TypedExpression::Ident(ident, _) = &left {
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

                    let old_var_ty = state.resolve_concrete_ty(left.get_type(), state.subst);

                    let var = Variable::from_u32(var_symbol.var_index as u32);

                    if old_var_ty.need_gc() {
                        let old_val = state.builder.use_var(var);

                        state.update_ptr(new_val, old_val);
                    }

                    state.builder.def_var(var, new_val);

                    // unit
                    return Ok(state
                        .builder
                        .ins()
                        .iconst(convert_type_to_cranelift_type(&Ty::Unit), 0));
                } else if let TypedExpression::FieldAccess(_, obj, field, _) = left {
                    let obj = state.get_expr_ref(obj).clone();

                    let new_val = Self::compile_expr(state, &right)?;

                    // 编译对象表达式
                    let obj_ptr = Self::compile_expr(state, &obj)?;

                    // 获取对象类型，确保是 struct
                    let obj_ty = state.resolve_concrete_ty(obj.get_type(), state.subst);

                    let name = if let Ty::Struct { name, .. } = &obj_ty {
                        name
                    } else if let Ty::AppliedGeneric(it, _) = &obj_ty {
                        it
                    } else {
                        return Err(format!(
                            "field access on `{}` type",
                            display_ty(&obj_ty, state.tcx_ref())
                        ));
                    };

                    let concrete_name = match &obj_ty {
                        Ty::Struct { name, .. } => name.clone(), // 普通结构体，直接用原名
                        Ty::AppliedGeneric(base_name, type_args) => {
                            instantiate_struct(state, base_name, type_args)?
                        }

                        _ => {
                            return Err(format!(
                                "field access on `{}` type",
                                display_ty(&obj_ty, state.tcx_ref())
                            ));
                        }
                    };

                    // 从符号表获取结构体布局
                    let sym = state
                        .table
                        .borrow_mut()
                        .get(&concrete_name)
                        .ok_or_else(|| format!("undefined struct: `{}`", name))?;

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

                    // unit
                    return Ok(state
                        .builder
                        .ins()
                        .iconst(convert_type_to_cranelift_type(&Ty::Unit), 0));
                } else if let TypedExpression::Prefix {
                    op,
                    right: ptr_expr_id,
                    ..
                } = left
                {
                    if op.as_ref() == "*" {
                        // 编译指针所在的表达式, 得到内存地址
                        let ptr_val = state.get_expr_ref(ptr_expr_id).clone();
                        let addr_val = Self::compile_expr(state, &ptr_val)?;

                        // 获取新值的类型, 以便正确 retain/release
                        let val_ty = state
                            .resolve_concrete_ty(right.get_type(), state.subst)
                            .clone();

                        let new_val = Self::compile_expr(state, &right)?;

                        // 处理引用计数
                        if val_ty.need_gc() {
                            // 加载旧值用于 release
                            let cranelift_ty = convert_type_to_cranelift_type(&val_ty);
                            let old_val = state.builder.ins().load(
                                cranelift_ty,
                                MemFlags::new(),
                                addr_val,
                                0,
                            );

                            state.retain_if_needed(new_val, &val_ty);
                            state
                                .builder
                                .ins()
                                .store(MemFlags::new(), new_val, addr_val, 0);
                            state.release_if_needed(old_val, &val_ty);
                        } else {
                            // 普通类型直接 store
                            state
                                .builder
                                .ins()
                                .store(MemFlags::new(), new_val, addr_val, 0);
                        }

                        // unit
                        return Ok(state
                            .builder
                            .ins()
                            .iconst(convert_type_to_cranelift_type(&Ty::Unit), 0));
                    }

                    todo!()
                } else {
                    return Err("assign target must be ident or field or ptr".into());
                };
            }

            TypedExpression::Function {
                name,
                params,
                block: block_ast,
                generics_params: generics,
                ty: tyid,
                ..
            } => {
                let Ty::Function {
                    params_type,
                    ret_type,
                    ..
                } = state.tcx_ref().get(*tyid)
                else {
                    unreachable!()
                };

                if let Some(name) = name.as_ref() {
                    let name = &name.value;

                    // 1. 检查是否为泛型
                    if !generics.is_empty() {
                        let param_names = params
                            .iter()
                            .map(|it| {
                                let it = state.get_expr_ref(*it);

                                let TypedExpression::TypeHint(it, _, _) = it else {
                                    unreachable!()
                                };
                                it.value.clone()
                            })
                            .collect::<Vec<Arc<str>>>();

                        let generic_params = param_names
                            .iter()
                            .zip(params_type)
                            .map(|(name, id)| (name, state.tcx_ref().get(*id)))
                            .filter(|(_, it)| matches!(it, Ty::Generic(_, _)))
                            .map(|(name, ty)| (name.clone(), ty.to_string().into()))
                            .collect();

                        let all_params = param_names
                            .iter()
                            .zip(params_type)
                            .map(|(name, id)| (name.clone(), *id))
                            .collect();

                        state.push_generic(
                            name.to_string(),
                            GenericInfo::Function {
                                generic: generics
                                    .iter()
                                    .filter(|it| {
                                        matches!(
                                            state.get_expr_ref(**it),
                                            TypedExpression::Ident(..)
                                        )
                                    })
                                    .map(|it| {
                                        let it = state.get_expr_ref(*it);

                                        let TypedExpression::Ident(s, _) = it else {
                                            unreachable!()
                                        };
                                        s.value.clone()
                                    })
                                    .collect::<Vec<Arc<str>>>(),
                                generic_params,
                                all_params,
                                block: Box::new(state.get_expr_cloned(*block_ast)),
                                ret_ty: *ret_type,
                            },
                        );

                        return Ok(state.builder.ins().null(platform_width_to_int_type()));
                    }

                    let signature = make_signature(
                        &params_type
                            .iter()
                            .map(|it| state.tcx_ref().get(*it))
                            .collect::<Vec<&Ty>>(),
                        state.tcx_ref().get(*ret_type),
                    );

                    // 2. 首先声明函数
                    let func_id =
                        match state
                            .module
                            .declare_function(&name, Linkage::Export, &signature)
                        {
                            Ok(it) => it,
                            Err(it) => Err(it.to_string())?,
                        };

                    // 3. 立即将函数ID注册到function_map中
                    state.function_map.insert(name.to_string(), func_id);

                    let block_ast = state.get_expr_ref(*block_ast).clone();

                    return compile_function(
                        state,
                        signature,
                        &params
                            .iter()
                            .filter(|it| {
                                matches!(state.get_expr_ref(**it), TypedExpression::TypeHint(..))
                            })
                            .map(|it| {
                                let it = state.get_expr_ref(*it);

                                if let TypedExpression::TypeHint(it, _, id) = it {
                                    (it.value.clone(), *id)
                                } else {
                                    unreachable!()
                                }
                            })
                            .collect(),
                        block_ast.get_type(),
                        &block_ast,
                        name,
                        state.subst,
                    );
                }

                todo!()
            }

            TypedExpression::Call {
                func,
                args,
                func_ty,
                ..
            } => {
                let func = state.get_expr_ref(*func).clone();

                compile_call(state, &func, &args, func_ty)
            }

            TypedExpression::If {
                condition,
                consequence,
                else_block,
                ty,
                ..
            } => {
                let condition = state.get_expr_ref(*condition).clone();
                let consequence = state.get_expr_ref(*consequence).clone();

                let then_block = state.builder.create_block();
                let end_block = state.builder.create_block();

                let ty = state.resolve_concrete_ty(*ty, state.subst).clone();

                state
                    .builder
                    .append_block_param(end_block, convert_type_to_cranelift_type(&ty));

                let else_block_label = match else_block {
                    Some(_) => Some(state.builder.create_block()),
                    None => None,
                };

                let else_args = if else_block_label.is_none() {
                    vec![
                        state
                            .builder
                            .ins()
                            .iconst(convert_type_to_cranelift_type(&ty), 0),
                    ]
                } else {
                    vec![]
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
                    &else_args,
                );

                state.builder.switch_to_block(then_block);
                let val = Self::compile_expr(state, &consequence)?;
                state.builder.ins().jump(end_block, &[val]);
                state.builder.seal_block(then_block);

                if let Some(else_block_label) = else_block_label {
                    state.builder.switch_to_block(else_block_label);
                    let expr = state.get_expr_ref(else_block.unwrap()).clone();
                    let else_val = Self::compile_expr(state, &expr)?;
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
            } => {
                let left = state.get_expr_ref(*left).clone();
                let right = state.get_expr_ref(*right).clone();

                compile_infix(state, op.clone(), &left, &right)
            }

            TypedExpression::Block(_, it, ty) => {
                let ty = state.resolve_concrete_ty(*ty, state.subst);

                let mut ret_val = state
                    .builder
                    .ins()
                    .iconst(convert_type_to_cranelift_type(&ty), 0);

                for stmt in it {
                    let stmt = state.get_stmt_ref(*stmt).clone();

                    ret_val = Self::compile_stmt(state, &stmt)?;

                    if state.terminated {
                        break;
                    }
                }

                if ty == Ty::Unit {
                    Ok(state.builder.ins().iconst(convert_type_to_cranelift_type(&ty), 0))
                } else {
                    Ok(ret_val)
                }
            }

            TypedExpression::BoolAnd { left, right, .. } => {
                let left = state.get_expr_ref(*left).clone();
                let right = state.get_expr_ref(*right).clone();

                let true_block = state.builder.create_block();
                let false_block = state.builder.create_block();
                let merge_block = state.builder.create_block();

                state.builder.append_block_param(merge_block, types::I8);

                let lval = Self::compile_expr(state, &left)?;
                let zero = state.builder.ins().iconst(types::I8, 0);

                let cond = state.builder.ins().icmp(IntCC::NotEqual, lval, zero);

                // if left != 0 -> true_block else false_block
                state
                    .builder
                    .ins()
                    .brif(cond, true_block, &[], false_block, &[]);

                // true: 继续算 right
                state.builder.switch_to_block(true_block);
                let rval = Self::compile_expr(state, &right)?;
                state.builder.ins().jump(merge_block, &[rval]);
                state.builder.seal_block(true_block);

                // false: 直接 0
                state.builder.switch_to_block(false_block);
                state.builder.ins().jump(merge_block, &[zero]);
                state.builder.seal_block(false_block);

                state.builder.switch_to_block(merge_block);
                state.builder.seal_block(merge_block);

                Ok(state.builder.block_params(merge_block)[0])
            }

            TypedExpression::BoolOr { left, right, .. } => {
                let left = state.get_expr_ref(*left).clone();
                let right = state.get_expr_ref(*right).clone();

                let true_block = state.builder.create_block();
                let false_block = state.builder.create_block();
                let merge_block = state.builder.create_block();

                state.builder.append_block_param(merge_block, types::I8);

                let lval = Self::compile_expr(state, &left)?;
                let zero = state.builder.ins().iconst(types::I8, 0);

                let cond = state.builder.ins().icmp(IntCC::NotEqual, lval, zero);

                // if left != 0 -> true_block else false_block
                state
                    .builder
                    .ins()
                    .brif(cond, true_block, &[], false_block, &[]);

                // true: 直接 1
                state.builder.switch_to_block(true_block);
                let one = state.builder.ins().iconst(types::I8, 1);
                state.builder.ins().jump(merge_block, &[one]);
                state.builder.seal_block(true_block);

                // false: 才算 right
                state.builder.switch_to_block(false_block);
                let rval = Self::compile_expr(state, &right)?;
                state.builder.ins().jump(merge_block, &[rval]);
                state.builder.seal_block(false_block);

                state.builder.switch_to_block(merge_block);
                state.builder.seal_block(merge_block);

                Ok(state.builder.block_params(merge_block)[0])
            }

            TypedExpression::Prefix { op, right, ty, .. } => {
                if op.as_ref() == "*" {
                    // 1. 编译右侧表达式，得到指针的地址值
                    let ptr_val = Self::compile_expr(state, &state.get_expr_ref(*right).clone())?;

                    // 获取目标类型 (即指针指向的类型)
                    // 注: 这里的 ty 应该是解引用后的类型（例如从 *i32 变成 i32）
                    let target_ty = state.resolve_concrete_ty(*ty, state.subst);
                    let cranelift_ty = convert_type_to_cranelift_type(&target_ty);

                    // 3. 执行 load 操作
                    return Ok(state.builder.ins().load(
                        cranelift_ty,    // 读取出来的数据类型
                        MemFlags::new(), // 内存标记
                        ptr_val,         // 地址 Value
                        0,               // 偏移量（通常为 0）
                    ));
                }

                todo!("todo op {op}")
            }

            TypedExpression::Cast { val, ty, .. } => {
                let val_expr = state.get_expr_cloned(*val);
                let val = Self::compile_expr(state, &val_expr)?;
                compile_cast(state, val, val_expr.get_type(), *ty)
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
                    generic_map: &mut self.generic_map,
                    compiled_generic_map: &mut self.compiled_generic_map,
                    subst: &IndexMap::new(),

                    table: Rc::new(RefCell::new(SymbolTable::from_outer(self.table))),
                    typed_module: &mut self.typed_module,

                    arc_alloc: self.arc_alloc,
                    arc_retain: self.arc_retain,
                    arc_release: self.arc_release,

                    terminated: false,
                };

                for stmt in statements {
                    let stmt = state.typed_module.get_stmt(stmt).unwrap().clone();

                    ret_val = Self::compile_stmt(&mut state, &stmt)?;

                    if state.terminated {
                        break;
                    }
                }

                if !state.terminated {
                    state.builder.ins().return_(&[ret_val]);
                }

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
                generic_map: &mut self.generic_map,
                compiled_generic_map: &mut self.compiled_generic_map,

                table: self.table,
                typed_module: &mut self.typed_module,

                arc_alloc: self.arc_alloc,
                arc_retain: self.arc_retain,
                arc_release: self.arc_release,
            };

            for stmt in statements {
                let stmt = state.typed_module.get_stmt(stmt).unwrap().clone();

                if !Self::is_top_level_stmt(&state, &stmt) {
                    continue;
                }

                Self::compile_top_level_stmt(&mut state, &stmt)?;
            }

            match self.context.verify(self.target_isa.as_ref()) {
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

        let obj = self.module.finish();
        Ok(obj.emit().unwrap().to_vec())
    }
}

impl<'a> Compiler<'a> {
    pub fn substitute(
        tcx: &ant_type_checker::ty_context::TypeContext,
        param_ty: &ant_type_checker::ty::Ty,
        arg_tyid: ant_type_checker::ty::TyId,
        mapping: &mut indexmap::IndexMap<std::sync::Arc<str>, ant_type_checker::ty::TyId>,
    ) {
        use ant_type_checker::ty::Ty;

        // 获取实参的真实类型
        let arg_ty = tcx.get(arg_tyid);

        match (param_ty, arg_ty) {
            (Ty::Generic(..), Ty::Generic(..)) if param_ty == arg_ty => {
                // 给给(Generic, Generic)列车, 不许发车!
            }

            (Ty::Generic(name, _), _) => {
                mapping.insert(name.clone(), arg_tyid);
            }

            (Ty::Ptr(p_inner), Ty::Ptr(a_inner)) => {
                Self::substitute(tcx, tcx.get(*p_inner), *a_inner, mapping);
            }

            (
                Ty::AppliedGeneric(param_name, param_args),
                Ty::AppliedGeneric(applied_name, applied_args),
            ) => {
                if param_name == applied_name {
                    for (param, arg) in param_args.iter().zip(applied_args.iter()) {
                        Self::substitute(tcx, tcx.get(*param), *arg, mapping);
                    }
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, path::Path, rc::Rc};

    use ant_lexer::Lexer;
    use ant_parser::Parser;

    use ant_type_checker::{
        TypeChecker,
        module::TypedModule,
        ty_context::TypeContext,
        type_infer::{TypeInfer, infer_context::InferContext},
    };

    use crate::compiler::{Compiler, compile_to_executable, create_target_isa, table::SymbolTable};

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

        let mut tcx = TypeContext::new();

        let mut module = TypedModule::new(&mut tcx);

        let checker = &mut TypeChecker::new(&mut module);

        let typed_node = checker.check_node(node).unwrap();

        let constraints = checker.get_constraints().to_vec();

        let mut infer_ctx = InferContext::new(&mut module);

        let mut type_infer = TypeInfer::new(&mut infer_ctx);

        type_infer.unify_all(constraints).unwrap();

        // 创建编译器实例
        let table = SymbolTable::new();

        let compiler = Compiler::new(
            target_isa,
            "__simple_program__".into(),
            Rc::new(RefCell::new(table)),
            module,
        );

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
