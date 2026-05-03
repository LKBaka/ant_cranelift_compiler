use std::sync::Arc;

use ant_ast::node::GetToken;
use ant_id::ExprId;
use ant_token::{token::Token, token_type::TokenType};
use ant_ty::{Ty, TyId};
use ant_typed_ast::{GetType, typed_expr::TypedExpression, typed_expressions::ident::Ident};
use ant_typed_module::display_ty;
use cranelift::prelude::{AbiParam, InstBuilder, MemFlags, Signature, Value, types};
use cranelift_codegen::{ir::FuncRef, isa::CallConv};
use cranelift_module::{Linkage, Module};
use indexmap::IndexMap;

use crate::compiler::{
    CompileResult, Compiler, FunctionState,
    compile_state_impl::PushGetGeneric,
    constants::CALL_CONV,
    convert_type::convert_type_to_cranelift_type,
    function::{compile_function, make_signature},
    generic::{CompiledGenericInfo, GenericInfo, mangle_generic},
    imm::platform_width_to_int_type,
};

fn compile_call_method(
    state: &mut FunctionState,
    func: &TypedExpression,
    args: &Vec<ExprId>,
    func_ty: &TyId,

    original_func_ret_ty: &usize,
    original_func_generics: &Vec<Arc<str>>,

    field: &Ident,

    params_type: &Vec<usize>,
    ret_type: &usize,

    name: &Arc<str>,
) -> CompileResult<Value> {
    // 函数名重命名
    let func_name = format!("{}__{}", name, field.value);

    let func_ty_id = state.tcx().alloc(Ty::Function {
        params_type: params_type.clone(),
        ret_type: ret_type.clone(),
        is_variadic: false,
        generics: original_func_generics.clone(),
    });

    let call_expr = TypedExpression::Call {
        token: Token::new(
            "(".into(),
            TokenType::LParen,
            func.token().value,
            func.token().line,
            func.token().column,
        ), // 充填伪造 Token
        func: state.typed_module.alloc_expr(TypedExpression::Ident(
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
            None,
        )),
        args: args.clone(),
        func_ty: func_ty_id,
        ret_ty: *original_func_ret_ty,
    };

    return Compiler::compile_expr(state, &call_expr);
}

fn compile_call_generic(
    state: &mut FunctionState,
    func: &TypedExpression,
    args: &Vec<ExprId>,
    func_ty: &TyId,
) -> CompileResult<Value> {
    let TypedExpression::Ident(name_ident, ..) = func else {
        return Err(format!("unsupport generic lambda fucntion"));
    };

    let ty = state.resolve_concrete_ty(*func_ty, state.subst);
    let Ty::Function {
        ret_type: ret_type_infer,
        ..
    } = ty
    else {
        return Err(format!(
            "not a function: {}",
            display_ty(&ty, state.tcx_ref())
        ));
    };

    let name = &name_ident.value;

    let GenericInfo::Function {
        all_params,
        ret_ty,
        block,
        ..
    } = state
        .generic_map
        .get(name.as_ref())
        .map_or_else(
            || Err(format!("generic function `{name}` not in generic map")),
            |it| Ok(it),
        )?
        .clone()
    else {
        return Err(format!("`{name}` is not a generic function"));
    };

    let arg_tyids = args
        .iter()
        .map(|it| {
            let expr_ty = state.get_expr_ref(*it).get_type();
            let resolved = state.resolve_concrete_ty(expr_ty, state.subst);
            state.tcx().alloc(resolved)
        })
        .collect::<Vec<TyId>>();

    let arg_types = arg_tyids
        .iter()
        .map(|it| state.typed_module.tcx_ref().get(*it).clone())
        .collect::<Vec<_>>();

    let mangled_func_name = mangle_generic(&name, &arg_types);

    let mut generic_param_to_real_types = IndexMap::new();

    for ((_, generic_param_id), arg_tyid) in all_params.iter().zip(&arg_tyids) {
        let param_ty = state.tcx_ref().get(*generic_param_id).clone();
        Compiler::substitute(
            state.tcx_ref(),
            &param_ty,
            *arg_tyid,
            &mut generic_param_to_real_types,
        );
    }

    let def_ret_ty_obj = state.tcx_ref().get(ret_ty).clone();
    Compiler::substitute(
        state.tcx_ref(),
        &def_ret_ty_obj,
        ret_type_infer,
        &mut generic_param_to_real_types,
    );

    let concrete_param_types: Vec<_> = all_params
        .iter()
        .map(|(name, id)| {
            (
                name.clone(),
                state
                    .resolve_concrete_ty(*id, &generic_param_to_real_types)
                    .clone(),
            )
        })
        .collect();

    let concrete_ret_ty = state.resolve_concrete_ty(ret_ty, &generic_param_to_real_types);

    let signature = make_signature(
        concrete_param_types
            .clone()
            .into_iter()
            .map(|(_, tyid)| tyid)
            .collect::<Vec<_>>()
            .as_slice(),
        &concrete_ret_ty,
    );

    // 声明并注册函数
    let func_id =
        match state
            .module
            .declare_function(&mangled_func_name, Linkage::Export, &signature)
        {
            Ok(it) => it,
            Err(it) => Err(it.to_string())?,
        };

    state
        .function_map
        .insert(mangled_func_name.to_string(), func_id);

    let block = block.clone();

    let concrete_param_ty_ids = concrete_param_types
        .clone()
        .into_iter()
        .map(|(name, ty)| (name, state.tcx().alloc(ty)))
        .collect::<Vec<_>>();

    let concrete_ret_ty_id = state.tcx().alloc(concrete_ret_ty);

    state.push_compiled_generic(
        name.to_string(),
        CompiledGenericInfo::Function {
            new_name: mangled_func_name.clone(),
            new_params: concrete_param_ty_ids.clone().into_iter().collect(),
            new_ret_ty: concrete_ret_ty_id,
        },
    );

    compile_function(
        state,
        signature,
        &concrete_param_ty_ids,
        concrete_ret_ty_id,
        &block,
        &mangled_func_name,
        &generic_param_to_real_types,
    )?;

    compile_call(state, func, args, func_ty)
}

pub fn compile_call(
    state: &mut FunctionState,
    func: &TypedExpression,
    args: &Vec<ExprId>,
    func_ty: &TyId,
) -> CompileResult<Value> {
    let (mut params_type, mut ret_ty, va_arg, generics) =
        match state.tcx_ref().get(*func_ty).clone() {
            Ty::Function {
                params_type,
                ret_type,
                is_variadic,
                generics,
                ..
            } => (params_type.clone(), ret_type, is_variadic, generics),
            _ => unreachable!(),
        };

    if let TypedExpression::FieldAccess(_, obj, field, _) = func {
        let obj_ty = state.resolve_concrete_ty(state.get_expr_ref(*obj).get_type(), state.subst);

        // 提取结构体名称
        let struct_name = match obj_ty {
            Ty::Struct { name, .. } => name.clone(),
            Ty::AppliedGeneric(name, _) => name.clone(),
            _ => return Err(format!("call method on non-struct type")),
        };

        return compile_call_method(
            state,
            func,
            args,
            func_ty,
            &ret_ty,
            &generics,
            field,
            &params_type,
            &ret_ty,
            &struct_name,
        );
    }

    if let TypedExpression::Ident(Ident { value: name, .. }, ..) = func
        && let Some(GenericInfo::Function { .. }) = state.generic_map.get(name.as_ref())
        && !state.compiled_generic_map.contains_key(name.as_ref())
    {
        return compile_call_generic(state, func, args, func_ty);
    }

    let mut func_id = if let TypedExpression::Ident(ident, ..) = func {
        state.function_map.get(&ident.value.to_string()).copied()
    } else {
        None
    };

    if let TypedExpression::Ident(Ident { value: name, .. }, ..) = func
        && let Some(GenericInfo::Function { .. }) = state.generic_map.get(name.as_ref())
        && state.compiled_generic_map.contains_key(name.as_ref())
        && let Some(CompiledGenericInfo::Function {
            new_params,
            new_ret_ty,
            new_name,
        }) = state.get_compiled_generic(name.as_ref()).cloned()
    {
        params_type = new_params.iter().map(|(_, id)| *id).collect();
        ret_ty = new_ret_ty;

        func_id = state.function_map.get(&new_name).copied()
    }

    let direct_func: Option<FuncRef> = func_id.map(|fid| {
        state
            .module
            .declare_func_in_func(fid, &mut state.builder.func)
    });

    // 编译所有参数
    let mut arg_values = Vec::new();

    if va_arg {
        for arg in args {
            let arg = state.get_expr_ref(*arg).clone();
            let cl_ty = convert_type_to_cranelift_type(state.tcx_ref().get(arg.get_type()));

            let mut arg_val = Compiler::compile_expr(state, &arg)?;

            if (cl_ty == types::F64 || cl_ty == types::F32)
                && CALL_CONV == CallConv::WindowsFastcall
            {
                let f64_val = if cl_ty != types::F64 {
                    state.builder.ins().fpromote(types::F64, arg_val)
                } else {
                    arg_val
                };

                // Windows 变长参数规则：将浮点数通过位转换视为整数传递
                // 这样它会进入 RDX/R8/R9 寄存器，而不是只在 XMM 里
                arg_val = state
                    .builder
                    .ins()
                    .bitcast(types::I64, MemFlags::new(), f64_val);
            }

            arg_values.push(arg_val);
        }
    } else {
        for (arg, arg_ty) in args.iter().zip(params_type.iter()) {
            let arg = state.get_expr_ref(*arg).clone();

            let v = Compiler::compile_expr(state, &arg)?;
            let arg_ty = state.tcx().get(*arg_ty).clone();
            state.retain_if_needed(v, &arg_ty);
            arg_values.push(v);
        }
    }

    if let Some(fref) = direct_func
        && !va_arg
    {
        // 直接 call
        let call = state.builder.ins().call(fref, &arg_values);
        return Ok(state
            .builder
            .inst_results(call)
            .first()
            .copied()
            .unwrap_or_else(|| state.builder.ins().iconst(platform_width_to_int_type(), 0)));
    }

    let func_val = Compiler::compile_expr(state, &func)?;

    // 创建函数签名
    let mut sig = Signature::new(CALL_CONV);

    if va_arg {
        for arg in args {
            let arg = state.get_expr_ref(*arg).clone();

            let cl_ty = convert_type_to_cranelift_type(state.tcx().get(arg.get_type()));

            sig.params.push(AbiParam::new(
                if (cl_ty == types::F64 || cl_ty == types::F32)
                    && CALL_CONV == CallConv::WindowsFastcall
                {
                    types::I64
                } else {
                    cl_ty
                },
            ));
        }
    } else {
        for param_ty in &params_type {
            sig.params
                .push(AbiParam::new(convert_type_to_cranelift_type(
                    state.tcx().get(*param_ty),
                )));
        }
    }

    let ret_ty = state.tcx().get(ret_ty);

    if *ret_ty != Ty::Unit {
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
        let ty = state.tcx().get(*ty).clone();
        state.release_if_needed(*val, &ty);
    }

    Ok(result)
}
