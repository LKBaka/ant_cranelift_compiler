use std::sync::Arc;

use ant_ast::node::GetToken;
use ant_token::{token::Token, token_type::TokenType};
use ant_type_checker::{
    ty::{Ty, TyId},
    typed_ast::{GetType, typed_expr::TypedExpression, typed_expressions::ident::Ident},
};
use cranelift::prelude::{AbiParam, InstBuilder, Signature, Value};
use cranelift_codegen::ir::FuncRef;
use cranelift_module::{Linkage, Module};
use indexmap::IndexMap;

use crate::compiler::{
    CompileResult, Compiler, FunctionState,
    compile_state_impl::PushGetGeneric,
    constants::CALL_CONV,
    convert_type::convert_type_to_cranelift_type,
    function::{compile_function, make_signature, mangle_func},
    generic::{CompiledGenericInfo, GenericInfo},
    imm::platform_width_to_int_type,
};

fn compile_call_method(
    state: &mut FunctionState,
    func: &Box<TypedExpression>,
    args: &Vec<Box<TypedExpression>>,
    func_ty: &TyId,

    original_func_ret_ty: &usize,

    field: &Ident,

    params_type: &Vec<usize>,
    ret_type: &usize,

    name: &Arc<str>,
) -> CompileResult<Value> {
    // 函数名重命名
    let func_name = format!("{}__{}", name, field.value);

    let func_ty_id = state.tcx.alloc(Ty::Function {
        params_type: params_type.clone(),
        ret_type: ret_type.clone(),
        is_variadic: false,
    });

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
        func_ty: func_ty_id,
        ret_ty: *original_func_ret_ty,
    };

    return Compiler::compile_expr(state, &call_expr);
}

fn compile_call_generic(
    state: &mut FunctionState,
    func: &Box<TypedExpression>,
    args: &Vec<Box<TypedExpression>>,
    func_ty: &TyId,
) -> CompileResult<Value> {
    let TypedExpression::Ident(name_ident, _) = &**func else {
        return Err(format!("unsupport generic lambda fucntion"));
    };

    let name = &name_ident.value;

    let GenericInfo::Function {
        all_params,
        ret_ty,
        block,
        ..
    } = state.generic_map.get(name.as_ref()).map_or_else(
        || Err(format!("generic function `{name}` not in generic map")),
        |it| Ok(it),
    )?
    else {
        return Err(format!("`{name}` is not a generic function"));
    };

    let arg_tyids = args.iter().map(|it| it.get_type()).collect::<Vec<TyId>>();

    let arg_types = arg_tyids
        .iter()
        .map(|it| state.tcx.get(*it))
        .collect::<Vec<&Ty>>();

    let mangled_func_name = mangle_func(&name, &arg_types);

    let mut new_param_types = vec![];
    let mut generic_param_to_real_types = IndexMap::new();

    for ((param_name, param_tyid), arg_tyid) in all_params.iter().zip(arg_tyids) {
        let param_ty = state.tcx.get(*param_tyid);

        if !matches!(param_ty, Ty::Generic(..)) {
            // 不是泛型参数 选用默认的形参类型
            new_param_types.push((param_name.clone(), *param_tyid));
        } else if let Ty::Generic(name, _need_traits) = param_ty {
            // 是泛型参数 选用实参的类型
            new_param_types.push((param_name.clone(), arg_tyid));

            generic_param_to_real_types.insert(name.clone(), arg_tyid);
        }
    }

    let ret_tyid = if let Ty::Generic(name, _need_traits) = state.tcx.get(*ret_ty) {
        // 是泛型 从泛型表中获取实际类型
        let Some(real_ty) = generic_param_to_real_types.get(name.as_ref()) else {
            return Err(format!("type `{name}` not found"));
        };

        *real_ty
    } else {
        // 不是泛型 直接使用原有类型
        *ret_ty
    };

    let signature = make_signature(
        &new_param_types
            .iter()
            .map(|(_, tyid)| state.tcx.get(*tyid))
            .collect::<Vec<_>>(),
        state.tcx.get(ret_tyid),
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

    let _ = all_params;
    let _ = ret_ty;

    let block = block.clone();

    state.push_compiled_generic(
        name.to_string(),
        CompiledGenericInfo::Function {
            new_name: mangled_func_name.clone(),
            new_params: new_param_types.clone().into_iter().collect(),
            new_ret_ty: ret_tyid,
        },
    );

    compile_function(
        state,
        signature,
        &new_param_types,
        ret_tyid,
        &block,
        &mangled_func_name,
    )?;

    compile_call(state, func, args, func_ty)
}

pub fn compile_call(
    state: &mut FunctionState,
    func: &Box<TypedExpression>,
    args: &Vec<Box<TypedExpression>>,
    func_ty: &TyId,
) -> CompileResult<Value> {
    let (mut params_type, mut ret_ty, va_arg) = match state.tcx.get(*func_ty) {
        Ty::Function {
            params_type,
            ret_type,
            is_variadic,
        } => (params_type.clone(), *ret_type, *is_variadic),
        _ => unreachable!(),
    };

    if let TypedExpression::FieldAccess(obj, field, _) = &**func
        && let Ty::Struct { name, fields, .. } = state.tcx.get(obj.get_type()).clone()
        && let Some(Ty::Function {
            params_type,
            ret_type,
            ..
        }) = fields
            .get(&field.value)
            .and_then(|ty| Some(state.tcx.get(*ty)))
            .cloned()
    {
        return compile_call_method(
            state,
            func,
            args,
            func_ty,
            &ret_ty,
            field,
            &params_type,
            &ret_type,
            &name,
        );
    }

    if let TypedExpression::Ident(Ident { value: name, .. }, _) = &**func
        && let Some(GenericInfo::Function { .. }) = state.generic_map.get(name.as_ref())
        && !state.compiled_generic_map.contains_key(name.as_ref())
    {
        return compile_call_generic(state, func, args, func_ty);
    }

    let mut func_id = if let TypedExpression::Ident(ident, _) = &**func {
        state.function_map.get(&ident.value.to_string()).copied()
    } else {
        None
    };

    if let TypedExpression::Ident(Ident { value: name, .. }, _) = &**func
        && let Some(GenericInfo::Function { .. }) = state.generic_map.get(name.as_ref())
        && state.compiled_generic_map.contains_key(name.as_ref())
        && let Some(CompiledGenericInfo::Function {
            new_params,
            new_ret_ty,
            new_name,
        }) = state.pop_compiled_generic(name.as_ref())
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
            let arg_val = Compiler::compile_expr(state, arg)?;
            arg_values.push(arg_val);
        }
    } else {
        for (arg, arg_ty) in args.iter().zip(params_type.iter()) {
            let v = Compiler::compile_expr(state, arg)?;
            let arg_ty = state.tcx.get(*arg_ty).clone();
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
            sig.params
                .push(AbiParam::new(convert_type_to_cranelift_type(
                    state.tcx.get(arg.get_type()),
                )));
        }
    } else {
        for param_ty in &params_type {
            sig.params
                .push(AbiParam::new(convert_type_to_cranelift_type(
                    state.tcx.get(*param_ty),
                )));
        }
    }

    let ret_ty = state.tcx.get(ret_ty);

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
        let ty = state.tcx.get(*ty).clone();
        state.release_if_needed(*val, &ty);
    }

    Ok(result)
}
