use std::sync::Arc;

use ant_type_checker::{
    ty::TyId,
    typed_ast::{GetType, typed_expr::TypedExpression, typed_expressions::ident::Ident},
};
use cranelift::prelude::{InstBuilder, MemFlags, Value};
use indexmap::IndexMap;

use crate::compiler::{
    CompileResult, CompileState, Compiler, FunctionState,
    compile_state_impl::PushGetGeneric,
    generic::GenericInfo,
    generic::mangle_generic,
    imm::platform_width_to_int_type,
    table::{StructLayout, SymbolTy},
};

fn get_or_build_struct_layout(
    state: &mut FunctionState,
    struct_name: &str,
    fields: &IndexMap<Ident, TypedExpression>,
) -> CompileResult<StructLayout> {
    if let Some(GenericInfo::Struct { .. }) = state.get_generic(struct_name) {
        let mut field_to_val_ty_mapping = IndexMap::new();

        for (field, val_expr) in fields {
            field_to_val_ty_mapping.insert(field.value.clone(), val_expr.get_type());
        }

        let mut fields = vec![];

        for (field, tyid) in &field_to_val_ty_mapping {
            // 相信外部数据 这里不作检查
            fields.push((field.clone(), state.tcx_ref().get(*tyid).clone()));
        }

        let mut field_refs = vec![];

        for (_, tyid) in field_to_val_ty_mapping {
            // 相信外部数据 这里不作检查
            field_refs.push(state.tcx_ref().get(tyid));
        }

        let mangled = mangle_generic(struct_name, &field_refs);

        let layout =
            Compiler::compile_struct_layout(state, &mangled.clone().into(), fields.as_slice())?;

        state
            .table
            .borrow_mut()
            .define_struct_type(&mangled, layout.clone());

        Ok(layout)
    } else if let SymbolTy::Struct(layout) =
        state.table.borrow_mut().get(&struct_name).map_or_else(
            || Err(format!("undefined struct: {struct_name}")),
            |it| Ok(it.symbol_ty),
        )?
    {
        Ok(layout)
    } else {
        Err(format!("not a struct: {struct_name}"))
    }
}

pub fn compile_build_struct(
    state: &mut FunctionState,
    struct_name: &Ident,
    fields: &IndexMap<Ident, TypedExpression>,
) -> CompileResult<Value> {
    let layout = get_or_build_struct_layout(state, &struct_name.value, fields)?;

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

        let field_val = Compiler::compile_expr(state, field_expr)?;
        state
            .builder
            .ins()
            .store(MemFlags::new(), field_val, field_ptr, 0);
    }

    // ref_count = 1，由 arc.c 保证
    Ok(struct_ptr)
}

pub fn instantiate_struct<'aa, 'b>(
    state: &mut FunctionState<'aa, 'b>,
    base_name: &Arc<str>,
    type_args: &[TyId],
) -> CompileResult<Arc<str>> {
    let concrete_name: Arc<str> = mangle_generic(
        base_name,
        &type_args
            .iter()
            .map(|it| state.get_typed_module_ref().tcx_ref().get(*it))
            .collect::<Vec<_>>(),
    )
    .into();

    // 检查是否已经实例化过
    if state.get_table().borrow_mut().get(&concrete_name).is_some() {
        return Ok(concrete_name);
    }

    // 从 generic_map 获取模板
    let info = state
        .get_generic_map()
        .get(&base_name.to_string())
        .cloned()
        .ok_or_else(|| format!("{} not a generic struct", base_name))?;

    if let GenericInfo::Struct {
        generic_params,
        fields,
        ..
    } = info
    {
        // 构造映射表
        let mut subst = IndexMap::new();
        for (param_name, concrete_ty_id) in generic_params.iter().zip(type_args) {
            subst.insert(param_name.clone(), *concrete_ty_id);
        }

        let mut new_fields = Vec::new();
        for (field_name, field_ty) in fields {
            if field_name.as_ref() == "__ref_count__" {
                continue;
            }

            let resolved_ty = state.resolve_concrete_ty(field_ty, &subst);

            new_fields.push((field_name, resolved_ty));
        }

        let layout = Compiler::compile_struct_layout(state, &concrete_name, &new_fields)?;

        state
            .get_table()
            .borrow_mut()
            .define_struct_type(&concrete_name, layout);
    }

    Ok(concrete_name)
}
