use ant_type_checker::typed_ast::{
    GetType, typed_expr::TypedExpression, typed_expressions::ident::Ident,
};
use cranelift::prelude::{InstBuilder, MemFlags, Value};
use indexmap::IndexMap;

use crate::compiler::{
    CompileResult, Compiler, FunctionState, compile_state_impl::PushGetGeneric, generic::GenericInfo, imm::platform_width_to_int_type, table::{StructLayout, SymbolTy}
};

fn get_or_build_struct_layout(
    state: &mut FunctionState,
    struct_name: &Ident,
    fields: &IndexMap<Ident, TypedExpression>,
) -> CompileResult<StructLayout> {
    if let Some(GenericInfo::Struct { .. }) = state.get_generic(&struct_name.to_string()) {
        let mut field_to_val_ty_mapping = IndexMap::new();

        for (field, val_expr) in fields {
            field_to_val_ty_mapping.insert(field.value.clone(), val_expr.get_type());
        }

        let mut fields = vec![];

        for (field, tyid) in field_to_val_ty_mapping {
            // 相信外部数据 这里不作检查
            fields.push((field, state.tcx.get(tyid).clone()));
        }

        let layout = Compiler::compile_struct_layout(state, &struct_name.value, fields.as_slice())?;

        state
            .table
            .borrow_mut()
            .define_struct_type(&struct_name.value, layout.clone());

        Ok(layout)
    } else if let SymbolTy::Struct(layout) = state
        .table
        .borrow_mut()
        .get(&struct_name.value)
        .map_or_else(
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
    let layout = get_or_build_struct_layout(state, struct_name, fields)?;

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
