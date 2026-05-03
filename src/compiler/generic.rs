use std::sync::Arc;

use ant_ty::{Ty, TyId};
use ant_typed_ast::typed_expr::TypedExpression;
use indexmap::IndexMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GenericInfo {
    Struct {
        /// T, K, V ...
        generic_params: Vec<Arc<str>>,

        /// field, field_ty
        fields: IndexMap<Arc<str>, TyId>,
    },

    Function {
        /// T, K, V
        generic: Vec<Arc<str>>,

        all_params: IndexMap<Arc<str>, TyId>,

        block: Box<TypedExpression>,

        ret_ty: TyId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompiledGenericInfo {
    Function {
        new_name: String,

        new_params: IndexMap<Arc<str>, TyId>,

        new_ret_ty: TyId,
    },
}

pub fn mangle_generic(name: &str, args: &[Ty]) -> String {
    if args.is_empty() {
        return name.to_string();
    }

    let mut res = name.to_string();
    res.push_str("__");

    for arg in args {
        res.push_str(&format!("_{}", arg));
    }

    res
}

pub fn mangle_method(name: &str, parent_ty: &str) -> String {
    if parent_ty.is_empty() {
        return name.to_string();
    }

    format!("{name}__{parent_ty}")
}