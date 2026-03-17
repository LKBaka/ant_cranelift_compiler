use std::sync::Arc;

use ant_type_checker::{ty::{Ty, TyId}, typed_ast::typed_expr::TypedExpression};
use indexmap::IndexMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GenericInfo {
    Struct {
        /// T, K, V ...
        generic_params: Vec<Arc<str>>,

        /// field, field_ty
        fields: IndexMap<Arc<str>, TyId>
    },

    Function {
        /// T, K, V
        generic: Vec<Arc<str>>,

        /// item: T, k: K, v: V
        generic_params: IndexMap<Arc<str>, Arc<str>>,

        all_params: IndexMap<Arc<str>, TyId>,

        block: Box<TypedExpression>,

        ret_ty: TyId,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompiledGenericInfo {
    Function {
        new_name: String,
        
        new_params: IndexMap<Arc<str>, TyId>,

        new_ret_ty: TyId,
    }
}

pub fn mangle_generic(name: &str, args: &[&Ty]) -> String {
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