use std::sync::Arc;

use ant_type_checker::{ty::TyId, typed_ast::typed_expr::TypedExpression};
use indexmap::IndexMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GenericInfo {
    Struct {
        /// T, K, V ...
        generic_params: Vec<Arc<str>>,

        /// generic_field, generic_param_name
        generic_fields: IndexMap<Arc<str>, Arc<str>>
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