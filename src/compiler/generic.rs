use std::sync::Arc;

use indexmap::IndexMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GenericInfo {
    Struct {
        generic_params: Vec<Arc<str>>,

        /// generic_field, generic_param_name
        generic_fields: IndexMap<Arc<str>, Arc<str>>
    }
}