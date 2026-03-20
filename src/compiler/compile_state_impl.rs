use std::{cell::RefCell, rc::Rc};

use ant_ast::{ExprId, StmtId};
use ant_type_checker::{
    ty::Ty,
    ty_context::TypeContext,
    typed_ast::{typed_expr::TypedExpression, typed_stmt::TypedStatement},
};

use crate::compiler::{
    CompileState, FunctionState, GlobalState,
    generic::{CompiledGenericInfo, GenericInfo},
    table::SymbolTable,
};

#[allow(unused)]
impl<'a> FunctionState<'a, '_> {
    pub fn enter_scope(&mut self) {
        let outer = self.table.clone();

        self.table = Rc::new(RefCell::new(SymbolTable::new()));
        self.table.borrow_mut().outer = Some(outer);
    }

    pub fn leave_scope(&mut self) -> Option<Rc<RefCell<SymbolTable>>> {
        let outer = self.table.borrow().outer.as_ref()?.clone();

        let before_leave_table = self.table.clone();

        self.table = outer;

        Some(before_leave_table)
    }
}

pub trait PushGetGeneric {
    fn push_compiled_generic(&mut self, name: String, info: CompiledGenericInfo);
    fn get_compiled_generic(&mut self, name: &str) -> Option<&CompiledGenericInfo>;

    fn push_generic(&mut self, name: String, info: GenericInfo);

    fn get_mut_generic(&mut self, name: &str) -> Option<&mut GenericInfo>;
    fn get_generic(&mut self, name: &str) -> Option<GenericInfo>;
}

impl<'b, 'a, T: CompileState<'a, 'b>> PushGetGeneric for T {
    fn push_generic(&mut self, name: String, info: GenericInfo) {
        self.get_generic_map().insert(name, info);
    }

    fn get_generic(&mut self, name: &str) -> Option<GenericInfo> {
        self.get_generic_map().get(name).cloned()
    }

    fn get_mut_generic(&mut self, name: &str) -> Option<&mut GenericInfo> {
        self.get_generic_map().get_mut(name)
    }

    fn push_compiled_generic(&mut self, name: String, info: CompiledGenericInfo) {
        self.get_compiled_generic_map().insert(name, info);
    }

    fn get_compiled_generic(&mut self, name: &str) -> Option<&CompiledGenericInfo> {
        self.get_compiled_generic_map().get(name)
    }
}

impl FunctionState<'_, '_> {
    pub fn get_expr_ref(&self, id: ExprId) -> &TypedExpression {
        self.typed_module.get_expr(id).unwrap()
    }

    pub fn get_expr_cloned(&self, id: ExprId) -> TypedExpression {
        self.typed_module.get_expr(id).unwrap().clone()
    }

    pub fn get_stmt_ref(&self, id: StmtId) -> &TypedStatement {
        self.typed_module.get_stmt(id).unwrap()
    }

    pub fn get_stmt_cloned(&self, id: StmtId) -> TypedStatement {
        self.typed_module.get_stmt(id).unwrap().clone()
    }
}

impl GlobalState<'_, '_> {
    pub fn get_expr_ref(&self, id: ExprId) -> &TypedExpression {
        self.typed_module.get_expr(id).unwrap()
    }

    pub fn get_expr_cloned(&self, id: ExprId) -> TypedExpression {
        self.typed_module.get_expr(id).unwrap().clone()
    }

    pub fn get_stmt_ref(&self, id: StmtId) -> &TypedStatement {
        self.typed_module.get_stmt(id).unwrap()
    }

    pub fn get_stmt_cloned(&self, id: StmtId) -> TypedStatement {
        self.typed_module.get_stmt(id).unwrap().clone()
    }
}

impl<'b, 'a> FunctionState<'a, 'b> {
    pub fn tcx(&mut self) -> &mut TypeContext {
        self.typed_module.tcx_mut()
    }

    pub fn tcx_ref(&self) -> &TypeContext {
        self.typed_module.tcx_ref()
    }
}

impl<'b, 'a> GlobalState<'a, 'b> {
    pub fn tcx(&mut self) -> &mut TypeContext {
        self.typed_module.tcx_mut()
    }

    pub fn tcx_ref(&self) -> &TypeContext {
        self.typed_module.tcx_ref()
    }
}

impl<'b, 'a> FunctionState<'a, 'b> {
    pub fn resolve_concrete_ty(
        &mut self,
        id: ant_type_checker::ty::TyId,
        mapping: &indexmap::IndexMap<std::sync::Arc<str>, ant_type_checker::ty::TyId>,
    ) -> Ty {
        let ty = self.typed_module.tcx_ref().get(id).clone();

        match ty {
            Ty::Generic(ref name, _) => {
                let Some(&real_tyid) = mapping.get(name) else {
                    return ty; // 查不到原样返回
                };

                // 检查防止无限递归
                if real_tyid == id {
                    return self.tcx_ref().get(real_tyid).clone();
                }

                self.resolve_concrete_ty(real_tyid, mapping)
            }

            Ty::Ptr(inner) => {
                let concrete = self.resolve_concrete_ty(inner, mapping);
                Ty::Ptr(self.typed_module.tcx_mut().alloc(concrete))
            }

            Ty::AppliedGeneric(name, args) => {
                let concrete_args = args
                    .iter()
                    .map(|id| {
                        let resolved = self.resolve_concrete_ty(*id, mapping);

                        self.typed_module.tcx_mut().alloc(resolved)
                    })
                    .collect();

                Ty::AppliedGeneric(name, concrete_args)
            }

            Ty::Function {
                params_type,
                ret_type,
                is_variadic,
                generics,
            } => {
                let concrete_params_type = params_type
                    .iter()
                    .map(|it| self.resolve_concrete_ty(*it, mapping))
                    .collect::<Vec<_>>()
                    .into_iter()
                    .map(|it| self.tcx().alloc(it))
                    .collect();

                let concrete_ret_type = self.resolve_concrete_ty(ret_type, mapping);
                let concrete_ret_type = self.tcx().alloc(concrete_ret_type);

                Ty::Function {
                    params_type: concrete_params_type,
                    ret_type: concrete_ret_type,
                    is_variadic,
                    generics,
                }
            }

            _ => ty,
        }
    }
}
