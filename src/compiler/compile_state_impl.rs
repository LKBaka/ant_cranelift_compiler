use std::{cell::RefCell, rc::Rc};

use ant_ast::{ExprId, StmtId};
use ant_type_checker::{ty_context::TypeContext, typed_ast::{typed_expr::TypedExpression, typed_stmt::TypedStatement}};

use crate::compiler::{CompileState, FunctionState, GlobalState, generic::{CompiledGenericInfo, GenericInfo}, table::SymbolTable};

#[allow(unused)]
impl<'a> FunctionState<'a, '_> {
    pub fn enter_scope(&mut self) {
        let outer = self.table.clone();

        self.table = Rc::new(RefCell::new(SymbolTable::new()));
        self.table.borrow_mut().outer = Some(outer);
    }

    pub fn leave_scope(&mut self) -> Option<Rc<RefCell<SymbolTable>>> {
        let outer = self.table
            .borrow()
            .outer.as_ref()?
            .clone();

        let before_leave_table = self.table.clone();

        self.table = outer;

        Some(before_leave_table)
    }
}

pub trait PushGetGeneric {
    fn push_compiled_generic(&mut self, name: String, info: CompiledGenericInfo);
    fn pop_compiled_generic(&mut self, name: &str) -> Option<CompiledGenericInfo>;
    
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

    fn pop_compiled_generic(&mut self, name: &str) -> Option<CompiledGenericInfo> {
        self.get_compiled_generic_map().shift_remove(name)
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