use std::{cell::RefCell, rc::Rc};

use crate::compiler::{FunctionState, table::SymbolTable};

#[allow(unused)]
impl<'a> FunctionState<'a> {
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