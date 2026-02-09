use std::{cell::RefCell, rc::Rc};

use crate::compiler::{CompileState, FunctionState, generic::GenericInfo, table::SymbolTable};

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

pub trait PushGetGeneric {
    fn push_generic(&mut self, name: String, info: GenericInfo);

    fn get_mut_generic(&mut self, name: String) -> Option<&mut GenericInfo>;
    fn get_generic(&mut self, name: String) -> Option<GenericInfo>;
} 

impl<T: CompileState> PushGetGeneric for T {
    fn push_generic(&mut self, name: String, info: GenericInfo) {
        self.get_generic_map().insert(name, info);
    }

    fn get_generic(&mut self, name: String) -> Option<GenericInfo> {
        self.get_generic_map().get(&name).cloned()
    }

    fn get_mut_generic(&mut self, name: String) -> Option<&mut GenericInfo> {
        self.get_generic_map().get_mut(&name)
    }
}