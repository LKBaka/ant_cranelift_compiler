use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolScope {
    Local,
    Global,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    pub name: Rc<str>,
    pub scope: SymbolScope,
    pub index: usize,
}

impl Symbol {
    pub fn new(name: Rc<str>, scope: SymbolScope, index: usize) -> Self {
        Self { name, scope, index }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub outer: Option<Rc<RefCell<SymbolTable>>>,

    pub def_count: usize,
    pub map: HashMap<Rc<str>, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            outer: None,
            def_count: 0,
            map: HashMap::new(),
        }
    }

    pub fn from_outer(outer: Rc<RefCell<SymbolTable>>) -> Self {
        Self {
            outer: Some(outer),
            def_count: 0,
            map: HashMap::new(),
        }
    }
}

impl SymbolTable {
    pub fn get(&self, name: &str) -> Option<Symbol> {
        if let Some(it) = self.map.get(name) {
            return Some(it.clone());
        }

        if let Some(outer) = &self.outer {
            return outer.borrow().get(name);
        }

        None
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let symbol = Symbol::new(
            name.into(),
            if self.outer.is_some() {
                SymbolScope::Local
            } else {
                SymbolScope::Global
            },
            self.def_count,
        );

        self.def_count += 1;

        self.map.insert(name.into(), symbol.clone());

        symbol
    }

    pub fn find(&self, def_index: usize) -> Option<Symbol> {
        let symbols = self.map
            .values()
            .filter(|it| it.index == def_index)
            .map(|it| it.clone())
            .collect::<Vec<Symbol>>();

        if !symbols.is_empty() {
            return Some(symbols[0].clone())
        }

        if let Some(outer) = &self.outer {
            return outer.borrow().find(def_index);
        }

        None
    }
}
