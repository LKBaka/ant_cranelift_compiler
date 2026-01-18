use std::{cell::RefCell, collections::HashMap, rc::Rc, sync::Arc};

use crate::traits::NeedGc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolScope {
    Local,
    Global,
    Free
}

/// 编译期计算的完整 struct 信息
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructLayout {
    pub name: Arc<str>,
    pub fields: Vec<(Arc<str>, ant_type_checker::ty::Ty)>, // 字段名和类型
    pub offsets: Vec<u32>,                                // 编译期计算的偏移量
    pub size: u32,
    pub align: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolTy {
    Var,
    Function,
    Struct(StructLayout),
}

impl NeedGc for SymbolTy {
    fn need_gc(&self) -> bool {
        matches!(self, Self::Struct(_))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    /// 符号名
    pub name: Rc<str>,
    /// 符号作用域 目前看来没啥用
    pub scope: SymbolScope,
    /// 表中索引
    pub table_index: usize,
    /// 实际被压扁展开后的变量索引 (全局唯一)
    pub var_index: usize,
    /// 符号类型 (非 type_checker::Ty)
    pub symbol_ty: SymbolTy,
    /// 是否为变量/函数等有值符号
    pub is_val: bool
}

impl Symbol {
    pub fn new(
        name: Rc<str>,
        scope: SymbolScope,
        table_index: usize,
        var_index: usize,
        is_val: bool,
    ) -> Self {
        Self {
            name,
            scope,
            table_index,
            var_index,
            is_val,
            symbol_ty: SymbolTy::Var,
        }
    }

    pub fn create_func(
        name: Rc<str>,
        scope: SymbolScope,
        table_index: usize,
        var_index: usize,
        is_val: bool,
    ) -> Self {
        Self {
            name,
            scope,
            table_index,
            var_index,
            is_val,
            symbol_ty: SymbolTy::Function,
        }
    }

    pub fn create_struct(
        name: Rc<str>,
        scope: SymbolScope,
        table_index: usize,
        var_index: usize,
        is_val: bool,
        struct_layout: StructLayout,
    ) -> Self {
        Self {
            name,
            scope,
            table_index,
            var_index,
            is_val,
            symbol_ty: SymbolTy::Struct(struct_layout),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolTable {
    pub outer: Option<Rc<RefCell<SymbolTable>>>,

    pub def_count: usize,

    pub map: HashMap<Arc<str>, Symbol>,
    pub free_symbols: Vec<Symbol>,
    pub renamed_symbols: HashMap<Arc<str>, Arc<str>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            outer: None,
            def_count: 0,
            map: HashMap::new(),
            free_symbols: Vec::new(),
            renamed_symbols: HashMap::new(),
        }
    }

    pub fn from_outer(outer: Rc<RefCell<SymbolTable>>) -> Self {
        Self {
            outer: Some(outer),
            def_count: 0,
            map: HashMap::new(),
            free_symbols: Vec::new(),
            renamed_symbols: HashMap::new(),
        }
    }
}

fn symbol_counter(table: Rc<RefCell<SymbolTable>>) -> usize {
    let table = table.borrow();
    if table.outer.is_none() {
        table.def_count
    } else {
        symbol_counter(
            table
                .outer
                .clone()
                .map_or_else(|| Rc::new(RefCell::new(SymbolTable::new())), |it| it),
        ) + table.def_count
    }
}

impl SymbolTable {
    pub fn get(&mut self, name: &str) -> Option<Symbol> {
        if let Some(it) = self.map.get(name) {
            return Some(it.clone());
        }

        if let Some(new_name) = self.renamed_symbols.get(name) {
            return self.map.get(new_name).map_or(None, |it| Some(it.clone()));
        }

        if let Some(outer) = &self.outer {
            let result = outer.borrow_mut().get(name)?;

            if result.scope == SymbolScope::Global {
                return Some(result);
            }

            let free = self.define_free(result);
            return Some(free);
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
            symbol_counter(Rc::new(RefCell::new(self.clone()))),
            true,
        );

        self.def_count += 1;

        self.map.insert(name.into(), symbol.clone());

        symbol
    }

    pub fn define_free(&mut self, original: Symbol) -> Symbol {
        self.free_symbols.push(original.clone());

        let mut symbol = original;
        symbol.table_index = self.free_symbols.len() - 1;
        symbol.scope = SymbolScope::Free;

        self.map.insert(symbol.name.to_string().into(), symbol.clone());

        symbol
    }

    pub fn define_func(&mut self, name: &str) -> Symbol {
        let symbol = Symbol::create_func(
            name.into(),
            if self.outer.is_some() {
                SymbolScope::Local
            } else {
                SymbolScope::Global
            },
            self.def_count,
            symbol_counter(Rc::new(RefCell::new(self.clone()))),
            true,
        );

        self.def_count += 1;

        self.map.insert(name.into(), symbol.clone());

        symbol
    }

    pub fn define_struct(&mut self, name: &str, struct_layout: StructLayout) -> Symbol {
        let symbol = Symbol::create_struct(
            name.into(),
            if self.outer.is_some() {
                SymbolScope::Local
            } else {
                SymbolScope::Global
            },
            self.def_count,
            symbol_counter(Rc::new(RefCell::new(self.clone()))),
            true,
            struct_layout,
        );

        self.def_count += 1;

        self.map.insert(name.into(), symbol.clone());

        symbol
    }

    pub fn define_struct_type(&mut self, name: &str, struct_layout: StructLayout) -> Symbol {
        let symbol = Symbol::create_struct(
            name.into(),
            if self.outer.is_some() {
                SymbolScope::Local
            } else {
                SymbolScope::Global
            },
            self.def_count,
            symbol_counter(Rc::new(RefCell::new(self.clone()))),
            false,
            struct_layout,
        );

        self.def_count += 1;

        self.map.insert(name.into(), symbol.clone());

        symbol
    }

    pub fn find(&self, def_index: usize) -> Option<Symbol> {
        let symbols = self
            .map
            .values()
            .filter(|it| it.table_index == def_index)
            .map(|it| it.clone())
            .collect::<Vec<Symbol>>();

        if !symbols.is_empty() {
            return Some(symbols[0].clone());
        }

        if let Some(outer) = &self.outer {
            return outer.borrow().find(def_index);
        }

        None
    }
}
