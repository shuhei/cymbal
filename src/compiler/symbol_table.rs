use std::collections::HashMap;

pub type SymbolScope = String;

pub const GLOBAL_SCOPE: &str = "GLOBAL";

#[derive(Debug, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: u16,
}

pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    num_definitions: u16,
}

impl SymbolTable {
    pub fn new() -> Self {
        let store = HashMap::new();
        SymbolTable {
            store,
            num_definitions: 0,
        }
    }

    pub fn define(&mut self, name: &str) -> &Symbol {
        let symbol = Symbol {
            name: name.to_string(),
            index: self.num_definitions,
            scope: GLOBAL_SCOPE.to_string(),
        };
        self.store.insert(name.to_string(), symbol);
        self.num_definitions += 1;
        self.store.get(name).unwrap()
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        self.store.get(name)
    }
}
