use std::collections::HashMap;
use std::mem;

#[derive(Debug, PartialEq, Clone)]
pub enum SymbolScope {
    Global,
    Local,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: u16,
}

impl Symbol {
    pub fn is_global(&self) -> bool {
        self.scope == SymbolScope::Global
    }
}

pub struct SymbolTable {
    // The innermost store.
    store: HashMap<String, Symbol>,
    // The stack of outer stores. The first item is the outermost one (global) and the last item is
    // the 2nd innermost one.
    outers: Vec<HashMap<String, Symbol>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            store: HashMap::new(),
            outers: vec![],
        }
    }

    pub fn push(&mut self) {
        let outer = mem::replace(&mut self.store, HashMap::new());
        self.outers.push(outer);
    }

    pub fn pop(&mut self) {
        match self.outers.pop() {
            Some(outer) => {
                mem::replace(&mut self.store, outer);
            }
            None => {}
        }
    }

    pub fn define(&mut self, name: &str) -> &Symbol {
        // TODO: Check duplication.
        let scope = if self.outers.len() == 0 {
            SymbolScope::Global
        } else {
            SymbolScope::Local
        };
        let symbol = Symbol {
            name: name.to_string(),
            index: self.store.len() as u16,
            scope,
        };

        self.store.insert(name.to_string(), symbol);
        self.store.get(name).expect("inserted just now")
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        self.store.get(name).or_else(|| {
            // Try from the 2nd innermost store to the outermost one.
            for outer in self.outers.iter().rev() {
                if let Some(symbol) = outer.get(name) {
                    return Some(symbol);
                }
            }
            None
        })
    }

    pub fn num_definitions(&self) -> usize {
        self.store.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn define() {
        let mut table = SymbolTable::new();

        let a = table.define("a");
        assert_eq!(
            a,
            &Symbol {
                name: "a".to_string(),
                scope: SymbolScope::Global,
                index: 0
            }
        );
        let b = table.define("b");
        assert_eq!(
            b,
            &Symbol {
                name: "b".to_string(),
                scope: SymbolScope::Global,
                index: 1
            }
        );

        table.push();

        let c = table.define("c");
        assert_eq!(
            c,
            &Symbol {
                name: "c".to_string(),
                scope: SymbolScope::Local,
                index: 0
            }
        );
        let d = table.define("d");
        assert_eq!(
            d,
            &Symbol {
                name: "d".to_string(),
                scope: SymbolScope::Local,
                index: 1
            }
        );

        table.push();

        let e = table.define("e");
        assert_eq!(
            e,
            &Symbol {
                name: "e".to_string(),
                scope: SymbolScope::Local,
                index: 0
            }
        );
        let f = table.define("f");
        assert_eq!(
            f,
            &Symbol {
                name: "f".to_string(),
                scope: SymbolScope::Local,
                index: 1
            }
        );
    }

    #[test]
    fn resolve_global() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        test_resolve(
            &global,
            vec![
                Symbol {
                    name: "a".to_string(),
                    scope: SymbolScope::Global,
                    index: 0,
                },
                Symbol {
                    name: "b".to_string(),
                    scope: SymbolScope::Global,
                    index: 1,
                },
            ],
        );
    }

    #[test]
    fn resolve_local() {
        let mut table = SymbolTable::new();
        table.define("a");
        table.define("b");

        table.push();
        table.define("c");
        table.define("d");

        test_resolve(
            &table,
            vec![
                Symbol {
                    name: "a".to_string(),
                    scope: SymbolScope::Global,
                    index: 0,
                },
                Symbol {
                    name: "b".to_string(),
                    scope: SymbolScope::Global,
                    index: 1,
                },
                Symbol {
                    name: "c".to_string(),
                    scope: SymbolScope::Local,
                    index: 0,
                },
                Symbol {
                    name: "d".to_string(),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ],
        );
    }

    #[test]
    fn resolve_nested_local() {
        let mut table = SymbolTable::new();
        table.define("a");
        table.define("b");

        table.push();
        table.define("c");
        table.define("d");

        test_resolve(
            &table,
            vec![
                Symbol {
                    name: "a".to_string(),
                    scope: SymbolScope::Global,
                    index: 0,
                },
                Symbol {
                    name: "b".to_string(),
                    scope: SymbolScope::Global,
                    index: 1,
                },
                Symbol {
                    name: "c".to_string(),
                    scope: SymbolScope::Local,
                    index: 0,
                },
                Symbol {
                    name: "d".to_string(),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ],
        );

        table.push();
        table.define("e");
        table.define("f");

        test_resolve(
            &table,
            vec![
                Symbol {
                    name: "a".to_string(),
                    scope: SymbolScope::Global,
                    index: 0,
                },
                Symbol {
                    name: "b".to_string(),
                    scope: SymbolScope::Global,
                    index: 1,
                },
                Symbol {
                    name: "e".to_string(),
                    scope: SymbolScope::Local,
                    index: 0,
                },
                Symbol {
                    name: "f".to_string(),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ],
        );
    }

    fn test_resolve(table: &SymbolTable, expected: Vec<Symbol>) {
        for symbol in expected {
            let resolved = table
                .resolve(&symbol.name)
                .expect(&format!("expected `{}` to be resolved", symbol.name));
            assert_eq!(resolved, &symbol);
        }
    }
}
