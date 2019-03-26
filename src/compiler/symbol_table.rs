use std::collections::HashMap;
use std::mem;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SymbolScope {
    Global,
    Local,
    Builtin,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: u16,
}

// TODO: Better naming.
struct SymbolLayer {
    store: HashMap<String, Symbol>,

    // Keep track of non-builtin definitions.
    num_definitions: u16,
}

impl SymbolLayer {
    pub fn new() -> Self {
        SymbolLayer {
            store: HashMap::new(),
            num_definitions: 0,
        }
    }
}

pub struct SymbolTable {
    // The innermost store.
    current: SymbolLayer,

    // The stack of outer stores. The first item is the outermost one (global) and the last item is
    // the 2nd innermost one.
    outers: Vec<SymbolLayer>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            current: SymbolLayer::new(),
            outers: vec![],
        }
    }

    pub fn push(&mut self) {
        let outer = mem::replace(&mut self.current, SymbolLayer::new());
        self.outers.push(outer);
    }

    pub fn pop(&mut self) {
        match self.outers.pop() {
            Some(outer) => {
                mem::replace(&mut self.current, outer);
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
            index: self.current.num_definitions,
            scope,
        };
        self.current.num_definitions += 1;

        self.define_symbol(name, symbol)
    }

    pub fn define_builtin(&mut self, index: u16, name: &str) -> &Symbol {
        if self.outers.len() > 0 {
            panic!("builtin can be defined only on top-level scope");
        }

        let symbol = Symbol {
            name: name.to_string(),
            index,
            scope: SymbolScope::Builtin,
        };

        self.define_symbol(name, symbol)
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        self.current.store.get(name).or_else(|| {
            // Try from the 2nd innermost store to the outermost one.
            for outer in self.outers.iter().rev() {
                if let Some(symbol) = outer.store.get(name) {
                    return Some(symbol);
                }
            }
            None
        })
    }

    pub fn num_definitions(&self) -> u16 {
        self.current.num_definitions
    }

    fn define_symbol(&mut self, name: &str, symbol: Symbol) -> &Symbol {
        self.current.store.insert(name.to_string(), symbol);
        self.current.store.get(name).expect("inserted just now")
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
            &vec![
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
            &vec![
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
            &vec![
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
            &vec![
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

    #[test]
    fn resolve_builtin() {
        let mut table = SymbolTable::new();
        table.define_builtin(0, "a");
        table.define_builtin(1, "b");

        let tests = vec![
            Symbol {
                name: "a".to_string(),
                scope: SymbolScope::Builtin,
                index: 0,
            },
            Symbol {
                name: "b".to_string(),
                scope: SymbolScope::Builtin,
                index: 1,
            },
        ];

        test_resolve(&table, &tests);

        table.push();
        test_resolve(&table, &tests);

        table.push();
        test_resolve(&table, &tests);
    }

    fn test_resolve(table: &SymbolTable, expected: &Vec<Symbol>) {
        for symbol in expected {
            let resolved = table
                .resolve(&symbol.name)
                .expect(&format!("expected `{}` to be resolved", symbol.name));
            assert_eq!(resolved, symbol);
        }
    }
}
