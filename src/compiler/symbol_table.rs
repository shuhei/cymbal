use crate::object::builtin;
use std::collections::HashMap;
use std::mem;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SymbolScope {
    Global,
    Local,
    Free,
    Builtin,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Symbol {
    pub scope: SymbolScope,
    pub index: u16,
}

// TODO: Better naming.
struct SymbolLayer {
    // Either of:
    // - globals & builtins
    // - locals
    store: HashMap<String, Symbol>,

    // Keep track of non-builtin definitions.
    num_definitions: u16,

    // Original symbols of free symbols from the one-level outer scope.
    pub free_symbols: Vec<Symbol>,
}

impl SymbolLayer {
    pub fn new() -> Self {
        SymbolLayer {
            store: HashMap::new(),
            num_definitions: 0,
            free_symbols: vec![],
        }
    }

    pub fn define_free(&mut self, name: &str, original: Symbol) -> Symbol {
        let symbol = Symbol {
            index: self.free_symbols.len() as u16,
            scope: SymbolScope::Free,
        };

        self.free_symbols.push(original);
        *self.define_symbol(name, symbol)
    }

    pub fn define_symbol(&mut self, name: &str, symbol: Symbol) -> &Symbol {
        self.store.insert(name.to_string(), symbol);
        self.store.get(name).expect("inserted just now")
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

    pub fn new_with_builtins() -> Self {
        let mut symbol_table = SymbolTable::new();
        for (i, b) in builtin::BUILTINS.iter().enumerate() {
            symbol_table.define_builtin(i as u16, b.name);
        }
        symbol_table
    }

    pub fn push(&mut self) {
        let outer = mem::replace(&mut self.current, SymbolLayer::new());
        self.outers.push(outer);
    }

    pub fn pop(&mut self) -> Vec<Symbol> {
        match self.outers.pop() {
            Some(outer) => {
                let popped = mem::replace(&mut self.current, outer);
                popped.free_symbols
            }
            // TODO: Should this throw? Or `Result`?
            None => vec![],
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
            index: self.current.num_definitions,
            scope,
        };
        self.current.num_definitions += 1;

        self.current.define_symbol(name, symbol)
    }

    pub fn define_builtin(&mut self, index: u16, name: &str) -> &Symbol {
        if self.outers.len() > 0 {
            panic!("builtin can be defined only on top-level scope");
        }

        let symbol = Symbol {
            index,
            scope: SymbolScope::Builtin,
        };

        self.current.define_symbol(name, symbol)
    }

    pub fn resolve(&mut self, name: &str) -> Option<Symbol> {
        {
            // Silence the borrow checker.
            // https://users.rust-lang.org/t/solved-borrow-doesnt-drop-returning-this-value-requires-that/24182
            let maybe_symbol: Option<&Symbol> =
                unsafe { mem::transmute(self.current.store.get(name)) };
            if maybe_symbol.is_some() {
                return maybe_symbol.map(|s| *s);
            }
        }

        let num_outers = self.outers.len();
        // Try from the 2nd innermost store to the outermost one.
        for (i, outer) in self.outers.iter().rev().enumerate() {
            if let Some(original) = outer.store.get(name) {
                return match original.scope {
                    SymbolScope::Global | SymbolScope::Builtin => Some(*original),
                    SymbolScope::Local | SymbolScope::Free => {
                        // If the symbol doesn't exist in the current scope but exists in an outer
                        // scope, define it as a free scope in all the scopes between the current scope
                        // and the original scope.
                        let mut parent_symbol = *original;
                        // Propagate the free symbol from outer to inner.
                        for j in (num_outers - i)..num_outers {
                            let o = &mut self.outers[j];
                            parent_symbol = o.define_free(name, parent_symbol);
                        }
                        Some(self.current.define_free(name, parent_symbol))
                    }
                };
            }
        }
        None
    }

    pub fn num_definitions(&self) -> u16 {
        self.current.num_definitions
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
                scope: SymbolScope::Global,
                index: 0
            }
        );
        let b = table.define("b");
        assert_eq!(
            b,
            &Symbol {
                scope: SymbolScope::Global,
                index: 1
            }
        );

        table.push();

        let c = table.define("c");
        assert_eq!(
            c,
            &Symbol {
                scope: SymbolScope::Local,
                index: 0
            }
        );
        let d = table.define("d");
        assert_eq!(
            d,
            &Symbol {
                scope: SymbolScope::Local,
                index: 1
            }
        );

        table.push();

        let e = table.define("e");
        assert_eq!(
            e,
            &Symbol {
                scope: SymbolScope::Local,
                index: 0
            }
        );
        let f = table.define("f");
        assert_eq!(
            f,
            &Symbol {
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
            &mut global,
            &vec![
                (
                    "a".to_string(),
                    Symbol {
                        scope: SymbolScope::Global,
                        index: 0,
                    },
                ),
                (
                    "b".to_string(),
                    Symbol {
                        scope: SymbolScope::Global,
                        index: 1,
                    },
                ),
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
            &mut table,
            &vec![
                (
                    "a".to_string(),
                    Symbol {
                        scope: SymbolScope::Global,
                        index: 0,
                    },
                ),
                (
                    "b".to_string(),
                    Symbol {
                        scope: SymbolScope::Global,
                        index: 1,
                    },
                ),
                (
                    "c".to_string(),
                    Symbol {
                        scope: SymbolScope::Local,
                        index: 0,
                    },
                ),
                (
                    "d".to_string(),
                    Symbol {
                        scope: SymbolScope::Local,
                        index: 1,
                    },
                ),
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
            &mut table,
            &vec![
                (
                    "a".to_string(),
                    Symbol {
                        scope: SymbolScope::Global,
                        index: 0,
                    },
                ),
                (
                    "b".to_string(),
                    Symbol {
                        scope: SymbolScope::Global,
                        index: 1,
                    },
                ),
                (
                    "c".to_string(),
                    Symbol {
                        scope: SymbolScope::Local,
                        index: 0,
                    },
                ),
                (
                    "d".to_string(),
                    Symbol {
                        scope: SymbolScope::Local,
                        index: 1,
                    },
                ),
            ],
        );

        table.push();
        table.define("e");
        table.define("f");

        test_resolve(
            &mut table,
            &vec![
                (
                    "a".to_string(),
                    Symbol {
                        scope: SymbolScope::Global,
                        index: 0,
                    },
                ),
                (
                    "b".to_string(),
                    Symbol {
                        scope: SymbolScope::Global,
                        index: 1,
                    },
                ),
                (
                    "e".to_string(),
                    Symbol {
                        scope: SymbolScope::Local,
                        index: 0,
                    },
                ),
                (
                    "f".to_string(),
                    Symbol {
                        scope: SymbolScope::Local,
                        index: 1,
                    },
                ),
            ],
        );
    }

    #[test]
    fn resolve_builtin() {
        let mut table = SymbolTable::new();
        table.define_builtin(0, "a");
        table.define_builtin(1, "b");

        let tests = vec![
            (
                "a".to_string(),
                Symbol {
                    scope: SymbolScope::Builtin,
                    index: 0,
                },
            ),
            (
                "b".to_string(),
                Symbol {
                    scope: SymbolScope::Builtin,
                    index: 1,
                },
            ),
        ];

        test_resolve(&mut table, &tests);

        table.push();
        test_resolve(&mut table, &tests);

        table.push();
        test_resolve(&mut table, &tests);
    }

    #[test]
    fn resolve_free() {
        let mut table = SymbolTable::new();
        table.define("a");
        table.define("b");

        table.push();
        table.define("c");
        table.define("d");

        test_resolve(
            &mut table,
            &vec![
                (
                    "a".to_string(),
                    Symbol {
                        scope: SymbolScope::Global,
                        index: 0,
                    },
                ),
                (
                    "b".to_string(),
                    Symbol {
                        scope: SymbolScope::Global,
                        index: 1,
                    },
                ),
                (
                    "c".to_string(),
                    Symbol {
                        scope: SymbolScope::Local,
                        index: 0,
                    },
                ),
                (
                    "d".to_string(),
                    Symbol {
                        scope: SymbolScope::Local,
                        index: 1,
                    },
                ),
            ],
        );

        table.push();
        table.define("e");
        table.define("f");

        test_resolve(
            &mut table,
            &vec![
                (
                    "a".to_string(),
                    Symbol {
                        scope: SymbolScope::Global,
                        index: 0,
                    },
                ),
                (
                    "b".to_string(),
                    Symbol {
                        scope: SymbolScope::Global,
                        index: 1,
                    },
                ),
                (
                    "c".to_string(),
                    Symbol {
                        scope: SymbolScope::Free,
                        index: 0,
                    },
                ),
                (
                    "d".to_string(),
                    Symbol {
                        scope: SymbolScope::Free,
                        index: 1,
                    },
                ),
                (
                    "e".to_string(),
                    Symbol {
                        scope: SymbolScope::Local,
                        index: 0,
                    },
                ),
                (
                    "f".to_string(),
                    Symbol {
                        scope: SymbolScope::Local,
                        index: 1,
                    },
                ),
            ],
        );
        test_unresolvable(&mut table, &vec!["foo", "z"]);
    }

    #[test]
    fn nested_resolve_global() {
        let mut table = SymbolTable::new();
        table.define("a");
        table.push();
        table.push();

        let symbol = table.resolve("a").expect("expected \"a\" to be resolved");
        assert_eq!(symbol.index, 0);
        assert_eq!(symbol.scope, SymbolScope::Global);
    }

    #[test]
    fn nested_resolve_free_local() {
        let mut table = SymbolTable::new();
        table.push();
        table.define("a");
        table.push();

        let symbol = table.resolve("a").expect("expected \"a\" to be resolved");
        assert_eq!(symbol.index, 0);
        assert_eq!(symbol.scope, SymbolScope::Free);

        assert_eq!(
            table.pop(),
            vec![Symbol {
                scope: SymbolScope::Local,
                index: 0
            }]
        );
    }

    #[test]
    fn nested_resolve_free_symbols() {
        let mut table = SymbolTable::new();
        table.push();
        table.define("a");
        table.push();
        table.push();
        table.push();
        let symbol = table.resolve("a").expect("expected \"a\" to be resolved");
        assert_eq!(symbol.index, 0);
        assert_eq!(symbol.scope, SymbolScope::Free);

        assert_eq!(
            table.pop(),
            vec![Symbol {
                scope: SymbolScope::Free,
                index: 0
            }]
        );
        assert_eq!(
            table.pop(),
            vec![Symbol {
                scope: SymbolScope::Free,
                index: 0
            }]
        );
        assert_eq!(
            table.pop(),
            vec![Symbol {
                scope: SymbolScope::Local,
                index: 0
            }]
        );
        assert_eq!(table.pop(), vec![]);
    }

    fn test_resolve(table: &mut SymbolTable, expected: &[(String, Symbol)]) {
        for (name, symbol) in expected {
            let resolved = table
                .resolve(&name)
                .expect(&format!("expected `{}` to be resolved", name));
            assert_eq!(&resolved, symbol);
        }
    }

    fn test_unresolvable(table: &mut SymbolTable, expected: &Vec<&str>) {
        for name in expected {
            assert_eq!(table.resolve(name), None);
        }
    }
}
