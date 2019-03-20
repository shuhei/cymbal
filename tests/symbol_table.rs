extern crate cymbal;

#[cfg(test)]
mod symbol_table_tests {
    use cymbal::compiler::symbol_table::{SymbolTable, Symbol, GLOBAL_SCOPE};

    #[test]
    fn define() {
        let mut global = SymbolTable::new();

        let a = global.define("a");
        assert_eq!(a, &Symbol { name: "a".to_string(), scope: GLOBAL_SCOPE.to_string(), index: 0 });

        let b = global.define("b");
        assert_eq!(b, &Symbol { name: "b".to_string(), scope: GLOBAL_SCOPE.to_string(), index: 1 });
    }

    #[test]
    fn resolve_global() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        let expected = vec![
            Symbol { name: "a".to_string(), scope: GLOBAL_SCOPE.to_string(), index: 0 },
            Symbol { name: "b".to_string(), scope: GLOBAL_SCOPE.to_string(), index: 1 }
        ];

        for symbol in expected {
            let resolved = global.resolve(&symbol.name).expect(&format!("expected `{}` to be resolved", symbol.name));
            assert_eq!(resolved, &symbol);
        }
    }
}
