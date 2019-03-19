extern crate cymbal;

#[cfg(test)]
mod vm_tests {
    use cymbal::lexer::Lexer;
    use cymbal::parser::Parser;
    use cymbal::compiler::Compiler;
    use cymbal::vm::Vm;

    #[test]
    fn integer() {
        test_vm(vec![
            ("1", "1"),
            ("2", "2"),
            ("1 + 2", "3"),
            ("1 - 2", "-1"),
            ("2 * 3", "6"),
            ("4 / 2", "2"),
            ("50 / 2 * 2 + 10 - 5", "55"),
            ("5 * (2 + 10)", "60"),
            ("5 + 5 + 5 + 5 - 10", "10"),
            ("2 * 2 * 2 * 2 * 2", "32"),
            ("5 * 2 + 10", "20"),
            ("5 + 2 * 10", "25"),
            ("1 == 1", "true"),
            ("1 == 2", "false"),
            ("1 != 1", "false"),
            ("1 != 2", "true"),
            ("1 > 2", "false"),
            ("2 > 1", "true"),
            ("1 < 2", "true"),
            ("2 < 1", "false"),
        ]);
    }

    #[test]
    fn boolean() {
        test_vm(vec![
            ("true", "true"),
            ("false", "false"),
            ("true == true", "true"),
            ("false == false", "true"),
            ("true == false", "false"),
            ("true != true", "false"),
            ("false != false", "false"),
            ("true != false", "true"),
        ]);
    }

    #[test]
    fn prefix_minus() {
        test_vm(vec![
            ("-123", "-123"),
            ("-(1 + 3)", "-4"),
            ("-(10 - 23)", "13"),
        ]);
    }

    #[test]
    fn prefix_bang() {
        test_vm(vec![
            ("!true", "false"),
            ("!false", "true"),
            ("!0", "false"),
            ("!123", "false"),
            ("!-123", "false"),
            ("!!true", "true"),
            ("!!false", "false"),
            ("!!0", "true"),
            ("!!123", "true"),
            ("!!-123", "true"),
        ]);
    }

    #[test]
    fn if_expression() {
        test_vm(vec![
            ("if (true) { 10 }", "10"),
            ("if (true) { 10 } else { 20 }", "10"),
            ("if (false) { 10 } else { 20 }", "20"),
            ("if (1) { 10 }", "10"),
            ("if (1 < 2) { 10 }", "10"),
            ("if (1 < 2) { 10 } else { 20 }", "10"),
            ("if (1 > 2) { 10 } else { 20 }", "20"),
            ("if (false) { 10 }", "null"),
            ("if (1 > 2) { 10 }", "null"),
            ("if (if (false) { 10 }) { 10 }", "null"),
            ("if (if (false) { 10 }) { 10 } else { 20 }", "20"),
        ]);
    }

    fn test_vm(tests: Vec<(&str, &str)>) {
        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            let errors = parser.errors();
            if errors.len() > 0 {
                panic!(
                    "for input '{}', got parser errors: {:?}",
                    input,
                    errors
                );
            }

            let mut compiler = Compiler::new();
            match compiler.compile(&program) {
                Err(err) => {
                    panic!("error on compile for `{}`: {}", input, err);
                }
                _ => {}
            }
            let bytecode = compiler.bytecode();
            let mut vm = Vm::new(bytecode);
            match vm.run() {
                Err(err) => {
                    panic!("error on vm for `{}`: {}", input, err);
                }
                _ => {}
            }
            if let Some(obj) = vm.last_popped_stack_elem() {
                assert_eq!(&obj.to_string(), expected, "for `{}` {:?}", input, vm);
            } else {
                panic!("no stack top on vm for `{} {:?}`", input, vm);
            }
        }
    }
}
