extern crate cymbal;

#[cfg(test)]
mod evalator_tests {
    use cymbal::evaluator;
    use cymbal::lexer::Lexer;
    use cymbal::object::EvalResult;
    use cymbal::parser::Parser;

    #[test]
    fn eval_boolean() {
        test_eval(vec![
            // Prefix
            ("!true", "false"),
            ("!!true", "true"),
            ("!false", "true"),
            ("!!false", "false"),
            ("!null", "true"),
            ("!!null", "false"),
            ("!0", "false"),
            ("!3", "false"),
            ("!!3", "true"),
            // Infix
            // boolean -> boolean
            ("true == true", "true"),
            ("false == true", "false"),
            ("true != true", "false"),
            ("true != false", "true"),
            // integer -> boolean
            ("1 == 2", "false"),
            ("2 == 2", "true"),
            ("1 != 2", "true"),
            ("2 != 2", "false"),
            ("1 > 2", "false"),
            ("1 < 2", "true"),
        ]);
    }

    #[test]
    fn eval_integer() {
        test_eval(vec![
            // Prefix
            ("-123", "-123"),
            ("-(-123)", "123"),
            ("-(3 * 3)", "-9"),
            // Infix
            ("2 + 3", "5"),
            ("2 - 3", "-1"),
            ("2 * 3", "6"),
            ("9 / 3", "3"),
            ("-50 + 100 + -50", "0"),
            ("20 + 2 * -10", "0"),
            ("50 / 2 * 2 + 10", "60"),
            ("2 * (5 + 10)", "30"),
            ("3 * 3 * 3 + 10", "37"),
            ("3 * (3 * 3) + 10", "37"),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", "50"),
        ]);
    }

    #[test]
    fn eval_if() {
        test_eval(vec![
            ("if (true) { 10 }", "10"),
            ("if (false) { 10 }", "null"),
            ("if (null) { 1 } else { 2 }", "2"),
            ("if (2 > 1) { 3 } else { 4 }", "3"),
            ("if (2 < 1) { 3 } else { 4 }", "4"),
            ("if (1 < 2) { 3 }", "3"),
            ("if (1 > 2) { 3 }", "null"),
        ]);
    }

    #[test]
    fn eval_return() {
        test_eval(vec![
            ("return;", "null"),
            ("return 10;", "10"),
            ("1 + 2; return; 3 + 4", "null"),
            ("1 + 2; return 8; 3 + 4", "8"),
            ("3; return 8 * 2; 3 + 4", "16"),
            // Nested statements
            (
                "if (10 > 1) {
                if (10 > 1) {
                    return 10;
                }
                return 1;
            }",
                "10",
            ),
        ]);
    }

    #[test]
    fn error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "if (10 > 1) { true + false; }; 8;",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("(1 + true) * 4;", "type mismatch: INTEGER + BOOLEAN"),
            ("3 - (true * 2);", "type mismatch: BOOLEAN * INTEGER"),
            (
                "(3 + false) - (true * 2);",
                "type mismatch: INTEGER + BOOLEAN",
            ),
            ("!(1 + true);", "type mismatch: INTEGER + BOOLEAN"),
            ("return (1 + true) * 4;", "type mismatch: INTEGER + BOOLEAN"),
            (
                "if (3 == true) { 1 } else { 2 }",
                "type mismatch: INTEGER == BOOLEAN",
            ),
        ];
        for (input, expected_message) in &tests {
            match eval_input(input) {
                Ok(obj) => {
                    panic!("no error object returned. got={} for {}", obj, input);
                }
                Err(err) => {
                    assert_eq!(&err.to_string(), expected_message, "for {}", input);
                }
            }
        }
    }

    fn test_eval(tests: Vec<(&str, &str)>) {
        for (input, expected) in &tests {
            match eval_input(input) {
                Ok(obj) => {
                    assert_eq!(obj.to_string(), expected.to_string(), "for {}", input);
                }
                Err(err) => {
                    panic!("expected {}, but got error={} for {}", expected, err, input);
                }
            }
        }
    }

    fn eval_input(input: &str) -> EvalResult {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        evaluator::eval(program)
    }
}
