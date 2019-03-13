extern crate cymbal;

#[cfg(test)]
mod evalator_tests {
    use cymbal::evaluator;
    use cymbal::lexer::Lexer;
    use cymbal::object::{Environment, EvalResult};
    use cymbal::parser::Parser;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn eval_boolean() {
        expect_values(vec![
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
        expect_values(vec![
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
        expect_values(vec![
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
        expect_values(vec![
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
        expect_errors(vec![
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
            ("foobar", "identifier not found: foobar"),
        ]);
    }

    #[test]
    fn let_statement() {
        expect_values(vec![
            ("let a = 5; a;", "5"),
            ("let a = 5 * 5; a;", "25"),
            ("let a = 5; let b = a; b;", "5"),
            ("let a = 5; let b = a; let c = a + b + 5; c;", "15"),
        ]);
    }

    #[test]
    fn function_object() {
        expect_values(vec![("fn(x) { x + 2; }", "fn(x) {\n{ (x + 2); }\n}")]);
    }

    #[test]
    fn function_application() {
        expect_values(vec![
            ("let identity = fn(x) { x; }; identity(5);", "5"),
            ("let identity = fn(x) { return x; }; identity(5);", "5"),
            ("let double = fn(x) { x * 2; }; double(5);", "10"),
            ("let add = fn(x, y) { x + y; }; add(10, 23);", "33"),
            (
                "let add = fn(x, y) { x + y; }; add(3 + 4, add(10, 23));",
                "40",
            ),
            ("fn(x) { x; }(5);", "5"),
            ("fn(x) { x; }(1); 5;", "5"),
        ]);
        expect_errors(vec![(
            "let add = fn(x, y) { x + y }; add(123); 3;",
            "wrong number of arguments: expected 2, given 1",
        )]);
    }

    #[test]
    fn closure() {
        expect_values(vec![(
            "let newAdder = fn(x) { fn(y) { x + y } }; let addTwo = newAdder(2); addTwo(3);",
            "5",
        )]);
    }

    #[test]
    fn string() {
        expect_values(vec![
            (r#""Hello, World!""#, r#""Hello, World!""#),
            (r#""hello" + " " + "world""#, r#""hello world""#),
        ]);
        expect_errors(vec![(
            r#""hello world" - "hello""#,
            "unknown operator: STRING - STRING",
        )]);
    }

    #[test]
    fn builtin_function() {
        expect_values(vec![
            (r#"len("")"#, "0"),
            (r#"len("four")"#, "4"),
            (r#"len("hello world")"#, "11"),
            ("len([1, 2, 3])", "3"),
            ("len([])", "0"),
            ("first([1, 2, 3])", "1"),
            ("first([])", "null"),
            ("last([1, 2, 3])", "3"),
            ("last([])", "null"),
            ("rest([1, 2, 3])", "[2, 3]"),
            ("rest(rest([1, 2, 3]))", "[3]"),
            ("rest(rest(rest([1, 2, 3])))", "null"),
            ("rest([])", "null"),
        ]);
        expect_errors(vec![
            ("len(1)", "unsupported arguments to `len`: INTEGER"),
            (
                r#"len("one", "two")"#,
                "wrong number of arguments: expected 1, given 2",
            ),
            ("first(1)", "unsupported arguments to `first`: INTEGER"),
            (
                "first([1, 2], [3, 4])",
                "wrong number of arguments: expected 1, given 2",
            ),
            ("last(1)", "unsupported arguments to `last`: INTEGER"),
            (
                "last([1, 2], [3, 4])",
                "wrong number of arguments: expected 1, given 2",
            ),
            ("rest(1)", "unsupported arguments to `rest`: INTEGER"),
            (
                "rest([1, 2], [3, 4])",
                "wrong number of arguments: expected 1, given 2",
            ),
        ]);
    }

    #[test]
    fn array_literal() {
        expect_values(vec![("[1, 2 * 3, 4 + (5 - 6)]", "[1, 6, 3]")]);
    }

    #[test]
    fn array_index_expression() {
        expect_values(vec![
            ("[1, 2, 3][0]", "1"),
            ("[1, 2, 3][1]", "2"),
            ("[1, 2, 3][2]", "3"),
            ("let i = 0; [1][i];", "1"),
            ("[1, 2, 3][1 + 1];", "3"),
            ("let myArray = [1, 2, 3]; myArray[2];", "3"),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                "6",
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                "2",
            ),
            ("[1, 2, 3][3]", "null"),
            ("[1, 2, 3][-1]", "null"),
        ]);
    }

    fn expect_values(tests: Vec<(&str, &str)>) {
        for (input, expected) in &tests {
            match eval_input(input) {
                Ok(obj) => {
                    assert_eq!(obj.to_string(), expected.to_string(), "for `{}`", input);
                }
                Err(err) => {
                    panic!(
                        "expected `{}`, but got error=`{}` for `{}`",
                        expected, err, input
                    );
                }
            }
        }
    }

    fn expect_errors(tests: Vec<(&str, &str)>) {
        for (input, expected_message) in &tests {
            match eval_input(input) {
                Ok(obj) => {
                    panic!("no error object returned. got=`{}` for `{}`", obj, input);
                }
                Err(err) => {
                    assert_eq!(&err.to_string(), expected_message, "for `{}`", input);
                }
            }
        }
    }

    fn eval_input(input: &str) -> EvalResult {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let env = Rc::new(RefCell::new(Environment::new()));
        evaluator::eval(&program, env)
    }
}
