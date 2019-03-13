extern crate cymbal;

#[cfg(test)]
mod parser_tests {
    use cymbal::ast::{Expression, Infix, Prefix, Statement};
    use cymbal::lexer::Lexer;
    use cymbal::parser::Parser;

    #[test]
    fn let_statement() {
        let input = "
            let x = 5;
            let y = 10;
            let foobar = x + y;
        ";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements,
            vec![
                Statement::Let("x".to_string(), Expression::IntegerLiteral(5)),
                Statement::Let("y".to_string(), Expression::IntegerLiteral(10)),
                Statement::Let(
                    "foobar".to_string(),
                    Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Identifier("x".to_string())),
                        Box::new(Expression::Identifier("y".to_string()))
                    )
                )
            ]
        );
    }

    #[test]
    fn return_statement() {
        let input = "
            return;
            return 5;
            return 10;
            return 993322;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements,
            vec![
                Statement::Return(None),
                Statement::Return(Some(Expression::IntegerLiteral(5))),
                Statement::Return(Some(Expression::IntegerLiteral(10))),
                Statement::Return(Some(Expression::IntegerLiteral(993322))),
            ]
        );
    }

    #[test]
    fn identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements,
            vec![Statement::Expression(Expression::Identifier(
                "foobar".to_string()
            )),]
        );
    }

    #[test]
    fn integer_literal_expression() {
        let input = "5;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements,
            vec![Statement::Expression(Expression::IntegerLiteral(5)),]
        );
    }

    #[test]
    fn prefix_expression() {
        let tests = vec![
            ("!5;", Prefix::Bang, Expression::IntegerLiteral(5)),
            ("-15;", Prefix::Minus, Expression::IntegerLiteral(15)),
            ("!true;", Prefix::Bang, Expression::Boolean(true)),
            ("!false;", Prefix::Bang, Expression::Boolean(false)),
        ];
        for (input, operator, value) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(
                program.statements,
                vec![Statement::Expression(Expression::Prefix(
                    operator,
                    Box::new(value)
                ))]
            );
        }
    }

    #[test]
    fn infix_expression_integer() {
        let tests = vec![
            ("5 + 5;", 5, Infix::Plus, 5),
            ("5 - 5;", 5, Infix::Minus, 5),
            ("5 * 5;", 5, Infix::Asterisk, 5),
            ("5 / 5;", 5, Infix::Slash, 5),
            ("5 > 5;", 5, Infix::Gt, 5),
            ("5 < 5;", 5, Infix::Lt, 5),
            ("5 == 5;", 5, Infix::Eq, 5),
            ("5 != 5;", 5, Infix::NotEq, 5),
        ];
        for (input, left, operator, right) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(
                program.statements,
                vec![Statement::Expression(Expression::Infix(
                    operator,
                    Box::new(Expression::IntegerLiteral(left)),
                    Box::new(Expression::IntegerLiteral(right))
                ))]
            );
        }
    }

    #[test]
    fn infix_expression_boolean() {
        let tests = vec![
            ("true == true", true, Infix::Eq, true),
            ("true != false", true, Infix::NotEq, false),
            ("false == false", false, Infix::Eq, false),
        ];
        for (input, left, operator, right) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(
                program.statements,
                vec![Statement::Expression(Expression::Infix(
                    operator,
                    Box::new(Expression::Boolean(left)),
                    Box::new(Expression::Boolean(right))
                ))]
            );
        }
    }

    #[test]
    fn operator_precedence() {
        test_parsing(vec![
            ("-a * b", "((-a) * b);"),
            ("!-a", "(!(-a));"),
            ("a + b + c", "((a + b) + c);"),
            ("a + b - c", "((a + b) - c);"),
            ("a * b * c", "((a * b) * c);"),
            ("a * b / c", "((a * b) / c);"),
            ("a + b / c", "(a + (b / c));"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f);"),
            ("3 + 4; -5 * 5", "(3 + 4);((-5) * 5);"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4));"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4));"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
            ),
            ("true", "true;"),
            ("false", "false;"),
            ("3 > 5 == false", "((3 > 5) == false);"),
            ("3 < 5 == true", "((3 < 5) == true);"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4);"),
            ("(5 + 5) * 2", "((5 + 5) * 2);"),
            ("2 / (5 + 5)", "(2 / (5 + 5));"),
            ("-(5 + 5)", "(-(5 + 5));"),
            ("!(true == true)", "(!(true == true));"),
            ("if (x < y) { x }", "if (x < y) { x; };"),
            (
                "if (x < y) { x } else { y }",
                "if (x < y) { x; } else { y; };",
            ),
            ("return x", "return x;"),
            ("return x return 2 * 3", "return x;return (2 * 3);"),
            ("return 2 * 4 + 5;", "return ((2 * 4) + 5);"),
            ("fn() { 3 * 9; }", "fn() { (3 * 9); };"),
            ("fn(x) { x * 9; }", "fn(x) { (x * 9); };"),
            ("fn(x, y) { x + y; }", "fn(x, y) { (x + y); };"),
            ("call()", "call();"),
            ("add(1, 2 * 3, 4 + 5)", "add(1, (2 * 3), (4 + 5));"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d);"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));",
            ),
            ("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g));"),
            ("fn(x, y) { x + y; }(3, 4)", "fn(x, y) { (x + y); }(3, 4);"),
            ("let x = 3", "let x = 3;"),
            ("let x = 3 + f * 8;", "let x = (3 + (f * 8));"),
            ("\"hello world\"", "\"hello world\";"),
            ("let s = \"hello world\"", "let s = \"hello world\";"),
            ("a * [1, 2, 3, 4][b * c] * d", "((a * ([1, 2, 3, 4][(b * c)])) * d);"),
            ("add(a * b[2], b[1], 2 * [1, 2][1])", "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])));")
        ]);
    }

    #[test]
    fn hash() {
        test_parsing(vec![
            ("{}", "{};"),
            ("{1: 2, 2: 3}", "{1: 2, 2: 3};"),
            ("{true: 3}", "{true: 3};"),
            (r#"{"one": 1, "two": 2, "three": 3}"#, r#"{"one": 1, "three": 3, "two": 2};"#),
        ]);
    }

    fn test_parsing(tests: Vec<(&str, &str)>) {
        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.to_string(), expected);
        }
    }

    fn check_parser_errors(parser: &Parser) {
        let errors = parser.errors();
        if errors.len() > 0 {
            panic!(
                "for input '{}', got parser errors: {:?}",
                parser.input(),
                errors
            );
        }
    }
}
