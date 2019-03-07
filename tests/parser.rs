extern crate cymbal;

#[cfg(test)]
mod parser_tests {
    use cymbal::lexer::Lexer;
    use cymbal::parser::Parser;
    use cymbal::ast::{Expression, Statement, Infix, Prefix};

    #[test]
    fn let_statement() {
	let input = "
            let x = 5;
            let y = 10;
            let foobar = 838383;
        ";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements, vec![
            Statement::Let("x".to_string()),
            Statement::Let("y".to_string()),
            Statement::Let("foobar".to_string()),
        ]);
    }

    #[test]
    fn return_statement() {
        let input = "
            return 5;
            return 10;
            return 993322;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements, vec![
            Statement::Return,
            Statement::Return,
            Statement::Return,
        ]);
    }

    #[test]
    fn identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements, vec![
            Statement::Expression(Expression::Identifier("foobar".to_string())),
        ]);
    }

    #[test]
    fn integer_literal_expression() {
        let input = "5;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements, vec![
            Statement::Expression(Expression::IntegerLiteral(5)),
        ]);
    }

    #[test]
    fn prefix_expression() {
        let tests = vec![
            ("!5;", Prefix::Bang, 5),
            ("-15;", Prefix::Minus, 15),
        ];
        for (input, operator, value) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements, vec![
                Statement::Expression(
                    Expression::Prefix(
                        operator,
                        Box::new(Expression::IntegerLiteral(value))
                    )
                )
            ]);
        }
    }

    #[test]
    fn infix_expression() {
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

            assert_eq!(program.statements, vec![
                Statement::Expression(
                    Expression::Infix(
                        operator,
                        Box::new(Expression::IntegerLiteral(left)),
                        Box::new(Expression::IntegerLiteral(right))
                    )
                )
            ]);
        }
    }


    fn check_parser_errors(parser: &Parser) {
        assert_eq!(parser.errors.len(), 0, "parser errors: {:?}", parser.errors);
    }
}
