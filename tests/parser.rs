extern crate cymbal;

#[cfg(test)]
mod parser_tests {
    use cymbal::lexer::Lexer;
    use cymbal::parser::Parser;
    use cymbal::ast::{Expression, Statement};

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
            ("!5;", "!", 5),
            ("-15;", "-", 15),
        ];
        for (input, operator, value) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements, vec![
                Statement::Expression(
                    Expression::Prefix(
                        operator.to_string(),
                        Box::new(Expression::IntegerLiteral(value))
                    )
                )
            ]);
        }
    }

    fn check_parser_errors(parser: &Parser) {
        assert_eq!(parser.errors.len(), 0, "parser errors: {:?}", parser.errors);
    }
}
