extern crate cymbal;

#[cfg(test)]
mod parser_tests {
    use cymbal::token::Token;
    use cymbal::lexer::Lexer;
    use cymbal::parser::Parser;
    use cymbal::ast::Statement;

    #[test]
    fn let_statements() {
	let input = "
            let x = 5;
            let y = 10;
            let foobar = 838383;
        ";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(program.statements, vec![
            Statement::Let("x".to_string()),
            Statement::Let("y".to_string()),
            Statement::Let("foobar".to_string()),
        ]);
    }

    fn check_parser_errors(parser: &Parser) {
        assert_eq!(parser.errors.len(), 0, "parser errors: {:?}", parser.errors);
    }
}
