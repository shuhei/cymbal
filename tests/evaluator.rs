extern crate cymbal;

#[cfg(test)]
mod parser_tests {
    use cymbal::lexer::Lexer;
    use cymbal::parser::Parser;
    use cymbal::evaluator;

    #[test]
    fn eval() {
        let tests = [
            // Prefix
            // boolean -> boolean
            ("!true", "false"),
            ("!false", "true"),
            // integer -> integer
            ("-123", "-123"),
            ("-(-123)", "123"),
            ("-(3 * 3)", "-9"),

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
            // integer -> integer
            ("2 + 3", "5"),
            ("2 - 3", "-1"),
            ("2 * 3", "6"),
            ("9 / 3", "3"),
        ];
        for (input, expected) in &tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            let actual = evaluator::eval(program).to_string();

            assert_eq!(actual, expected.to_string(), "for {}", input);
        }
    }
}
