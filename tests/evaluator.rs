extern crate cymbal;

#[cfg(test)]
mod evalator_tests {
    use cymbal::lexer::Lexer;
    use cymbal::parser::Parser;
    use cymbal::evaluator;

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

    fn test_eval(tests: Vec<(&str, &str)>) {
        for (input, expected) in &tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            let actual = evaluator::eval(program).to_string();

            assert_eq!(actual, expected.to_string(), "for {}", input);
        }
    }
}
