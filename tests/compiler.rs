extern crate cymbal;

#[cfg(test)]
mod evalator_tests {
    use cymbal::code;
    use cymbal::code::OpCode;
    use cymbal::compiler::Compiler;
    use cymbal::lexer::Lexer;
    use cymbal::object::Object;
    use cymbal::parser::Parser;

    #[test]
    fn print_instructions() {
        let insts = vec![
            OpCode::constant(1),
            OpCode::constant(2),
            OpCode::constant(65535),
        ];
        let expected = "0000 OpConstant 1
0003 OpConstant 2
0006 OpConstant 65535";

        assert_eq!(&code::print_instructions(&insts.concat()), expected);
    }

    #[test]
    fn compile() {
        test_compile(vec![(
            "1 + 2",
            vec![
                Object::Integer(1),
                Object::Integer(2),
            ],
            "0000 OpConstant 0
0003 OpConstant 1",
        )]);
    }

    fn test_compile(tests: Vec<(&str, Vec<Object>, &str)>) {
        for (input, expected_constants, expected_instructions) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            let mut compiler = Compiler::new();
            match compiler.compile(&program) {
                Err(error) => panic!("failed to compile input `{}`: {}", input, error),
                _ => {}
            }
            let bytecode = compiler.bytecode();

            assert_eq!(
                code::print_instructions(&bytecode.instructions),
                expected_instructions
            );
            assert_eq!(bytecode.constants, expected_constants);
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
