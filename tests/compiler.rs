extern crate cymbal;

#[cfg(test)]
mod compiler_tests {
    use cymbal::ast::Program;
    use cymbal::code;
    use cymbal::code::OpCode;
    use cymbal::compiler::Compiler;
    use cymbal::lexer::Lexer;
    use cymbal::object::Object;
    use cymbal::parser::Parser;
    use std::borrow::Borrow;

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
        test_compile(vec![
            (
                "1 + 2",
                vec![Object::Integer(1), Object::Integer(2)],
                "0000 OpConstant 0\n0003 OpConstant 1\n0006 OpAdd\n0007 OpPop",
            ),
            (
                "1 - 2",
                vec![Object::Integer(1), Object::Integer(2)],
                "0000 OpConstant 0\n0003 OpConstant 1\n0006 OpSub\n0007 OpPop",
            ),
            (
                "1 * 2",
                vec![Object::Integer(1), Object::Integer(2)],
                "0000 OpConstant 0\n0003 OpConstant 1\n0006 OpMul\n0007 OpPop",
            ),
            (
                "2 / 1",
                vec![Object::Integer(2), Object::Integer(1)],
                "0000 OpConstant 0\n0003 OpConstant 1\n0006 OpDiv\n0007 OpPop",
            ),
            (
                "true",
                vec![],
                "0000 OpTrue\n0001 OpPop",
            ),
            (
                "false",
                vec![],
                "0000 OpFalse\n0001 OpPop",
            ),
        ]);
    }

    #[test]
    fn compile_error() {
        test_compile_error(vec![("1 == 2", "unknown operator: ==")]);
    }

    fn test_compile(tests: Vec<(&str, Vec<Object>, &str)>) {
        for (input, expected_constants, expected_instructions) in tests {
            let program = parse(input);

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
            // TODO: Better way?
            let constants = bytecode
                .constants
                .iter()
                .map(|c| {
                    let con: &Object = (*c).borrow();
                    con.clone()
                })
                .collect::<Vec<Object>>();
            assert_eq!(constants, expected_constants);
        }
    }

    fn test_compile_error(tests: Vec<(&str, &str)>) {
        for (input, expected_error) in tests {
            let program = parse(input);

            let mut compiler = Compiler::new();
            match compiler.compile(&program) {
                Err(error) => assert_eq!(error.to_string(), expected_error),
                _ => panic!("expected compile error for `{}`", input),
            }
        }
    }

    fn parse(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);
        program
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
