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
            vec![OpCode::Constant as u8],
            OpCode::u16(1),
            vec![OpCode::Constant as u8],
            OpCode::u16(2),
            vec![OpCode::Constant as u8],
            OpCode::u16(65535),
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
            ("true", vec![], "0000 OpTrue\n0001 OpPop"),
            ("false", vec![], "0000 OpFalse\n0001 OpPop"),
            (
                "1 == 2",
                vec![Object::Integer(1), Object::Integer(2)],
                "0000 OpConstant 0\n0003 OpConstant 1\n0006 OpEqual\n0007 OpPop",
            ),
            (
                "1 != 2",
                vec![Object::Integer(1), Object::Integer(2)],
                "0000 OpConstant 0\n0003 OpConstant 1\n0006 OpNotEqual\n0007 OpPop",
            ),
            (
                "1 > 2",
                vec![Object::Integer(1), Object::Integer(2)],
                "0000 OpConstant 0\n0003 OpConstant 1\n0006 OpGreaterThan\n0007 OpPop",
            ),
            (
                "1 < 2",
                vec![Object::Integer(2), Object::Integer(1)],
                "0000 OpConstant 0\n0003 OpConstant 1\n0006 OpGreaterThan\n0007 OpPop",
            ),
        ]);
    }

    #[test]
    fn prefix_expression() {
        test_compile(vec![
            ("!true", vec![], "0000 OpTrue\n0001 OpBang\n0002 OpPop"),
            (
                "-123",
                vec![Object::Integer(123)],
                "0000 OpConstant 0\n0003 OpMinus\n0004 OpPop",
            ),
        ]);
    }

    #[test]
    fn if_expression() {
        test_compile(vec![
            (
                "if (true) { 10 }; 3333;",
                vec![Object::Integer(10), Object::Integer(3333)],
                "0000 OpTrue
0001 OpJumpIfNotTruthy 10
0004 OpConstant 0
0007 OpJump 11
0010 OpNull
0011 OpPop
0012 OpConstant 1
0015 OpPop",
            ),
            (
                "if (true) { 10 } else { 20 }; 3333;",
                vec![
                    Object::Integer(10),
                    Object::Integer(20),
                    Object::Integer(3333),
                ],
                "0000 OpTrue
0001 OpJumpIfNotTruthy 10
0004 OpConstant 0
0007 OpJump 13
0010 OpConstant 1
0013 OpPop
0014 OpConstant 2
0017 OpPop",
            ),
        ]);
    }

    #[test]
    fn global_let_statements() {
        test_compile(vec![
            (
                "let one = 1; let two = 2;",
                vec![Object::Integer(1), Object::Integer(2)],
                "0000 OpConstant 0
0003 OpSetGlobal 0
0006 OpConstant 1
0009 OpSetGlobal 1",
            ),
            (
                "let one = 1; one;",
                vec![Object::Integer(1)],
                "0000 OpConstant 0
0003 OpSetGlobal 0
0006 OpGetGlobal 0
0009 OpPop",
            ),
            (
                "let one = 1; let two = one; two;",
                vec![Object::Integer(1)],
                "0000 OpConstant 0
0003 OpSetGlobal 0
0006 OpGetGlobal 0
0009 OpSetGlobal 1
0012 OpGetGlobal 1
0015 OpPop",
            ),
        ]);
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
                expected_instructions,
                "\nfor `{}`",
                input
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
            assert_eq!(constants, expected_constants, "\nfor {}", input);
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
