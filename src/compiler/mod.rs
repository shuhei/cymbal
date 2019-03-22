pub mod symbol_table;

use crate::ast::{BlockStatement, Expression, Infix, Prefix, Program, Statement};
use crate::code;
use crate::code::{Instructions, OpCode};
pub use crate::compiler::symbol_table::SymbolTable;
use crate::object::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::mem;
use std::rc::Rc;

const TENTATIVE_JUMP_POS: u16 = 9999;

pub struct Compiler {
    pub instructions: Instructions,
    pub constants: Rc<RefCell<Vec<Rc<Object>>>>,
    symbol_table: Rc<RefCell<SymbolTable>>,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: vec![],
            constants: Rc::new(RefCell::new(vec![])),
            symbol_table: Rc::new(RefCell::new(SymbolTable::new())),
            last_instruction: None,
            previous_instruction: None,
        }
    }

    pub fn new_with_state(
        symbol_table: Rc<RefCell<SymbolTable>>,
        constants: Rc<RefCell<Vec<Rc<Object>>>>,
    ) -> Self {
        Compiler {
            instructions: vec![],
            constants,
            symbol_table,
            last_instruction: None,
            previous_instruction: None,
        }
    }

    pub fn compile(&mut self, program: &Program) -> Result<(), CompileError> {
        for statement in &program.statements {
            self.compile_statement(statement)?;
        }
        Ok(())
    }

    pub fn compile_statement(&mut self, statement: &Statement) -> Result<(), CompileError> {
        match statement {
            Statement::Expression(exp) => {
                self.compile_expression(exp)?;
                self.emit(OpCode::Pop);
            }
            Statement::Let(name, exp) => {
                self.compile_expression(exp)?;
                let symbol_index = { self.symbol_table.borrow_mut().define(name).index };
                self.emit_with_operands(OpCode::SetGlobal, OpCode::u16(symbol_index));
            }
            _ => {}
        }
        Ok(())
    }

    pub fn compile_expression(&mut self, expression: &Expression) -> Result<(), CompileError> {
        match expression {
            Expression::Infix(infix, left, right) => {
                match infix {
                    Infix::Lt => {
                        // Convert `a < b` to `a > b` to keep the instruction set smaller.
                        // TODO: This is an issue if expressions have side effects though.
                        self.compile_expression(right)?;
                        self.compile_expression(left)?;
                    }
                    _ => {
                        self.compile_expression(left)?;
                        self.compile_expression(right)?;
                    }
                }
                match infix {
                    Infix::Plus => {
                        self.emit(OpCode::Add);
                    }
                    Infix::Minus => {
                        self.emit(OpCode::Sub);
                    }
                    Infix::Asterisk => {
                        self.emit(OpCode::Mul);
                    }
                    Infix::Slash => {
                        self.emit(OpCode::Div);
                    }
                    Infix::Eq => {
                        self.emit(OpCode::Equal);
                    }
                    Infix::NotEq => {
                        self.emit(OpCode::NotEqual);
                    }
                    Infix::Gt | Infix::Lt => {
                        self.emit(OpCode::GreaterThan);
                    }
                }
            }
            Expression::Prefix(prefix, right) => {
                self.compile_expression(right)?;

                match prefix {
                    Prefix::Bang => {
                        self.emit(OpCode::Bang);
                    }
                    Prefix::Minus => {
                        self.emit(OpCode::Minus);
                    }
                }
            }
            Expression::IntegerLiteral(value) => {
                let constant = Rc::new(Object::Integer(*value));
                let const_index = self.add_constant(constant);
                self.emit_with_operands(OpCode::Constant, OpCode::u16(const_index));
            }
            Expression::Boolean(true) => {
                self.emit(OpCode::True);
            }
            Expression::Boolean(false) => {
                self.emit(OpCode::False);
            }
            Expression::StringLiteral(value) => {
                let constant = Rc::new(Object::String(value.clone()));
                let const_index = self.add_constant(constant);
                self.emit_with_operands(OpCode::Constant, OpCode::u16(const_index));
            }
            // if (condition) { consequence }
            //
            // should emit:
            //
            // 1. condition
            // 2. jump to 5 if not truthy
            // 3. consequence (without the last pop)
            // 4. jump to 6
            // 5. push null
            // 6. pop
            //
            // --
            //
            // if (condition) { consequence } else { alternative }
            //
            // should emit:
            //
            // 1. condition
            // 2. jump to 5 if not truthy
            // 3. consequence (without the last pop)
            // 4. jump to 6
            // 5. alternative (without the last pop)
            // 6. pop
            Expression::If(condition, consequence, alternative) => {
                self.compile_expression(condition)?;
                let jump_not_truthy_pos = self
                    .emit_with_operands(OpCode::JumpIfNotTruthy, OpCode::u16(TENTATIVE_JUMP_POS));

                self.compile_block_statement(consequence)?;
                if self.last_instruction_is_pop() {
                    self.remove_last_pop();
                }

                let jump_pos =
                    self.emit_with_operands(OpCode::Jump, OpCode::u16(TENTATIVE_JUMP_POS));

                self.replace_operand(jump_not_truthy_pos, self.instructions.len() as u16);

                match alternative {
                    Some(alt) => {
                        self.compile_block_statement(alt)?;
                        if self.last_instruction_is_pop() {
                            self.remove_last_pop();
                        }
                    }
                    None => {
                        self.emit(OpCode::Null);
                    }
                }

                self.replace_operand(jump_pos, self.instructions.len() as u16);
            }
            Expression::Identifier(name) => {
                let symbol_index = {
                    match self.symbol_table.borrow().resolve(name) {
                        Some(symbol) => symbol.index,
                        None => return Err(CompileError::UndefinedVariable(name.to_string())),
                    }
                };
                self.emit_with_operands(OpCode::GetGlobal, OpCode::u16(symbol_index));
            }
            Expression::Array(exps) => {
                for exp in exps {
                    self.compile_expression(exp)?;
                }
                self.emit_with_operands(OpCode::Array, OpCode::u16(exps.len() as u16));
            }
            Expression::Hash(hash) => {
                let size = hash.pairs.len();
                // Have a stable order of hash pairs
                let mut str_keys = Vec::with_capacity(size);
                let mut map = HashMap::with_capacity(size);
                for (key, value) in &hash.pairs {
                    // Using `String` as key because it's hard to implement
                    // `PartialOrd` for `Expression`.
                    let str_key = key.to_string();
                    // TODO: Is it possible to do this without cloning?
                    str_keys.push(str_key.clone());
                    map.insert(str_key, (key, value));
                }
                str_keys.sort();

                for str_key in str_keys {
                    let (key, value) = map.get(&str_key).unwrap();
                    self.compile_expression(key)?;
                    self.compile_expression(value)?;
                }
                self.emit_with_operands(OpCode::Hash, OpCode::u16(size as u16));
            }
            Expression::Index(left, index) => {
                self.compile_expression(left)?;
                self.compile_expression(index)?;
                self.emit(OpCode::Index);
            }
            _ => {}
        }
        Ok(())
    }

    fn compile_block_statement(
        &mut self,
        block_statement: &BlockStatement,
    ) -> Result<(), CompileError> {
        for statement in &block_statement.statements {
            self.compile_statement(statement)?;
        }
        Ok(())
    }

    fn add_constant(&mut self, constant: Rc<Object>) -> u16 {
        let mut constants = self.constants.borrow_mut();
        constants.push(constant);
        // TODO: Check the limit
        (constants.len() - 1) as u16
    }

    fn emit(&mut self, op_code: OpCode) -> usize {
        // TODO: Isn't it slow to create `vec![]` each time?
        self.emit_with_operands(op_code, vec![])
    }

    fn emit_with_operands(&mut self, op_code: OpCode, operands: Vec<u8>) -> usize {
        let pos = self.instructions.len();
        self.instructions.push(op_code as u8);
        self.instructions.extend(operands);
        self.set_last_instruction(op_code, pos);
        pos
    }

    // Not updating last/previous_instruction because we still don't have cases
    // that require it.
    fn replace_operand(&mut self, op_code_pos: usize, operand: u16) {
        code::replace_uint16(&mut self.instructions, op_code_pos + 1, operand);
    }

    fn set_last_instruction(&mut self, op_code: OpCode, position: usize) {
        self.previous_instruction = mem::replace(
            &mut self.last_instruction,
            Some(EmittedInstruction { op_code, position }),
        );
    }

    fn last_instruction_is_pop(&self) -> bool {
        match &self.last_instruction {
            Some(emitted) => emitted.op_code == OpCode::Pop,
            None => false,
        }
    }

    fn remove_last_pop(&mut self) {
        if let Some(emitted) = &self.last_instruction {
            self.instructions.truncate(emitted.position);
            self.last_instruction = mem::replace(&mut self.previous_instruction, None);
        }
    }

    pub fn bytecode(self) -> Bytecode {
        Bytecode {
            instructions: self.instructions,
            constants: Rc::clone(&self.constants),
        }
    }
}

pub struct EmittedInstruction {
    pub op_code: OpCode,
    pub position: usize,
}

pub enum CompileError {
    UnknownOperator(Infix),
    UndefinedVariable(String),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompileError::UnknownOperator(infix) => write!(f, "unknown operator: {}", infix),
            CompileError::UndefinedVariable(name) => write!(f, "undefined variable: {}", name),
        }
    }
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Rc<RefCell<Vec<Rc<Object>>>>,
}

#[cfg(test)]
mod tests {
    use super::Compiler;
    use crate::ast::Program;
    use crate::code;
    use crate::code::OpCode;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;
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

    #[test]
    fn string_expressions() {
        test_compile(vec![
            (
                r#""hello""#,
                vec![Object::String("hello".to_string())],
                "0000 OpConstant 0\n0003 OpPop",
            ),
            (
                r#""hel" + "lo""#,
                vec![
                    Object::String("hel".to_string()),
                    Object::String("lo".to_string()),
                ],
                "0000 OpConstant 0
0003 OpConstant 1
0006 OpAdd
0007 OpPop",
            ),
        ]);
    }

    #[test]
    fn array_expressions() {
        test_compile(vec![
            ("[]", vec![], "0000 OpArray 0\n0003 OpPop"),
            (
                "[1, 2, 3]",
                vec![Object::Integer(1), Object::Integer(2), Object::Integer(3)],
                "0000 OpConstant 0
0003 OpConstant 1
0006 OpConstant 2
0009 OpArray 3
0012 OpPop",
            ),
            (
                "[1 - 2, 3 + 4, 5 * 6]",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                    Object::Integer(6),
                ],
                "0000 OpConstant 0
0003 OpConstant 1
0006 OpSub
0007 OpConstant 2
0010 OpConstant 3
0013 OpAdd
0014 OpConstant 4
0017 OpConstant 5
0020 OpMul
0021 OpArray 3
0024 OpPop",
            ),
        ]);
    }

    #[test]
    fn hash_expression() {
        test_compile(vec![
            ("{}", vec![], "0000 OpHash 0\n0003 OpPop"),
            (
                r#"{ 1: "hello", "foo": 1 + 2 }"#,
                vec![
                    Object::String("foo".to_string()),
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(1),
                    Object::String("hello".to_string()),
                ],
                "0000 OpConstant 0
0003 OpConstant 1
0006 OpConstant 2
0009 OpAdd
0010 OpConstant 3
0013 OpConstant 4
0016 OpHash 2
0019 OpPop",
            ),
        ]);
    }

    #[test]
    fn index_expression() {
        test_compile(vec![
            (
                "[1, 2][0]",
                vec![Object::Integer(1), Object::Integer(2), Object::Integer(0)],
                "0000 OpConstant 0
0003 OpConstant 1
0006 OpArray 2
0009 OpConstant 2
0012 OpIndex
0013 OpPop",
            ),
            (
                r#"{"foo": 1 + 2, "bar": 3 + 4}["bar"]"#,
                vec![
                    Object::String("bar".to_string()),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::String("foo".to_string()),
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::String("bar".to_string()),
                ],
                "0000 OpConstant 0
0003 OpConstant 1
0006 OpConstant 2
0009 OpAdd
0010 OpConstant 3
0013 OpConstant 4
0016 OpConstant 5
0019 OpAdd
0020 OpHash 2
0023 OpConstant 6
0026 OpIndex
0027 OpPop",
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
                .as_ref()
                .borrow()
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
