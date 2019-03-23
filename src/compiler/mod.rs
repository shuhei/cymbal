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

pub struct CompilationScope {
    pub instructions: Instructions,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

impl CompilationScope {
    pub fn new() -> Self {
        CompilationScope {
            instructions: vec![],
            last_instruction: None,
            previous_instruction: None,
        }
    }

    pub fn emit_with_operands(&mut self, op_code: OpCode, operands: Vec<u8>) -> usize {
        let pos = self.instructions.len();
        self.instructions.push(op_code as u8);
        self.instructions.extend(operands);
        self.set_last_instruction(op_code, pos);
        pos
    }

    // Not updating last/previous_instruction because we still don't have cases
    // that require it.
    pub fn replace_instruction(&mut self, pos: usize, new_instruction: Instructions) {
        for (i, byte) in new_instruction.iter().enumerate() {
            let offset = pos + i;
            if offset < self.instructions.len() {
                self.instructions[offset] = *byte;
            } else {
                self.instructions.push(*byte);
            }
        }
    }

    fn set_last_instruction(&mut self, op_code: OpCode, position: usize) {
        self.previous_instruction = mem::replace(
            &mut self.last_instruction,
            Some(EmittedInstruction { op_code, position }),
        );
    }

    pub fn last_instruction_is(&self, op_code: OpCode) -> bool {
        match &self.last_instruction {
            Some(emitted) => emitted.op_code == op_code,
            None => false,
        }
    }

    pub fn remove_last_pop(&mut self) {
        if let Some(emitted) = &self.last_instruction {
            self.instructions.truncate(emitted.position);
            self.last_instruction = mem::replace(&mut self.previous_instruction, None);
        }
    }

    pub fn replace_last_pop_with_return(&mut self) {
        if let Some(last) = &self.last_instruction {
            let position = last.position;
            self.replace_instruction(position, code::make(OpCode::ReturnValue));
            self.last_instruction = Some(EmittedInstruction {
                position: position,
                op_code: OpCode::ReturnValue,
            });
        }
    }
}

pub struct Compiler {
    pub constants: Rc<RefCell<Vec<Rc<Object>>>>,
    symbol_table: Rc<RefCell<SymbolTable>>,
    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

impl Compiler {
    pub fn new() -> Self {
        let main_scope = CompilationScope::new();
        Compiler {
            constants: Rc::new(RefCell::new(vec![])),
            symbol_table: Rc::new(RefCell::new(SymbolTable::new())),
            scopes: vec![main_scope],
            scope_index: 0,
        }
    }

    pub fn new_with_state(
        symbol_table: Rc<RefCell<SymbolTable>>,
        constants: Rc<RefCell<Vec<Rc<Object>>>>,
    ) -> Self {
        let main_scope = CompilationScope::new();
        Compiler {
            constants,
            symbol_table,
            scopes: vec![main_scope],
            scope_index: 0,
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
            Statement::Return(None) => {
                self.emit(OpCode::Return);
            }
            Statement::Return(Some(value)) => {
                self.compile_expression(value)?;
                self.emit(OpCode::ReturnValue);
            }
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
                if self.last_instruction_is(OpCode::Pop) {
                    self.remove_last_pop();
                }

                let jump_pos =
                    self.emit_with_operands(OpCode::Jump, OpCode::u16(TENTATIVE_JUMP_POS));

                self.replace_instruction(
                    jump_not_truthy_pos,
                    code::make_u16(
                        OpCode::JumpIfNotTruthy,
                        self.current_instructions().len() as u16,
                    ),
                );

                match alternative {
                    Some(alt) => {
                        self.compile_block_statement(alt)?;
                        if self.last_instruction_is(OpCode::Pop) {
                            self.remove_last_pop();
                        }
                    }
                    None => {
                        self.emit(OpCode::Null);
                    }
                }

                self.replace_instruction(
                    jump_pos,
                    code::make_u16(OpCode::Jump, self.current_instructions().len() as u16),
                );
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
            Expression::FunctionLiteral(_args, body) => {
                self.enter_scope();
                self.compile_block_statement(body)?;
                // Take care of implicit return like `fn() { 5 }`
                if self.last_instruction_is(OpCode::Pop) {
                    self.replace_last_pop_with_return();
                }
                // Take care of empty body like `fn() { }`
                if !self.last_instruction_is(OpCode::ReturnValue) {
                    self.emit(OpCode::Return);
                }
                let instructions = self.leave_scope();

                let compiled_function = Rc::new(Object::CompiledFunction(instructions));
                let const_index = self.add_constant(compiled_function);
                self.emit_with_operands(OpCode::Constant, OpCode::u16(const_index));
            }
            Expression::Call(func, _args) => {
                self.compile_expression(func)?;
                self.emit(OpCode::Call);
            }
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

    fn enter_scope(&mut self) {
        let scope = CompilationScope::new();
        self.scopes.push(scope);
        self.scope_index += 1;
    }

    fn leave_scope(&mut self) -> Instructions {
        let scope = self.scopes.pop().expect("no scope to leave from");
        self.scope_index -= 1;
        return scope.instructions;
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
        self.scopes[self.scope_index].emit_with_operands(op_code, operands)
    }

    fn current_instructions(&self) -> &Instructions {
        &self.scopes[self.scope_index].instructions
    }

    // Not updating last/previous_instruction because we still don't have cases
    // that require it.
    fn replace_instruction(&mut self, pos: usize, instruction: Instructions) {
        self.scopes[self.scope_index].replace_instruction(pos, instruction)
    }

    fn last_instruction_is(&self, op_code: OpCode) -> bool {
        self.scopes[self.scope_index].last_instruction_is(op_code)
    }

    fn remove_last_pop(&mut self) {
        self.scopes[self.scope_index].remove_last_pop()
    }

    fn replace_last_pop_with_return(&mut self) {
        self.scopes[self.scope_index].replace_last_pop_with_return()
    }

    pub fn bytecode(self) -> Bytecode {
        let scope = &self.scopes[self.scope_index];
        Bytecode {
            // TODO: Can't this be done without cloning? Compiler's ownership moves to Bytecode
            // anyway...
            instructions: scope.instructions.clone(),
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
    use crate::code::{make, make_u16, print_instructions, Instructions, OpCode};
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;
    use std::borrow::Borrow;

    #[test]
    fn test_print_instructions() {
        let insts = vec![
            make_u16(OpCode::Constant, 1),
            make_u16(OpCode::Constant, 2),
            make_u16(OpCode::Constant, 65535),
        ]
        .concat();
        let expected = "0000 OpConstant 1
0003 OpConstant 2
0006 OpConstant 65535";

        assert_eq!(&print_instructions(&insts), expected);
    }

    #[test]
    fn compile() {
        test_compile(vec![
            (
                "1 + 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::Add),
                    make(OpCode::Pop),
                ],
            ),
            (
                "1 - 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::Sub),
                    make(OpCode::Pop),
                ],
            ),
            (
                "1 * 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::Mul),
                    make(OpCode::Pop),
                ],
            ),
            (
                "2 / 1",
                vec![Object::Integer(2), Object::Integer(1)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::Div),
                    make(OpCode::Pop),
                ],
            ),
            ("true", vec![], vec![make(OpCode::True), make(OpCode::Pop)]),
            (
                "false",
                vec![],
                vec![make(OpCode::False), make(OpCode::Pop)],
            ),
            (
                "1 == 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::Equal),
                    make(OpCode::Pop),
                ],
            ),
            (
                "1 != 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::NotEqual),
                    make(OpCode::Pop),
                ],
            ),
            (
                "1 > 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::GreaterThan),
                    make(OpCode::Pop),
                ],
            ),
            (
                "1 < 2",
                vec![Object::Integer(2), Object::Integer(1)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::GreaterThan),
                    make(OpCode::Pop),
                ],
            ),
        ]);
    }

    #[test]
    fn prefix_expression() {
        test_compile(vec![
            (
                "!true",
                vec![],
                vec![make(OpCode::True), make(OpCode::Bang), make(OpCode::Pop)],
            ),
            (
                "-123",
                vec![Object::Integer(123)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make(OpCode::Minus),
                    make(OpCode::Pop),
                ],
            ),
        ]);
    }

    #[test]
    fn if_expression() {
        test_compile(vec![
            (
                "if (true) { 10 }; 3333;",
                vec![Object::Integer(10), Object::Integer(3333)],
                vec![
                    make(OpCode::True),
                    make_u16(OpCode::JumpIfNotTruthy, 10),
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Jump, 11),
                    make(OpCode::Null),
                    make(OpCode::Pop),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::Pop),
                ],
            ),
            (
                "if (true) { 10 } else { 20 }; 3333;",
                vec![
                    Object::Integer(10),
                    Object::Integer(20),
                    Object::Integer(3333),
                ],
                vec![
                    make(OpCode::True),
                    make_u16(OpCode::JumpIfNotTruthy, 10),
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Jump, 13),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::Pop),
                    make_u16(OpCode::Constant, 2),
                    make(OpCode::Pop),
                ],
            ),
        ]);
    }

    #[test]
    fn global_let_statements() {
        test_compile(vec![
            (
                "let one = 1; let two = 2;",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::SetGlobal, 0),
                    make_u16(OpCode::Constant, 1),
                    make_u16(OpCode::SetGlobal, 1),
                ],
            ),
            (
                "let one = 1; one;",
                vec![Object::Integer(1)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::SetGlobal, 0),
                    make_u16(OpCode::GetGlobal, 0),
                    make(OpCode::Pop),
                ],
            ),
            (
                "let one = 1; let two = one; two;",
                vec![Object::Integer(1)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::SetGlobal, 0),
                    make_u16(OpCode::GetGlobal, 0),
                    make_u16(OpCode::SetGlobal, 1),
                    make_u16(OpCode::GetGlobal, 1),
                    make(OpCode::Pop),
                ],
            ),
        ]);
    }

    #[test]
    fn string_expressions() {
        test_compile(vec![
            (
                r#""hello""#,
                vec![Object::String("hello".to_string())],
                vec![make_u16(OpCode::Constant, 0), make(OpCode::Pop)],
            ),
            (
                r#""hel" + "lo""#,
                vec![
                    Object::String("hel".to_string()),
                    Object::String("lo".to_string()),
                ],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::Add),
                    make(OpCode::Pop),
                ],
            ),
        ]);
    }

    #[test]
    fn array_expressions() {
        test_compile(vec![
            (
                "[]",
                vec![],
                vec![make_u16(OpCode::Array, 0), make(OpCode::Pop)],
            ),
            (
                "[1, 2, 3]",
                vec![Object::Integer(1), Object::Integer(2), Object::Integer(3)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make_u16(OpCode::Constant, 2),
                    make_u16(OpCode::Array, 3),
                    make(OpCode::Pop),
                ],
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
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::Sub),
                    make_u16(OpCode::Constant, 2),
                    make_u16(OpCode::Constant, 3),
                    make(OpCode::Add),
                    make_u16(OpCode::Constant, 4),
                    make_u16(OpCode::Constant, 5),
                    make(OpCode::Mul),
                    make_u16(OpCode::Array, 3),
                    make(OpCode::Pop),
                ],
            ),
        ]);
    }

    #[test]
    fn hash_expression() {
        test_compile(vec![
            (
                "{}",
                vec![],
                vec![make_u16(OpCode::Hash, 0), make(OpCode::Pop)],
            ),
            (
                r#"{ 1: "hello", "foo": 1 + 2 }"#,
                vec![
                    Object::String("foo".to_string()),
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(1),
                    Object::String("hello".to_string()),
                ],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make_u16(OpCode::Constant, 2),
                    make(OpCode::Add),
                    make_u16(OpCode::Constant, 3),
                    make_u16(OpCode::Constant, 4),
                    make_u16(OpCode::Hash, 2),
                    make(OpCode::Pop),
                ],
            ),
        ]);
    }

    #[test]
    fn index_expression() {
        test_compile(vec![
            (
                "[1, 2][0]",
                vec![Object::Integer(1), Object::Integer(2), Object::Integer(0)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make_u16(OpCode::Array, 2),
                    make_u16(OpCode::Constant, 2),
                    make(OpCode::Index),
                    make(OpCode::Pop),
                ],
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
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make_u16(OpCode::Constant, 2),
                    make(OpCode::Add),
                    make_u16(OpCode::Constant, 3),
                    make_u16(OpCode::Constant, 4),
                    make_u16(OpCode::Constant, 5),
                    make(OpCode::Add),
                    make_u16(OpCode::Hash, 2),
                    make_u16(OpCode::Constant, 6),
                    make(OpCode::Index),
                    make(OpCode::Pop),
                ],
            ),
        ]);
    }

    #[test]
    fn function_call() {
        test_compile(vec![
            (
                "fn() { return 5 + 10 }",
                vec![
                    Object::Integer(5),
                    Object::Integer(10),
                    Object::CompiledFunction(
                        vec![
                            make_u16(OpCode::Constant, 0),
                            make_u16(OpCode::Constant, 1),
                            make(OpCode::Add),
                            make(OpCode::ReturnValue),
                        ]
                        .concat(),
                    ),
                ],
                vec![make_u16(OpCode::Constant, 2), make(OpCode::Pop)],
            ),
            (
                "fn() { 5 + 10 }",
                vec![
                    Object::Integer(5),
                    Object::Integer(10),
                    Object::CompiledFunction(
                        vec![
                            make_u16(OpCode::Constant, 0),
                            make_u16(OpCode::Constant, 1),
                            make(OpCode::Add),
                            make(OpCode::ReturnValue),
                        ]
                        .concat(),
                    ),
                ],
                vec![make_u16(OpCode::Constant, 2), make(OpCode::Pop)],
            ),
            (
                "fn() { 1; 2 }",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::CompiledFunction(
                        vec![
                            make_u16(OpCode::Constant, 0),
                            make(OpCode::Pop),
                            make_u16(OpCode::Constant, 1),
                            make(OpCode::ReturnValue),
                        ]
                        .concat(),
                    ),
                ],
                vec![make_u16(OpCode::Constant, 2), make(OpCode::Pop)],
            ),
            (
                "fn() { }",
                vec![Object::CompiledFunction(make(OpCode::Return))],
                vec![make_u16(OpCode::Constant, 0), make(OpCode::Pop)],
            ),
        ]);
    }

    #[test]
    fn function_calls() {
        test_compile(vec![
            (
                "fn() { 24 }();",
                vec![
                    Object::Integer(24),
                    Object::CompiledFunction(
                        vec![make_u16(OpCode::Constant, 0), make(OpCode::ReturnValue)].concat(),
                    ),
                ],
                vec![
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::Call),
                    make(OpCode::Pop),
                ],
            ),
            (
                "let noArg = fn() { 24 }; noArg();",
                vec![
                    Object::Integer(24),
                    Object::CompiledFunction(
                        vec![make_u16(OpCode::Constant, 0), make(OpCode::ReturnValue)].concat(),
                    ),
                ],
                vec![
                    make_u16(OpCode::Constant, 1),
                    make_u16(OpCode::SetGlobal, 0),
                    make_u16(OpCode::GetGlobal, 0),
                    make(OpCode::Call),
                    make(OpCode::Pop),
                ],
            ),
        ]);
    }

    fn test_compile(tests: Vec<(&str, Vec<Object>, Vec<Instructions>)>) {
        for (input, expected_constants, expected_instructions) in tests {
            let program = parse(input);

            let mut compiler = Compiler::new();
            match compiler.compile(&program) {
                Err(error) => panic!("failed to compile input `{}`: {}", input, error),
                _ => {}
            }
            let bytecode = compiler.bytecode();

            assert_eq!(
                print_instructions(&bytecode.instructions),
                print_instructions(&expected_instructions.concat()),
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
