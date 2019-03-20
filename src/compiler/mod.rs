pub mod symbol_table;

use crate::ast::{BlockStatement, Expression, Infix, Prefix, Program, Statement};
use crate::code;
use crate::code::{Instructions, OpCode};
use crate::compiler::symbol_table::SymbolTable;
use crate::object::Object;
use std::fmt;
use std::mem;
use std::rc::Rc;

const TENTATIVE_JUMP_POS: u16 = 9999;

pub struct Compiler {
    pub instructions: Instructions,
    pub constants: Vec<Rc<Object>>,
    symbol_table: SymbolTable,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: vec![],
            constants: vec![],
            symbol_table: SymbolTable::new(),
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
                let symbol_index = self.symbol_table.define(name).index;
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
            Expression::Identifier(name) => match self.symbol_table.resolve(name) {
                Some(symbol) => {
                    self.emit_with_operands(OpCode::GetGlobal, OpCode::u16(symbol.index));
                }
                None => return Err(CompileError::UndefinedVariable(name.to_string())),
            },
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
        self.constants.push(constant);
        // TODO: Check the limit
        (self.constants.len() - 1) as u16
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
            constants: self.constants,
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
    pub constants: Vec<Rc<Object>>,
}
