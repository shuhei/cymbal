use crate::ast::{Expression, Program, Statement};
use crate::code::{Instructions, OpCode};
use crate::object::Object;
use std::fmt;

pub struct Compiler {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: vec![],
            constants: vec![],
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
            Statement::Expression(exp) => self.compile_expression(exp)?,
            _ => {}
        }
        Ok(())
    }

    pub fn compile_expression(&mut self, expression: &Expression) -> Result<(), CompileError> {
        match expression {
            Expression::Infix(_, left, right) => {
                self.compile_expression(left)?;
                self.compile_expression(right)?;
            }
            Expression::IntegerLiteral(value) => {
                let constant = Object::Integer(*value);
                let ins = OpCode::constant(self.add_constant(constant));
                self.add_instruction(ins);
            }
            Expression::StringLiteral(value) => {
                self.constants.push(Object::String(value.clone()));
            }
            _ => {}
        }
        Ok(())
    }

    fn add_constant(&mut self, constant: Object) -> usize {
        self.constants.push(constant);
        self.constants.len() - 1
    }

    fn add_instruction(&mut self, ins: Vec<u8>) -> usize {
        let width = ins.len();
        self.instructions.extend(ins);
        self.instructions.len() - width
    }

    pub fn bytecode(self) -> Bytecode {
        Bytecode {
            instructions: self.instructions,
            constants: self.constants,
        }
    }
}

pub enum CompileError {
    Foo,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompileError::Foo => write!(f, "foo"),
        }
    }
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}
