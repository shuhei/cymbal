use crate::ast::{Expression, Infix, Program, Statement};
use crate::code;
use crate::code::Instructions;
use crate::object::Object;
use std::fmt;
use std::rc::Rc;

pub struct Compiler {
    pub instructions: Instructions,
    pub constants: Vec<Rc<Object>>,
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
            Expression::Infix(infix, left, right) => {
                self.compile_expression(left)?;
                self.compile_expression(right)?;
                match infix {
                    Infix::Plus => {
                        self.add_instruction(code::add());
                    }
                    inf => {
                        return Err(CompileError::UnknownOperator(inf.clone()));
                    }
                }
            }
            Expression::IntegerLiteral(value) => {
                let constant = Rc::new(Object::Integer(*value));
                let ins = code::constant(self.add_constant(constant));
                self.add_instruction(ins);
            }
            Expression::StringLiteral(value) => {
                let constant = Rc::new(Object::String(value.clone()));
                let ins = code::constant(self.add_constant(constant));
                self.add_instruction(ins);
            }
            _ => {}
        }
        Ok(())
    }

    fn add_constant(&mut self, constant: Rc<Object>) -> u16 {
        self.constants.push(constant);
        // TODO: Check the limit
        (self.constants.len() - 1) as u16
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
    UnknownOperator(Infix),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompileError::UnknownOperator(infix) => write!(f, "unknown operator: {}", infix),
        }
    }
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Rc<Object>>,
}
