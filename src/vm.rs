use crate::ast::{Infix, Prefix};
use crate::code;
use crate::code::{Instructions, OpCode};
use crate::compiler::Bytecode;
use crate::object::Object;
use std::fmt;
use std::rc::Rc;

pub const STACK_SIZE: usize = 2048;
pub const GLOBAL_SIZE: usize = 65536;
pub const NULL: Object = Object::Null;

#[derive(Debug)]
pub struct Vm {
    constants: Vec<Rc<Object>>,
    instructions: Instructions,

    stack: Vec<Rc<Object>>,
    sp: usize, // Stack pointer. Always points to the next value. Top of the stack is stack[sp - 1];

    globals: Vec<Rc<Object>>,
}

impl Vm {
    pub fn new(bytecode: Bytecode) -> Self {
        Vm {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: Vec::with_capacity(STACK_SIZE),
            sp: 0,
            globals: Vec::with_capacity(GLOBAL_SIZE),
        }
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        let mut ip = 0;
        while ip < self.instructions.len() {
            // Fetch
            let op_code_byte = self.instructions[ip];
            match OpCode::from_byte(op_code_byte) {
                Some(OpCode::Constant) => {
                    let const_index = code::read_uint16(&self.instructions, ip + 1) as usize;
                    ip += 2;

                    if const_index < self.constants.len() {
                        let constant = Rc::clone(&self.constants[const_index]);
                        self.push(constant)?;
                    } else {
                        return Err(VmError::InvalidConstIndex(
                            const_index,
                            self.constants.len(),
                        ));
                    }
                }
                Some(OpCode::Pop) => {
                    self.pop()?;
                }
                Some(OpCode::Add) => {
                    self.execute_binary_operation(OpCode::Add)?;
                }
                Some(OpCode::Sub) => {
                    self.execute_binary_operation(OpCode::Sub)?;
                }
                Some(OpCode::Mul) => {
                    self.execute_binary_operation(OpCode::Mul)?;
                }
                Some(OpCode::Div) => {
                    self.execute_binary_operation(OpCode::Div)?;
                }
                Some(OpCode::True) => {
                    self.push(Rc::new(Object::Boolean(true)))?;
                }
                Some(OpCode::False) => {
                    self.push(Rc::new(Object::Boolean(false)))?;
                }
                Some(OpCode::Equal) => {
                    self.execute_comparison(OpCode::Equal)?;
                }
                Some(OpCode::NotEqual) => {
                    self.execute_comparison(OpCode::NotEqual)?;
                }
                Some(OpCode::GreaterThan) => {
                    self.execute_comparison(OpCode::GreaterThan)?;
                }
                Some(OpCode::Minus) => {
                    let right = self.pop()?;
                    match &*right {
                        Object::Integer(value) => {
                            self.push(Rc::new(Object::Integer(-value)))?;
                        }
                        obj => {
                            return Err(VmError::UnsupportedPrefix(Prefix::Minus, obj.clone()));
                        }
                    }
                }
                Some(OpCode::Bang) => {
                    let right = self.pop()?;
                    self.push(Rc::new(Object::Boolean(!right.is_truthy())))?;
                }
                Some(OpCode::JumpIfNotTruthy) => {
                    let pos = code::read_uint16(&self.instructions, ip + 1) as usize;
                    ip += 2;

                    let condition = self.pop()?;
                    if !condition.is_truthy() {
                        // `pos - 1` because `ip` will be incremented later.
                        ip = pos - 1;
                    }
                }
                Some(OpCode::Jump) => {
                    let pos = code::read_uint16(&self.instructions, ip + 1) as usize;
                    // `pos - 1` because `ip` will be incremented later.
                    ip = pos - 1;
                }
                Some(OpCode::Null) => {
                    // TODO: This `Rc` is not neccessary because NULL is a constant...
                    self.push(Rc::new(NULL))?;
                }
                Some(OpCode::GetGlobal) => {
                    let global_index = code::read_uint16(&self.instructions, ip + 1) as usize;
                    ip += 2;

                    self.push(Rc::clone(&self.globals[global_index]))?;
                }
                Some(OpCode::SetGlobal) => {
                    let global_index = code::read_uint16(&self.instructions, ip + 1) as usize;
                    ip += 2;

                    let popped = self.pop()?;
                    if global_index == self.globals.len() {
                        self.globals.push(popped);
                    } else {
                        self.globals[global_index] = popped;
                    }
                }
                None => {
                    return Err(VmError::UnknownOpCode(op_code_byte));
                }
            }
            ip += 1;
        }
        Ok(())
    }

    fn execute_binary_operation(&mut self, op_code: OpCode) -> Result<(), VmError> {
        let right = self.pop()?;
        let left = self.pop()?;
        match (&*left, &*right) {
            (Object::Integer(l), Object::Integer(r)) => {
                self.execute_integer_binary_operation(op_code, l, r)
            }
            (l, r) => Err(VmError::TypeMismatch(l.clone(), r.clone())),
        }
    }

    fn execute_integer_binary_operation(
        &mut self,
        op_code: OpCode,
        left: &i64,
        right: &i64,
    ) -> Result<(), VmError> {
        let result = match op_code {
            OpCode::Add => left + right,
            OpCode::Sub => left - right,
            OpCode::Mul => left * right,
            OpCode::Div => left / right,
            _ => {
                // This happens only when this vm is wrong.
                panic!("not integer binary operation: {:?}", op_code);
            }
        };

        self.push(Rc::new(Object::Integer(result)))
    }

    fn execute_comparison(&mut self, op_code: OpCode) -> Result<(), VmError> {
        let right = self.pop()?;
        let left = self.pop()?;

        match (&*left, &*right) {
            (Object::Integer(l), Object::Integer(r)) => {
                match op_code {
                    OpCode::Equal => self.push(Rc::new(Object::Boolean(l == r))),
                    OpCode::NotEqual => self.push(Rc::new(Object::Boolean(l != r))),
                    OpCode::GreaterThan => self.push(Rc::new(Object::Boolean(l > r))),
                    _ => {
                        // This happens only when this vm is wrong.
                        panic!("unknown operator: {:?}", op_code);
                    }
                }
            }
            (Object::Boolean(l), Object::Boolean(r)) => {
                match op_code {
                    OpCode::Equal => self.push(Rc::new(Object::Boolean(l == r))),
                    OpCode::NotEqual => self.push(Rc::new(Object::Boolean(l != r))),
                    OpCode::GreaterThan => Err(VmError::UnsupportedInfix(
                        Infix::Gt,
                        Object::Boolean(*l),
                        Object::Boolean(*r),
                    )),
                    _ => {
                        // This happens only when this vm is wrong.
                        panic!("unknown operator: {:?}", op_code);
                    }
                }
            }
            (l, r) => Err(VmError::TypeMismatch(l.clone(), r.clone())),
        }
    }

    fn push(&mut self, obj: Rc<Object>) -> Result<(), VmError> {
        if self.sp >= STACK_SIZE {
            return Err(VmError::StackOverflow);
        }
        if self.sp < self.stack.len() {
            self.stack[self.sp] = obj;
        } else {
            // `Vec` doesn't allow index assignment if the index is not filled yet.
            self.stack.push(obj);
        }
        self.sp += 1;
        Ok(())
    }

    fn pop(&mut self) -> Result<Rc<Object>, VmError> {
        let popped = self.stack.get(self.sp - 1);
        self.sp -= 1;
        popped.map(|o| Rc::clone(o)).ok_or(VmError::StackEmpty)
    }

    pub fn last_popped_stack_elem(&self) -> Option<Rc<Object>> {
        self.stack.get(self.sp).map(|o| Rc::clone(o))
    }
}

pub enum VmError {
    UnknownOpCode(u8),
    InvalidConstIndex(usize, usize),
    StackOverflow,
    StackEmpty,
    TypeMismatch(Object, Object),
    UnsupportedInfix(Infix, Object, Object),
    UnsupportedPrefix(Prefix, Object),
}

impl fmt::Display for VmError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VmError::UnknownOpCode(op_code) => write!(f, "unknown op code: {}", op_code),
            VmError::InvalidConstIndex(given, length) => {
                write!(f, "invalid const index: {} / {}", given, length)
            }
            VmError::StackOverflow => write!(f, "stack overflow"),
            VmError::StackEmpty => write!(f, "stack empty"),
            VmError::TypeMismatch(left, right) => write!(
                f,
                "type mismatch: {} {}",
                left.type_name(),
                right.type_name()
            ),
            VmError::UnsupportedInfix(infix, left, right) => write!(
                f,
                "unsupported infix: {} {} {}",
                left.type_name(),
                infix,
                right.type_name()
            ),
            VmError::UnsupportedPrefix(prefix, right) => {
                write!(f, "unsupported prefix: {} {}", prefix, right.type_name())
            }
        }
    }
}
