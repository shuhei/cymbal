use crate::code;
use crate::code::{Instructions, OpCode};
use crate::compiler::Bytecode;
use crate::object::Object;
use std::fmt;
use std::rc::Rc;

pub const STACK_SIZE: usize = 2048;

#[derive(Debug)]
pub struct Vm {
    constants: Vec<Rc<Object>>,
    instructions: Instructions,

    stack: Vec<Rc<Object>>,
    sp: usize, // Stack pointer. Always points to the next value. Top of the stack is stack[sp - 1];
}

impl Vm {
    pub fn new(bytecode: Bytecode) -> Self {
        Vm {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: Vec::with_capacity(STACK_SIZE),
            sp: 0,
        }
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        let mut ip = 0;
        while ip < self.instructions.len() {
            // Fetch
            match OpCode::from_byte(self.instructions[ip]) {
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
                Some(OpCode::Add) => {
                    let right = self.pop()?;
                    let left = self.pop()?;
                    match (&*left, &*right) {
                        (Object::Integer(l), Object::Integer(r)) => {
                            self.push(Rc::new(Object::Integer(l + r)))?;
                        }
                        (l, r) => {
                            return Err(VmError::TypeMismatch(l.clone(), r.clone()));
                        }
                    }
                }
                Some(OpCode::Pop) => {
                    self.pop()?;
                }
                None => {}
            }
            ip += 1;
        }
        Ok(())
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
    InvalidConstIndex(usize, usize),
    StackOverflow,
    StackEmpty,
    TypeMismatch(Object, Object),
}

impl fmt::Display for VmError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VmError::InvalidConstIndex(given, length) => {
                write!(f, "invalid const index: {} / {}", given, length)
            }
            VmError::StackOverflow => write!(f, "stack overflow"),
            VmError::StackEmpty => write!(f, "stack empty"),
            VmError::TypeMismatch(left, right) => write!(
                f,
                "type mismatch {} {}",
                left.type_name(),
                right.type_name()
            ),
        }
    }
}
