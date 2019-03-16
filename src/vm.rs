use crate::code;
use crate::code::{Instructions, OP_CONSTANT};
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
            let op = self.instructions[ip];
            if op == OP_CONSTANT {
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
            ip += 1;
        }
        Ok(())
    }

    fn push(&mut self, obj: Rc<Object>) -> Result<(), VmError> {
        if self.sp >= STACK_SIZE {
            return Err(VmError::StackOverflow);
        }
        self.stack.push(obj);
        self.sp += 1;
        Ok(())
    }

    pub fn stack_top(&self) -> Option<Rc<Object>> {
        if self.sp > 0 {
            self.stack.get(self.sp - 1).map(|o| Rc::clone(o))
        } else {
            None
        }
    }
}

pub enum VmError {
    InvalidConstIndex(usize, usize),
    StackOverflow,
}

impl fmt::Display for VmError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VmError::InvalidConstIndex(given, length) => {
                write!(f, "invalid const index: {} / {}", given, length)
            }
            VmError::StackOverflow => write!(f, "stack overflow"),
        }
    }
}
