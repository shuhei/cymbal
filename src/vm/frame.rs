use crate::code::Instructions;
use crate::object::Closure;

#[derive(Debug)]
pub struct Frame {
    closure: Closure,
    pub ip: usize,
    pub base_pointer: usize,
}

impl Frame {
    pub fn new(closure: Closure, base_pointer: usize) -> Self {
        Frame {
            closure,
            ip: 0,
            base_pointer,
        }
    }

    pub fn instructions(&self) -> &Instructions {
        &self.closure.func.instructions
    }
}
