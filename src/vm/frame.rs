use crate::code::Instructions;
use crate::object::CompiledFunction;

#[derive(Debug)]
pub struct Frame {
    func: CompiledFunction,
    pub ip: usize,
    pub base_pointer: usize,
}

impl Frame {
    pub fn new(func: CompiledFunction, base_pointer: usize) -> Self {
        Frame {
            func,
            ip: 0,
            base_pointer,
        }
    }

    pub fn instructions(&self) -> &Instructions {
        &self.func.instructions
    }
}
