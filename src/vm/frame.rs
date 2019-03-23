use crate::code::Instructions;
use crate::object::CompiledFunction;

#[derive(Debug)]
pub struct Frame {
    func: CompiledFunction,
    pub ip: usize,
}

impl Frame {
    pub fn new(func: CompiledFunction) -> Self {
        Frame { func, ip: 0 }
    }

    pub fn instructions(&self) -> &Instructions {
        &self.func.instructions
    }
}
