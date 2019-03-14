// Instructions are a series of bytes.
pub type Instructions = Vec<u8>;

pub fn print_instructions(insts: &Instructions) -> String {
    let mut result = String::new();
    let mut i = 0;
    while i < insts.len() {
        // TODO: Use `From` trait, or check if it's a valid op code.
        if let Some(op_code) = OpCode::from_byte(insts[i]) {
            if i > 0 {
                result.push('\n');
            }
            result.push_str(&format!("{:04} ", i));
            i += 1;
            let (operands, offset) = op_code.read_operands(insts, i);
            result.push_str(&format!("{}", op_code.name()));
            for operand in operands {
                result.push_str(&format!(" {}", operand));
            }
            i += offset;
        } else {
            // TODO: Return result?
            return "".to_string();
        }
    }
    result
}

pub enum OpCode {
    Constant = 0,
}

impl OpCode {
    pub fn from_byte(byte: u8) -> Option<OpCode> {
        if byte == (OpCode::Constant as u8) {
            Some(OpCode::Constant)
        } else {
            None
        }
    }

    pub fn constant(i: usize) -> Vec<u8> {
        let bytes = (i as u16).to_be_bytes();
        vec![OpCode::Constant as u8, bytes[0], bytes[1]]
    }

    pub fn name(&self) -> String {
        match self {
            OpCode::Constant => "OpConstant".to_string()
        }
    }

    pub fn widths(&self) -> Vec<usize> {
        match self {
            OpCode::Constant => vec![2],
        }
    }

    pub fn read_operands(&self, insts: &Instructions, start: usize) -> (Vec<usize>, usize) {
        let widths = self.widths();
        let mut offset = 0;
        let mut operands = Vec::with_capacity(widths.len());
        for width in widths {
            match width {
                2 => {
                    operands.push(u16::from_be_bytes([
                        insts[start + offset],
                        insts[start + offset + 1],
                    ]) as usize);
                }
                _ => {}
            }
            offset += width;
        }
        (operands, offset)
    }
}
