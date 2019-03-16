// Instructions are a series of bytes.
pub type Instructions = Vec<u8>;

pub fn print_instructions(insts: &Instructions) -> String {
    let mut result = String::new();
    let mut i = 0;
    while i < insts.len() {
        let op_code = insts[i];
        if let Some(def) = lookup(op_code) {
            if i > 0 {
                result.push('\n');
            }
            result.push_str(&format!("{:04} ", i));
            i += 1;
            let (operands, offset) = read_operands(&def, insts, i);
            result.push_str(&format!("{}", def.name));
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

pub const OP_CONSTANT: u8 = 0;
pub const OP_ADD: u8 = 1;

pub fn constant(i: u16) -> Vec<u8> {
    let bytes = i.to_be_bytes();
    vec![OP_CONSTANT, bytes[0], bytes[1]]
}

pub fn add() -> Vec<u8> {
    vec![OP_ADD]
}

pub fn read_operands(def: &Definition, insts: &Instructions, start: usize) -> (Vec<usize>, usize) {
    let mut offset = 0;
    let mut operands = Vec::with_capacity(def.widths.len());
    for width in &def.widths {
        match width {
            2 => {
                operands.push(read_uint16(insts, start + offset) as usize);
            }
            _ => {}
        }
        offset += width;
    }
    (operands, offset)
}

pub fn read_uint16(insts: &Instructions, start: usize) -> u16 {
    u16::from_be_bytes([insts[start], insts[start + 1]])
}

pub struct Definition {
    pub name: String,
    pub widths: Vec<usize>,
}

pub fn lookup(op_code: u8) -> Option<Definition> {
    match op_code {
        OP_CONSTANT => Some(Definition {
            name: "OpConstant".to_string(),
            widths: vec![2],
        }),
        OP_ADD => Some(Definition {
            name: "OpAdd".to_string(),
            widths: vec![],
        }),
        _ => None,
    }
}
