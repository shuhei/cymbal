// Instructions are a series of bytes.
pub type Instructions = Vec<u8>;

// Generates an enum that can be easily converted from u8.
//
// ```
// byte_enum!(Something, [Foo, Bar]);
// ```
//
// generates:
//
// ```
// #repr(u8)
// pub enum Something {
//     Foo,
//     Bar,
// }
//
// impl Something {
//     fn from_byte(byte: u8) -> Option<Something> {
//         if (byte == 0u8) {
//             return Some(Something::Foo);
//         }
//         if (byte == 1u8) {
//             return Some(Something::Bar);
//         }
//         None
//     }
// }
// ```
macro_rules! byte_enum {
    (@step $_idx:expr, $name:ident, $_byte:ident, []) => {
        None as Option<$name>
    };
    (@step $idx:expr, $name:ident, $byte:ident, [$head:ident, $($tail:ident,)*]) => {
        if $byte == $idx {
            return Some($name::$head);
        }
        byte_enum!(@step $idx + 1u8, $name, $byte, [$($tail,)*]);
    };
    ($name:ident, [$($var: ident),+]) => {
        #[repr(u8)]
        pub enum $name {
            $($var,)+
        }
        impl $name {
            pub fn from_byte(byte: u8) -> Option<$name> {
                byte_enum!(@step 0u8, $name, byte, [$($var,)+]);
                None
            }
        }
    };
}

byte_enum!(OpCode, [Constant, Pop, Add]);

pub fn print_instructions(insts: &Instructions) -> String {
    let mut result = String::new();
    let mut i = 0;
    while i < insts.len() {
        let op_code = insts[i];
        if let Some(def) = lookup_definition(op_code) {
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

pub fn constant(i: u16) -> Vec<u8> {
    let bytes = i.to_be_bytes();
    vec![OpCode::Constant as u8, bytes[0], bytes[1]]
}

pub fn add() -> Vec<u8> {
    vec![OpCode::Add as u8]
}

pub fn pop() -> Vec<u8> {
    vec![OpCode::Pop as u8]
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

fn lookup_definition(op_code: u8) -> Option<Definition> {
    match OpCode::from_byte(op_code) {
        Some(OpCode::Constant) => Some(Definition {
            name: "OpConstant".to_string(),
            widths: vec![2],
        }),
        Some(OpCode::Add) => Some(Definition {
            name: "OpAdd".to_string(),
            widths: vec![],
        }),
        Some(OpCode::Pop) => Some(Definition {
            name: "OpPop".to_string(),
            widths: vec![],
        }),
        _ => None,
    }
}
