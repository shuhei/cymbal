use std::fmt;

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Constant>,
}

impl Bytecode {
    pub fn new(instructions: Instructions, constants: Vec<Constant>) -> Self {
        Bytecode {
            instructions,
            constants,
        }
    }
}

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
        #[derive(Debug, Clone, Copy, PartialEq)]
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

byte_enum!(
    OpCode,
    [
        Constant,
        Pop,
        Add,
        Sub,
        Mul,
        Div,
        True,
        False,
        Equal,
        NotEqual,
        GreaterThan,
        Minus,
        Bang,
        JumpIfNotTruthy,
        Jump,
        Null,
        GetGlobal,
        SetGlobal,
        Array,
        Hash,
        Index,
        Call,
        ReturnValue,
        Return,
        GetLocal,
        SetLocal,
        GetBuiltin,
        Closure,
        GetFree
    ]
);

impl OpCode {
    pub fn u16(i: u16) -> Vec<u8> {
        let bytes = i.to_be_bytes();
        vec![bytes[0], bytes[1]]
    }

    pub fn u16_u8(first: u16, second: u8) -> Vec<u8> {
        let bytes = first.to_be_bytes();
        vec![bytes[0], bytes[1], second]
    }
}

pub fn print_instructions(insts: &[u8]) -> String {
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
            result.push_str(&def.name);
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

fn read_operands(def: &Definition, insts: &[u8], start: usize) -> (Vec<usize>, usize) {
    let mut offset = 0;
    let mut operands = Vec::with_capacity(def.widths.len());
    for width in &def.widths {
        match width {
            2 => {
                operands.push(read_uint16(insts, start + offset) as usize);
            }
            1 => {
                operands.push(insts[start + offset] as usize);
            }
            _ => {}
        }
        offset += width;
    }
    (operands, offset)
}

pub fn read_uint16(insts: &[u8], start: usize) -> u16 {
    u16::from_be_bytes([insts[start], insts[start + 1]])
}

pub fn make(op_code: OpCode) -> Instructions {
    vec![op_code as u8]
}

pub fn make_u8(op_code: OpCode, operand: u8) -> Instructions {
    vec![op_code as u8, operand]
}

pub fn make_u16(op_code: OpCode, operand: u16) -> Instructions {
    let bytes = u16::to_be_bytes(operand);
    vec![op_code as u8, bytes[0], bytes[1]]
}

pub fn make_u16_u8(op_code: OpCode, first: u16, second: u8) -> Instructions {
    let bytes = u16::to_be_bytes(first);
    vec![op_code as u8, bytes[0], bytes[1], second]
}

pub struct Definition {
    pub name: String,
    pub widths: Vec<usize>,
}

fn lookup_definition(byte: u8) -> Option<Definition> {
    OpCode::from_byte(byte).map(|op_code| match op_code {
        OpCode::Constant => Definition {
            name: "OpConstant".to_string(),
            widths: vec![2],
        },
        OpCode::Pop => Definition {
            name: "OpPop".to_string(),
            widths: vec![],
        },
        OpCode::Add => Definition {
            name: "OpAdd".to_string(),
            widths: vec![],
        },
        OpCode::Sub => Definition {
            name: "OpSub".to_string(),
            widths: vec![],
        },
        OpCode::Mul => Definition {
            name: "OpMul".to_string(),
            widths: vec![],
        },
        OpCode::Div => Definition {
            name: "OpDiv".to_string(),
            widths: vec![],
        },
        OpCode::True => Definition {
            name: "OpTrue".to_string(),
            widths: vec![],
        },
        OpCode::False => Definition {
            name: "OpFalse".to_string(),
            widths: vec![],
        },
        OpCode::Equal => Definition {
            name: "OpEqual".to_string(),
            widths: vec![],
        },
        OpCode::NotEqual => Definition {
            name: "OpNotEqual".to_string(),
            widths: vec![],
        },
        OpCode::GreaterThan => Definition {
            name: "OpGreaterThan".to_string(),
            widths: vec![],
        },
        OpCode::Minus => Definition {
            name: "OpMinus".to_string(),
            widths: vec![],
        },
        OpCode::Bang => Definition {
            name: "OpBang".to_string(),
            widths: vec![],
        },
        OpCode::JumpIfNotTruthy => Definition {
            name: "OpJumpIfNotTruthy".to_string(),
            widths: vec![2],
        },
        OpCode::Jump => Definition {
            name: "OpJump".to_string(),
            widths: vec![2],
        },
        OpCode::Null => Definition {
            name: "OpNull".to_string(),
            widths: vec![],
        },
        OpCode::GetGlobal => Definition {
            name: "OpGetGlobal".to_string(),
            widths: vec![2],
        },
        OpCode::SetGlobal => Definition {
            name: "OpSetGlobal".to_string(),
            widths: vec![2],
        },
        OpCode::Array => Definition {
            name: "OpArray".to_string(),
            widths: vec![2],
        },
        OpCode::Hash => Definition {
            name: "OpHash".to_string(),
            widths: vec![2],
        },
        OpCode::Index => Definition {
            name: "OpIndex".to_string(),
            widths: vec![],
        },
        OpCode::Call => Definition {
            name: "OpCall".to_string(),
            widths: vec![1],
        },
        OpCode::ReturnValue => Definition {
            name: "OpReturnValue".to_string(),
            widths: vec![],
        },
        OpCode::Return => Definition {
            name: "OpReturn".to_string(),
            widths: vec![],
        },
        OpCode::GetLocal => Definition {
            name: "OpGetLocal".to_string(),
            widths: vec![1],
        },
        OpCode::SetLocal => Definition {
            name: "OpSetLocal".to_string(),
            widths: vec![1],
        },
        OpCode::GetBuiltin => Definition {
            name: "OpGetBuiltin".to_string(),
            widths: vec![1],
        },
        OpCode::Closure => Definition {
            name: "OpClosure".to_string(),
            widths: vec![2, 1],
        },
        OpCode::GetFree => Definition {
            name: "OpGetFree".to_string(),
            widths: vec![1],
        },
    })
}

#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
    Integer(i64),
    Float(f64),
    String(String),
    CompiledFunction(CompiledFunction),
}

impl Constant {
    pub fn type_name(&self) -> &str {
        match self {
            Constant::Integer(_) => "INTEGER",
            Constant::Float(_) => "FLOAT",
            Constant::String(_) => "STRING",
            Constant::CompiledFunction(_) => "COMPILED_FUNCTION",
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constant::Integer(value) => write!(f, "{}", value),
            Constant::Float(value) => write!(f, "{}", value),
            Constant::String(value) => write!(f, "\"{}\"", value),
            Constant::CompiledFunction(cf) => write!(f, "{}", cf),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompiledFunction {
    pub instructions: Instructions,
    pub num_locals: u8,
    pub num_parameters: u8,
}

impl fmt::Display for CompiledFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "compiled function ({} locals, {} parameters): {}",
            self.num_locals,
            self.num_parameters,
            print_instructions(&self.instructions)
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::code::{make, make_u16, make_u16_u8, make_u8, print_instructions, OpCode};

    #[test]
    fn test_print_instructions() {
        let insts = vec![
            make_u16(OpCode::Constant, 1),
            make_u16(OpCode::Constant, 2),
            make_u16(OpCode::Constant, 65535),
            make_u8(OpCode::GetLocal, 1),
            make_u16_u8(OpCode::Closure, 65535, 255),
            make(OpCode::Pop),
        ]
        .concat();
        let expected = "0000 OpConstant 1
0003 OpConstant 2
0006 OpConstant 65535
0009 OpGetLocal 1
0011 OpClosure 65535 255
0015 OpPop";

        assert_eq!(&print_instructions(&insts), expected);
    }
}
