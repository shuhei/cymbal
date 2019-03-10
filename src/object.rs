use std::fmt;

pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Null => write!(f, "null"),
            Object::Return(value) => write!(f, "{}", *value),
        }
    }
}
