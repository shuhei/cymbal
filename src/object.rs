use crate::ast::{BlockStatement, Infix, Prefix};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub type EvalResult = Result<Object, EvalError>;

#[derive(Clone)]
pub enum Object {
    Boolean(bool),
    Integer(i64),
    String(String),
    Null,
    Return(Box<Object>),
    Function(Vec<String>, BlockStatement, Rc<RefCell<Environment>>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Boolean(value) => write!(f, "{}", value),
            Object::Integer(value) => write!(f, "{}", value),
            Object::String(value) => write!(f, "\"{}\"", value),
            Object::Null => write!(f, "null"),
            Object::Return(value) => write!(f, "{}", *value),
            Object::Function(params, body, _) => {
                write!(f, "fn({}) {{\n{}\n}}", params.join(", "), body)
            }
        }
    }
}

impl Object {
    pub fn type_name(&self) -> &str {
        match self {
            Object::Boolean(_) => "BOOLEAN",
            Object::Integer(_) => "INTEGER",
            Object::String(_) => "STRING",
            Object::Null => "NULL",
            Object::Return(_) => "RETURN",
            Object::Function(_, _, _) => "FUNCTION",
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Boolean(value) => *value,
            Object::Null => false,
            _ => true,
        }
    }
}

pub enum EvalError {
    TypeMismatch(Infix, Object, Object),
    UnknownPrefixOperator(Prefix, Object),
    UnknownInfixOperator(Infix, Object, Object),
    IdentifierNotFound(String),
    NotFunction(Object),
    WrongArgumentCount { expected: usize, given: usize },
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvalError::TypeMismatch(infix, left, right) => write!(
                f,
                "type mismatch: {} {} {}",
                left.type_name(),
                infix,
                right.type_name()
            ),
            EvalError::UnknownPrefixOperator(prefix, right) => {
                write!(f, "unknown operator: {}{}", prefix, right.type_name())
            }
            EvalError::UnknownInfixOperator(infix, left, right) => write!(
                f,
                "unknown operator: {} {} {}",
                left.type_name(),
                infix,
                right.type_name()
            ),
            EvalError::IdentifierNotFound(name) => write!(f, "identifier not found: {}", name),
            EvalError::NotFunction(obj) => write!(f, "not a function: {}", obj.type_name()),
            EvalError::WrongArgumentCount { expected, given } => write!(
                f,
                "wrong number of arguments: expected {}, given {}",
                expected, given
            ),
        }
    }
}

pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        let mut env = Environment {
            store: HashMap::new(),
            outer: None,
        };
        // TODO: Should `null` be a reserved word?
        env.set("null", Object::Null);
        env
    }

    pub fn extend(outer: Rc<RefCell<Self>>) -> Environment {
        Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.get(name) {
            Some(value) => Some(value.clone()),
            None => self
                .outer
                .as_ref()
                .and_then(|o| o.borrow().get(name).clone()),
        }
    }

    pub fn set(&mut self, name: &str, val: Object) {
        self.store.insert(name.to_string(), val);
    }
}
