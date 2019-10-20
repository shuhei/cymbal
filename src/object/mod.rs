pub mod builtin;
pub mod constant;
pub mod environment;

use crate::ast::{BlockStatement, Infix, Prefix};
use crate::code;
use crate::code::{CompiledFunction, Constant};
pub use crate::object::environment::Environment;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub type EvalResult = Result<Object, EvalError>;
pub type BuiltinFunction = fn(Vec<Object>) -> EvalResult;

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Boolean(bool),
    Integer(i64),
    String(String),
    Array(Vec<Object>),
    Hash(HashMap<HashKey, Object>),
    Null,
    Return(Box<Object>),
    Function(Vec<String>, BlockStatement, Rc<RefCell<Environment>>),
    Builtin(BuiltinFunction),
    CompiledFunction(CompiledFunction),
    Closure(Closure),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Boolean(value) => write!(f, "{}", value),
            Object::Integer(value) => write!(f, "{}", value),
            Object::String(value) => write!(f, "\"{}\"", value),
            Object::Array(values) => {
                let value_list = values
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "[{}]", value_list)
            }
            Object::Hash(pairs) => {
                // Print keys with a stable order for testing.
                let mut items = pairs
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>();
                items.sort();
                write!(f, "{{{}}}", items.join(", "))
            }
            Object::Null => write!(f, "null"),
            Object::Return(value) => write!(f, "{}", *value),
            Object::Function(params, body, _) => {
                write!(f, "fn({}) {{\n{}\n}}", params.join(", "), body)
            }
            Object::Builtin(_) => write!(f, "builtin function"),
            Object::CompiledFunction(cf) => write!(f, "{}", cf),
            Object::Closure(closure) => {
                let free_list = closure
                    .free
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(
                    f,
                    "closure ({}) ({}): {}",
                    closure.func.num_locals,
                    free_list,
                    code::print_instructions(&closure.func.instructions),
                )
            }
        }
    }
}

impl Object {
    pub fn from_constant(constant: &Constant) -> Object {
        match constant {
            Constant::Integer(value) => Object::Integer(*value),
            Constant::String(value) => Object::String(value.clone()),
            // TODO: Can I avoid cloning constants?
            Constant::CompiledFunction(value) => Object::CompiledFunction(value.clone()),
        }
    }

    pub fn type_name(&self) -> &str {
        match self {
            Object::Boolean(_) => "BOOLEAN",
            Object::Integer(_) => "INTEGER",
            Object::String(_) => "STRING",
            Object::Array(_) => "ARRAY",
            Object::Hash(_) => "HASH",
            Object::Null => "NULL",
            Object::Return(_) => "RETURN",
            Object::Function(_, _, _) => "FUNCTION",
            Object::Builtin(_) => "BUILTIN",
            Object::CompiledFunction(_) => "COMPILED_FUNCTION",
            Object::Closure(_) => "CLOSURE",
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

// Having a dedicated type to restrict what can be a hash key,
// and implement `Hash` trait.
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum HashKey {
    Integer(i64),
    String(String),
    Boolean(bool),
}

impl fmt::Display for HashKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HashKey::Integer(value) => write!(f, "{}", value),
            HashKey::String(value) => write!(f, "\"{}\"", value),
            HashKey::Boolean(value) => write!(f, "{}", value),
        }
    }
}

impl HashKey {
    pub fn from_object(obj: &Object) -> Result<HashKey, EvalError> {
        match obj {
            Object::Integer(value) => Ok(HashKey::Integer(*value)),
            Object::String(value) => Ok(HashKey::String(value.to_string())),
            Object::Boolean(value) => Ok(HashKey::Boolean(*value)),
            _ => Err(EvalError::UnsupportedHashKey(obj.clone())),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Closure {
    pub func: CompiledFunction,
    pub free: Vec<Rc<Object>>,
}

#[derive(Debug)]
pub enum EvalError {
    TypeMismatch(Infix, Object, Object),
    UnknownPrefixOperator(Prefix, Object),
    UnknownInfixOperator(Infix, Object, Object),
    IdentifierNotFound(String),
    NotCallable(Object),
    WrongArgumentCount { expected: usize, given: usize },
    UnsupportedArguments(String, Vec<Object>),
    UnknownIndexOperator(Object, Object),
    UnsupportedHashKey(Object),
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
            EvalError::NotCallable(obj) => write!(
                f,
                "not a closure or a builtin function: {}",
                obj.type_name()
            ),
            EvalError::WrongArgumentCount { expected, given } => write!(
                f,
                "wrong number of arguments: expected {}, given {}",
                expected, given
            ),
            EvalError::UnsupportedArguments(name, arguments) => write!(
                f,
                "unsupported arguments to `{}`: {}",
                name,
                arguments
                    .iter()
                    .map(|a| a.type_name())
                    .collect::<Vec<&str>>()
                    .join(", ")
            ),
            EvalError::UnknownIndexOperator(left, index) => write!(
                f,
                "unknown operator: {}[{}]",
                left.type_name(),
                index.type_name()
            ),
            EvalError::UnsupportedHashKey(key) => {
                write!(f, "unusable as hash key: {}", key.type_name())
            }
        }
    }
}

pub fn assert_argument_count(expected: usize, arguments: &[Object]) -> Result<(), EvalError> {
    if arguments.len() != expected {
        return Err(EvalError::WrongArgumentCount {
            expected,
            given: arguments.len(),
        });
    }
    Ok(())
}
