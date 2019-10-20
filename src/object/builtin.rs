use crate::object::{assert_argument_count, EvalError, EvalResult, Object};

// Builtin functions must have a static order in order to have static indices in compiled bytecodes.

pub struct Builtin {
    pub name: &'static str,
    pub builtin: Object,
}

macro_rules! builtin {
    ($name:ident) => {
        Builtin {
            name: stringify!($name),
            builtin: Object::Builtin($name),
        }
    };
}

pub const BUILTINS: &[Builtin] = &[
    builtin!(len),
    builtin!(first),
    builtin!(last),
    builtin!(rest),
    builtin!(push),
    builtin!(puts),
    builtin!(exit),
];

pub fn lookup(name: &str) -> Option<Object> {
    // TODO: Move `null` to somewhere else.
    // TODO: Make `null` a reserved word. Otherwise it can be overridden like `undefined`
    // of JavaScript non-strict mode.
    if name == "null" {
        return Some(Object::Null);
    }
    for b in BUILTINS {
        if b.name == name {
            return Some(b.builtin.clone());
        }
    }
    None
}

fn len(arguments: Vec<Object>) -> EvalResult {
    assert_argument_count(1, &arguments)?;
    match &arguments[0] {
        Object::String(value) => Ok(Object::Integer(value.len() as i64)),
        Object::Array(values) => Ok(Object::Integer(values.len() as i64)),
        _ => Err(EvalError::UnsupportedArguments(
            "len".to_string(),
            arguments,
        )),
    }
}

fn first(arguments: Vec<Object>) -> EvalResult {
    assert_argument_count(1, &arguments)?;
    match &arguments[0] {
        Object::Array(values) => Ok(match values.first() {
            Some(item) => item.clone(),
            None => Object::Null,
        }),
        _ => Err(EvalError::UnsupportedArguments(
            "first".to_string(),
            arguments,
        )),
    }
}

fn last(arguments: Vec<Object>) -> EvalResult {
    assert_argument_count(1, &arguments)?;
    match &arguments[0] {
        Object::Array(values) => Ok(match values.last() {
            Some(item) => item.clone(),
            None => Object::Null,
        }),
        _ => Err(EvalError::UnsupportedArguments(
            "last".to_string(),
            arguments,
        )),
    }
}

fn rest(arguments: Vec<Object>) -> EvalResult {
    assert_argument_count(1, &arguments)?;
    match &arguments[0] {
        Object::Array(values) => {
            if values.is_empty() {
                Ok(Object::Null)
            } else {
                Ok(Object::Array(values[1..].to_vec()))
            }
        }
        _ => Err(EvalError::UnsupportedArguments(
            "rest".to_string(),
            arguments,
        )),
    }
}

fn push(arguments: Vec<Object>) -> EvalResult {
    assert_argument_count(2, &arguments)?;
    match &arguments[0] {
        Object::Array(values) => {
            // TODO: How can I just consume the first and second items of `arguments`?
            let mut items = values.clone();
            items.push(arguments[1].clone());
            Ok(Object::Array(items))
        }
        _ => Err(EvalError::UnsupportedArguments(
            "push".to_string(),
            arguments,
        )),
    }
}

fn puts(arguments: Vec<Object>) -> EvalResult {
    for arg in arguments {
        // TODO: Remove `""` of Object::String
        println!("{}", arg);
    }
    Ok(Object::Null)
}

fn exit(_arguments: Vec<Object>) -> EvalResult {
    std::process::exit(0);
}
