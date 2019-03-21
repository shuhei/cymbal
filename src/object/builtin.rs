use crate::object::{EvalError, EvalResult, Object, assert_argument_count};

pub fn lookup(name: &str) -> Option<Object> {
    match name {
        // TODO: Should `null` be a reserved word? Otherwise it can be overridden like `undefined`
        // of JavaScript non-strict mode.
        "null" => Some(Object::Null),
        "len" => Some(Object::Builtin(len)),
        "first" => Some(Object::Builtin(first)),
        "last" => Some(Object::Builtin(last)),
        "rest" => Some(Object::Builtin(rest)),
        "push" => Some(Object::Builtin(push)),
        "puts" => Some(Object::Builtin(puts)),
        _ => None,
    }
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
            if values.len() > 0 {
                Ok(Object::Array(values[1..].to_vec()))
            } else {
                Ok(Object::Null)
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
