use crate::ast::{BlockStatement, Expression, Infix, Prefix, Program, Statement};
use crate::object::{Environment, EvalError, EvalResult, Object};
use std::cell::RefCell;
use std::rc::Rc;

// Evaluate a program
pub fn eval(program: &Program, env: Rc<RefCell<Environment>>) -> EvalResult {
    let mut result = Object::Null;
    for statement in &program.statements {
        result = eval_statement(statement, Rc::clone(&env))?;

        // Stop evaluation if return
        match result {
            Object::Return(value) => {
                // Unwrap the returned value because here's the root of the program.
                return Ok(*value);
            }
            _ => {}
        }
    }
    Ok(result)
}

fn eval_block_statement(block: &BlockStatement, env: Rc<RefCell<Environment>>) -> EvalResult {
    let mut result = Object::Null;
    for statement in &block.statements {
        result = eval_statement(statement, Rc::clone(&env))?;

        // Stop evaluation if return
        if let Object::Return(_) = result {
            // Don't unwrap the returned value to keep propagating the return.
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_statement(statement: &Statement, env: Rc<RefCell<Environment>>) -> EvalResult {
    match statement {
        Statement::Expression(exp) => eval_expression(exp, env),
        Statement::Return(Some(exp)) => {
            let result = eval_expression(exp, env)?;
            Ok(Object::Return(Box::new(result)))
        }
        Statement::Return(None) => Ok(Object::Return(Box::new(Object::Null))),
        Statement::Let(name, exp) => {
            let result = eval_expression(exp, Rc::clone(&env))?;
            // TODO: Is this `clone()` the right way to do?
            env.borrow_mut().set(name, result.clone());
            Ok(result)
        }
    }
}

fn eval_expression(expression: &Expression, env: Rc<RefCell<Environment>>) -> EvalResult {
    match expression {
        Expression::IntegerLiteral(int) => Ok(Object::Integer(*int)),
        Expression::StringLiteral(s) => Ok(Object::String(s.to_string())),
        Expression::Boolean(value) => Ok(Object::Boolean(*value)),
        Expression::Prefix(prefix, exp) => eval_prefix_expression(prefix, exp.as_ref(), env),
        Expression::Infix(infix, left, right) => {
            eval_infix_expression(infix, left.as_ref(), right.as_ref(), env)
        }
        Expression::If(condition, consequence, alternative) => {
            eval_if_expression(condition.as_ref(), consequence, &alternative.as_ref(), env)
        }
        Expression::Identifier(name) => eval_identifier(name, env),
        Expression::FunctionLiteral(params, body) => {
            // TODO: Pass a mutable reference of env...
            Ok(Object::Function(params.to_vec(), body.clone(), env.clone()))
        }
        Expression::Call(func, args) => {
            let function = eval_expression(func, Rc::clone(&env))?;
            let arguments = eval_expressions(args, env)?;
            apply_function(function, arguments)
        }
    }
}

fn apply_function(function: Object, arguments: Vec<Object>) -> EvalResult {
    match function {
        Object::Function(params, body, env) => {
            assert_argument_count(params.len(), &arguments)?;
            let new_env = extend_function_env(params, arguments, env);
            let evaluated = eval_block_statement(&body, new_env)?;
            unwrap_return_value(evaluated)
        }
        Object::Builtin(func) => func(arguments),
        _ => Err(EvalError::NotFunction(function.clone())),
    }
}

fn extend_function_env(
    params: Vec<String>,
    arguments: Vec<Object>,
    env: Rc<RefCell<Environment>>,
) -> Rc<RefCell<Environment>> {
    let new_env = Rc::new(RefCell::new(Environment::extend(env)));
    for (i, param) in params.iter().enumerate() {
        let arg = arguments.get(i).unwrap_or(&Object::Null);
        new_env.borrow_mut().set(param, arg.clone());
    }
    new_env
}

fn unwrap_return_value(obj: Object) -> EvalResult {
    match obj {
        Object::Return(value) => Ok(*value),
        _ => Ok(obj),
    }
}

fn eval_prefix_expression(
    prefix: &Prefix,
    exp: &Expression,
    env: Rc<RefCell<Environment>>,
) -> EvalResult {
    let obj = eval_expression(exp, env)?;

    match prefix {
        // `!` works like JavaScript :P
        Prefix::Bang => Ok(Object::Boolean(!obj.is_truthy())),
        Prefix::Minus => match obj {
            Object::Integer(value) => Ok(Object::Integer(-value)),
            _ => Err(EvalError::UnknownPrefixOperator(prefix.clone(), obj)),
        },
    }
}

fn eval_infix_expression(
    infix: &Infix,
    left_exp: &Expression,
    right_exp: &Expression,
    env: Rc<RefCell<Environment>>,
) -> EvalResult {
    let left_obj = eval_expression(left_exp, Rc::clone(&env))?;
    let right_obj = eval_expression(right_exp, env)?;

    match (left_obj, right_obj) {
        (Object::Boolean(left), Object::Boolean(right)) => {
            eval_boolean_infix_expression(infix, left, right)
        }
        (Object::Integer(left), Object::Integer(right)) => {
            eval_integer_infix_expression(infix, left, right)
        }
        (Object::String(left), Object::String(right)) => {
            eval_string_infix_expression(infix, &left, &right)
        }
        (left, right) => Err(EvalError::TypeMismatch(infix.clone(), left, right)),
    }
}

fn eval_boolean_infix_expression(infix: &Infix, left: bool, right: bool) -> EvalResult {
    match infix {
        Infix::Eq => Ok(Object::Boolean(left == right)),
        Infix::NotEq => Ok(Object::Boolean(left != right)),
        _ => Err(EvalError::UnknownInfixOperator(
            infix.clone(),
            Object::Boolean(left),
            Object::Boolean(right),
        )),
    }
}

fn eval_integer_infix_expression(infix: &Infix, left: i64, right: i64) -> EvalResult {
    Ok(match infix {
        Infix::Eq => Object::Boolean(left == right),
        Infix::NotEq => Object::Boolean(left != right),
        Infix::Lt => Object::Boolean(left < right),
        Infix::Gt => Object::Boolean(left > right),
        Infix::Plus => Object::Integer(left + right),
        Infix::Minus => Object::Integer(left - right),
        Infix::Asterisk => Object::Integer(left * right),
        Infix::Slash => Object::Integer(left / right),
    })
}

fn eval_string_infix_expression(infix: &Infix, left: &str, right: &str) -> EvalResult {
    match infix {
        // `concat()` seems to be kind of fast...
        // https://github.com/hoodie/concatenation_benchmarks-rs
        Infix::Plus => Ok(Object::String([left, right].concat())),
        _ => Err(EvalError::UnknownInfixOperator(
            infix.clone(),
            Object::String(left.to_string()),
            Object::String(right.to_string()),
        )),
    }
}

fn eval_if_expression(
    condition: &Expression,
    consequence: &BlockStatement,
    alternative: &Option<&BlockStatement>,
    env: Rc<RefCell<Environment>>,
) -> EvalResult {
    let result = eval_expression(condition, Rc::clone(&env))?;

    if result.is_truthy() {
        eval_block_statement(consequence, env)
    } else {
        alternative
            .map(|a| eval_block_statement(a, env))
            .unwrap_or(Ok(Object::Null))
    }
}

fn eval_identifier(name: &str, env: Rc<RefCell<Environment>>) -> EvalResult {
    if let Some(obj) = env.borrow().get(name) {
        return Ok(obj.clone());
    }
    if let Some(obj) = lookup_builtin(name) {
        return Ok(obj);
    }
    Err(EvalError::IdentifierNotFound(name.to_string()))
}

fn eval_expressions(
    exps: &Vec<Expression>,
    env: Rc<RefCell<Environment>>,
) -> Result<Vec<Object>, EvalError> {
    let mut results = vec![];
    for exp in exps {
        results.push(eval_expression(exp, Rc::clone(&env))?);
    }
    Ok(results)
}

fn lookup_builtin(name: &str) -> Option<Object> {
    match name {
        // TODO: Should `null` be a reserved word? Otherwise it can be overridden like `undefined`
        // of JavaScript non-strict mode.
        "null" => Some(Object::Null),
        "len" => Some(Object::Builtin(len)),
        _ => None,
    }
}

fn len(arguments: Vec<Object>) -> EvalResult {
    assert_argument_count(1, &arguments)?;
    match &arguments[0] {
        Object::String(value) => Ok(Object::Integer(value.len() as i64)),
        _ => Err(EvalError::UnsupportedArguments(
            "len".to_string(),
            arguments,
        )),
    }
}

fn assert_argument_count(expected: usize, arguments: &[Object]) -> Result<(), EvalError> {
    if arguments.len() != expected {
        return Err(EvalError::WrongArgumentCount {
            expected,
            given: arguments.len(),
        });
    }
    Ok(())
}
