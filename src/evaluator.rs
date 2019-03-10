use crate::ast::{BlockStatement, Expression, Infix, Prefix, Program, Statement};
use crate::object::{Environment, EvalError, EvalResult, Object};

// Evaluate a program
pub fn eval(program: &Program, env: &mut Environment) -> EvalResult {
    let mut result = Object::Null;
    for statement in &program.statements {
        result = eval_statement(statement, env)?;

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

fn eval_block_statement(block: &BlockStatement, env: &mut Environment) -> EvalResult {
    let mut result = Object::Null;
    for statement in &block.statements {
        result = eval_statement(statement, env)?;

        // Stop evaluation if return
        if let Object::Return(_) = result {
            // Don't unwrap the returned value to keep propagating the return.
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_statement(statement: &Statement, env: &mut Environment) -> EvalResult {
    match statement {
        Statement::Expression(exp) => eval_expression(exp, env),
        Statement::Return(Some(exp)) => {
            let result = eval_expression(exp, env)?;
            Ok(Object::Return(Box::new(result)))
        }
        Statement::Return(None) => Ok(Object::Return(Box::new(Object::Null))),
        Statement::Let(name, exp) => {
            let result = eval_expression(exp, env)?;
            // TODO: Is this `clone()` the right way to do?
            // Not sure which of `env` and the returned value will live longer...
            env.set(name, result.clone());
            Ok(result)
        }
    }
}

fn eval_expression(expression: &Expression, env: &mut Environment) -> EvalResult {
    match expression {
        Expression::IntegerLiteral(int) => Ok(Object::Integer(*int)),
        Expression::Boolean(value) => Ok(Object::Boolean(*value)),
        Expression::Prefix(prefix, exp) => eval_prefix_expression(prefix, exp.as_ref(), env),
        Expression::Infix(infix, left, right) => {
            eval_infix_expression(infix, left.as_ref(), right.as_ref(), env)
        }
        Expression::If(condition, consequence, alternative) => {
            eval_if_expression(condition.as_ref(), consequence, &alternative.as_ref(), env)
        }
        Expression::Identifier(name) => eval_identifier(name, env),
        _ => Ok(Object::Null),
    }
}

fn eval_prefix_expression(prefix: &Prefix, exp: &Expression, env: &mut Environment) -> EvalResult {
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
    env: &mut Environment,
) -> EvalResult {
    let left_obj = eval_expression(left_exp, env)?;
    let right_obj = eval_expression(right_exp, env)?;

    match (left_obj, right_obj) {
        (Object::Boolean(left), Object::Boolean(right)) => {
            eval_boolean_infix_expression(infix, left, right)
        }
        (Object::Integer(left), Object::Integer(right)) => {
            eval_integer_infix_expression(infix, left, right)
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

fn eval_if_expression(
    condition: &Expression,
    consequence: &BlockStatement,
    alternative: &Option<&BlockStatement>,
    env: &mut Environment,
) -> EvalResult {
    let result = eval_expression(condition, env)?;

    if result.is_truthy() {
        eval_block_statement(consequence, env)
    } else {
        alternative
            .map(|a| eval_block_statement(a, env))
            .unwrap_or(Ok(Object::Null))
    }
}

fn eval_identifier(name: &str, env: &Environment) -> EvalResult {
    match env.get(name) {
        Some(obj) => Ok(obj.clone()),
        None => Err(EvalError::IdentifierNotFound(name.to_string())),
    }
}
