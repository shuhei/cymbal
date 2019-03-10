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
    if let Object::Function(params, body, env) = function {
        let new_env = Rc::new(RefCell::new(Environment::extend(env)));
        for (i, param) in params.iter().enumerate() {
            // TODO: Check the number of arguments?
            let arg = arguments.get(i).unwrap_or(&Object::Null);
            new_env.borrow_mut().set(param, arg.clone());
        }

        let result = eval_block_statement(&body, new_env)?;
        // Unwrap the returned value.
        match result {
            Object::Return(value) => Ok(*value),
            _ => Ok(result),
        }
    } else {
        Err(EvalError::NotFunction(function.clone()))
    }
}

fn eval_prefix_expression(prefix: &Prefix, exp: &Expression, env: Rc<RefCell<Environment>>) -> EvalResult {
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
    match env.borrow().get(name) {
        Some(obj) => Ok(obj.clone()),
        None => Err(EvalError::IdentifierNotFound(name.to_string())),
    }
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
