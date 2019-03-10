use crate::ast::{BlockStatement, Expression, Infix, Prefix, Program, Statement};
use crate::object::{EvalError, Object};

// Evaluate a program
pub fn eval(program: Program) -> Object {
    let mut result = Object::Null;
    for statement in program.statements {
        result = eval_statement(statement);

        // Stop evaluation if return or error
        match result {
            Object::Return(value) => {
                // Unwrap the returned value because here's the root of the program.
                return *value;
            }
            Object::Error(_) => {
                return result;
            }
            _ => {}
        }
    }
    result
}

fn eval_block_statement(block: BlockStatement) -> Object {
    let mut result = Object::Null;
    for statement in block.statements {
        result = eval_statement(statement);

        // Stop evaluation if return or error
        if let Object::Return(_) = result {
            // Don't unwrap the returned value to keep propagating the return.
            return result;
        }
    }
    result
}

fn eval_statement(statement: Statement) -> Object {
    match statement {
        Statement::Expression(exp) => eval_expression(exp),
        Statement::Return(Some(exp)) => Object::Return(Box::new(eval_expression(exp))),
        Statement::Return(None) => Object::Return(Box::new(Object::Null)),
        _ => Object::Null,
    }
}

fn eval_expression(expression: Expression) -> Object {
    match expression {
        Expression::IntegerLiteral(int) => Object::Integer(int),
        Expression::Boolean(value) => Object::Boolean(value),
        Expression::Prefix(prefix, exp) => eval_prefix_expression(prefix, *exp),
        Expression::Infix(infix, left, right) => eval_infix_expression(infix, *left, *right),
        Expression::If(condition, consequence, alternative) => {
            eval_if_expression(*condition, consequence, alternative)
        }
        _ => Object::Null,
    }
}

fn eval_prefix_expression(prefix: Prefix, exp: Expression) -> Object {
    let obj = eval_expression(exp);

    match prefix {
        // `!` works like JavaScript :P
        Prefix::Bang => Object::Boolean(!is_truthy(obj)),
        Prefix::Minus => match obj {
            Object::Integer(value) => Object::Integer(-value),
            _ => Object::Error(Box::new(EvalError::UnknownPrefixOperator(prefix, obj))),
        },
    }
}

fn eval_infix_expression(infix: Infix, left_exp: Expression, right_exp: Expression) -> Object {
    let left_obj = eval_expression(left_exp);
    let right_obj = eval_expression(right_exp);

    match (left_obj, right_obj) {
        (Object::Boolean(left), Object::Boolean(right)) => {
            eval_boolean_infix_expression(infix, left, right)
        }
        (Object::Integer(left), Object::Integer(right)) => {
            eval_integer_infix_expression(infix, left, right)
        }
        (left, right) => Object::Error(Box::new(EvalError::TypeMismatch(infix, left, right))),
    }
}

fn eval_boolean_infix_expression(infix: Infix, left: bool, right: bool) -> Object {
    match infix {
        Infix::Eq => Object::Boolean(left == right),
        Infix::NotEq => Object::Boolean(left != right),
        _ => Object::Error(Box::new(EvalError::UnknownInfixOperator(
            infix,
            Object::Boolean(left),
            Object::Boolean(right),
        ))),
    }
}

fn eval_integer_infix_expression(infix: Infix, left: i64, right: i64) -> Object {
    match infix {
        Infix::Eq => Object::Boolean(left == right),
        Infix::NotEq => Object::Boolean(left != right),
        Infix::Lt => Object::Boolean(left < right),
        Infix::Gt => Object::Boolean(left > right),
        Infix::Plus => Object::Integer(left + right),
        Infix::Minus => Object::Integer(left - right),
        Infix::Asterisk => Object::Integer(left * right),
        Infix::Slash => Object::Integer(left / right),
    }
}

fn eval_if_expression(
    condition: Expression,
    consequence: BlockStatement,
    alternative: Option<BlockStatement>,
) -> Object {
    if is_truthy(eval_expression(condition)) {
        eval_block_statement(consequence)
    } else {
        alternative
            .map(|a| eval_block_statement(a))
            .unwrap_or(Object::Null)
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Boolean(value) => value,
        Object::Null => false,
        _ => true,
    }
}
