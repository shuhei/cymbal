use crate::ast::{BlockStatement, Expression, Infix, Prefix, Program, Statement};
use crate::object::{
    assert_argument_count, builtin, Environment, EvalError, EvalResult, HashKey, Object,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// Evaluate a program
pub fn eval(program: &Program, env: Rc<RefCell<Environment>>) -> EvalResult {
    let mut result = Object::Null;
    for statement in &program.statements {
        result = eval_statement(statement, Rc::clone(&env))?;

        // Stop evaluation if return
        if let Object::Return(value) = result {
            // Unwrap the returned value because here's the root of the program.
            return Ok(*value);
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
        Expression::IntegerLiteral(value) => Ok(Object::Integer(*value)),
        Expression::FloatLiteral(value) => Ok(Object::Float(*value)),
        Expression::StringLiteral(s) => Ok(Object::String(s.to_string())),
        Expression::Boolean(value) => Ok(Object::Boolean(*value)),
        Expression::Array(values) => eval_array_literal(values, env),
        Expression::Hash(pairs) => eval_hash_literal(pairs, env),
        Expression::Index(left, index) => eval_index_expression(left, index, env),
        Expression::Prefix(prefix, exp) => eval_prefix_expression(prefix, exp.as_ref(), env),
        Expression::Infix(infix, left, right) => {
            eval_infix_expression(infix, left.as_ref(), right.as_ref(), env)
        }
        Expression::If(condition, consequence, alternative) => {
            eval_if_expression(condition.as_ref(), consequence, alternative.as_ref(), env)
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

fn eval_array_literal(exps: &[Expression], env: Rc<RefCell<Environment>>) -> EvalResult {
    let values = eval_expressions(exps, env)?;
    Ok(Object::Array(values))
}

fn eval_hash_literal(
    pairs: &[(Expression, Expression)],
    env: Rc<RefCell<Environment>>,
) -> EvalResult {
    let mut map = HashMap::new();
    for (k, v) in pairs {
        let key = eval_expression(k, env.clone())?;
        let value = eval_expression(v, env.clone())?;
        let hash_key = HashKey::from_object(&key)?;
        map.insert(hash_key, value);
    }
    Ok(Object::Hash(map))
}

fn eval_index_expression(
    left: &Expression,
    index: &Expression,
    env: Rc<RefCell<Environment>>,
) -> EvalResult {
    let left_evaluated = eval_expression(left, env.clone())?;
    let index_evaluated = eval_expression(index, env)?;
    match (left_evaluated, index_evaluated) {
        (Object::Array(array), Object::Integer(value)) => Ok(or_null(array.get(value as usize))),
        (Object::Hash(pairs), Object::Integer(value)) => {
            Ok(or_null(pairs.get(&HashKey::Integer(value))))
        }
        (Object::Hash(pairs), Object::String(value)) => {
            Ok(or_null(pairs.get(&HashKey::String(value))))
        }
        (Object::Hash(pairs), Object::Boolean(value)) => {
            Ok(or_null(pairs.get(&HashKey::Boolean(value))))
        }
        (Object::Hash(_), key) => Err(EvalError::UnsupportedHashKey(key)),
        (l, i) => Err(EvalError::UnknownIndexOperator(l, i)),
    }
}

fn or_null(option: Option<&Object>) -> Object {
    option.cloned().unwrap_or(Object::Null)
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
        _ => Err(EvalError::NotCallable(function.clone())),
    }
}

fn extend_function_env(
    params: Vec<String>,
    arguments: Vec<Object>,
    env: Rc<RefCell<Environment>>,
) -> Rc<RefCell<Environment>> {
    let new_env = Rc::new(RefCell::new(Environment::extend(env)));
    for (i, param) in params.iter().enumerate() {
        let arg = or_null(arguments.get(i));
        new_env.borrow_mut().set(param, arg);
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
            Object::Float(value) => Ok(Object::Float(-value)),
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
        (Object::Integer(left), Object::Float(right)) => {
            eval_float_infix_expression(infix, left as f64, right)
        }
        (Object::Float(left), Object::Integer(right)) => {
            eval_float_infix_expression(infix, left, right as f64)
        }
        (Object::Float(left), Object::Float(right)) => {
            eval_float_infix_expression(infix, left, right)
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

#[allow(clippy::float_cmp)]
fn eval_float_infix_expression(infix: &Infix, left: f64, right: f64) -> EvalResult {
    Ok(match infix {
        // Use exact comparison for floats for now.
        Infix::Eq => Object::Boolean(left == right),
        Infix::NotEq => Object::Boolean(left != right),
        Infix::Lt => Object::Boolean(left < right),
        Infix::Gt => Object::Boolean(left > right),
        Infix::Plus => Object::Float(left + right),
        Infix::Minus => Object::Float(left - right),
        Infix::Asterisk => Object::Float(left * right),
        Infix::Slash => Object::Float(left / right),
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
    alternative: Option<&BlockStatement>,
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
    if let Some(obj) = builtin::lookup(name) {
        return Ok(obj);
    }
    Err(EvalError::IdentifierNotFound(name.to_string()))
}

fn eval_expressions(
    exps: &[Expression],
    env: Rc<RefCell<Environment>>,
) -> Result<Vec<Object>, EvalError> {
    let mut results = vec![];
    for exp in exps {
        results.push(eval_expression(exp, Rc::clone(&env))?);
    }
    Ok(results)
}

#[cfg(test)]
mod evalator_tests {
    use crate::evaluator;
    use crate::lexer::Lexer;
    use crate::object::{Environment, EvalResult};
    use crate::parser::Parser;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn eval_boolean() {
        expect_values(vec![
            // Prefix
            ("!true", "false"),
            ("!!true", "true"),
            ("!false", "true"),
            ("!!false", "false"),
            ("!null", "true"),
            ("!!null", "false"),
            ("!0", "false"),
            ("!3", "false"),
            ("!!3", "true"),
            // Infix
            // boolean -> boolean
            ("true == true", "true"),
            ("false == true", "false"),
            ("true != true", "false"),
            ("true != false", "true"),
            // integer -> boolean
            ("1 == 2", "false"),
            ("2 == 2", "true"),
            ("1 != 2", "true"),
            ("2 != 2", "false"),
            ("1 > 2", "false"),
            ("1 < 2", "true"),
        ]);
    }

    #[test]
    fn eval_integer() {
        expect_values(vec![
            // Prefix
            ("-123", "-123"),
            ("-(-123)", "123"),
            ("-(3 * 3)", "-9"),
            // Infix
            ("2 + 3", "5"),
            ("2 - 3", "-1"),
            ("2 * 3", "6"),
            ("9 / 3", "3"),
            ("-50 + 100 + -50", "0"),
            ("20 + 2 * -10", "0"),
            ("50 / 2 * 2 + 10", "60"),
            ("2 * (5 + 10)", "30"),
            ("3 * 3 * 3 + 10", "37"),
            ("3 * (3 * 3) + 10", "37"),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", "50"),
        ]);
    }

    #[test]
    fn eval_float() {
        expect_values(vec![
            // Prefix
            ("-12.3", "-12.3"),
            ("-(-12.3)", "12.3"),
            ("-(2.2 * 3.3)", "-7.26"),
            // Infix
            ("2.8 + 3.4", "6.199999999999999"),
            ("2.0 - 3.1", "-1.1"),
            ("2.2 * 3.3", "7.26"),
            ("6.16 / 2.8", "2.2"),
            ("-50.4 + 100.4 + -50.0", "0.000000000000007105427357601002"),
        ]);
    }

    #[test]
    fn eval_integer_and_float() {}

    #[test]
    fn eval_if() {
        expect_values(vec![
            ("if (true) { 10 }", "10"),
            ("if (false) { 10 }", "null"),
            ("if (null) { 1 } else { 2 }", "2"),
            ("if (2 > 1) { 3 } else { 4 }", "3"),
            ("if (2 < 1) { 3 } else { 4 }", "4"),
            ("if (1 < 2) { 3 }", "3"),
            ("if (1 > 2) { 3 }", "null"),
        ]);
    }

    #[test]
    fn eval_return() {
        expect_values(vec![
            ("return;", "null"),
            ("return 10;", "10"),
            ("1 + 2; return; 3 + 4", "null"),
            ("1 + 2; return 8; 3 + 4", "8"),
            ("3; return 8 * 2; 3 + 4", "16"),
            // Nested statements
            (
                "if (10 > 1) {
                if (10 > 1) {
                    return 10;
                }
                return 1;
            }",
                "10",
            ),
        ]);
    }

    #[test]
    fn error_handling() {
        expect_errors(vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "if (10 > 1) { true + false; }; 8;",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("(1 + true) * 4;", "type mismatch: INTEGER + BOOLEAN"),
            ("3 - (true * 2);", "type mismatch: BOOLEAN * INTEGER"),
            (
                "(3 + false) - (true * 2);",
                "type mismatch: INTEGER + BOOLEAN",
            ),
            ("!(1 + true);", "type mismatch: INTEGER + BOOLEAN"),
            ("return (1 + true) * 4;", "type mismatch: INTEGER + BOOLEAN"),
            (
                "if (3 == true) { 1 } else { 2 }",
                "type mismatch: INTEGER == BOOLEAN",
            ),
            ("foobar", "identifier not found: foobar"),
        ]);
    }

    #[test]
    fn let_statement() {
        expect_values(vec![
            ("let a = 5; a;", "5"),
            ("let a = 5 * 5; a;", "25"),
            ("let a = 5; let b = a; b;", "5"),
            ("let a = 5; let b = a; let c = a + b + 5; c;", "15"),
        ]);
    }

    #[test]
    fn function_object() {
        expect_values(vec![("fn(x) { x + 2; }", "fn(x) {\n{ (x + 2); }\n}")]);
    }

    #[test]
    fn function_application() {
        expect_values(vec![
            ("let identity = fn(x) { x; }; identity(5);", "5"),
            ("let identity = fn(x) { return x; }; identity(5);", "5"),
            ("let double = fn(x) { x * 2; }; double(5);", "10"),
            ("let add = fn(x, y) { x + y; }; add(10, 23);", "33"),
            (
                "let add = fn(x, y) { x + y; }; add(3 + 4, add(10, 23));",
                "40",
            ),
            ("fn(x) { x; }(5);", "5"),
            ("fn(x) { x; }(1); 5;", "5"),
        ]);
        expect_errors(vec![(
            "let add = fn(x, y) { x + y }; add(123); 3;",
            "wrong number of arguments: expected 2, given 1",
        )]);
    }

    #[test]
    fn closure() {
        expect_values(vec![(
            "let newAdder = fn(x) { fn(y) { x + y } }; let addTwo = newAdder(2); addTwo(3);",
            "5",
        )]);
    }

    #[test]
    fn string() {
        expect_values(vec![
            (r#""Hello, World!""#, r#""Hello, World!""#),
            (r#""hello" + " " + "world""#, r#""hello world""#),
        ]);
        expect_errors(vec![(
            r#""hello world" - "hello""#,
            "unknown operator: STRING - STRING",
        )]);
    }

    #[test]
    fn builtin_function() {
        expect_values(vec![
            (r#"len("")"#, "0"),
            (r#"len("four")"#, "4"),
            (r#"len("hello world")"#, "11"),
            ("len([1, 2, 3])", "3"),
            ("len([])", "0"),
            ("first([1, 2, 3])", "1"),
            ("first([])", "null"),
            ("last([1, 2, 3])", "3"),
            ("last([])", "null"),
            ("rest([1, 2, 3])", "[2, 3]"),
            ("rest(rest([1, 2, 3]))", "[3]"),
            ("rest(rest(rest([1, 2, 3])))", "[]"),
            ("rest(rest(rest(rest([1, 2, 3]))))", "null"),
            ("rest([1])", "[]"),
            ("rest([])", "null"),
            ("push([1, 2, 3], 4)", "[1, 2, 3, 4]"),
            ("push(push(push([], 1), 2), 3)", "[1, 2, 3]"),
            ("push([], 1)", "[1]"),
        ]);
        expect_errors(vec![
            ("len(1)", "unsupported arguments to `len`: INTEGER"),
            (
                r#"len("one", "two")"#,
                "wrong number of arguments: expected 1, given 2",
            ),
            ("first(1)", "unsupported arguments to `first`: INTEGER"),
            (
                "first([1, 2], [3, 4])",
                "wrong number of arguments: expected 1, given 2",
            ),
            ("last(1)", "unsupported arguments to `last`: INTEGER"),
            (
                "last([1, 2], [3, 4])",
                "wrong number of arguments: expected 1, given 2",
            ),
            ("rest(1)", "unsupported arguments to `rest`: INTEGER"),
            (
                "rest([1, 2], [3, 4])",
                "wrong number of arguments: expected 1, given 2",
            ),
            (
                "push(1, 2)",
                "unsupported arguments to `push`: INTEGER, INTEGER",
            ),
            (
                "push([1, 2])",
                "wrong number of arguments: expected 2, given 1",
            ),
        ]);
    }

    #[test]
    fn array_literal() {
        expect_values(vec![("[1, 2 * 3, 4 + (5 - 6)]", "[1, 6, 3]")]);
    }

    #[test]
    fn array_index_expression() {
        expect_values(vec![
            ("[1, 2, 3][0]", "1"),
            ("[1, 2, 3][1]", "2"),
            ("[1, 2, 3][2]", "3"),
            ("let i = 0; [1][i];", "1"),
            ("[1, 2, 3][1 + 1];", "3"),
            ("let myArray = [1, 2, 3]; myArray[2];", "3"),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                "6",
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                "2",
            ),
            ("[1, 2, 3][3]", "null"),
            ("[1, 2, 3][-1]", "null"),
        ]);
    }

    #[test]
    fn array_map() {
        let map = "
            let map = fn(array, func) {
                let iter = fn(arr, acc) {
                    if (len(arr) == 0) {
                        acc
                    } else {
                        iter(rest(arr), push(acc, func(first(arr))))
                    }
                };
                iter(array, [])
            };
        ";
        expect_values(vec![
            (
                &with_def(map, "map([1, 2, 3], fn(x) { x * x })"),
                "[1, 4, 9]",
            ),
            (&with_def(map, "map([2], fn(x) { 3 * x })"), "[6]"),
            (&with_def(map, "map([], fn(x) { x * x })"), "[]"),
        ]);
    }

    #[test]
    fn array_reduce() {
        let reduce = "
            let reduce = fn(array, init, func) {
                let iter = fn (arr, acc) {
                    if (len(arr) == 0) {
                        acc
                    } else {
                        iter(rest(arr), func(acc, first(arr)))
                    }
                };
                iter(array, init);
            };
        ";
        expect_values(vec![(
            &with_def(reduce, "reduce([2, 3, 5], 1, fn(acc, x) { acc * x })"),
            "30",
        )]);
    }

    #[test]
    fn hash() {
        expect_values(vec![
            (r#"{"foo": 123, "bar": 234}"#, r#"{"bar": 234, "foo": 123}"#),
            (r#"{"foo": 123, "bar": 234}["baz"]"#, "null"),
            (r#"{"foo": 123, "bar": 234}["foo"]"#, "123"),
            (r#"{1: 123, 2: 234}[2]"#, "234"),
            (r#"{true: 3 * 4, false: 2 * 8}[true]"#, "12"),
            (r#"{true: 3 * 4, false: 2 * 8}[false]"#, "16"),
            (r#"{"thr" + "ee": 6 / 2, 1: 1}["th" + "ree"]"#, "3"),
            (r#"let key = "foo"; {"foo": 5}[key]"#, "5"),
        ]);
        expect_errors(vec![(
            "{12: 234}[fn(x) { x }];",
            "unusable as hash key: FUNCTION",
        )]);
    }

    fn with_def(def: &str, code: &str) -> String {
        def.to_string() + code
    }

    fn expect_values(tests: Vec<(&str, &str)>) {
        for (input, expected) in &tests {
            match eval_input(input) {
                Ok(obj) => {
                    assert_eq!(obj.to_string(), expected.to_string(), "for `{}`", input);
                }
                Err(err) => {
                    panic!(
                        "expected `{}`, but got error=`{}` for `{}`",
                        expected, err, input
                    );
                }
            }
        }
    }

    fn expect_errors(tests: Vec<(&str, &str)>) {
        for (input, expected_message) in &tests {
            match eval_input(input) {
                Ok(obj) => {
                    panic!("no error object returned. got=`{}` for `{}`", obj, input);
                }
                Err(err) => {
                    assert_eq!(&err.to_string(), expected_message, "for `{}`", input);
                }
            }
        }
    }

    fn eval_input(input: &str) -> EvalResult {
        let lexer = Lexer::new(input.to_owned());
        let parser = Parser::new(lexer);

        let program = parser.parse_program().expect("parser error");
        let env = Rc::new(RefCell::new(Environment::new()));
        evaluator::eval(&program, env)
    }
}
