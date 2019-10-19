pub mod frame;

use crate::ast::{Infix, Prefix};
use crate::code;
use crate::code::{Bytecode, CompiledFunction, Constant, Instructions, OpCode};
use crate::object::{builtin, BuiltinFunction, Closure, EvalError, HashKey, Object};
pub use crate::vm::frame::Frame;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub const STACK_SIZE: usize = 2048;
pub const GLOBAL_SIZE: usize = 65536;
pub const MAX_FRAMES: usize = 1024;
pub const NULL: Object = Object::Null;

#[derive(Debug)]
pub struct Vm {
    pub constants: Vec<Constant>,

    stack: Vec<Rc<Object>>,
    sp: usize, // Stack pointer. Always points to the next value. Top of the stack is stack[sp - 1];

    pub globals: Rc<RefCell<Vec<Rc<Object>>>>,

    frames: Vec<Frame>,
    // TODO: Is this index necessary?
    frames_index: usize,
}

pub fn new_globals() -> Vec<Rc<Object>> {
    Vec::with_capacity(GLOBAL_SIZE)
}

fn new_frames(instructions: Instructions) -> Vec<Frame> {
    let main_function = CompiledFunction {
        instructions,
        num_locals: 0,
        num_parameters: 0,
    };
    let main_closure = Closure {
        func: main_function,
        free: vec![],
    };
    let main_frame = Frame::new(main_closure, 0);
    let mut frames = Vec::with_capacity(MAX_FRAMES);
    frames.push(main_frame);
    frames
}

impl Vm {
    pub fn new(bytecode: Bytecode) -> Self {
        Vm::new_with_globals_store(bytecode, Rc::new(RefCell::new(new_globals())))
    }

    pub fn new_with_globals_store(
        bytecode: Bytecode,
        globals: Rc<RefCell<Vec<Rc<Object>>>>,
    ) -> Self {
        let mut stack = Vec::with_capacity(STACK_SIZE);
        // Pre-fill the stack so that we can easily put values with stack pointer.
        for _ in 0..STACK_SIZE {
            stack.push(Rc::new(NULL));
        }

        Vm {
            constants: bytecode.constants,
            stack,
            sp: 0,
            globals,
            frames: new_frames(bytecode.instructions),
            frames_index: 1,
        }
    }

    pub fn run(mut self) -> Result<Rc<Object>, VmError> {
        while self.current_frame().ip < self.current_frame().instructions().len() {
            let ip = self.current_frame().ip;
            let ins = self.current_frame().instructions();
            let op_code_byte = ins[ip];

            match OpCode::from_byte(op_code_byte) {
                Some(OpCode::Constant) => {
                    let const_index = code::read_uint16(ins, ip + 1) as usize;
                    self.increment_ip(2);

                    let len = self.constants.len();
                    if const_index < len {
                        let constant = Object::from_constant(&self.constants[const_index]);
                        self.push(Rc::new(constant))?;
                    } else {
                        return Err(VmError::InvalidConstIndex(const_index, len));
                    }
                }
                Some(OpCode::Pop) => {
                    self.pop()?;
                }
                Some(OpCode::Add) => {
                    self.execute_binary_operation(OpCode::Add)?;
                }
                Some(OpCode::Sub) => {
                    self.execute_binary_operation(OpCode::Sub)?;
                }
                Some(OpCode::Mul) => {
                    self.execute_binary_operation(OpCode::Mul)?;
                }
                Some(OpCode::Div) => {
                    self.execute_binary_operation(OpCode::Div)?;
                }
                Some(OpCode::True) => {
                    self.push(Rc::new(Object::Boolean(true)))?;
                }
                Some(OpCode::False) => {
                    self.push(Rc::new(Object::Boolean(false)))?;
                }
                Some(OpCode::Equal) => {
                    self.execute_comparison(OpCode::Equal)?;
                }
                Some(OpCode::NotEqual) => {
                    self.execute_comparison(OpCode::NotEqual)?;
                }
                Some(OpCode::GreaterThan) => {
                    self.execute_comparison(OpCode::GreaterThan)?;
                }
                Some(OpCode::Minus) => {
                    let right = self.pop()?;
                    match &*right {
                        Object::Integer(value) => {
                            self.push(Rc::new(Object::Integer(-value)))?;
                        }
                        obj => {
                            return Err(VmError::Eval(EvalError::UnknownPrefixOperator(
                                Prefix::Minus,
                                obj.clone(),
                            )));
                        }
                    }
                }
                Some(OpCode::Bang) => {
                    let right = self.pop()?;
                    self.push(Rc::new(Object::Boolean(!right.is_truthy())))?;
                }
                Some(OpCode::JumpIfNotTruthy) => {
                    let pos = code::read_uint16(ins, ip + 1) as usize;
                    self.increment_ip(2);

                    let condition = self.pop()?;
                    if !condition.is_truthy() {
                        // `pos - 1` because `ip` will be incremented later.
                        self.set_ip(pos - 1);
                    }
                }
                Some(OpCode::Jump) => {
                    let pos = code::read_uint16(ins, ip + 1) as usize;
                    // `pos - 1` because `ip` will be incremented later.
                    self.set_ip(pos - 1);
                }
                Some(OpCode::Null) => {
                    // TODO: This `Rc` is not neccessary because NULL is a constant...
                    self.push(Rc::new(NULL))?;
                }
                Some(OpCode::GetGlobal) => {
                    let global_index = code::read_uint16(ins, ip + 1) as usize;
                    self.increment_ip(2);

                    let global = Rc::clone(&self.globals.borrow()[global_index]);
                    self.push(global)?;
                }
                Some(OpCode::SetGlobal) => {
                    let global_index = code::read_uint16(ins, ip + 1) as usize;
                    self.increment_ip(2);

                    let popped = self.pop()?;
                    let mut globals = self.globals.borrow_mut();
                    if global_index == globals.len() {
                        globals.push(popped);
                    } else {
                        globals[global_index] = popped;
                    }
                }
                Some(OpCode::Array) => {
                    let size = code::read_uint16(ins, ip + 1) as usize;
                    self.increment_ip(2);

                    let mut items = Vec::with_capacity(size);
                    for _ in 0..size {
                        // TODO: Don't clone an object from Rc!
                        items.push((*self.pop()?).clone());
                    }
                    items.reverse();

                    self.push(Rc::new(Object::Array(items)))?;
                }
                Some(OpCode::Hash) => {
                    let size = code::read_uint16(ins, ip + 1) as usize;
                    self.increment_ip(2);

                    let mut items = HashMap::with_capacity(size);
                    for _ in 0..size {
                        // TODO: Don't clone an object from Rc!
                        let value = (*self.pop()?).clone();
                        let key = HashKey::from_object(&*self.pop()?)
                            .or_else(|e| Err(VmError::Eval(e)))?;
                        items.insert(key, value);
                    }

                    self.push(Rc::new(Object::Hash(items)))?;
                }
                Some(OpCode::Index) => {
                    let index = self.pop()?;
                    let obj = self.pop()?;

                    match &*obj {
                        Object::Array(values) => {
                            if let Object::Integer(i) = &*index {
                                // TODO: Don't clone!
                                let item = values.get(*i as usize).unwrap_or(&NULL).clone();
                                self.push(Rc::new(item))?;
                            } else {
                                return Err(VmError::Eval(EvalError::UnknownIndexOperator(
                                    (*obj).clone(),
                                    (*index).clone(),
                                )));
                            }
                        }
                        Object::Hash(hash) => {
                            let key = match &*index {
                                Object::Integer(value) => HashKey::Integer(*value),
                                // TODO Don't clone!
                                Object::String(value) => HashKey::String(value.clone()),
                                Object::Boolean(value) => HashKey::Boolean(*value),
                                _ => {
                                    return Err(VmError::Eval(EvalError::UnknownIndexOperator(
                                        (*obj).clone(),
                                        (*index).clone(),
                                    )));
                                }
                            };
                            let value = hash.get(&key).unwrap_or(&NULL);
                            self.push(Rc::new(value.clone()))?;
                        }
                        _ => {
                            return Err(VmError::Eval(EvalError::UnknownIndexOperator(
                                (*obj).clone(),
                                (*index).clone(),
                            )));
                        }
                    }
                }
                Some(OpCode::Call) => {
                    let num_args = ins[ip + 1] as usize;
                    self.increment_ip(1);

                    self.execute_call(num_args)?;
                    // `continue` to avoid incrementing `self.current_frame().ip` because we want
                    // to start with the first instruction in the frame.
                    continue;
                }
                Some(OpCode::ReturnValue) => {
                    let returned = self.pop()?;

                    let base_pointer = self.pop_frame().base_pointer;
                    // Remove local bindings and the executed function.
                    self.sp = base_pointer - 1;

                    self.push(returned)?;
                }
                Some(OpCode::Return) => {
                    let base_pointer = self.pop_frame().base_pointer;
                    self.sp = base_pointer - 1;

                    self.push(Rc::new(NULL))?;
                }
                Some(OpCode::SetLocal) => {
                    let local_index = ins[ip + 1] as usize;
                    self.increment_ip(1);

                    let popped = self.pop()?;

                    let base_pointer = self.current_frame().base_pointer;
                    self.stack[base_pointer + local_index] = popped;
                }
                Some(OpCode::GetLocal) => {
                    let local_index = ins[ip + 1] as usize;
                    self.increment_ip(1);

                    let base_pointer = self.current_frame().base_pointer;

                    let local = Rc::clone(&self.stack[base_pointer + local_index]);
                    self.push(local)?;
                }
                Some(OpCode::GetBuiltin) => {
                    let builtin_index = ins[ip + 1] as usize;
                    self.increment_ip(1);

                    let builtin_function =
                        Rc::new(builtin::BUILTINS[builtin_index].builtin.clone());

                    self.push(builtin_function)?;
                }
                Some(OpCode::Closure) => {
                    let const_index = code::read_uint16(ins, ip + 1) as usize;
                    let num_frees = ins[ip + 3] as usize;
                    self.increment_ip(3);

                    let len = self.constants.len();
                    if const_index >= len {
                        return Err(VmError::InvalidConstIndex(const_index, len));
                    }

                    // TODO: How can I remove this `clone()`?
                    // Otherwise the compiler complains about `&self.pop()`.
                    let constant = self.constants[const_index].clone();
                    if let Constant::CompiledFunction(cf) = constant {
                        let mut free = Vec::with_capacity(num_frees);
                        for _ in 0..num_frees {
                            free.push(Rc::clone(&self.pop()?));
                        }
                        free.reverse();

                        let closure = Closure { func: cf, free };
                        self.push(Rc::new(Object::Closure(closure)))?;
                    } else {
                        return Err(VmError::NotFunction(constant));
                    }
                }
                Some(OpCode::GetFree) => {
                    let free_index = ins[ip + 1] as usize;
                    self.increment_ip(1);

                    let free = self.current_frame().free_at(free_index);
                    self.push(free)?;
                }
                None => {
                    return Err(VmError::UnknownOpCode(op_code_byte));
                }
            }
            self.increment_ip(1);
        }

        self.stack
            .get(self.sp)
            .map(|o| Rc::clone(o))
            .ok_or(VmError::StackEmpty)
    }

    fn execute_binary_operation(&mut self, op_code: OpCode) -> Result<(), VmError> {
        let right = self.pop()?;
        let left = self.pop()?;
        match (&*left, &*right) {
            (Object::Integer(l), Object::Integer(r)) => {
                self.execute_integer_binary_operation(op_code, *l, *r)
            }
            (Object::String(l), Object::String(r)) => {
                self.execute_string_binary_operation(op_code, l, r)
            }
            (l, r) => {
                let infix = infix_from_op_code(op_code).expect("not binary operation");
                Err(VmError::Eval(EvalError::TypeMismatch(
                    infix,
                    l.clone(),
                    r.clone(),
                )))
            }
        }
    }

    fn execute_integer_binary_operation(
        &mut self,
        op_code: OpCode,
        left: i64,
        right: i64,
    ) -> Result<(), VmError> {
        let result = match op_code {
            OpCode::Add => left + right,
            OpCode::Sub => left - right,
            OpCode::Mul => left * right,
            OpCode::Div => left / right,
            _ => {
                // This happens only when this vm is wrong.
                panic!("not integer binary operation: {:?}", op_code);
            }
        };

        self.push(Rc::new(Object::Integer(result)))
    }

    fn execute_string_binary_operation(
        &mut self,
        op_code: OpCode,
        left: &str,
        right: &str,
    ) -> Result<(), VmError> {
        match op_code {
            OpCode::Add => {
                let result = format!("{}{}", left, right);
                self.push(Rc::new(Object::String(result)))
            }
            OpCode::Sub | OpCode::Mul | OpCode::Div => {
                Err(VmError::Eval(EvalError::UnknownInfixOperator(
                    infix_from_op_code(op_code).expect("not string binary operation"),
                    Object::String(left.to_string()),
                    Object::String(right.to_string()),
                )))
            }
            _ => {
                // This happens only when this vm is wrong.
                panic!("not string binary operation: {:?}", op_code);
            }
        }
    }

    fn execute_comparison(&mut self, op_code: OpCode) -> Result<(), VmError> {
        let right = self.pop()?;
        let left = self.pop()?;

        match (&*left, &*right) {
            (Object::Integer(l), Object::Integer(r)) => {
                match op_code {
                    OpCode::Equal => self.push(Rc::new(Object::Boolean(l == r))),
                    OpCode::NotEqual => self.push(Rc::new(Object::Boolean(l != r))),
                    OpCode::GreaterThan => self.push(Rc::new(Object::Boolean(l > r))),
                    _ => {
                        // This happens only when this vm is wrong.
                        panic!("unknown operator: {:?}", op_code);
                    }
                }
            }
            (Object::Boolean(l), Object::Boolean(r)) => {
                match op_code {
                    OpCode::Equal => self.push(Rc::new(Object::Boolean(l == r))),
                    OpCode::NotEqual => self.push(Rc::new(Object::Boolean(l != r))),
                    OpCode::GreaterThan => Err(VmError::Eval(EvalError::UnknownInfixOperator(
                        Infix::Gt,
                        Object::Boolean(*l),
                        Object::Boolean(*r),
                    ))),
                    _ => {
                        // This happens only when this vm is wrong.
                        panic!("unknown operator: {:?}", op_code);
                    }
                }
            }
            (l, r) => {
                let infix = infix_from_op_code(op_code).expect("not comparison");
                Err(VmError::Eval(EvalError::TypeMismatch(
                    infix,
                    l.clone(),
                    r.clone(),
                )))
            }
        }
    }

    fn execute_call(&mut self, num_args: usize) -> Result<(), VmError> {
        // TODO: Don't clone...
        let callee = (*self.stack[self.sp - num_args - 1]).clone();
        match callee {
            Object::Closure(closure) => self.call_closure(num_args, closure),
            Object::Builtin(func) => self.call_builtin(num_args, func),
            obj => Err(VmError::Eval(EvalError::NotCallable(obj))),
        }
    }

    // When there are two arguments:
    //
    // sp --> |          | <-- base_pointer + 2
    //        |   arg 2  | <-- base_pointer + 1
    //        |   arg 1  | <-- base_pointer
    //        | function |
    //        |   ....   |
    //
    fn call_closure(&mut self, num_args: usize, closure: Closure) -> Result<(), VmError> {
        if closure.func.num_parameters as usize != num_args {
            return Err(VmError::Eval(EvalError::WrongArgumentCount {
                expected: closure.func.num_parameters as usize,
                given: num_args,
            }));
        }

        let num_locals = closure.func.num_locals as usize;
        let base_pointer = self.sp - num_args;
        // Keep the stack pointer to come back after calling the function.
        self.push_frame(Frame::new(closure, base_pointer));

        // Reserve space for local bindings.
        // `num_locals` includes `num_args` in itself.
        self.sp = base_pointer + num_locals;
        Ok(())
    }

    fn call_builtin(&mut self, num_args: usize, func: BuiltinFunction) -> Result<(), VmError> {
        let mut args = Vec::with_capacity(num_args);
        for _ in 0..num_args {
            let arg = self.pop()?;
            // TODO: Don't clone!
            args.push((*arg).clone());
        }
        args.reverse();

        let _function = self.pop()?;

        match func(args) {
            Ok(result) => {
                self.push(Rc::new(result))?;
            }
            Err(error) => {
                return Err(VmError::Eval(error));
            }
        }

        self.increment_ip(1);

        Ok(())
    }

    fn push(&mut self, obj: Rc<Object>) -> Result<(), VmError> {
        if self.sp >= STACK_SIZE {
            return Err(VmError::StackOverflow);
        }
        self.stack[self.sp] = obj;
        self.sp += 1;
        Ok(())
    }

    fn pop(&mut self) -> Result<Rc<Object>, VmError> {
        let popped = self.stack.get(self.sp - 1);
        self.sp -= 1;
        popped.map(|o| Rc::clone(o)).ok_or(VmError::StackEmpty)
    }

    fn current_frame(&self) -> &Frame {
        &self.frames[self.frames_index - 1]
    }

    fn increment_ip(&mut self, diff: usize) {
        self.frames[self.frames_index - 1].ip += diff;
    }

    fn set_ip(&mut self, to: usize) {
        self.frames[self.frames_index - 1].ip = to;
    }

    fn push_frame(&mut self, frame: Frame) {
        self.frames.push(frame);
        self.frames_index += 1;
    }

    fn pop_frame(&mut self) -> Frame {
        self.frames_index -= 1;
        self.frames.pop().expect("empty frames")
    }
}

fn infix_from_op_code(op_code: OpCode) -> Option<Infix> {
    match op_code {
        OpCode::Add => Some(Infix::Plus),
        OpCode::Sub => Some(Infix::Minus),
        OpCode::Mul => Some(Infix::Asterisk),
        OpCode::Div => Some(Infix::Slash),
        OpCode::Equal => Some(Infix::Eq),
        OpCode::NotEqual => Some(Infix::NotEq),
        OpCode::GreaterThan => Some(Infix::Gt),
        _ => None,
    }
}

#[derive(Debug)]
pub enum VmError {
    UnknownOpCode(u8),
    InvalidConstIndex(usize, usize),
    StackOverflow,
    StackEmpty,
    NotFunction(Constant),
    Eval(EvalError),
}

impl fmt::Display for VmError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VmError::UnknownOpCode(op_code) => write!(f, "unknown op code: {}", op_code),
            VmError::InvalidConstIndex(given, length) => {
                write!(f, "invalid const index: {} / {}", given, length)
            }
            VmError::StackOverflow => write!(f, "stack overflow"),
            VmError::StackEmpty => write!(f, "stack empty"),
            VmError::NotFunction(con) => write!(f, "not a function: {}", con.type_name()),
            VmError::Eval(eval_error) => write!(f, "{}", eval_error),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::code::Bytecode;
    use crate::compiler::Compiler;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;
    use crate::vm::Vm;

    #[test]
    fn empty_bytecode() {
        let bytecode = Bytecode::new(vec![], vec![]);
        let vm = Vm::new(bytecode);
        match vm.run() {
            Ok(obj) => {
                assert_eq!(
                    *obj,
                    Object::Null,
                    "VM should return null for empty bytecode"
                );
            }
            Err(err) => {
                panic!("vm failed {}", err);
            }
        }
    }

    #[test]
    fn integer() {
        expect_values(vec![
            ("1", "1"),
            ("2", "2"),
            ("1 + 2", "3"),
            ("1 - 2", "-1"),
            ("2 * 3", "6"),
            ("4 / 2", "2"),
            ("50 / 2 * 2 + 10 - 5", "55"),
            ("5 * (2 + 10)", "60"),
            ("5 + 5 + 5 + 5 - 10", "10"),
            ("2 * 2 * 2 * 2 * 2", "32"),
            ("5 * 2 + 10", "20"),
            ("5 + 2 * 10", "25"),
            ("1 == 1", "true"),
            ("1 == 2", "false"),
            ("1 != 1", "false"),
            ("1 != 2", "true"),
            ("1 > 2", "false"),
            ("2 > 1", "true"),
            ("1 < 2", "true"),
            ("2 < 1", "false"),
        ]);
    }

    #[test]
    fn boolean() {
        expect_values(vec![
            ("true", "true"),
            ("false", "false"),
            ("true == true", "true"),
            ("false == false", "true"),
            ("true == false", "false"),
            ("true != true", "false"),
            ("false != false", "false"),
            ("true != false", "true"),
        ]);
    }

    #[test]
    fn prefix_minus() {
        expect_values(vec![
            ("-123", "-123"),
            ("-(1 + 3)", "-4"),
            ("-(10 - 23)", "13"),
        ]);
    }

    #[test]
    fn prefix_bang() {
        expect_values(vec![
            ("!true", "false"),
            ("!false", "true"),
            ("!0", "false"),
            ("!123", "false"),
            ("!-123", "false"),
            ("!!true", "true"),
            ("!!false", "false"),
            ("!!0", "true"),
            ("!!123", "true"),
            ("!!-123", "true"),
            ("!(if (false) { 10 })", "true"),
        ]);
    }

    #[test]
    fn if_expression() {
        expect_values(vec![
            ("if (true) { 10 }", "10"),
            ("if (true) { 10 } else { 20 }", "10"),
            ("if (false) { 10 } else { 20 }", "20"),
            ("if (1) { 10 }", "10"),
            ("if (1 < 2) { 10 }", "10"),
            ("if (1 < 2) { 10 } else { 20 }", "10"),
            ("if (1 > 2) { 10 } else { 20 }", "20"),
            ("if (false) { 10 }", "null"),
            ("if (1 > 2) { 10 }", "null"),
            ("if (if (false) { 10 }) { 10 }", "null"),
            ("if (if (false) { 10 }) { 10 } else { 20 }", "20"),
        ]);
    }

    #[test]
    fn global_let_statements() {
        expect_values(vec![
            ("let one = 1; one", "1"),
            ("let one = 1; let two = 2; one + two", "3"),
            ("let one = 1; let two = one + one; one + two", "3"),
        ]);
    }

    #[test]
    fn string_expressions() {
        expect_values(vec![
            (r#""hello""#, r#""hello""#),
            (r#""hello" + " world""#, r#""hello world""#),
            (r#""foo" + "bar" + "baz""#, r#""foobarbaz""#),
        ]);
    }

    #[test]
    fn array_literals() {
        expect_values(vec![
            ("[1, 2, 3]", "[1, 2, 3]"),
            ("[1, 2 + 3, 4 + 5 + 6]", "[1, 5, 15]"),
        ]);
    }

    #[test]
    fn hash_literals() {
        expect_values(vec![
            ("{}", "{}"),
            ("{1: 2, 2: 3}", "{1: 2, 2: 3}"),
            ("{1 + 1: 2 * 2, 3 + 3: 4 * 4}", "{2: 4, 6: 16}"),
        ]);
    }

    #[test]
    fn index_expression() {
        expect_values(vec![
            ("[][1]", "null"),
            ("[1, 2][-1]", "null"),
            ("[1, 2][0]", "1"),
            (r#"{}["foo"]"#, "null"),
            (r#"{"foo": 1 + 2, "bar": 3 + 4}["bar"]"#, "7"),
        ]);
    }

    #[test]
    fn function_call_without_arguments() {
        expect_values(vec![
            (
                "let fivePlusTen = fn() { 5 + 10; };
                 fivePlusTen();",
                "15",
            ),
            (
                "let one = fn() { 1 };
                 let two = fn() { 2 };
                 one() + two()",
                "3",
            ),
            (
                "let a = fn() { 1 };
                 let b = fn() { a() + 2 };
                 let c = fn() { b() + 3 };
                 c();",
                "6",
            ),
            (
                "let earlyExit = fn() { return 99; 100 };
                 earlyExit();",
                "99",
            ),
        ]);
    }

    #[test]
    fn function_call_without_return_value() {
        expect_values(vec![(
            "let noReturn = fn() {};
             noReturn();",
            "null",
        )]);
    }

    #[test]
    fn first_call_function() {
        expect_values(vec![(
            "let returnsOne = fn() { 1 };
             let returnsOneReturner = fn() { returnsOne };
             returnsOneReturner()();",
            "1",
        )]);
    }

    #[test]
    fn calling_functions_with_bindings() {
        expect_values(vec![
            ("let one = fn() { let one = 1; one }; one();", "1"),
            (
                "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
                 oneAndTwo();",
                "3",
            ),
            (
                "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
                 let threeAndFour = fn() { let three = 3; let four = 4; three + four; };
                 oneAndTwo() + threeAndFour();",
                "10",
            ),
            (
                "let firstFoobar = fn() { let foobar = 50; foobar; };
                 let secondFoobar = fn() { let foobar = 100; foobar; };
                 firstFoobar() + secondFoobar();",
                "150",
            ),
            (
                "let globalSeed = 50;
                 let minusOne = fn() { let num = 1; globalSeed - num; };
                 let minusTwo = fn() { let num = 2; globalSeed - num; };
                 minusOne() + minusTwo();",
                "97",
            ),
        ]);
    }

    #[test]
    fn calling_functions_with_arguments_and_bindings() {
        expect_values(vec![
            ("let identity = fn(x) { x; }; identity(4);", "4"),
            ("let sum = fn(a, b) { a + b; }; sum(1, 2);", "3"),
            ("let sum = fn(a, b) { let c = a + b; c; }; sum(1, 2);", "3"),
            (
                "let sum = fn(a, b) { let c = a + b; c; };
                 sum(1, 2) + sum(3, 4);",
                "10",
            ),
            (
                "let sum = fn(a, b) { let c = a + b; c; };
                 let outer = fn() { sum(1, 2) + sum(3, 4); };
                 outer();",
                "10",
            ),
            (
                "let globalNum = 10;
                 let sum = fn(a, b) {
                    let c = a + b;
                    c + globalNum;
                 };
                 let outer = fn() {
                    sum(1, 2) + sum(3, 4) + globalNum;
                 }
                 outer() + globalNum;",
                "50",
            ),
        ]);
    }

    #[test]
    fn calling_functions_with_wrong_arguments() {
        expect_errors(vec![
            (
                "fn() { 1; }(1);",
                "wrong number of arguments: expected 0, given 1",
            ),
            (
                "fn(a) { a; }();",
                "wrong number of arguments: expected 1, given 0",
            ),
            (
                "fn(a, b) { a + b; }(1);",
                "wrong number of arguments: expected 2, given 1",
            ),
        ]);
    }

    #[test]
    fn calling_builtin_functions() {
        expect_values(vec![
            (r#"len("")"#, "0"),
            (r#"len("four")"#, "4"),
            (r#"len("hello world")"#, "11"),
            ("len([]);", "0"),
            ("len([1, 2, 3]);", "3"),
            (r#"puts("hello", "world!")"#, "null"),
            ("first([1, 2, 3])", "1"),
            ("first([])", "null"),
            ("last([1, 2, 3])", "3"),
            ("last([])", "null"),
            ("rest([1, 2, 3])", "[2, 3]"),
            ("rest([1])", "[]"),
            ("rest([])", "null"),
            ("push([], 1)", "[1]"),
        ]);
        expect_errors(vec![
            ("len(1)", "unsupported arguments to `len`: INTEGER"),
            (
                r#"len("one", "two")"#,
                "wrong number of arguments: expected 1, given 2",
            ),
            ("first(1)", "unsupported arguments to `first`: INTEGER"),
            ("last(1)", "unsupported arguments to `last`: INTEGER"),
            (
                "push(1, 1)",
                "unsupported arguments to `push`: INTEGER, INTEGER",
            ),
        ]);
    }

    #[test]
    fn closures() {
        expect_values(vec![
            (
                "let newClosure = fn(a) {
                 fn() { a; };
             };
             let closure = newClosure(99);
             closure();",
                "99",
            ),
            (
                "let newAdder = fn(a, b) {
                     fn(c) { a + b + c; };
                 };
                 let adder = newAdder(1, 2);
                 adder(8);",
                "11",
            ),
            (
                "let newAdder = fn(a, b) {
                     let c = a + b;
                     fn(d) { c + d; };
                 };
                 let adder = newAdder(1, 2);
                 adder(8);",
                "11",
            ),
            (
                "let newAdderOuter = fn(a, b) {
                     let c = a + b;
                     fn(d) {
                         let e = c + d;
                         fn(f) { e + f };
                     };
                 };
                 let newAdderInner = newAdderOuter(1, 2);
                 let adder = newAdderInner(3);
                 adder(8);",
                "14",
            ),
            (
                "let a = 1;
                 let newAdderOuter = fn(b) {
                     fn(c) {
                         fn(d) { a + b + c + d };
                     };
                 };
                 let newAdderInner = newAdderOuter(2);
                 let adder = newAdderInner(3);
                 adder(8);",
                "14",
            ),
            (
                "let newClosure = fn(a, b) {
                     let one = fn() { a; };
                     let two = fn() { b; };
                     fn() { one() + two(); }
                 };
                 let closure = newClosure(1, 2);
                 closure();",
                "3",
            ),
        ]);
    }

    #[test]
    fn recursive_fibonacci() {
        expect_values(vec![(
            "let fibonacci = fn(x) {
                 if (x == 0) {
                     0
                 } else {
                     if (x == 1) {
                         1
                     } else {
                         fibonacci(x - 1) + fibonacci(x - 2)
                     }
                 }
             };
             fibonacci(15);",
            "610",
        )]);
    }

    fn expect_values(tests: Vec<(&str, &str)>) {
        for (input, expected) in tests {
            let vm = make_vm(input);
            match vm.run() {
                Ok(obj) => {
                    assert_eq!(&obj.to_string(), expected, "for `{}`", input);
                }
                Err(err) => {
                    panic!("error on vm for `{}`: {}", input, err);
                }
            }
        }
    }

    fn expect_errors(tests: Vec<(&str, &str)>) {
        for (input, expected) in tests {
            let vm = make_vm(input);
            match vm.run() {
                Err(err) => {
                    assert_eq!(&err.to_string(), expected, "for `{}`", input);
                }
                _ => {
                    panic!("expected vm error: {} for `{}`", expected, input);
                }
            }
        }
    }

    fn make_vm(input: &str) -> Vm {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let errors = parser.errors();
        if errors.len() > 0 {
            panic!("for input '{}', got parser errors: {:?}", input, errors);
        }

        let compiler = Compiler::new();
        let bytecode = match compiler.compile(&program) {
            Ok(bytecode) => bytecode,
            Err(err) => {
                panic!("error on compile for `{}`: {}", input, err);
            }
        };
        Vm::new(bytecode)
    }
}
