use crate::compiler::{Compiler, SymbolTable};
use crate::evaluator;
use crate::lexer::Lexer;
use crate::mode::Mode;
use crate::object::Environment;
use crate::parser::Parser;
use crate::vm::Vm;
use crate::vm;
use std::cell::RefCell;
use std::rc::Rc;
use std::time::Instant;

pub fn run(mode: Mode) {
    let code =
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
         fibonacci(25);";

    let lexer = Lexer::new(code);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    match mode {
        Mode::Eval => {
            let start = Instant::now();

            let env = Rc::new(RefCell::new(Environment::new()));
            let result = evaluator::eval(&program, env).expect("eval failed");

            let elapsed = start.elapsed();
            println!("{} seconds {} nanoseconds, result: {}", elapsed.as_secs(), elapsed.subsec_nanos(), result);
        }
        Mode::Compile => {
            let constants = Rc::new(RefCell::new(Vec::new()));
            let symbol_table = Rc::new(RefCell::new(SymbolTable::new_with_builtins()));
            let mut compiler = Compiler::new_with_state(symbol_table, constants);
            compiler.compile(&program).expect("compile failed");
            let bytecode = compiler.bytecode();

            let start = Instant::now();

            let globals = Rc::new(RefCell::new(vm::new_globals()));
            let mut vm = Vm::new_with_globals_store(bytecode, globals);
            vm.run().expect("vm failed");
            let result = vm.last_popped_stack_elem().expect("vm result should exist");

            let elapsed = start.elapsed();
            println!("{} seconds {} nanoseconds, result: {}", elapsed.as_secs(), elapsed.subsec_nanos(), result);
        }
    }
}
