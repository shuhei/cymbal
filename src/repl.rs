use crate::compiler::{Compiler, SymbolTable};
use crate::evaluator;
use crate::lexer::Lexer;
use crate::object::Environment;
use crate::parser::Parser;
use crate::vm;
use crate::vm::Vm;
use std::cell::RefCell;
use std::io;
use std::io::Write;
use std::rc::Rc;

pub enum Mode {
    Eval,
    Compile,
}

pub fn start(mode: Mode) {
    let mut stdout = io::stdout();
    let stdin = io::stdin();
    let mut input = String::new();

    // For evaluator
    let env = Rc::new(RefCell::new(Environment::new()));

    // For compiler and vm
    let constants = Rc::new(RefCell::new(Vec::new()));
    let globals = Rc::new(RefCell::new(vm::new_globals()));
    let symbol_table = Rc::new(RefCell::new(SymbolTable::new_with_builtins()));

    loop {
        print!(">> ");
        stdout.flush().expect("Failed to flush stdout");
        stdin
            .read_line(&mut input)
            .expect("Failed to read line from stdin");

        let lexer = Lexer::new(input.trim());
        input.clear();
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        if parser.errors().len() > 0 {
            println!("Woops! We ran into some monkey business here!");
            println!(" parser errors:");
            for error in parser.errors() {
                println!("\t{:?}", error);
            }
            continue;
        }

        match mode {
            Mode::Eval => match evaluator::eval(&program, Rc::clone(&env)) {
                Ok(obj) => {
                    println!("{}", obj);
                }
                Err(err) => {
                    println!("ERROR: {}", err);
                }
            },
            Mode::Compile => {
                let mut compiler =
                    Compiler::new_with_state(Rc::clone(&symbol_table), Rc::clone(&constants));
                match compiler.compile(&program) {
                    Err(err) => {
                        println!("Woops! Compilation failed: {}", err);
                        continue;
                    }
                    _ => {}
                }
                let bytecode = compiler.bytecode();
                let mut vm = Vm::new_with_globals_store(bytecode, Rc::clone(&globals));
                match vm.run() {
                    Err(err) => {
                        println!("Woops! Executing bytecode failed: {}", err);
                        continue;
                    }
                    _ => {}
                }
                if let Some(result) = vm.last_popped_stack_elem() {
                    println!("{}", result);
                }
            }
        }
    }
}
