use crate::evaluator;
use crate::lexer::Lexer;
use crate::object::Environment;
use crate::parser::Parser;
use crate::compiler::Compiler;
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

    let env = Rc::new(RefCell::new(Environment::new()));

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
            Mode::Eval => {
                match evaluator::eval(&program, Rc::clone(&env)) {
                    Ok(obj) => {
                        println!("{}", obj);
                    }
                    Err(err) => {
                        println!("ERROR: {}", err);
                    }
                }
            }
            Mode::Compile => {
                let mut compiler = Compiler::new();
                match compiler.compile(&program) {
                    Err(err) => {
                        println!("Woops! Compilation failed: {}", err);
                        continue;
                    }
                    _ => {}
                }
                let bytecode = compiler.bytecode();
                let mut vm = Vm::new(bytecode);
                match vm.run() {
                    Err(err) => {
                        println!("Woops! Executing bytecode failed: {}", err);
                        continue;
                    }
                    _ => {}
                }
                if let Some(result) = vm.stack_top() {
                    println!("{}", result);
                }
            }
        }
    }
}
