use crate::evaluator;
use crate::lexer::Lexer;
use crate::object::Environment;
use crate::parser::Parser;
use std::cell::RefCell;
use std::io;
use std::io::Write;
use std::rc::Rc;

pub fn start() {
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

        match evaluator::eval(&program, Rc::clone(&env)) {
            Ok(obj) => {
                println!("{}", obj);
            }
            Err(err) => {
                println!("ERROR: {}", err);
            }
        }
    }
}
