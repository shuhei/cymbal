use cymbal::benchmark;
use cymbal::code::{Bytecode, Serializable};
use cymbal::compiler::Compiler;
use cymbal::lexer::Lexer;
use cymbal::mode::Mode;
use cymbal::parser::Parser;
use cymbal::repl;
use cymbal::vm::Vm;
use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.get(1) {
        Some(subcommand) => match subcommand.as_ref() {
            "repl" => repl::start(eval_or_compile()),
            "benchmark" => benchmark::run(eval_or_compile()),
            "compile" => {
                let source_path = args
                    .get(2)
                    .expect("error: specify a source file to compile");
                compile(source_path);
            }
            "run" => {
                let source_path = args.get(2).expect("error: specify a bytecode file to run");
                run(source_path);
            }
            "help" => {
                help();
            }
            unknown => {
                unknown_subcommand(unknown);
            }
        },
        None => {
            help();
        }
    }
}

// -- Actions

fn compile(source_path: &str) {
    let source = fs::read_to_string(source_path).expect("error: failed to read a source file");
    let parser = Parser::new(Lexer::new(source));
    let program = parser
        .parse_program()
        .expect("error: Failed to parse source");
    let compiler = Compiler::new();
    let bytecode = compiler
        .compile(&program)
        .expect("error: Failed to compile");
    // TODO: Make the output path flexible.
    let mut file = fs::File::create("out.mo").expect("error: Failed to open an output file");
    bytecode
        .serialize(&mut file)
        .expect("error: Failed to serialize bytecode");
    println!("Wrote bytecode into 'out.mo'");
}

fn run(source_path: &str) {
    let bytes = fs::read(source_path).expect("error: failed to read a bytecode file");
    let bytecode = Bytecode::from_bytes(&bytes).expect("error: Failed to deserialize bytecode");
    let vm = Vm::new(bytecode);
    vm.run().expect("error: Failed to run bytecode");
}

fn help() {
    println!(
        r#"Usage: cymbal SUBCOMMAND [OPTIONS]

Subcommands:
    cymbal repl
    cymbal repl --eval
    cymbal repl --compile

    cymbal benchmark --eval
    cymbal benchmark --compile
"#
    );
}

fn unknown_subcommand(subcommand: &str) {
    println!("cymbal: '{}' is not a valid subcommand\n", subcommand);
    help();
    process::exit(1);
}

// -- Helpers

fn has_flag(flag: &str) -> bool {
    env::args().any(|arg| arg == flag)
}

fn eval_or_compile() -> Mode {
    if has_flag("--compile") {
        Mode::Compile
    } else {
        Mode::Eval
    }
}
