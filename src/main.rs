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
                let source =
                    fs::read_to_string(source_path).expect("error: failed to read a source file");
                match compile(source) {
                    Ok(_) => {}
                    Err(_) => {
                        process::exit(1);
                    }
                }
            }
            "run" => {
                let source_path = args.get(2).expect("error: specify a bytecode file to run");
                let bytes = fs::read(source_path).expect("error: failed to read a bytecode file");
                let bytecode =
                    Bytecode::from_bytes(&bytes).expect("error: Failed to deserialize bytecode");
                let vm = Vm::new(bytecode);
                vm.run().expect("error: Failed to run bytecode");
            }
            unknown => {
                println!("cymbal: '{}' is not a valid subcommand\n", unknown);
                help();
                process::exit(1);
            }
        },
        None => {
            help();
        }
    }
}

// TODO: Clean up error handling.
fn compile(source: String) -> Result<(), ()> {
    let parser = Parser::new(Lexer::new(source));
    let program = match parser.parse_program() {
        Ok(pg) => pg,
        Err(err) => {
            println!("{:?}", err);
            return Err(());
        }
    };
    let compiler = Compiler::new();
    let bytecode = compiler
        .compile(&program)
        .expect("error: Failed to compile");
    // TODO: Make the output path flexible.
    let mut file = fs::File::create("out.mko").expect("error: Failed to open an output file");
    bytecode
        .serialize(&mut file)
        .expect("error: Failed to serialize bytecode");
    println!("Wrote bytecode into 'out.mko'");
    Ok(())
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
