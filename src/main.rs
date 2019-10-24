use cymbal::benchmark;
use cymbal::compiler::Compiler;
use cymbal::lexer::Lexer;
use cymbal::mode::Mode;
use cymbal::parser::Parser;
use cymbal::repl;
use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.get(1) {
        Some(subcommand) => match subcommand.as_ref() {
            "repl" => repl::start(eval_or_compile()),
            "benchmark" => benchmark::run(eval_or_compile()),
            "compile" => match args.get(2) {
                Some(source_path) => {
                    let source = fs::read_to_string(source_path)
                        .expect("error: failed to read a source file");
                    match compile(source) {
                        Ok(_) => {}
                        Err(_) => {
                            process::exit(1);
                        }
                    }
                }
                None => {
                    println!("error: specify a source file to compile");
                    process::exit(1);
                }
            },
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
    match compiler.compile(&program) {
        Ok(_) => {
            // TOOD: Save the bytecode into a file.
            println!("Succeeded to parse");
        }
        Err(err) => {
            println!("{}", err);
            return Err(());
        }
    }
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
