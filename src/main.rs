use cymbal::benchmark;
use cymbal::mode::Mode;
use cymbal::repl;
use std::env;
use std::process;

fn main() {
    let maybe_subcommand = env::args().nth(1);
    match maybe_subcommand {
        Some(subcommand) => {
            let mode = if has_flag("--compile") {
                Mode::Compile
            } else {
                Mode::Eval
            };
            match subcommand.as_ref() {
                "repl" => repl::start(mode),
                "benchmark" => benchmark::run(mode),
                unknown => {
                    println!("cymbal: '{}' is not a valid subcommand\n", unknown);
                    help();
                    process::exit(1);
                }
            }
        }
        None => {
            help();
        }
    }
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
