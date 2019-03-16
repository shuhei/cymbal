use std::env;
use cymbal::repl;
use cymbal::repl::Mode;

fn main() {
    let mode = get_repl_mode();
    let username = env::var("LOGNAME").unwrap_or("anonymous".to_string());
    println!("Hello {}! This is the Monkey programming language!", username);
    println!("Feel free to type in commands");
    repl::start(mode);
}

fn get_repl_mode() -> Mode {
    for arg in env::args() {
        if arg == "--compile" {
            return Mode::Compile;
        }
    }
    Mode::Eval
}
