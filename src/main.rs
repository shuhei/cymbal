use std::env;
use cymbal::repl;
use cymbal::mode::Mode;
use cymbal::benchmark;

fn main() {
    let mode = if has_flag("--compile") {
        Mode::Compile
    } else {
        Mode::Eval
    };

    if has_flag("--benchmark") {
        benchmark::run(mode);
    } else {
        let username = env::var("LOGNAME").unwrap_or("anonymous".to_string());
        println!("Hello {}! This is the Monkey programming language!", username);
        println!("Feel free to type in commands");
        repl::start(mode);
    }
}

fn has_flag(flag: &str) -> bool {
    for arg in env::args() {
        if arg == flag {
            return true;
        }
    }
    return false;
}
