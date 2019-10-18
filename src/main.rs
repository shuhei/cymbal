use cymbal::benchmark;
use cymbal::mode::Mode;
use cymbal::repl;
use std::env;

fn main() {
    let mode = if has_flag("--compile") {
        Mode::Compile
    } else {
        Mode::Eval
    };

    if has_flag("--benchmark") {
        benchmark::run(mode);
    } else {
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
