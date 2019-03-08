use std::env;
use cymbal::repl;

fn main() {
    let username = env::var("LOGNAME").unwrap_or("anonymous".to_string());
    println!("Hello {}! This is the Monkey programming language!", username);
    println!("Feel free to type in commands");
    repl::start();
}
