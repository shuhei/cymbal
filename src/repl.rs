use std::io;
use std::io::Write;
use crate::lexer::Lexer;
use crate::token::TokenKind;

pub fn start() {
    let mut stdout = io::stdout();
    let stdin = io::stdin();
    let mut input = String::new();

    loop {
        print!(">> ");
        stdout.flush().expect("Failed to flush stdout");
        stdin.read_line(&mut input).expect("Failed to read line from stdin");

        let mut lexer = Lexer::new(input.trim());
        let mut tok = lexer.next_token();
        while tok.kind != TokenKind::Eof {
            println!("{:?}", tok);
            tok = lexer.next_token();
        }

        input.clear();
    }
}
