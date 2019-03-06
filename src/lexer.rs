use crate::token;
use crate::token::{Token, TokenKind};

pub struct Lexer {
    input: String,
    // Current position in input (points to current char)
    position: usize,
    // current reading position in input (after current char)
    read_position: usize,
    // current char under examination
    ch: char, }

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer {
            input: input.to_string(),
            position: 0,
            read_position: 0,
            ch: '\u{0}',
        };
        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok: Token;
        match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    let literal = "==";
                    tok = Token {
                        kind: TokenKind::Eq,
                        literal: literal.to_owned(),
                    };
                } else {
                    tok = new_token(TokenKind::Assign, self.ch);
                }
            }
            ';' => {
                tok = new_token(TokenKind::Semicolon, self.ch);
            }
            '(' => {
                tok = new_token(TokenKind::Lparen, self.ch);
            }
            ')' => {
                tok = new_token(TokenKind::Rparen, self.ch);
            }
            ',' => {
                tok = new_token(TokenKind::Comma, self.ch);
            }
            '+' => {
                tok = new_token(TokenKind::Plus, self.ch);
            }
            '-' => {
                tok = new_token(TokenKind::Minus, self.ch);
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    let literal = "!=";
                    tok = Token {
                        kind: TokenKind::NotEq,
                        literal: literal.to_owned(),
                    };
                } else {
                    tok = new_token(TokenKind::Bang, self.ch);
                }
            }
            '*' => {
                tok = new_token(TokenKind::Asterisk, self.ch);
            }
            '/' => {
                tok = new_token(TokenKind::Slash, self.ch);
            }
            '<' => {
                tok = new_token(TokenKind::Lt, self.ch);
            }
            '>' => {
                tok = new_token(TokenKind::Gt, self.ch);
            }
            '{' => {
                tok = new_token(TokenKind::Lbrace, self.ch);
            }
            '}' => {
                tok = new_token(TokenKind::Rbrace, self.ch);
            }
            '\u{0}' => {
                tok = Token {
                    kind: TokenKind::Eof,
                    literal: "".to_owned(),
                }
            }
            _ => {
                if is_letter(self.ch) {
                    let ident = self.read_identifier();
                    return Token {
                        kind: token::lookup_ident(ident),
                        literal: ident.to_owned(),
                    };
                } else if is_digit(self.ch) {
                    return Token {
                        kind: TokenKind::Int,
                        literal: self.read_number().to_owned(),
                    };
                } else {
                    tok = new_token(TokenKind::Illegal, self.ch);
                }
            }
        }

        self.read_char();
        tok
    }

    // TODO: Support unicode.
    fn read_char(&mut self) {
        // TODO: Better way of indexing a string.
        self.ch = self
            .input
            .chars()
            .nth(self.read_position)
            .unwrap_or('\u{0}');
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> &str {
        let position = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }
        &self.input[position..self.position]
    }

    fn read_number(&mut self) -> &str {
        let position = self.position;
        while is_digit(self.ch) {
            self.read_char();
        }
        &self.input[position..self.position]
    }

    fn skip_whitespace(&mut self) {
        while is_whitespace(self.ch) {
            self.read_char();
        }
    }

    fn peek_char(&self) -> char {
        self.input
            .chars()
            .nth(self.read_position)
            .unwrap_or('\u{0}')
    }
}

fn is_letter(ch: char) -> bool {
    'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}

fn is_whitespace(ch: char) -> bool {
    ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
}

fn new_token(kind: TokenKind, ch: char) -> Token {
    Token {
        kind,
        literal: ch.to_string(),
    }
}
