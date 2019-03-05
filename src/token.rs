#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Illegal,
    Eof,

    // Identifiers + literals
    Ident, // add, foobar, x, y, ...
    Int, // 123456

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Lt,
    Gt,

    Eq,
    NotEq,

    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

pub struct Token {
    // `type` is a keyword in Rust
    pub kind: TokenKind,
    pub literal: String,
}

pub fn lookup_ident(ident: &str) -> TokenKind {
    keyword_to_token_kind(ident).unwrap_or(TokenKind::Ident)
}

fn keyword_to_token_kind(keyword: &str) -> Option<TokenKind> {
    match keyword {
        "fn" => Some(TokenKind::Function),
        "let" => Some(TokenKind::Let),
        "true" => Some(TokenKind::True),
        "false" => Some(TokenKind::False),
        "if" => Some(TokenKind::If),
        "else" => Some(TokenKind::Else),
        "return" => Some(TokenKind::Return),
        _ => None,
    }
}
