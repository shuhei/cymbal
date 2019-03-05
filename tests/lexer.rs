extern crate monkey;

#[cfg(test)]
mod lexer_tests {
    use monkey::token::TokenKind;
    use monkey::lexer::Lexer;

    #[test]
    fn next_token() {
        let input = "let five = 5;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
        ";

        let tests = [
            (TokenKind::Let, "let"),
            (TokenKind::Ident, "five"),
            (TokenKind::Assign, "="),
            (TokenKind::Int, "5"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Let, "let"),
            (TokenKind::Ident, "ten"),
            (TokenKind::Assign, "="),
            (TokenKind::Int, "10"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Let, "let"),
            (TokenKind::Ident, "add"),
            (TokenKind::Assign, "="),
            (TokenKind::Function, "fn"),
            (TokenKind::Lparen, "("),
            (TokenKind::Ident, "x"),
            (TokenKind::Comma, ","),
            (TokenKind::Ident, "y"),
            (TokenKind::Rparen, ")"),
            (TokenKind::Lbrace, "{"),
            (TokenKind::Ident, "x"),
            (TokenKind::Plus, "+"),
            (TokenKind::Ident, "y"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Rbrace, "}"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Let, "let"),
            (TokenKind::Ident, "result"),
            (TokenKind::Assign, "="),
            (TokenKind::Ident, "add"),
            (TokenKind::Lparen, "("),
            (TokenKind::Ident, "five"),
            (TokenKind::Comma, ","),
            (TokenKind::Ident, "ten"),
            (TokenKind::Rparen, ")"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Bang, "!"),
            (TokenKind::Minus, "-"),
            (TokenKind::Slash, "/"),
            (TokenKind::Asterisk, "*"),
            (TokenKind::Int, "5"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Int, "5"),
            (TokenKind::Lt, "<"),
            (TokenKind::Int, "10"),
            (TokenKind::Gt, ">"),
            (TokenKind::Int, "5"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::If, "if"),
            (TokenKind::Lparen, "("),
            (TokenKind::Int, "5"),
            (TokenKind::Lt, "<"),
            (TokenKind::Int, "10"),
            (TokenKind::Rparen, ")"),
            (TokenKind::Lbrace, "{"),
            (TokenKind::Return, "return"),
            (TokenKind::True, "true"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Rbrace, "}"),
            (TokenKind::Else, "else"),
            (TokenKind::Lbrace, "{"),
            (TokenKind::Return, "return"),
            (TokenKind::False, "false"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Rbrace, "}"),
            (TokenKind::Int, "10"),
            (TokenKind::Eq, "=="),
            (TokenKind::Int, "10"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Int, "10"),
            (TokenKind::NotEq, "!="),
            (TokenKind::Int, "9"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Eof, ""),
        ];

        let mut lexer = Lexer::new(input);

        for (i, (kind, literal)) in tests.iter().enumerate() {
            let tok = lexer.next_token();

            assert_eq!(&tok.kind, kind, "tests[{}] - token type", i);
            assert_eq!(&tok.literal, literal, "tests[{}] - token literal", i);
        }
    }
}
