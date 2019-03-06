use crate::lexer::Lexer;
use crate::token::Token;
use crate::ast::{Expression, Program, Statement};

enum Precedence {
    Lowest,
    Equals, // ==
    Lessgreater, // > or <
    Sum, // +
    Product, // *
    Prefix, // -X or !X
    Call, // myFunction(X)
}

pub struct Parser {
    lexer: Lexer,
    pub errors: Vec<String>,

    cur_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut p = Parser {
            lexer,
            errors: vec![],
            cur_token: Token::Illegal,
            peek_token: Token::Illegal,
        };
        p.next_token();
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        // TODO: Without the `.clone()`, rustc complains
        // `cannot move out of borrowed content`... Why?
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements = vec![];

        while self.cur_token != Token::Eof {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();
        }

        Program { statements }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let name;
        match self.peek_token.clone() {
            Token::Ident(ident) => {
                self.next_token();
                name = ident;
            },
            _ => {
                self.peek_error("identifier");
                return None;
            },
        }

        if !self.expect_peek(Token::Assign) {
            return None;
        }

	// TODO: Skipping the expressions until we encounter a semicolon
        while self.cur_token != Token::Semicolon {
            self.next_token();
        }

        // TODO: Let assignment.
        Some(Statement::Let(name))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

	// TODO: Skipping the expressions until we encounter a semicolon
        while self.cur_token != Token::Semicolon {
            self.next_token();
        }

        // TODO: Return value.
        Some(Statement::Return)
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        expression.map(|exp| Statement::Expression(exp))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        self.parse_prefix(self.cur_token.clone())
    }

    fn parse_prefix(&mut self, token: Token) -> Option<Expression> {
        match token {
            Token::Ident(ident) => Some(Expression::Identifier(ident)),
            Token::Int(int) => {
                match int.parse() {
                    Ok(value) => Some(Expression::IntegerLiteral(value)),
                    Err(_) => {
                        let msg = format!("could not parse '{}' as integer", int);
                        self.errors.push(msg);
                        None
                    }
                }
            },
            Token::Bang => self.parse_prefix_expression(),
            Token::Minus => self.parse_prefix_expression(),
            _ => None,
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let operator = self.cur_token.clone();
        self.next_token();
        let expression = self.parse_expression(Precedence::Prefix);

        expression.map(|exp| Expression::Prefix(operator.to_string(), Box::new(exp)))
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        if self.peek_token == token {
            self.next_token();
            return true;
        } else {
            self.peek_error(&token.to_string());
            return false;
        }
    }

    fn peek_error(&mut self, expected: &str) {
        let msg = format!("expected next token to be {}, got {} instead", expected, self.peek_token);
        self.errors.push(msg);
    }
}
