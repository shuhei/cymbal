use crate::ast::{Expression, Infix, Prefix, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
                 // Call, // myFunction(X)
}

type PrefixParseFn = fn(&mut Parser) -> Option<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> Option<Expression>;

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
            }
            _ => {
                self.peek_error("identifier");
                return None;
            }
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
        if let Some(prefix) = self.prefix_parse_fn() {
            if let Some(left) = prefix(self) {
                let mut left_exp = left;
                while self.peek_token != Token::Semicolon
                    && precedence < self.infix_token(&self.peek_token).0
                {
                    if let Some(infix) = self.infix_parse_fn() {
                        self.next_token();
                        if let Some(i) = infix(self, left_exp) {
                            left_exp = i;
                        } else {
                            return None;
                        }
                    } else {
                        return Some(left_exp);
                    }
                }
                Some(left_exp)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn prefix_parse_fn(&self) -> Option<PrefixParseFn> {
        match &self.cur_token {
            Token::Ident(_) => Some(Parser::parse_identifier),
            Token::Int(_) => Some(Parser::parse_integer_literal),
            Token::Bang => Some(Parser::parse_prefix_expression),
            Token::Minus => Some(Parser::parse_prefix_expression),
            _ => None,
        }
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        if let Token::Ident(ident) = &self.cur_token {
            Some(Expression::Identifier(ident.to_string()))
        } else {
            None
        }
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        if let Token::Int(int) = &self.cur_token {
            match int.parse() {
                Ok(value) => Some(Expression::IntegerLiteral(value)),
                Err(_) => {
                    let msg = format!("could not parse '{}' as integer", int);
                    self.errors.push(msg);
                    None
                }
            }
        } else {
            None
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        self.prefix_token(&self.cur_token).and_then(|p| {
            self.next_token();
            let expression = self.parse_expression(Precedence::Prefix);

            expression.map(|exp| Expression::Prefix(p, Box::new(exp)))
        })
    }

    fn infix_parse_fn(&self) -> Option<InfixParseFn> {
        match self.peek_token.clone() {
            Token::Plus => Some(Parser::parse_infix_expression),
            Token::Minus => Some(Parser::parse_infix_expression),
            Token::Asterisk => Some(Parser::parse_infix_expression),
            Token::Slash => Some(Parser::parse_infix_expression),
            Token::Eq => Some(Parser::parse_infix_expression),
            Token::NotEq => Some(Parser::parse_infix_expression),
            Token::Lt => Some(Parser::parse_infix_expression),
            Token::Gt => Some(Parser::parse_infix_expression),
            _ => None,
        }
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let (precedence, infix) = self.infix_token(&self.cur_token);
        infix.and_then(|i| {
            self.next_token();
            let right = self.parse_expression(precedence);

            right.map(|r| Expression::Infix(i, Box::new(left), Box::new(r)))
        })
    }

    fn prefix_token(&self, token: &Token) -> Option<Prefix> {
        match token {
            Token::Bang => Some(Prefix::Bang),
            Token::Minus => Some(Prefix::Minus),
            _ => None,
        }
    }

    fn infix_token(&self, token: &Token) -> (Precedence, Option<Infix>) {
        match token {
            Token::Eq => (Precedence::Equals, Some(Infix::Eq)),
            Token::NotEq => (Precedence::Equals, Some(Infix::NotEq)),
            Token::Lt => (Precedence::LessGreater, Some(Infix::Lt)),
            Token::Gt => (Precedence::LessGreater, Some(Infix::Gt)),
            Token::Plus => (Precedence::Sum, Some(Infix::Plus)),
            Token::Minus => (Precedence::Sum, Some(Infix::Minus)),
            Token::Slash => (Precedence::Product, Some(Infix::Slash)),
            Token::Asterisk => (Precedence::Product, Some(Infix::Asterisk)),
            _ => (Precedence::Lowest, None),
        }
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
        let msg = format!(
            "expected next token to be {}, got {} instead",
            expected, self.peek_token
        );
        self.errors.push(msg);
    }
}
