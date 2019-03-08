use crate::ast::{Expression, Infix, Prefix, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

type Result<T> = std::result::Result<T, ParserError>;

enum ParserError {
    // TODO: Have more detailed error kinds.
    Error
}

type PrefixParseFn = fn(&mut Parser) -> Result<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> Result<Expression>;

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
            // TODO: Don't ignore errors?
            if let Ok(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();
        }

        Program { statements }
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement> {
        let name;
        match self.peek_token.clone() {
            Token::Ident(ident) => {
                self.next_token();
                name = ident;
            }
            _ => {
                self.peek_error("identifier");
                return Err(ParserError::Error);
            }
        }

        if !self.expect_peek(Token::Assign) {
            return Err(ParserError::Error);
        }

        // TODO: Skipping the expressions until we encounter a semicolon
        while self.cur_token != Token::Semicolon {
            self.next_token();
        }

        // TODO: Let assignment.
        Ok(Statement::Let(name))
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        self.next_token();

        // TODO: Skipping the expressions until we encounter a semicolon
        while self.cur_token != Token::Semicolon {
            self.next_token();
        }

        // TODO: Return value.
        Ok(Statement::Return)
    }

    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        expression.map(|exp| Statement::Expression(exp))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression> {
        let prefix = self.prefix_parse_fn().ok_or(ParserError::Error)?;
        let mut left_exp = prefix(self)?;
        while self.peek_token != Token::Semicolon
            && precedence < self.infix_token(&self.peek_token).0
        {
            if let Some(infix) = self.infix_parse_fn() {
                self.next_token();
                left_exp = infix(self, left_exp)?;
            } else {
                return Ok(left_exp);
            }
        }
        Ok(left_exp)
    }

    fn prefix_parse_fn(&self) -> Option<PrefixParseFn> {
        match &self.cur_token {
            Token::Ident(_) => Some(Parser::parse_identifier),
            Token::Int(_) => Some(Parser::parse_integer_literal),
            Token::True => Some(Parser::parse_boolean),
            Token::False => Some(Parser::parse_boolean),
            Token::Bang => Some(Parser::parse_prefix_expression),
            Token::Minus => Some(Parser::parse_prefix_expression),
            Token::Lparen => Some(Parser::parse_grouped_expression),
            _ => None,
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression> {
        if let Token::Ident(ident) = &self.cur_token {
            Ok(Expression::Identifier(ident.to_string()))
        } else {
            Err(ParserError::Error)
        }
    }

    fn parse_integer_literal(&mut self) -> Result<Expression> {
        if let Token::Int(int) = &self.cur_token {
            match int.parse() {
                Ok(value) => Ok(Expression::IntegerLiteral(value)),
                Err(_) => {
                    let msg = format!("could not parse '{}' as integer", int);
                    self.errors.push(msg);
                    Err(ParserError::Error)
                }
            }
        } else {
            Err(ParserError::Error)
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression> {
        let p = self.prefix_token(&self.cur_token)?;
        self.next_token();
        let exp = self.parse_expression(Precedence::Prefix)?;

        Ok(Expression::Prefix(p, Box::new(exp)))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(Token::Rparen) {
            return Err(ParserError::Error);
        }
        Ok(exp)
    }

    fn parse_boolean(&mut self) -> Result<Expression> {
        match &self.cur_token {
            Token::True => Ok(Expression::Boolean(true)),
            Token::False => Ok(Expression::Boolean(false)),
            _ => Err(ParserError::Error)
        }
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

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression> {
        let (precedence, infix) = self.infix_token(&self.cur_token);
        let i = infix.ok_or(ParserError::Error)?;
        self.next_token();
        let right = self.parse_expression(precedence)?;

        Ok(Expression::Infix(i, Box::new(left), Box::new(right)))
    }

    fn prefix_token(&self, token: &Token) -> Result<Prefix> {
        match token {
            Token::Bang => Ok(Prefix::Bang),
            Token::Minus => Ok(Prefix::Minus),
            _ => Err(ParserError::Error),
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
