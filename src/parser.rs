use crate::ast::{BlockStatement, Expression, HashLiteral, Infix, Prefix, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use std::collections::HashMap;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
    Index,       // array[index]
}

type Result<T> = std::result::Result<T, ParserError>;

#[derive(Debug)]
pub enum ParserError {
    ExpectedPrefixToken(Token),
    ExpectedInfixToken(Token),
    ExpectedIdentifierToken(Token),
    ExpectedBooleanToken(Token),
    ExpectedIntegerToken(Token),
    ExpectedStringToken(Token),
    ExpectedLparen(Token),
    ExpectedRparen(Token),
    ExpectedLbrace(Token),
    ExpectedRbrace(Token),
    ExpectedRbracket(Token),
    ExpectedAssign(Token),
    ExpectedSemicolon(Token),
    ExpectedComma(Token),
    ExpectedColon(Token),
    ParseInt(String),
}

type PrefixParseFn = fn(&mut Parser) -> Result<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> Result<Expression>;

pub struct Parser {
    lexer: Lexer,
    errors: Vec<ParserError>,

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

    pub fn input(&self) -> &str {
        self.lexer.input()
    }

    pub fn errors(&self) -> &[ParserError] {
        &self.errors
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
            match self.parse_statement() {
                Ok(stmt) => {
                    statements.push(stmt);
                }
                Err(err) => {
                    self.errors.push(err);
                }
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
        // cur_token: let
        let name;
        if let Token::Ident(ident) = self.peek_token.clone() {
            self.next_token();
            name = ident;
        } else {
            return Err(ParserError::ExpectedIdentifierToken(
                self.peek_token.clone(),
            ));
        }

        self.expect_peek(Token::Assign, ParserError::ExpectedAssign)?;
        // cur_token: =
        self.next_token();
        // cur_token: the first token of the value expression

        let value = self.parse_expression(Precedence::Lowest)?;
        // cur_token: the last token of the value expression

        if self.peek_token == Token::Semicolon {
            self.next_token();
            // cur_token: ;
        }

        Ok(Statement::Let(name, value))
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        // cur_token: return
        self.next_token();
        // cur_token: ; or the first token of the expression

        if self.cur_token == Token::Semicolon {
            return Ok(Statement::Return(None));
        }

        let expression = self.parse_expression(Precedence::Lowest)?;
        // cur_token: the last token of the expression

        if self.peek_token == Token::Semicolon {
            self.next_token();
            // cur_token: ;
        }

        Ok(Statement::Return(Some(expression)))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        expression.map(|exp| Statement::Expression(exp))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement> {
        let mut statements = vec![];

        self.next_token();
        while self.cur_token != Token::Rbrace && self.cur_token != Token::Eof {
            statements.push(self.parse_statement()?);
            self.next_token();
        }

        Ok(BlockStatement { statements })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression> {
        // cur_token: the first token of the expression
        let prefix = self
            .prefix_parse_fn()
            .ok_or_else(|| ParserError::ExpectedPrefixToken(self.cur_token.clone()))?;
        let mut left_exp = prefix(self)?;
        // cur_token: the last token of the left expression

        while self.peek_token != Token::Semicolon
            && precedence < self.infix_token(&self.peek_token).0
        {
            if let Some(infix) = self.infix_parse_fn() {
                self.next_token();
                // cur_token: the infix token
                left_exp = infix(self, left_exp)?;
            // cur_token: the last token of the right expression
            } else {
                // No infix operator
                return Ok(left_exp);
            }
        }
        Ok(left_exp)
    }

    fn prefix_parse_fn(&self) -> Option<PrefixParseFn> {
        match &self.cur_token {
            Token::Ident(_) => Some(Parser::parse_identifier),
            Token::Int(_) => Some(Parser::parse_integer_literal),
            Token::String(_) => Some(Parser::parse_string_literal),
            Token::True => Some(Parser::parse_boolean),
            Token::False => Some(Parser::parse_boolean),
            Token::Bang => Some(Parser::parse_prefix_expression),
            Token::Minus => Some(Parser::parse_prefix_expression),
            Token::Lparen => Some(Parser::parse_grouped_expression),
            Token::Lbracket => Some(Parser::parse_array_literal),
            Token::Lbrace => Some(Parser::parse_hash_literal),
            Token::If => Some(Parser::parse_if_expression),
            Token::Function => Some(Parser::parse_function_literal),
            _ => None,
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression> {
        self.parse_identifier_string().map(Expression::Identifier)
    }

    fn parse_identifier_string(&self) -> Result<String> {
        if let Token::Ident(ident) = &self.cur_token {
            Ok(ident.to_string())
        } else {
            Err(ParserError::ExpectedIdentifierToken(self.cur_token.clone()))
        }
    }

    fn parse_integer_literal(&mut self) -> Result<Expression> {
        if let Token::Int(int) = &self.cur_token {
            match int.parse() {
                Ok(value) => Ok(Expression::IntegerLiteral(value)),
                Err(_) => Err(ParserError::ParseInt(int.to_string())),
            }
        } else {
            Err(ParserError::ExpectedIntegerToken(self.cur_token.clone()))
        }
    }

    fn parse_string_literal(&mut self) -> Result<Expression> {
        if let Token::String(s) = &self.cur_token {
            Ok(Expression::StringLiteral(s.to_string()))
        } else {
            Err(ParserError::ExpectedStringToken(self.cur_token.clone()))
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression> {
        // cur_token: the prefix token like `!`
        let p = self.prefix_token(&self.cur_token)?;
        self.next_token();
        // cur_token: the first token of the expression
        let exp = self.parse_expression(Precedence::Prefix)?;
        // cur_token: the last token of the expression

        Ok(Expression::Prefix(p, Box::new(exp)))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression> {
        // cur_token: (
        self.next_token();
        // cur_token: the first token of the expression

        let exp = self.parse_expression(Precedence::Lowest)?;
        // cur_token: the last token of the expression

        self.expect_peek(Token::Rparen, ParserError::ExpectedRparen)?;
        // cur_token: )

        Ok(exp)
    }

    fn parse_array_literal(&mut self) -> Result<Expression> {
        // cur_token: [
        let exps = self.parse_expressions(Token::Rbracket, ParserError::ExpectedRbracket)?;
        // cur_token: ]

        Ok(Expression::Array(exps))
    }

    fn parse_hash_literal(&mut self) -> Result<Expression> {
        // cur_token: {
        let mut pairs = HashMap::new();

        while self.peek_token != Token::Rbrace {
            // cur_token: { or ,
            self.next_token();
            // cur_token: the first token of the key expression
            let key = self.parse_expression(Precedence::Lowest)?;
            // cur_token: the last token of the key expression

            self.expect_peek(Token::Colon, ParserError::ExpectedColon)?;
            // cur_token: :
            self.next_token();
            // cur_token: the first token of the value expression

            let value = self.parse_expression(Precedence::Lowest)?;
            // cur_token: the last token of the value expression

            pairs.insert(key, value);

            if self.peek_token != Token::Rbrace {
                self.expect_peek(Token::Comma, ParserError::ExpectedComma)?;
                // cur_token: ,
            }
        }

        self.expect_peek(Token::Rbrace, ParserError::ExpectedRbrace)?;
        // cur_token: }

        Ok(Expression::Hash(HashLiteral { pairs }))
    }

    fn parse_if_expression(&mut self) -> Result<Expression> {
        // cur_token: if
        self.expect_peek(Token::Lparen, ParserError::ExpectedLparen)?;
        // cur_token: (

        self.next_token();
        // cur_token: condition
        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(Token::Rparen, ParserError::ExpectedRparen)?;
        // cur_token: (
        self.expect_peek(Token::Lbrace, ParserError::ExpectedLbrace)?;
        // cur_token: {

        let consequence = self.parse_block_statement()?;
        // cur_token: }

        let mut alternative = None;
        if self.peek_token == Token::Else {
            self.next_token();
            // cur_token: else
            self.expect_peek(Token::Lbrace, ParserError::ExpectedLbrace)?;
            // cur_token: {
            alternative = Some(self.parse_block_statement()?);
            // cur_token: }
        }

        Ok(Expression::If(
            Box::new(condition),
            consequence,
            alternative,
        ))
    }

    fn parse_function_literal(&mut self) -> Result<Expression> {
        // cur_token: fn
        self.expect_peek(Token::Lparen, ParserError::ExpectedLparen)?;
        // cur_token: (

        let parameters = self.parse_function_parameters()?;
        // cur_token: )

        self.expect_peek(Token::Lbrace, ParserError::ExpectedLbrace)?;
        // cur_token: {

        let body = self.parse_block_statement()?;
        // cur_token: }

        Ok(Expression::FunctionLiteral(parameters, body))
    }

    fn parse_boolean(&mut self) -> Result<Expression> {
        match &self.cur_token {
            Token::True => Ok(Expression::Boolean(true)),
            Token::False => Ok(Expression::Boolean(false)),
            _ => Err(ParserError::ExpectedBooleanToken(self.cur_token.clone())),
        }
    }

    fn infix_parse_fn(&self) -> Option<InfixParseFn> {
        match &self.peek_token {
            Token::Plus => Some(Parser::parse_infix_expression),
            Token::Minus => Some(Parser::parse_infix_expression),
            Token::Asterisk => Some(Parser::parse_infix_expression),
            Token::Slash => Some(Parser::parse_infix_expression),
            Token::Eq => Some(Parser::parse_infix_expression),
            Token::NotEq => Some(Parser::parse_infix_expression),
            Token::Lt => Some(Parser::parse_infix_expression),
            Token::Gt => Some(Parser::parse_infix_expression),
            Token::Lparen => Some(Parser::parse_call_expression),
            Token::Lbracket => Some(Parser::parse_index_expression),
            _ => None,
        }
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression> {
        let (precedence, infix) = self.infix_token(&self.cur_token);
        let i = infix.ok_or_else(|| ParserError::ExpectedInfixToken(self.cur_token.clone()))?;
        self.next_token();
        let right = self.parse_expression(precedence)?;

        Ok(Expression::Infix(i, Box::new(left), Box::new(right)))
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression> {
        // cur_token: (
        let arguments = self.parse_expressions(Token::Rparen, ParserError::ExpectedRparen)?;
        // cur_token: )
        Ok(Expression::Call(Box::new(function), arguments))
    }

    fn parse_index_expression(&mut self, array: Expression) -> Result<Expression> {
        // cur_token: [
        self.next_token();
        // cur_token: the first token of the index expression
        let index = self.parse_expression(Precedence::Lowest)?;
        // cur_token: the last token of the index expression

        self.expect_peek(Token::Rbracket, ParserError::ExpectedRbracket)?;
        // cur_token: ]

        Ok(Expression::Index(Box::new(array), Box::new(index)))
    }

    fn parse_expressions(
        &mut self,
        closing_token: Token,
        expected: fn(Token) -> ParserError,
    ) -> Result<Vec<Expression>> {
        // cur_token: opening_token
        let mut exps = vec![];

        // No parameters
        if self.peek_token == closing_token {
            self.next_token();
            // cur_token: closing_token
            return Ok(exps);
        }

        self.next_token();
        // cur_token: the first token of the first expression

        exps.push(self.parse_expression(Precedence::Lowest)?);
        // cur_token: the last token of the first expression

        while self.peek_token == Token::Comma {
            self.next_token();
            // cur_token: ,
            self.next_token();
            // cur_token: the first token of the current expression
            exps.push(self.parse_expression(Precedence::Lowest)?);
            // cur_token: the last token of the current expression
        }

        self.expect_peek(closing_token, expected)?;
        // cur_token: closing_token

        Ok(exps)
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<String>> {
        // cur_token: (
        let mut identifiers = vec![];

        // No parameters
        if self.peek_token == Token::Rparen {
            self.next_token();
            // cur_token: )
            return Ok(identifiers);
        }

        self.next_token();
        // cur_token: the first parameter

        identifiers.push(self.parse_identifier_string()?);

        while self.peek_token == Token::Comma {
            // cur_token: the previous parameter
            self.next_token();
            // cur_token: comma
            self.next_token();
            // cur_token: the current parameter

            identifiers.push(self.parse_identifier_string()?);
        }

        self.expect_peek(Token::Rparen, ParserError::ExpectedRparen)?;
        // cur_token: )

        Ok(identifiers)
    }

    fn prefix_token(&self, token: &Token) -> Result<Prefix> {
        match token {
            Token::Bang => Ok(Prefix::Bang),
            Token::Minus => Ok(Prefix::Minus),
            token @ _ => Err(ParserError::ExpectedPrefixToken(token.clone())),
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
            Token::Lparen => (Precedence::Call, None),
            Token::Lbracket => (Precedence::Index, None),
            _ => (Precedence::Lowest, None),
        }
    }

    // Assert that `peek_token` is the expected one. If so, proceed one token. Otherwise,
    // return an error.
    //
    // For example:
    // When `cur_token` is `if`, the parser expects `peek_token` to be `(`.
    // If the expectation is matched, it proceeds to the next token (`cur_token` is `(` and
    // `peek_token` is the first token of the condition.
    fn expect_peek(&mut self, token: Token, expected: fn(Token) -> ParserError) -> Result<()> {
        if self.peek_token != token {
            return Err(expected(self.peek_token.clone()));
        }
        self.next_token();
        Ok(())
    }
}
