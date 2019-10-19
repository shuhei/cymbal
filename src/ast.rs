use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.statements {
            write!(f, "{{ {} }}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Statement {
    Let(String, Expression),
    Return(Option<Expression>),
    Expression(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(ident, value) => write!(f, "let {} = {};", ident, value),
            Statement::Return(None) => write!(f, "return;"),
            Statement::Return(Some(exp)) => write!(f, "return {};", exp),
            Statement::Expression(exp) => write!(f, "{};", exp),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Expression {
    Identifier(String),
    IntegerLiteral(i64),
    StringLiteral(String),
    Boolean(bool),
    Array(Vec<Expression>),
    Hash(HashLiteral),
    Index(Box<Expression>, Box<Expression>),
    Prefix(Prefix, Box<Expression>),
    Infix(Infix, Box<Expression>, Box<Expression>),
    If(Box<Expression>, BlockStatement, Option<BlockStatement>),
    FunctionLiteral(Vec<String>, BlockStatement),
    Call(Box<Expression>, Vec<Expression>),
}

// Have a separate struct to implement `Hash` trait.
#[derive(Debug, Clone)]
pub struct HashLiteral {
    pub pairs: HashMap<Expression, Expression>,
}

// `HashMap` cannot derive `Hash` trait.
impl Hash for HashLiteral {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for (k, v) in &self.pairs {
            k.hash(state);
            v.hash(state);
        }
    }
}

// Clippy complains about Hash impl with derived PartialEq
impl PartialEq for HashLiteral {
    fn eq(&self, other: &HashLiteral) -> bool {
        if self.pairs.len() != other.pairs.len() {
            return false;
        }
        self.pairs
            .iter()
            .all(|(key, value)| other.pairs.get(key).map_or(false, |v| *value == *v))
    }
}

impl Eq for HashLiteral {}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(ident) => write!(f, "{}", ident),
            Expression::IntegerLiteral(int) => write!(f, "{}", int),
            // TODO: Escape `"`
            Expression::StringLiteral(s) => write!(f, "\"{}\"", s),
            Expression::Boolean(value) => write!(f, "{}", value),
            Expression::Array(values) => write!(f, "[{}]", comma_separated(values)),
            Expression::Hash(HashLiteral { pairs }) => {
                // Print items in a stable order for testing.
                let mut items = pairs
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>();
                items.sort();
                write!(f, "{{{}}}", items.join(", "))
            }
            Expression::Index(left, index) => write!(f, "({}[{}])", left, index),
            Expression::Prefix(operator, exp) => write!(f, "({}{})", operator, exp),
            Expression::Infix(operator, left, right) => {
                write!(f, "({} {} {})", left, operator, right)
            }
            Expression::If(condition, consequence, alternative) => {
                write!(f, "if {} {}", condition, consequence)?;
                if let Some(alt) = alternative {
                    write!(f, " else {}", alt)?;
                }
                Ok(())
            }
            Expression::FunctionLiteral(parameters, body) => {
                write!(f, "fn({}) {}", parameters.join(", "), body)
            }
            Expression::Call(function, arguments) => {
                write!(f, "{}({})", function, comma_separated(arguments))
            }
        }
    }
}

fn comma_separated(exps: &[Expression]) -> String {
    exps.iter()
        .map(|a| a.to_string())
        .collect::<Vec<String>>()
        .join(", ")
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Infix {
    Eq,
    NotEq,
    Lt,
    Gt,
    Plus,
    Minus,
    Asterisk,
    Slash,
}

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Infix::Eq => write!(f, "=="),
            Infix::NotEq => write!(f, "!="),
            Infix::Lt => write!(f, "<"),
            Infix::Gt => write!(f, ">"),
            Infix::Plus => write!(f, "+"),
            Infix::Minus => write!(f, "-"),
            Infix::Asterisk => write!(f, "*"),
            Infix::Slash => write!(f, "/"),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Prefix {
    Bang,
    Minus,
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Prefix::Bang => write!(f, "!"),
            Prefix::Minus => write!(f, "-"),
        }
    }
}
