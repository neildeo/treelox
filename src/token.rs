use std::fmt::{Debug, Display};

use crate::value::TypeError;

#[derive(Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: LiteralValue,
    pub line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, literal: LiteralValue, line: usize) -> Self {
        Token {
            token_type,
            lexeme,
            literal,
            line,
        }
    }
}

impl TryFrom<Token> for String {
    type Error = TypeError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match Self::try_from(value.literal.clone()) {
            Ok(s) => Ok(s),
            Err(e) => Err(TypeError::new(value, &e)),
        }
    }
}

impl TryFrom<Token> for f64 {
    type Error = TypeError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match Self::try_from(value.literal.clone()) {
            Ok(x) => Ok(x),
            Err(e) => Err(TypeError::new(value, &e)),
        }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let token_string = match &self.token_type {
            TokenType::LeftParen => "LEFT_PAREN ( null",
            TokenType::RightParen => "RIGHT_PAREN ) null",
            TokenType::EOF => "EOF  null",
            TokenType::LeftBrace => "LEFT_BRACE { null",
            TokenType::RightBrace => "RIGHT_BRACE } null",
            TokenType::Comma => "COMMA , null",
            TokenType::Dot => "DOT . null",
            TokenType::Minus => "MINUS - null",
            TokenType::Plus => "PLUS + null",
            TokenType::Semicolon => "SEMICOLON ; null",
            TokenType::Star => "STAR * null",
            TokenType::Slash => "SLASH / null",
            TokenType::Bang => "BANG ! null",
            TokenType::BangEqual => "BANG_EQUAL != null",
            TokenType::Equal => "EQUAL = null",
            TokenType::EqualEqual => "EQUAL_EQUAL == null",
            TokenType::Greater => "GREATER > null",
            TokenType::GreaterEqual => "GREATER_EQUAL >= null",
            TokenType::Less => "LESS < null",
            TokenType::LessEqual => "LESS_EQUAL <= null",
            TokenType::String => &format!("STRING {} {}", self.lexeme, self.literal),
            TokenType::Number => &format!("NUMBER {} {}", self.lexeme, self.literal),
            TokenType::Identifier => &format!("IDENTIFIER {} null", self.lexeme),
            TokenType::And => "AND and null",
            TokenType::Class => "CLASS class null",
            TokenType::Else => "ELSE else null",
            TokenType::False => "FALSE false null",
            TokenType::Fun => "FUN fun null",
            TokenType::For => "FOR for null",
            TokenType::If => "IF if null",
            TokenType::Nil => "NIL nil null",
            TokenType::Or => "OR or null",
            TokenType::Print => "PRINT print null",
            TokenType::Return => "RETURN return null",
            TokenType::Super => "SUPER super null",
            TokenType::This => "THIS this null",
            TokenType::True => "TRUE true null",
            TokenType::Var => "VAR var null",
            TokenType::While => "WHILE while null",
        };

        write!(f, "{token_string}")
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let token_string = match &self.token_type {
            TokenType::LeftParen => "(",
            TokenType::RightParen => ")",
            TokenType::EOF => "EOF",
            TokenType::LeftBrace => "{",
            TokenType::RightBrace => "}",
            TokenType::Comma => ",",
            TokenType::Dot => ".",
            TokenType::Minus => "-",
            TokenType::Plus => "+",
            TokenType::Semicolon => ";",
            TokenType::Star => "*",
            TokenType::Slash => "/",
            TokenType::Bang => "!",
            TokenType::BangEqual => "!=",
            TokenType::Equal => "=",
            TokenType::EqualEqual => "==",
            TokenType::Greater => ">",
            TokenType::GreaterEqual => ">=",
            TokenType::Less => "<",
            TokenType::LessEqual => "<=",
            TokenType::String => &format!("{}", self.literal),
            TokenType::Number => &format!("{}", self.literal),
            TokenType::Identifier => &self.lexeme,
            TokenType::And => "and",
            TokenType::Class => "class",
            TokenType::Else => "else",
            TokenType::False => "false",
            TokenType::Fun => "fun",
            TokenType::For => "for",
            TokenType::If => "if",
            TokenType::Nil => "nil",
            TokenType::Or => "or",
            TokenType::Print => "print",
            TokenType::Return => "return",
            TokenType::Super => "super",
            TokenType::This => "this",
            TokenType::True => "true",
            TokenType::Var => "var",
            TokenType::While => "while",
        };

        write!(f, "{token_string}")
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TokenType {
    // Single character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    String,
    Number,
    Identifier,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // End of file
    #[allow(clippy::upper_case_acronyms)]
    EOF,
}

impl From<&Token> for TokenType {
    fn from(value: &Token) -> Self {
        value.token_type.clone()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralValue {
    String(String),
    Number(f64),
    Null,
}

impl Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            LiteralValue::String(s) => s,
            LiteralValue::Number(x) => &format!("{x}"),
            LiteralValue::Null => "NULL",
        };

        write!(f, "{}", str)
    }
}

impl From<String> for LiteralValue {
    fn from(value: String) -> Self {
        LiteralValue::String(value)
    }
}

impl TryFrom<LiteralValue> for String {
    type Error = String;

    fn try_from(value: LiteralValue) -> Result<Self, Self::Error> {
        match value {
            LiteralValue::String(s) => Ok(s),
            v => Err(format!("Invalid literal for string: {v}")),
        }
    }
}

impl From<f64> for LiteralValue {
    fn from(value: f64) -> Self {
        LiteralValue::Number(value)
    }
}

impl TryFrom<LiteralValue> for f64 {
    type Error = String;

    fn try_from(value: LiteralValue) -> Result<Self, Self::Error> {
        match value {
            LiteralValue::Number(x) => Ok(x),
            v => Err(format!("Invalid literal for number: {v}")),
        }
    }
}
