use std::fmt::{Debug, Display};

#[derive(Clone, PartialEq)]
pub enum Token {
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
    String(String, String),
    Number(String, f64),
    Identifier(String),

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

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let token_string = match &self {
            Token::LeftParen => "LEFT_PAREN ( null",
            Token::RightParen => "RIGHT_PAREN ) null",
            Token::EOF => "EOF  null",
            Token::LeftBrace => "LEFT_BRACE { null",
            Token::RightBrace => "RIGHT_BRACE } null",
            Token::Comma => "COMMA , null",
            Token::Dot => "DOT . null",
            Token::Minus => "MINUS - null",
            Token::Plus => "PLUS + null",
            Token::Semicolon => "SEMICOLON ; null",
            Token::Star => "STAR * null",
            Token::Slash => "SLASH / null",
            Token::Bang => "BANG ! null",
            Token::BangEqual => "BANG_EQUAL != null",
            Token::Equal => "EQUAL = null",
            Token::EqualEqual => "EQUAL_EQUAL == null",
            Token::Greater => "GREATER > null",
            Token::GreaterEqual => "GREATER_EQUAL >= null",
            Token::Less => "LESS < null",
            Token::LessEqual => "LESS_EQUAL <= null",
            Token::String(lexeme, literal) => &format!("STRING {lexeme} {literal}"),
            Token::Number(lexeme, literal) => {
                if literal.to_string().contains('.') {
                    &format!("NUMBER {lexeme} {literal}")
                } else {
                    &format!("NUMBER {lexeme} {literal}.0")
                }
            }
            Token::Identifier(lexeme) => &format!("IDENTIFIER {lexeme} null"),
            Token::And => "AND and null",
            Token::Class => "CLASS class null",
            Token::Else => "ELSE else null",
            Token::False => "FALSE false null",
            Token::Fun => "FUN fun null",
            Token::For => "FOR for null",
            Token::If => "IF if null",
            Token::Nil => "NIL nil null",
            Token::Or => "OR or null",
            Token::Print => "PRINT print null",
            Token::Return => "RETURN return null",
            Token::Super => "SUPER super null",
            Token::This => "THIS this null",
            Token::True => "TRUE true null",
            Token::Var => "VAR var null",
            Token::While => "WHILE while null",
        };

        write!(f, "{token_string}")
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let token_string = match &self {
            Token::LeftParen => "(",
            Token::RightParen => ")",
            Token::EOF => "EOF",
            Token::LeftBrace => "{",
            Token::RightBrace => "}",
            Token::Comma => ",",
            Token::Dot => ".",
            Token::Minus => "-",
            Token::Plus => "+",
            Token::Semicolon => ";",
            Token::Star => "*",
            Token::Slash => "/",
            Token::Bang => "!",
            Token::BangEqual => "!=",
            Token::Equal => "=",
            Token::EqualEqual => "==",
            Token::Greater => ">",
            Token::GreaterEqual => ">=",
            Token::Less => "<",
            Token::LessEqual => "<=",
            Token::String(lexeme, _literal) => lexeme,
            Token::Number(_lexeme, literal) => {
                if literal.to_string().contains('.') {
                    &format!("{literal}")
                } else {
                    &format!("{literal}.0")
                }
            }
            Token::Identifier(lexeme) => lexeme,
            Token::And => "and",
            Token::Class => "class",
            Token::Else => "else",
            Token::False => "false",
            Token::Fun => "fun",
            Token::For => "for",
            Token::If => "if",
            Token::Nil => "nil",
            Token::Or => "or",
            Token::Print => "print",
            Token::Return => "return",
            Token::Super => "super",
            Token::This => "this",
            Token::True => "true",
            Token::Var => "var",
            Token::While => "while",
        };

        write!(f, "{token_string}")
    }
}

#[derive(Debug, PartialEq, Eq)]
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
        match *value {
            Token::LeftParen => TokenType::LeftParen,
            Token::RightParen => TokenType::RightParen,
            Token::LeftBrace => TokenType::LeftBrace,
            Token::RightBrace => TokenType::RightBrace,
            Token::Comma => TokenType::Comma,
            Token::Dot => TokenType::Dot,
            Token::Minus => TokenType::Minus,
            Token::Plus => TokenType::Plus,
            Token::Semicolon => TokenType::Semicolon,
            Token::Slash => TokenType::Slash,
            Token::Star => TokenType::Star,
            Token::Bang => TokenType::Bang,
            Token::BangEqual => TokenType::BangEqual,
            Token::Equal => TokenType::Equal,
            Token::EqualEqual => TokenType::EqualEqual,
            Token::Greater => TokenType::Greater,
            Token::GreaterEqual => TokenType::GreaterEqual,
            Token::Less => TokenType::Less,
            Token::LessEqual => TokenType::LessEqual,
            Token::String(_, _) => TokenType::String,
            Token::Number(_, _) => TokenType::Number,
            Token::Identifier(_) => TokenType::Identifier,
            Token::And => TokenType::And,
            Token::Class => TokenType::Class,
            Token::Else => TokenType::Else,
            Token::False => TokenType::False,
            Token::Fun => TokenType::Fun,
            Token::For => TokenType::For,
            Token::If => TokenType::If,
            Token::Nil => TokenType::Nil,
            Token::Or => TokenType::Or,
            Token::Print => TokenType::Print,
            Token::Return => TokenType::Return,
            Token::Super => TokenType::Super,
            Token::This => TokenType::This,
            Token::True => TokenType::True,
            Token::Var => TokenType::Var,
            Token::While => TokenType::While,
            Token::EOF => TokenType::EOF,
        }
    }
}
