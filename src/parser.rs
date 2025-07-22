use crate::expr::{Assign, Binary, Expr, Grouping, Literal, Unary, Variable};
use crate::stmt::{Expression, Print, Stmt, Var};
use crate::token::{Token, TokenType};
use crate::value::Value;
use std::error::Error;
use std::fmt::Display;
use std::iter::Peekable;

#[derive(Clone, Debug)]
pub struct ParseError {
    token: Token,
    message: String,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[line {}] ParseError at '{}': {}",
            self.token.line, self.token.lexeme, self.message
        )
    }
}

impl ParseError {
    fn new(token: Token, message: &str) -> Self {
        ParseError {
            token,
            message: message.to_string(),
        }
    }

    fn unexpected_eof() -> Self {
        ParseError {
            token: Token::new(
                TokenType::EOF,
                String::from(""),
                crate::token::LiteralValue::Null,
                0,
            ),
            message: "Unexpected EOF.".to_string(),
        }
    }
}

impl Error for ParseError {}

pub type Result<T> = std::result::Result<T, ParseError>;

pub fn parse(tokens: Vec<Token>) -> Vec<Box<dyn Stmt>> {
    let mut tokens = tokens.into_iter().peekable();
    let mut statements = Vec::<Box<dyn Stmt>>::new();

    while let Some(token) = tokens.peek() {
        if token.token_type == TokenType::EOF {
            break;
        }

        match declaration(&mut tokens) {
            Ok(stmt) => statements.push(stmt),
            Err(e) => eprintln!("{}", e.clone()),
        }
    }

    statements
}

pub fn consume(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    token_type: TokenType,
    message: &str,
) -> Result<Token> {
    let token = tokens.next().unwrap();
    if TokenType::from(&token) != token_type {
        return Err(ParseError::new(token, message));
    }

    Ok(token)
}

fn primary(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Box<dyn Expr>> {
    let t = tokens.next().ok_or(ParseError::unexpected_eof())?;
    let result: Result<Box<dyn Expr>> = match t.token_type {
        TokenType::True => Ok(Box::new(Literal::new(Value::True))),
        TokenType::False => Ok(Box::new(Literal::new(Value::False))),
        TokenType::Nil => Ok(Box::new(Literal::new(Value::Nil))),
        TokenType::String => match String::try_from(t.literal.clone()) {
            Ok(s) => Ok(Box::new(Literal::new(Value::String(s)))),
            Err(e) => Err(ParseError::new(t, &e)),
        },
        TokenType::Number => match f64::try_from(t.literal.clone()) {
            Ok(x) => Ok(Box::new(Literal::new(Value::Number(x)))),
            Err(e) => Err(ParseError::new(t, &e)),
        },
        TokenType::LeftParen => {
            let expr = expression(tokens)?;
            match tokens.next() {
                Some(t) if t.token_type == TokenType::RightParen => {
                    Ok(Box::new(Grouping::new(expr)))
                }
                Some(t) => Err(ParseError::new(t, "Expected closing parenthesis.")),
                None => Err(ParseError::unexpected_eof()),
            }
        }
        TokenType::Identifier => Ok(Box::new(Variable::new(t))),
        _ => Err(ParseError::new(t, "Unexpected token in expression")),
    };

    if let Err(e) = &result {
        eprintln!("{}", e.clone());
    }

    result
}

fn unary(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Box<dyn Expr>> {
    if tokens
        .peek()
        .is_some_and(|t| t.token_type == TokenType::Bang || t.token_type == TokenType::Minus)
    {
        let operator = tokens.next().unwrap();
        let expr = unary(tokens)?;
        return Ok(Box::new(Unary::new(operator, expr)));
    }

    primary(tokens)
}

fn factor(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Box<dyn Expr>> {
    let mut left = unary(tokens)?;

    while tokens
        .peek()
        .is_some_and(|t| t.token_type == TokenType::Star || t.token_type == TokenType::Slash)
    {
        let operator = tokens.next().unwrap();
        let right = unary(tokens)?;
        left = Box::new(Binary::new(left, operator, right))
    }

    Ok(left)
}

fn term(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Box<dyn Expr>> {
    let mut left = factor(tokens)?;

    while tokens
        .peek()
        .is_some_and(|t| t.token_type == TokenType::Plus || t.token_type == TokenType::Minus)
    {
        let operator = tokens.next().unwrap();
        let right = factor(tokens)?;
        left = Box::new(Binary::new(left, operator, right))
    }

    Ok(left)
}

fn comparison(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Box<dyn Expr>> {
    let mut left = term(tokens)?;

    while tokens.peek().is_some_and(|t| {
        t.token_type == TokenType::Greater
            || t.token_type == TokenType::GreaterEqual
            || t.token_type == TokenType::Less
            || t.token_type == TokenType::LessEqual
    }) {
        let operator = tokens.next().unwrap();
        let right = term(tokens)?;
        left = Box::new(Binary::new(left, operator, right))
    }

    Ok(left)
}

fn equality(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Box<dyn Expr>> {
    let mut left = comparison(tokens)?;

    while tokens.peek().is_some_and(|t| {
        t.token_type == TokenType::BangEqual || t.token_type == TokenType::EqualEqual
    }) {
        let operator = tokens.next().unwrap();
        let right = comparison(tokens)?;
        left = Box::new(Binary::new(left, operator, right));
    }

    Ok(left)
}

fn assignment(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Box<dyn Expr>> {
    let expr = equality(tokens)?;

    if tokens
        .peek()
        .is_some_and(|t| t.token_type == TokenType::Equal)
    {
        let equals = tokens.next().unwrap();
        let value = assignment(tokens)?;

        if let Some(name) = expr.get_name() {
            return Ok(Box::new(Assign::new(name, value)));
        }

        return Err(ParseError::new(equals, "Invalid assignment target."));
    }

    Ok(expr)
}

fn expression(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Box<dyn Expr>> {
    assignment(tokens)
}

fn statement(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Box<dyn Stmt>> {
    let t = tokens.peek().ok_or(ParseError::unexpected_eof())?;
    match t.token_type {
        TokenType::Print => {
            // Consume PRINT
            tokens.next();
            print_statement(tokens)
        }
        _ => expression_statement(tokens),
    }
}

fn expression_statement(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
) -> Result<Box<dyn Stmt>> {
    let expr = expression(tokens)?;
    consume(tokens, TokenType::Semicolon, "Expect ';' after expression.")?;

    Ok(Box::new(Expression::new(expr)))
}

fn print_statement(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Box<dyn Stmt>> {
    let expr = expression(tokens)?;
    consume(tokens, TokenType::Semicolon, "Expect ';' after expression.")?;

    Ok(Box::new(Print::new(expr)))
}

fn declaration(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Box<dyn Stmt>> {
    if tokens
        .peek()
        .is_some_and(|t| t.token_type == TokenType::Var)
    {
        match var_declaration(tokens) {
            Ok(v) => Ok(v),
            Err(e) => {
                println!("{}", &e);
                synchronise(tokens);
                Err(e)
            }
        }
    } else {
        statement(tokens)
    }
}

fn var_declaration(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Box<dyn Stmt>> {
    consume(tokens, TokenType::Var, "Expect VAR keyword.")?;

    let name = consume(tokens, TokenType::Identifier, "Expect variable name")?;
    let initialiser = if tokens
        .peek()
        .is_some_and(|t| t.token_type == TokenType::Equal)
    {
        tokens.next();
        Some(expression(tokens)?)
    } else {
        None
    };

    consume(
        tokens,
        TokenType::Semicolon,
        "Expect ';' after variable declaration.",
    )?;
    Ok(Box::new(Var::new(name, initialiser)))
}

fn synchronise(tokens: &mut Peekable<impl Iterator<Item = Token>>) {
    tokens.next();
    while let Some(t) = tokens.peek() {
        if t.token_type == TokenType::Semicolon {
            tokens.next();
            return;
        }

        if matches!(
            t.token_type,
            TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return
        ) {
            return;
        }
    }
}
