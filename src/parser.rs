use crate::expr::{Binary, Expr, Grouping, Literal, Unary};
use crate::stmt::{Expression, Print, Stmt};
use crate::token::Token;
use crate::value::Value;
use std::error::Error;
use std::fmt::Display;
use std::iter::Peekable;

#[derive(Debug)]
pub struct ParseError {
    message: String,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error: {}", self.message)
    }
}

impl ParseError {
    fn new(message: &str) -> Self {
        ParseError {
            message: message.to_string(),
        }
    }
}

impl Error for ParseError {}

pub type Result<T> = std::result::Result<T, ParseError>;

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Box<dyn Stmt>>> {
    let mut tokens = tokens.into_iter().peekable();
    let mut statements = Vec::<Box<dyn Stmt>>::new();

    while let Some(token) = tokens.peek() {
        if *token == Token::EOF {
            break;
        }

        statements.push(statement(&mut tokens)?);
    }

    Ok(statements)
}

fn primary(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Box<dyn Expr>> {
    let t = tokens
        .next()
        .ok_or(ParseError::new("Value token required but none found"))?;
    match t {
        Token::True => Ok(Box::new(Literal::new(Value::True))),
        Token::False => Ok(Box::new(Literal::new(Value::False))),
        Token::Nil => Ok(Box::new(Literal::new(Value::Nil))),
        Token::String(_, lit) => Ok(Box::new(Literal::new(Value::String(lit)))),
        Token::Number(_, x) => Ok(Box::new(Literal::new(Value::Number(x)))),
        Token::LeftParen => {
            let expr = expression(tokens)?;
            match tokens.next() {
                Some(Token::RightParen) => Ok(Box::new(Grouping::new(expr))),
                _ => Err(ParseError::new("Closing parenthesis missing")),
            }
        }
        _ => Err(ParseError::new(
            "Reached EOF before expression was completed",
        )),
    }
}

fn unary(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Box<dyn Expr>> {
    if matches!(tokens.peek(), Some(Token::Bang) | Some(Token::Minus)) {
        let operator = tokens.next().unwrap();
        let expr = unary(tokens)?;
        return Ok(Box::new(Unary::new(operator, expr)));
    }

    primary(tokens)
}

fn factor(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Box<dyn Expr>> {
    let mut left = unary(tokens)?;

    while matches!(tokens.peek(), Some(Token::Star) | Some(Token::Slash)) {
        let operator = tokens.next().unwrap();
        let right = unary(tokens)?;
        left = Box::new(Binary::new(left, operator, right))
    }

    Ok(left)
}

fn term(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Box<dyn Expr>> {
    let mut left = factor(tokens)?;

    while matches!(tokens.peek(), Some(Token::Plus) | Some(Token::Minus)) {
        let operator = tokens.next().unwrap();
        let right = factor(tokens)?;
        left = Box::new(Binary::new(left, operator, right))
    }

    Ok(left)
}

fn comparison(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Box<dyn Expr>> {
    let mut left = term(tokens)?;

    while matches!(
        tokens.peek(),
        Some(Token::Greater)
            | Some(Token::GreaterEqual)
            | Some(Token::Less)
            | Some(Token::LessEqual)
    ) {
        let operator = tokens.next().unwrap();
        let right = term(tokens)?;
        left = Box::new(Binary::new(left, operator, right))
    }

    Ok(left)
}

fn equality(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Box<dyn Expr>> {
    let mut left = comparison(tokens)?;

    while matches!(
        tokens.peek(),
        Some(Token::BangEqual) | Some(Token::EqualEqual)
    ) {
        let operator = tokens.next().unwrap();
        let right = comparison(tokens)?;
        left = Box::new(Binary::new(left, operator, right));
    }

    Ok(left)
}

fn expression(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Box<dyn Expr>> {
    equality(tokens)
}

fn statement(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Box<dyn Stmt>> {
    let t = tokens
        .peek()
        .ok_or(ParseError::new("Non-EOF token required but none found"))?;
    match *t {
        Token::Print => {
            // Consume PRINT
            tokens.next();
            print_statement(tokens)
        }
        _ => expression_statement(tokens),
    }
}

fn consume_semicolon(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<()> {
    let maybe_semicolon = tokens
        .next()
        .ok_or(ParseError::new("Expect ';' after value."))?;
    if maybe_semicolon != Token::Semicolon {
        return Err(ParseError::new("Expect ';' after value."));
    }

    Ok(())
}

fn expression_statement(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
) -> Result<Box<dyn Stmt>> {
    let expr = expression(tokens)?;
    consume_semicolon(tokens)?;

    Ok(Box::new(Expression::new(expr)))
}

fn print_statement(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Box<dyn Stmt>> {
    let expr = expression(tokens)?;
    consume_semicolon(tokens)?;

    Ok(Box::new(Print::new(expr)))
}
