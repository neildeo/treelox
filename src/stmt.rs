use crate::expr::Expr;
use crate::token::{Token, TokenType};
use core::panic;

pub enum Stmt {
    Expression(Expression),
    Print(Print),
    Var(Var),
    Block(Block),
    If(If),
}

pub struct Expression {
    pub expression: Expr,
}

impl Expression {
    pub fn new(expression: Expr) -> Self {
        Expression { expression }
    }
}
pub struct Print {
    pub expression: Expr,
}

impl Print {
    pub fn new(expression: Expr) -> Self {
        Print { expression }
    }
}

pub struct Var {
    pub name: Token,
    pub initialiser: Option<Expr>,
}

impl Var {
    pub fn new(name: Token, initialiser: Option<Expr>) -> Self {
        match name.token_type {
            TokenType::Identifier => Var { name, initialiser },
            _ => panic!("Variable name token is not an identifier."),
        }
    }
}

pub struct Block {
    pub statements: Vec<Stmt>,
}

impl Block {
    pub fn new(statements: Vec<Stmt>) -> Self {
        Block { statements }
    }
}

pub struct If {
    pub condition: Expr,
    pub body: Box<Stmt>,
    pub else_stmt: Option<Box<Stmt>>,
}

impl If {
    pub fn new(condition: Expr, body: Stmt, else_stmt: Option<Stmt>) -> Self {
        If {
            condition,
            body: Box::new(body),
            else_stmt: else_stmt.map(Box::new),
        }
    }
}
