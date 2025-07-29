use crate::expr::Expr;
use crate::token::{Token, TokenType};
use core::panic;

#[derive(Clone, Debug)]
pub enum Stmt {
    Expression(Expression),
    Print(Print),
    Var(Var),
    Block(Block),
    If(If),
    While(While),
    Function(Function),
    Return(Return),
}

#[derive(Clone, Debug)]
pub struct Expression {
    pub expression: Expr,
}

impl Expression {
    pub fn new(expression: Expr) -> Self {
        Expression { expression }
    }
}

#[derive(Clone, Debug)]
pub struct Print {
    pub expression: Expr,
}

impl Print {
    pub fn new(expression: Expr) -> Self {
        Print { expression }
    }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct Block {
    pub statements: Vec<Stmt>,
}

impl Block {
    pub fn new(statements: Vec<Stmt>) -> Self {
        Block { statements }
    }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct While {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

impl While {
    pub fn new(condition: Expr, body: Stmt) -> Self {
        While {
            condition,
            body: Box::new(body),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}

impl Function {
    pub fn new(name: Token, params: Vec<Token>, body: Vec<Stmt>) -> Self {
        Function { name, params, body }
    }
}

#[derive(Clone, Debug)]
pub struct Return {
    pub keyword: Token,
    pub value: Expr,
}

impl Return {
    pub fn new(keyword: Token, value: Expr) -> Self {
        Return { keyword, value }
    }
}
