use crate::environment::Environment;
use crate::expr::Expr;
use crate::interpreter::Result;
use crate::token::{Token, TokenType};
use crate::value::Value;
use core::panic;

pub trait Stmt {
    fn interpret(&self, env: &mut Environment) -> Result<Option<Value>>;
}

impl Stmt for Box<dyn Stmt> {
    fn interpret(&self, env: &mut Environment) -> Result<Option<Value>> {
        (**self).interpret(env)
    }
}

pub struct Expression {
    expression: Box<dyn Expr>,
}

impl Expression {
    pub fn new(expression: Box<dyn Expr>) -> Self {
        Expression { expression }
    }
}

impl Stmt for Expression {
    fn interpret(&self, env: &mut Environment) -> Result<Option<Value>> {
        let value = self.expression.interpret(env)?;
        Ok(Some(value))
    }
}

pub struct Print {
    expression: Box<dyn Expr>,
}

impl Print {
    pub fn new(expression: Box<dyn Expr>) -> Self {
        Print { expression }
    }
}

impl Stmt for Print {
    fn interpret(&self, env: &mut Environment) -> Result<Option<Value>> {
        let value = self.expression.interpret(env)?;
        println!("{}", value);
        Ok(None)
    }
}

pub struct Var {
    name: Token,
    initialiser: Option<Box<dyn Expr>>,
}

impl Var {
    pub fn new(name: Token, initialiser: Option<Box<dyn Expr>>) -> Self {
        match name.token_type {
            TokenType::Identifier => Var { name, initialiser },
            _ => panic!("Variable name token is not an identifier."),
        }
    }
}

impl Stmt for Var {
    fn interpret(&self, env: &mut Environment) -> Result<Option<Value>> {
        let value = match &self.initialiser {
            Some(expr) => expr.interpret(env)?,
            None => crate::value::Value::Nil,
        };

        env.define(self.name.clone(), value);

        Ok(None)
    }
}

pub struct Block {
    statements: Vec<Box<dyn Stmt>>,
}

impl Block {
    pub fn new(statements: Vec<Box<dyn Stmt>>) -> Self {
        Block { statements }
    }
}

impl Stmt for Block {
    fn interpret(&self, env: &mut Environment) -> Result<Option<Value>> {
        let mut block_scope = Environment::new(Some(env));
        let mut value = None;
        for stmt in &self.statements {
            value = stmt.interpret(&mut block_scope)?;
        }

        Ok(value)
    }
}
