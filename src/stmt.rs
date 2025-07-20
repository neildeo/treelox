use core::panic;

use crate::environment::Environment;
use crate::expr::Expr;
use crate::interpreter::Result;
use crate::token::Token;

pub trait Stmt {
    fn interpret(&self, env: &mut Environment) -> Result<()>;
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
    fn interpret(&self, env: &mut Environment) -> Result<()> {
        self.expression.interpret(env)?;
        Ok(())
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
    fn interpret(&self, env: &mut Environment) -> Result<()> {
        let value = self.expression.interpret(env)?;
        println!("{}", value);
        Ok(())
    }
}

pub struct Var {
    name: String,
    initialiser: Option<Box<dyn Expr>>,
}

impl Var {
    pub fn new(name: Token, initialiser: Option<Box<dyn Expr>>) -> Self {
        let name = match name {
            Token::Identifier(name) => name,
            _ => panic!("Variable name token is not an identifier."),
        };
        Var { name, initialiser }
    }
}

impl Stmt for Var {
    fn interpret(&self, env: &mut Environment) -> Result<()> {
        let value = match &self.initialiser {
            Some(expr) => expr.interpret(env)?,
            None => crate::value::Value::Nil,
        };

        env.define(self.name.clone(), value);

        Ok(())
    }
}
