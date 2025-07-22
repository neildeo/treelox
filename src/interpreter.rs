use std::{error::Error, fmt::Display};

use crate::{environment::Environment, stmt::Stmt, token::Token, value::TypeError};

pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Environment::new(),
        }
    }

    pub fn interpret(&mut self, statements: Vec<Box<dyn Stmt>>) -> Result<()> {
        for stmt in statements {
            stmt.interpret(&mut self.env)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    token: Token,
    message: String,
}

impl RuntimeError {
    pub fn new(token: Token, message: &str) -> Self {
        RuntimeError {
            token,
            message: message.to_string(),
        }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[line {}] Runtime Error at '{}': {}",
            self.token.line, self.token.lexeme, self.message
        )
    }
}

impl Error for RuntimeError {}

impl From<TypeError> for RuntimeError {
    fn from(value: TypeError) -> Self {
        RuntimeError::new(value.token, &value.message)
    }
}

pub type Result<T> = std::result::Result<T, RuntimeError>;
