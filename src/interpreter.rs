use std::{error::Error, fmt::Display};

use crate::{environment::Environment, stmt::Stmt, value::TypeError};

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
    message: String,
}

impl RuntimeError {
    pub fn new(message: &str) -> Self {
        RuntimeError {
            message: message.to_string(),
        }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Runtime Error: {}", self.message)
    }
}

impl Error for RuntimeError {}

impl From<TypeError> for RuntimeError {
    fn from(value: TypeError) -> Self {
        RuntimeError {
            message: format!("{}", value),
        }
    }
}

pub type Result<T> = std::result::Result<T, RuntimeError>;
