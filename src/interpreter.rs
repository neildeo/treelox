use std::{error::Error, fmt::Display};

use crate::{expr::Expr, value::TypeError};

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

pub fn interpret(expr: impl Expr) -> Result<String> {
    let value = expr.interpret()?;

    Ok(format!("{}", value))
}
