use std::fmt::Display;

use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    True,
    False,
    Nil,
    String(String),
    Number(f64),
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::True => true,
            Value::False => false,
            Value::Nil => false,
            Value::String(_) => true,
            Value::Number(_) => true,
        }
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Value::String(_))
    }

    pub fn is_equal(&self, other: &Value) -> bool {
        if self == &Value::Nil && other == &Value::Nil {
            return true;
        }

        if self == &Value::Nil {
            return false;
        }

        self == other
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        match value {
            true => Value::True,
            false => Value::False,
        }
    }
}

#[derive(Debug)]
pub struct TypeError {
    pub token: Token,
    pub message: String,
}

impl TypeError {
    pub fn new(token: Token, message: &str) -> Self {
        TypeError {
            token,
            message: message.to_string(),
        }
    }
}

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[line {}] Type error at '{}': {}",
            self.token.line, self.token, self.message
        )
    }
}

impl TryFrom<Value> for f64 {
    type Error = String;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(x) => Ok(x),
            v => Err(format!("Value {v:?} is not a number")),
        }
    }
}

impl TryFrom<Value> for String {
    type Error = String;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(s) => Ok(s),
            v => Err(format!("Value {v:?} is not a string")),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value_string = match self {
            Value::True => "true",
            Value::False => "false",
            Value::Nil => "nil",
            Value::String(s) => s,
            Value::Number(x) => &format!("{x}"),
        };

        write!(f, "{value_string}")
    }
}
