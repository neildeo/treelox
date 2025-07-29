use std::{fmt::Display, time::UNIX_EPOCH};

use crate::{
    lox_callable::{LoxCallable, LoxFunction},
    token::Token,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    True,
    False,
    Nil,
    String(String),
    Number(f64),
    Function(LoxFunction),
    // Native functions
    Clock(Clock),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Clock;

impl LoxCallable for Clock {
    fn call(
        &self,
        _interpreter: &mut crate::interpreter::Interpreter,
        _args: &[Value],
    ) -> crate::interpreter::Result<Value> {
        let time = UNIX_EPOCH.elapsed().unwrap().as_secs_f64();

        Ok(Value::Number(time))
    }

    fn arity(&self) -> usize {
        0
    }
}

impl Display for Clock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn>clock")
    }
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::True => true,
            Value::False => false,
            Value::Nil => false,
            Value::String(_) => true,
            Value::Number(_) => true,
            Value::Clock(_) => true,
            Value::Function(_) => true,
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

    pub fn is_callable(&self) -> bool {
        matches!(self, Value::Function(_) | Value::Clock(_))
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
            Value::Number(x) => &x.to_string(),
            Value::Clock(clock) => &clock.to_string(),
            Value::Function(lox_function) => &lox_function.to_string(),
        };

        write!(f, "{value_string}")
    }
}
