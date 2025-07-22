use std::collections::HashMap;

use crate::{interpreter::RuntimeError, token::Token, value::Value};

pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: Token, value: Value) {
        self.values.insert(name.lexeme, value);
    }

    pub fn get(&self, name: &Token) -> Result<&Value, RuntimeError> {
        self.values.get(&name.lexeme).ok_or(RuntimeError::new(
            name.clone(),
            &format!("Undefined variable '{}'.", name.lexeme),
        ))
    }

    pub fn assign(&mut self, name: Token, value: Value) -> Result<(), RuntimeError> {
        match self.values.contains_key(&name.lexeme) {
            true => {
                self.values.insert(name.lexeme, value);
                Ok(())
            }
            false => Err(RuntimeError::new(
                name.clone(),
                &format!("Undefined variable '{}'.", name.lexeme),
            )),
        }
    }
}
