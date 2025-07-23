use std::collections::HashMap;

use crate::{interpreter::RuntimeError, token::Token, value::Value};

pub struct Environment<'a> {
    values: HashMap<String, Value>,
    enclosing: Option<&'a mut Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn new(enclosing: Option<&'a mut Environment<'a>>) -> Self {
        Environment {
            values: HashMap::new(),
            enclosing,
        }
    }

    pub fn define(&mut self, name: Token, value: Value) {
        self.values.insert(name.lexeme, value);
    }

    pub fn get(&self, name: &Token) -> Result<&Value, RuntimeError> {
        match self.values.get(&name.lexeme) {
            Some(value) => Ok(value),
            None => {
                if let Some(encl) = &self.enclosing {
                    encl.get(name)
                } else {
                    Err(RuntimeError::new(
                        name.clone(),
                        &format!("Undefined variable '{}'.", name.lexeme),
                    ))
                }
            }
        }
    }

    pub fn assign(&mut self, name: Token, value: Value) -> Result<(), RuntimeError> {
        match self.values.contains_key(&name.lexeme) {
            true => {
                self.values.insert(name.lexeme, value);
                Ok(())
            }
            false => {
                if let Some(encl) = &mut self.enclosing {
                    encl.assign(name, value)
                } else {
                    Err(RuntimeError::new(
                        name.clone(),
                        &format!("Undefined variable '{}'.", name.lexeme),
                    ))
                }
            }
        }
    }
}
