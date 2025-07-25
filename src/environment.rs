use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{interpreter::RuntimeError, token::Token, value::Value};

pub struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn new_with_enclosing(enclosing: &Rc<RefCell<Environment>>) -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: Some(enclosing.clone()),
        }
    }

    pub fn define(&mut self, name: Token, value: Value) {
        self.values.insert(name.lexeme, value);
    }

    pub fn get(&self, name: &Token) -> Result<Value, RuntimeError> {
        let key = &name.lexeme;
        if let Some(value) = self.values.get(key) {
            Ok(value.clone())
        } else if let Some(outer) = &self.enclosing {
            outer.borrow().get(name)
        } else {
            Err(RuntimeError::new(
                name.clone(),
                &format!("Undefined variable '{}'.", name.lexeme),
            ))
        }
    }

    pub fn assign(&mut self, name: Token, value: Value) -> Result<(), RuntimeError> {
        match self.values.contains_key(&name.lexeme) {
            true => {
                self.values.insert(name.lexeme, value);
                Ok(())
            }
            false => {
                if let Some(outer) = &self.enclosing {
                    outer.borrow_mut().assign(name, value)
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
