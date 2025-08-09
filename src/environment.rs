use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{interpreter::RuntimeError, token::Token, value::Value};

#[derive(Debug)]
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

    pub fn define(&mut self, name: &Token, value: Value) {
        self.values.insert(name.lexeme.clone(), value);
    }

    pub fn define_value(&mut self, name: &str, value: Value) {
        self.values.insert(name.to_string(), value);
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

    pub fn get_value(&self, name: &str) -> Result<Value, RuntimeError> {
        if let Some(value) = self.values.get(name) {
            Ok(value.clone())
        } else if let Some(outer) = &self.enclosing {
            outer.borrow().get_value(name)
        } else {
            Err(RuntimeError::new(
                Token::new(
                    crate::token::TokenType::This,
                    name.to_string(),
                    crate::token::LiteralValue::Null,
                    0,
                ),
                &format!("Undefined variable '{}'.", name),
            ))
        }
    }

    pub fn get_at_depth(&self, depth: usize, name: &Token) -> Result<Value, RuntimeError> {
        if depth == 0 {
            self.get(name)
        } else {
            self.enclosing
                .as_ref()
                .expect("Correct number of enclosing environments exist at runtime.")
                .borrow()
                .get_at_depth(depth - 1, name)
        }
    }

    pub fn get_value_at_depth(&self, depth: usize, name: &str) -> Result<Value, RuntimeError> {
        if depth == 0 {
            self.get_value(name)
        } else {
            self.enclosing
                .as_ref()
                .expect("Correct number of enclosing environments exist at runtime.")
                .borrow()
                .get_value_at_depth(depth - 1, name)
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

    fn unchecked_assign(&mut self, name: Token, value: Value) {
        self.values.insert(name.lexeme, value);
    }

    pub fn assign_at_depth(
        &mut self,
        depth: usize,
        name: Token,
        value: Value,
    ) -> Result<(), RuntimeError> {
        if depth == 0 {
            self.unchecked_assign(name, value);
        } else {
            self.enclosing
                .as_mut()
                .unwrap()
                .borrow_mut()
                .unchecked_assign(name, value);
        }
        Ok(())
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}
