use std::collections::HashMap;

use crate::{interpreter::RuntimeError, value::Value};

pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &String) -> Result<&Value, RuntimeError> {
        self.values.get(name).ok_or(RuntimeError::new(&format!(
            "Undefined variable '{}'.",
            name
        )))
    }
}
