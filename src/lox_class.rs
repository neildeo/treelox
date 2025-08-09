use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{
    interpreter::RuntimeError,
    lox_callable::{LoxCallable, LoxFunction},
    token::Token,
    value::Value,
};

#[derive(Clone, Debug, PartialEq)]
pub struct LoxClass {
    name: String,
    methods: HashMap<String, LoxFunction>,
}

impl LoxClass {
    pub fn new(name: &Token, methods: HashMap<String, LoxFunction>) -> Self {
        LoxClass {
            name: name.lexeme.clone(),
            methods,
        }
    }

    fn find_method(&self, name: &str) -> Option<LoxFunction> {
        self.methods.get(name).cloned()
    }
}

impl Display for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl LoxCallable for LoxClass {
    fn call(
        &self,
        interpreter: &mut crate::interpreter::Interpreter,
        args: &[crate::value::Value],
    ) -> crate::interpreter::Result<crate::value::Value> {
        let instance: Value = LoxInstance::new(&Rc::new(RefCell::new(self.clone()))).into();
        if let Some(initialiser) = self.find_method("init") {
            initialiser.bind(&instance).call(interpreter, args)?;
        }

        Ok(instance)
    }

    fn arity(&self) -> usize {
        if let Some(initialiser) = self.find_method("init") {
            initialiser.arity()
        } else {
            0
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LoxInstance {
    class: Rc<RefCell<LoxClass>>,
    fields: HashMap<String, Value>,
}

impl LoxInstance {
    pub fn new(class: &Rc<RefCell<LoxClass>>) -> Self {
        LoxInstance {
            class: class.clone(),
            fields: HashMap::new(),
        }
    }

    pub fn get(&self, name: &Token) -> Result<Value, RuntimeError> {
        match self.fields.get(&name.lexeme) {
            Some(val) => Ok(val.clone()),
            None => match self.class.borrow().find_method(&name.lexeme) {
                Some(method) => Ok(Value::Function(method.bind(&self.clone().into()))),
                None => Err(RuntimeError::new(
                    name.clone(),
                    &format!("Undefined property '{}'.", name.lexeme),
                )),
            },
        }
    }

    pub fn set(&mut self, name: Token, value: Value) {
        self.fields.insert(name.lexeme, value);
    }
}

impl Display for LoxInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance", self.class.borrow())
    }
}
