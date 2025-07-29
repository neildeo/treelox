use std::{cell::RefCell, fmt::Display, iter::zip, rc::Rc};

use crate::{
    environment::Environment,
    interpreter::{Interpreter, Result},
    stmt::Function,
    value::Value,
};

pub trait LoxCallable {
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value>;

    fn arity(&self) -> usize;
}

impl LoxCallable for Value {
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value> {
        match self {
            Value::Function(f) => f.call(interpreter, args),
            Value::Clock(clock) => clock.call(interpreter, args),
            _ => unreachable!("Cannot call non-callable value"),
        }
    }

    fn arity(&self) -> usize {
        match self {
            Value::Function(f) => f.arity(),
            Value::Clock(clock) => clock.arity(),
            _ => unreachable!("Cannot call non-callable value"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct LoxFunction {
    declaration: Function,
}

impl LoxFunction {
    pub fn new(declaration: &Function) -> Self {
        LoxFunction {
            declaration: declaration.clone(),
        }
    }
}

// TODO: update this
impl PartialEq for LoxFunction {
    fn eq(&self, other: &Self) -> bool {
        false
    }
}

impl Display for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.declaration.name)
    }
}

impl LoxCallable for LoxFunction {
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value> {
        let mut fn_env = Environment::new_with_enclosing(&interpreter.globals);
        for (param, arg) in zip(&self.declaration.params, args) {
            fn_env.define(param, arg.clone());
            // println!("Bound var: {} = {}", param, arg);
        }

        let previous = interpreter.env.clone();
        interpreter.env = Rc::new(RefCell::new(fn_env));
        let result = interpreter
            .interpret(&self.declaration.body)
            .map(|x| x.unwrap_or(Value::Nil));
        interpreter.env = previous;
        result
    }

    fn arity(&self) -> usize {
        self.declaration.params.len()
    }
}
