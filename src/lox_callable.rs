use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    iter::zip,
    rc::Rc,
};

use crate::{
    environment::Environment,
    interpreter::{Interpreter, Result, RuntimeException},
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
            Value::Class(c) => c.borrow().call(interpreter, args),
            Value::Clock(clock) => clock.call(interpreter, args),
            _ => unreachable!("Cannot call non-callable value"),
        }
    }

    fn arity(&self) -> usize {
        match self {
            Value::Function(f) => f.arity(),
            Value::Class(c) => c.borrow().arity(),
            Value::Clock(clock) => clock.arity(),
            _ => unreachable!("Cannot call non-callable value"),
        }
    }
}

#[derive(Clone)]
pub struct LoxFunction {
    declaration: Function,
    closure: Rc<RefCell<Environment>>,
    is_initialiser: bool,
}

impl Debug for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LoxFunction")
            .field("declaration", &self.declaration)
            .field("is_initialiser", &self.is_initialiser)
            .finish()
    }
}

impl LoxFunction {
    pub fn new(
        declaration: &Function,
        closure: &Rc<RefCell<Environment>>,
        is_initialiser: bool,
    ) -> Self {
        LoxFunction {
            declaration: declaration.clone(),
            closure: closure.clone(),
            is_initialiser,
        }
    }

    pub fn bind(&self, instance: &Value) -> Self {
        if !matches!(instance, Value::ClassInstance(_)) {
            panic!(
                "Cannot bind value {} to method {}: value is not a class instance.",
                instance, &self
            );
        }
        let mut env = Environment::new_with_enclosing(&self.closure);
        env.define_value("this", instance.clone());
        LoxFunction::new(
            &self.declaration,
            &Rc::new(RefCell::new(env)),
            self.is_initialiser,
        )
    }
}

// TODO: update this
impl PartialEq for LoxFunction {
    fn eq(&self, _other: &Self) -> bool {
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
        let mut fn_env = Environment::new_with_enclosing(&self.closure);
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

        if self.is_initialiser {
            return self
                .closure
                .borrow()
                .get_value_at_depth(0, "this")
                .map_err(RuntimeException::RuntimeError);
        }
        result
    }

    fn arity(&self) -> usize {
        self.declaration.params.len()
    }
}
