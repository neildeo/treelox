use crate::{
    environment::Environment,
    interpreter::Result,
    token::{Token, TokenType},
    value::{TypeError, Value},
};
use core::panic;
use std::fmt::Display;

pub trait GetName {
    fn get_name(&self) -> Option<Token> {
        None
    }
}

pub trait Expr: Display + GetName {
    fn interpret(&self, env: &mut Environment) -> Result<Value>;
}

pub struct Binary<L: Expr, R: Expr> {
    pub left: L,
    pub operator: Token,
    pub right: R,
}

impl<L: Expr, R: Expr> Binary<L, R> {
    pub fn new(left: L, operator: Token, right: R) -> Self {
        Binary {
            left,
            operator,
            right,
        }
    }
}

impl<L: Expr, R: Expr> Display for Binary<L, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.operator, self.left, self.right)
    }
}

impl<L: Expr, R: Expr> GetName for Binary<L, R> {}

impl<L: Expr, R: Expr> Expr for Binary<L, R> {
    fn interpret(&self, env: &mut Environment) -> Result<Value> {
        let left = self.left.interpret(env)?;
        let right = self.right.interpret(env)?;

        match self.operator.token_type {
            TokenType::Minus => Ok(Value::Number(
                f64::try_from(left).map_err(|e| TypeError::new(self.operator.clone(), &e))?
                    - f64::try_from(right)
                        .map_err(|e| TypeError::new(self.operator.clone(), &e))?,
            )),
            TokenType::Slash => Ok(Value::Number(
                f64::try_from(left).map_err(|e| TypeError::new(self.operator.clone(), &e))?
                    / f64::try_from(right)
                        .map_err(|e| TypeError::new(self.operator.clone(), &e))?,
            )),
            TokenType::Star => Ok(Value::Number(
                f64::try_from(left).map_err(|e| TypeError::new(self.operator.clone(), &e))?
                    * f64::try_from(right)
                        .map_err(|e| TypeError::new(self.operator.clone(), &e))?,
            )),
            TokenType::Plus => {
                if left.is_number() && right.is_number() {
                    Ok(Value::Number(
                        f64::try_from(left).unwrap() + f64::try_from(right).unwrap(),
                    ))
                } else if left.is_string() && right.is_string() {
                    let s =
                        String::try_from(left).unwrap() + String::try_from(right).unwrap().as_str();
                    Ok(Value::String(s))
                } else {
                    Err(TypeError::new(
                        self.operator.clone(),
                        &format!("Cannot add values {left:?} and {right:?}"),
                    )
                    .into())
                }
            }
            TokenType::Greater => Ok((f64::try_from(left)
                .map_err(|e| TypeError::new(self.operator.clone(), &e))?
                > f64::try_from(right).map_err(|e| TypeError::new(self.operator.clone(), &e))?)
            .into()),
            TokenType::GreaterEqual => Ok((f64::try_from(left)
                .map_err(|e| TypeError::new(self.operator.clone(), &e))?
                >= f64::try_from(right).map_err(|e| TypeError::new(self.operator.clone(), &e))?)
            .into()),
            TokenType::Less => Ok((f64::try_from(left)
                .map_err(|e| TypeError::new(self.operator.clone(), &e))?
                < f64::try_from(right).map_err(|e| TypeError::new(self.operator.clone(), &e))?)
            .into()),
            TokenType::LessEqual => Ok((f64::try_from(left)
                .map_err(|e| TypeError::new(self.operator.clone(), &e))?
                <= f64::try_from(right).map_err(|e| TypeError::new(self.operator.clone(), &e))?)
            .into()),
            TokenType::BangEqual => Ok((!left.is_equal(&right)).into()),
            TokenType::EqualEqual => Ok(left.is_equal(&right).into()),
            _ => todo!(),
        }
    }
}

pub struct Grouping<E: Expr> {
    pub expr: E,
}

impl<E: Expr> Grouping<E> {
    pub fn new(expr: E) -> Self {
        Grouping { expr }
    }
}

impl<E: Expr> Display for Grouping<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(group {})", self.expr)
    }
}

impl<E: Expr> GetName for Grouping<E> {}

impl<E: Expr> Expr for Grouping<E> {
    fn interpret(&self, env: &mut Environment) -> Result<Value> {
        self.expr.interpret(env)
    }
}

pub struct Literal {
    pub value: Value,
}

impl Literal {
    pub fn new(value: Value) -> Self {
        Literal { value }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl GetName for Literal {}

impl Expr for Literal {
    fn interpret(&self, _env: &mut Environment) -> Result<Value> {
        Ok(self.value.clone())
    }
}

pub struct Unary<E: Expr> {
    pub operator: Token,
    pub expr: E,
}

impl<E: Expr> Unary<E> {
    pub fn new(operator: Token, expr: E) -> Self {
        Unary { operator, expr }
    }
}

impl<E: Expr> Display for Unary<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {})", self.operator, self.expr)
    }
}

impl<E: Expr> GetName for Unary<E> {}

impl<E: Expr> Expr for Unary<E> {
    fn interpret(&self, env: &mut Environment) -> Result<Value> {
        let right = self.expr.interpret(env)?;

        match self.operator.token_type {
            TokenType::Minus => match right {
                Value::Number(x) => Ok(Value::Number(-x)),
                v => panic!("Cannot negate value {v:?}"),
            },
            TokenType::Bang => Ok((!right.is_truthy()).into()),
            _ => panic!("Unrecognised unary operator: {:?}", self.operator),
        }
    }
}

impl GetName for Box<dyn Expr> {
    fn get_name(&self) -> Option<Token> {
        (**self).get_name()
    }
}

impl Expr for Box<dyn Expr> {
    fn interpret(&self, env: &mut Environment) -> Result<Value> {
        (**self).interpret(env)
    }
}

pub struct Variable {
    pub name: Token,
}

impl Variable {
    pub fn new(name: Token) -> Self {
        match name.token_type {
            TokenType::Identifier => Variable { name },
            _ => panic!("Variable name token is not an identifier."),
        }
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl GetName for Variable {
    fn get_name(&self) -> Option<Token> {
        Some(self.name.clone())
    }
}

impl Expr for Variable {
    fn interpret(&self, env: &mut Environment) -> Result<Value> {
        env.get(&self.name).cloned()
    }
}

pub struct Assign<T: Expr> {
    pub name: Token,
    pub value: T,
}

impl<T: Expr> Assign<T> {
    pub fn new(name: Token, value: T) -> Self {
        Assign { name, value }
    }
}

impl<T: Expr> Display for Assign<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.name, self.value)
    }
}

impl<T: Expr> GetName for Assign<T> {}

impl<T: Expr> Expr for Assign<T> {
    fn interpret(&self, env: &mut Environment) -> Result<Value> {
        let value = self.value.interpret(env)?;

        env.assign(self.name.clone(), value.clone())?;

        Ok(value)
    }
}
