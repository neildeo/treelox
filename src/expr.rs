use crate::{
    environment::Environment,
    interpreter::Result,
    token::Token,
    value::{TypeError, Value},
};
use core::panic;
use std::fmt::Display;

pub trait Expr: Display {
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

impl<L: Expr, R: Expr> Expr for Binary<L, R> {
    fn interpret(&self, env: &mut Environment) -> Result<Value> {
        let left = self.left.interpret(env)?;
        let right = self.right.interpret(env)?;

        match self.operator {
            Token::Minus => Ok(Value::Number(f64::try_from(left)? - f64::try_from(right)?)),
            Token::Slash => Ok(Value::Number(f64::try_from(left)? / f64::try_from(right)?)),
            Token::Star => Ok(Value::Number(f64::try_from(left)? * f64::try_from(right)?)),
            Token::Plus => {
                if left.is_number() && right.is_number() {
                    Ok(Value::Number(
                        f64::try_from(left).unwrap() + f64::try_from(right).unwrap(),
                    ))
                } else if left.is_string() && right.is_string() {
                    let s =
                        String::try_from(left).unwrap() + String::try_from(right).unwrap().as_str();
                    Ok(Value::String(s))
                } else {
                    Err(TypeError::new(&format!("Cannot add values {left:?} and {right:?}")).into())
                }
            }
            Token::Greater => Ok((f64::try_from(left)? > f64::try_from(right)?).into()),
            Token::GreaterEqual => Ok((f64::try_from(left)? >= f64::try_from(right)?).into()),
            Token::Less => Ok((f64::try_from(left)? < f64::try_from(right)?).into()),
            Token::LessEqual => Ok((f64::try_from(left)? <= f64::try_from(right)?).into()),
            Token::BangEqual => Ok((!left.is_equal(&right)).into()),
            Token::EqualEqual => Ok(left.is_equal(&right).into()),
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

impl<E: Expr> Expr for Unary<E> {
    fn interpret(&self, env: &mut Environment) -> Result<Value> {
        let right = self.expr.interpret(env)?;

        match self.operator {
            Token::Minus => match right {
                Value::Number(x) => Ok(Value::Number(-x)),
                v => panic!("Cannot negate value {v:?}"),
            },
            Token::Bang => Ok((!right.is_truthy()).into()),
            _ => panic!("Unrecognised unary operator: {:?}", self.operator),
        }
    }
}

impl Expr for Box<dyn Expr> {
    fn interpret(&self, env: &mut Environment) -> Result<Value> {
        (**self).interpret(env)
    }
}

pub struct Variable {
    pub name: String,
}

impl Variable {
    pub fn new(name: String) -> Self {
        Variable { name }
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Expr for Variable {
    fn interpret(&self, env: &mut Environment) -> Result<Value> {
        env.get(&self.name).cloned()
    }
}
