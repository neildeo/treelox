use crate::token::Token;
use std::fmt::Display;

pub trait Expr: Display {}

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

pub enum Value {
    True,
    False,
    Nil,
    String(String),
    Number(f64),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value_string = match self {
            Value::True => "true",
            Value::False => "false",
            Value::Nil => "nil",
            Value::String(s) => s,
            Value::Number(x) => {
                if x.to_string().contains('.') {
                    &format!("{x}")
                } else {
                    &format!("{x}.0")
                }
            }
        };

        write!(f, "{value_string}")
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

impl<L: Expr, R: Expr> Expr for Binary<L, R> {}

impl<E: Expr> Expr for Grouping<E> {}

impl Expr for Literal {}

impl<E: Expr> Expr for Unary<E> {}

impl Expr for Box<dyn Expr> {}
