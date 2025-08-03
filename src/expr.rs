use itertools::Itertools;

use crate::{
    token::{Token, TokenType},
    value::Value,
};
use core::panic;
use std::{collections::HashSet, fmt::Display};

#[derive(Clone, Debug)]
pub struct Expr {
    pub id: usize,
    pub content: ExprContent,
}

impl Expr {
    pub fn new(content: ExprContent, id_list: &mut ExprIdList) -> Self {
        let id = id_list.generate_id();
        Expr { id, content }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.content)
    }
}

#[derive(Debug)]
pub struct ExprIdList {
    set: HashSet<usize>,
    next: usize,
}

impl ExprIdList {
    pub fn new() -> Self {
        ExprIdList {
            set: HashSet::new(),
            next: 0,
        }
    }

    fn generate_id(&mut self) -> usize {
        while self.set.contains(&self.next) {
            self.next += 1;
        }
        self.set.insert(self.next);
        self.next
    }
}

impl Default for ExprIdList {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug)]
pub enum ExprContent {
    Binary(Binary),
    Grouping(Grouping),
    Literal(Literal),
    Unary(Unary),
    Variable(Variable),
    Assign(Assign),
    Logical(Logical),
    Call(Call),
}

impl ExprContent {
    pub fn get_name(&self) -> Option<Token> {
        match self {
            ExprContent::Variable(var) => Some(var.name.clone()),
            _ => None,
        }
    }
}

impl Display for ExprContent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            ExprContent::Binary(binary) => binary.to_string(),
            ExprContent::Grouping(grouping) => grouping.to_string(),
            ExprContent::Literal(literal) => literal.to_string(),
            ExprContent::Unary(unary) => unary.to_string(),
            ExprContent::Variable(variable) => variable.to_string(),
            ExprContent::Assign(assign) => assign.to_string(),
            ExprContent::Logical(logical) => logical.to_string(),
            ExprContent::Call(call) => call.to_string(),
        };

        write!(f, "{}", str)
    }
}

#[derive(Clone, Debug)]
pub struct Binary {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

impl Binary {
    pub fn new(left: Expr, operator: Token, right: Expr) -> Self {
        Binary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }
}

impl Display for Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.operator, self.left, self.right)
    }
}

#[derive(Clone, Debug)]
pub struct Grouping {
    pub expr: Box<Expr>,
}

impl Grouping {
    pub fn new(expr: Expr) -> Self {
        Grouping {
            expr: Box::new(expr),
        }
    }
}

impl Display for Grouping {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(group {})", self.expr)
    }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct Unary {
    pub operator: Token,
    pub expr: Box<Expr>,
}

impl Unary {
    pub fn new(operator: Token, expr: Expr) -> Self {
        Unary {
            operator,
            expr: Box::new(expr),
        }
    }
}

impl Display for Unary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {})", self.operator, self.expr)
    }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct Assign {
    pub name: Token,
    pub value: Box<Expr>,
}

impl Assign {
    pub fn new(name: Token, value: Expr) -> Self {
        Assign {
            name,
            value: Box::new(value),
        }
    }
}

impl Display for Assign {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.name, self.value)
    }
}

#[derive(Clone, Debug)]
pub struct Logical {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

impl Logical {
    pub fn new(left: Expr, operator: Token, right: Expr) -> Self {
        Logical {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }
}

impl Display for Logical {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.operator, self.left, self.right)
    }
}

#[derive(Clone, Debug)]
pub struct Call {
    pub callee: Box<Expr>,
    pub paren: Token,
    pub args: Box<[Expr]>,
}

impl Call {
    pub fn new(callee: Expr, paren: Token, args: Vec<Expr>) -> Self {
        Call {
            callee: Box::new(callee),
            paren,
            args: args.into_boxed_slice(),
        }
    }
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let arg_string = self.args.iter().map(|x| x.to_string()).join(", ");

        write!(f, "{}({})", self.callee, arg_string)
    }
}
