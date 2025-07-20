use crate::expr::Expr;
use crate::interpreter::Result;

pub trait Stmt {
    fn interpret(&self) -> Result<()>;
}

pub struct Expression {
    expression: Box<dyn Expr>,
}

impl Expression {
    pub fn new(expression: Box<dyn Expr>) -> Self {
        Expression { expression }
    }
}

impl Stmt for Expression {
    fn interpret(&self) -> Result<()> {
        self.expression.interpret()?;
        Ok(())
    }
}

pub struct Print {
    expression: Box<dyn Expr>,
}

impl Print {
    pub fn new(expression: Box<dyn Expr>) -> Self {
        Print { expression }
    }
}

impl Stmt for Print {
    fn interpret(&self) -> Result<()> {
        let value = self.expression.interpret()?;
        println!("{}", value);
        Ok(())
    }
}
