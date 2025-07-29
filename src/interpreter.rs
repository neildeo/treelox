use std::{cell::RefCell, error::Error, fmt::Display, rc::Rc};

use crate::{
    environment::Environment,
    expr::{self, Expr},
    lox_callable::{LoxCallable, LoxFunction},
    stmt::{self, Stmt},
    token::{Token, TokenType},
    value::{Clock, TypeError, Value},
};

pub struct Interpreter {
    pub globals: Rc<RefCell<Environment>>,
    pub env: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Environment::default()));

        globals
            .borrow_mut()
            .define_value("clock", Value::Clock(Clock {}));

        Interpreter {
            globals: globals.clone(),
            env: globals.clone(),
        }
    }

    fn push_scope(&mut self) -> Rc<RefCell<Environment>> {
        let outer = self.env.clone();
        let inner = Environment::new_with_enclosing(&self.env);
        self.env = Rc::new(RefCell::new(inner));
        outer
    }

    pub fn interpret(&mut self, statements: &[Stmt]) -> Result<Option<Value>> {
        let mut value = None;
        for stmt in statements {
            value = self.interpret_stmt(stmt)?;
        }

        Ok(value)
    }

    pub fn interpret_stmt(&mut self, stmt: &Stmt) -> Result<Option<Value>> {
        match stmt {
            Stmt::Expression(expression) => self.interpret_stmt_expression(expression),
            Stmt::Print(print) => self.interpret_stmt_print(print),
            Stmt::Var(var) => self.interpret_stmt_var(var),
            Stmt::Block(block) => self.interpret_stmt_block(block),
            Stmt::If(if_stmt) => self.interpret_stmt_if(if_stmt),
            Stmt::While(while_stmt) => self.interpret_stmt_while(while_stmt),
            Stmt::Function(function_stmt) => self.interpret_stmt_function(function_stmt),
            Stmt::Return(ret) => self.interpret_stmt_return(ret),
        }
    }

    pub fn interpret_expr(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Binary(binary) => self.interpret_expr_binary(binary),
            Expr::Grouping(grouping) => self.interpret_expr_grouping(grouping),
            Expr::Literal(literal) => self.interpret_expr_literal(literal),
            Expr::Unary(unary) => self.interpret_expr_unary(unary),
            Expr::Variable(variable) => self.interpret_expr_variable(variable),
            Expr::Assign(assign) => self.interpret_expr_assign(assign),
            Expr::Logical(logical) => self.interpret_expr_logical(logical),
            Expr::Call(call) => self.interpret_expr_call(call),
        }
    }

    fn interpret_stmt_expression(&mut self, stmt: &stmt::Expression) -> Result<Option<Value>> {
        let value = self.interpret_expr(&stmt.expression)?;
        Ok(Some(value))
    }

    fn interpret_stmt_print(&mut self, stmt: &stmt::Print) -> Result<Option<Value>> {
        let value = self.interpret_expr(&stmt.expression)?;
        println!("{}", value);
        Ok(None)
    }

    fn interpret_stmt_var(&mut self, stmt: &stmt::Var) -> Result<Option<Value>> {
        let value = match &stmt.initialiser {
            Some(expr) => self.interpret_expr(expr)?,
            None => crate::value::Value::Nil,
        };

        self.env.borrow_mut().define(&stmt.name, value);

        Ok(None)
    }

    fn interpret_stmt_block(&mut self, stmt: &stmt::Block) -> Result<Option<Value>> {
        let outer = self.push_scope();
        let maybe_value = self.interpret(&stmt.statements);
        self.env = outer;
        maybe_value
    }

    fn interpret_stmt_if(&mut self, stmt: &stmt::If) -> Result<Option<Value>> {
        if self.interpret_expr(&stmt.condition)?.is_truthy() {
            self.interpret_stmt(&stmt.body)
        } else if let Some(else_stmt) = &stmt.else_stmt {
            self.interpret_stmt(else_stmt)
        } else {
            Ok(None)
        }
    }

    fn interpret_stmt_while(&mut self, stmt: &stmt::While) -> Result<Option<Value>> {
        while self.interpret_expr(&stmt.condition)?.is_truthy() {
            self.interpret_stmt(&stmt.body)?;
        }

        Ok(None)
    }

    fn interpret_stmt_function(&mut self, stmt: &stmt::Function) -> Result<Option<Value>> {
        let function = LoxFunction::new(stmt, &self.env);
        self.env
            .borrow_mut()
            .define(&stmt.name, Value::Function(function));
        Ok(None)
    }

    fn interpret_stmt_return(&mut self, stmt: &stmt::Return) -> Result<Option<Value>> {
        // The return value has been set to nil if not defined already
        let value = self.interpret_expr(&stmt.value)?;
        Err(RuntimeException::ReturnValue(ReturnValue { value }))
    }

    fn interpret_expr_binary(&mut self, expr: &expr::Binary) -> Result<Value> {
        let left = self.interpret_expr(&expr.left)?;
        let right = self.interpret_expr(&expr.right)?;

        match expr.operator.token_type {
            TokenType::Minus => Ok(Value::Number(
                f64::try_from(left).map_err(|e| TypeError::new(expr.operator.clone(), &e))?
                    - f64::try_from(right)
                        .map_err(|e| TypeError::new(expr.operator.clone(), &e))?,
            )),
            TokenType::Slash => Ok(Value::Number(
                f64::try_from(left).map_err(|e| TypeError::new(expr.operator.clone(), &e))?
                    / f64::try_from(right)
                        .map_err(|e| TypeError::new(expr.operator.clone(), &e))?,
            )),
            TokenType::Star => Ok(Value::Number(
                f64::try_from(left).map_err(|e| TypeError::new(expr.operator.clone(), &e))?
                    * f64::try_from(right)
                        .map_err(|e| TypeError::new(expr.operator.clone(), &e))?,
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
                        expr.operator.clone(),
                        &format!("Cannot add values {left:?} and {right:?}"),
                    )
                    .into())
                }
            }
            TokenType::Greater => Ok((f64::try_from(left)
                .map_err(|e| TypeError::new(expr.operator.clone(), &e))?
                > f64::try_from(right).map_err(|e| TypeError::new(expr.operator.clone(), &e))?)
            .into()),
            TokenType::GreaterEqual => Ok((f64::try_from(left)
                .map_err(|e| TypeError::new(expr.operator.clone(), &e))?
                >= f64::try_from(right).map_err(|e| TypeError::new(expr.operator.clone(), &e))?)
            .into()),
            TokenType::Less => Ok((f64::try_from(left)
                .map_err(|e| TypeError::new(expr.operator.clone(), &e))?
                < f64::try_from(right).map_err(|e| TypeError::new(expr.operator.clone(), &e))?)
            .into()),
            TokenType::LessEqual => Ok((f64::try_from(left)
                .map_err(|e| TypeError::new(expr.operator.clone(), &e))?
                <= f64::try_from(right).map_err(|e| TypeError::new(expr.operator.clone(), &e))?)
            .into()),
            TokenType::BangEqual => Ok((!left.is_equal(&right)).into()),
            TokenType::EqualEqual => Ok(left.is_equal(&right).into()),
            _ => todo!(),
        }
    }

    fn interpret_expr_call(&mut self, expr: &expr::Call) -> Result<Value> {
        let callee = self.interpret_expr(&expr.callee)?;

        if !callee.is_callable() {
            return Err(RuntimeException::new(
                expr.paren.clone(),
                "Can only call functions and classes.",
            ));
        }

        let args = expr
            .args
            .iter()
            .map(|a| self.interpret_expr(a))
            .collect::<Result<Vec<Value>>>()?;

        if args.len() != callee.arity() {
            return Err(RuntimeException::new(
                expr.paren.clone(),
                &format!(
                    "Expected {} arguments but got {}.",
                    callee.arity(),
                    args.len()
                ),
            ));
        }

        match callee.call(self, &args) {
            Ok(val) => Ok(val),
            Err(RuntimeException::ReturnValue(return_val)) => Ok(return_val.value),
            Err(e) => Err(e),
        }
    }

    fn interpret_expr_logical(&mut self, expr: &expr::Logical) -> Result<Value> {
        let left = self.interpret_expr(&expr.left)?;

        if expr.operator.token_type == TokenType::Or {
            if left.is_truthy() {
                return Ok(left);
            }
        } else if !left.is_truthy() {
            return Ok(left);
        }

        self.interpret_expr(&expr.right)
    }

    fn interpret_expr_grouping(&mut self, expr: &expr::Grouping) -> Result<Value> {
        self.interpret_expr(&expr.expr)
    }

    fn interpret_expr_literal(&mut self, expr: &expr::Literal) -> Result<Value> {
        Ok(expr.value.clone())
    }

    fn interpret_expr_unary(&mut self, expr: &expr::Unary) -> Result<Value> {
        let right = self.interpret_expr(&expr.expr)?;

        match expr.operator.token_type {
            TokenType::Minus => match right {
                Value::Number(x) => Ok(Value::Number(-x)),
                v => panic!("Cannot negate value {v:?}"),
            },
            TokenType::Bang => Ok((!right.is_truthy()).into()),
            _ => panic!("Unrecognised unary operator: {:?}", expr.operator),
        }
    }

    fn interpret_expr_variable(&mut self, expr: &expr::Variable) -> Result<Value> {
        self.env
            .borrow()
            .get(&expr.name)
            .map_err(RuntimeException::RuntimeError)
    }

    fn interpret_expr_assign(&mut self, expr: &expr::Assign) -> Result<Value> {
        let value = self.interpret_expr(&expr.value)?;

        self.env
            .borrow_mut()
            .assign(expr.name.clone(), value.clone())?;

        Ok(value)
    }
}

#[derive(Clone, Debug)]
pub struct RuntimeError {
    token: Token,
    message: String,
}

impl RuntimeError {
    pub fn new(token: Token, message: &str) -> Self {
        RuntimeError {
            token,
            message: message.to_string(),
        }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[line {}] Runtime Error at '{}': {}",
            self.token.line, self.token.lexeme, self.message
        )
    }
}

impl Error for RuntimeError {}

#[derive(Clone, Debug, PartialEq)]
pub struct ReturnValue {
    value: Value,
}

impl Display for ReturnValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Error for ReturnValue {}

#[derive(Clone, Debug)]
pub enum RuntimeException {
    RuntimeError(RuntimeError),
    ReturnValue(ReturnValue),
}

impl RuntimeException {
    fn new(token: Token, message: &str) -> Self {
        RuntimeException::RuntimeError(RuntimeError::new(token, message))
    }
}

impl Display for RuntimeException {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            RuntimeException::RuntimeError(runtime_error) => runtime_error.to_string(),
            RuntimeException::ReturnValue(return_value) => return_value.to_string(),
        };

        write!(f, "{str}")
    }
}

impl From<TypeError> for RuntimeException {
    fn from(value: TypeError) -> Self {
        RuntimeException::RuntimeError(RuntimeError::new(value.token, &value.message))
    }
}

impl From<RuntimeError> for RuntimeException {
    fn from(value: RuntimeError) -> Self {
        RuntimeException::RuntimeError(value)
    }
}

impl Error for RuntimeException {}

pub type Result<T> = std::result::Result<T, RuntimeException>;
