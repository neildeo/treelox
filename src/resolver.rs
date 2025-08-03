use std::collections::HashMap;

use crate::{
    expr::{Expr, ExprContent},
    interpreter::{Interpreter, RuntimeException},
    stmt::{self, Stmt},
    token::Token,
};

pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: Scopes,
}

struct Scopes(Vec<HashMap<String, VarStatus>>);

impl Scopes {
    fn new() -> Self {
        Scopes(vec![HashMap::new()])
    }

    fn peek(&self) -> Option<&HashMap<String, VarStatus>> {
        if self.0.is_empty() {
            None
        } else {
            let end = self.0.len() - 1;
            Some(&self.0[end])
        }
    }

    fn peek_mut(&mut self) -> Option<&mut HashMap<String, VarStatus>> {
        if self.0.is_empty() {
            None
        } else {
            let end = self.0.len() - 1;
            Some(&mut self.0[end])
        }
    }

    fn push(&mut self, scope: HashMap<String, VarStatus>) {
        self.0.push(scope);
    }

    fn pop(&mut self) {
        self.0.pop().expect("Cannot pop from empty stack");
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum VarStatus {
    Defined,
    Undefined,
}

impl VarStatus {
    pub fn is_undefined(&self) -> bool {
        self == &VarStatus::Undefined
    }
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Resolver {
            interpreter,
            scopes: Scopes::new(),
        }
    }

    pub fn resolve(&mut self, stmts: &[Stmt]) -> Result<()> {
        for stmt in stmts {
            self.resolve_stmt(stmt)?;
        }

        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Block(block) => self.resolve_stmt_block(block),
            Stmt::Var(var) => self.resolve_stmt_var(var),
            Stmt::Function(function) => self.resolve_stmt_function(function),
            Stmt::Expression(expression) => self.resolve_stmt_expression(expression),
            Stmt::Print(print) => self.resolve_stmt_print(print),
            Stmt::If(if_stmt) => self.resolve_stmt_if(if_stmt),
            Stmt::While(while_stmt) => self.resolve_stmt_while(while_stmt),
            Stmt::Return(return_stmt) => self.resolve_stmt_return(return_stmt),
        }
    }

    fn resolve_stmt_block(&mut self, block: &stmt::Block) -> Result<()> {
        self.begin_scope();
        self.resolve(&block.statements)?;
        self.end_scope();
        Ok(())
    }

    fn resolve_stmt_var(&mut self, var: &stmt::Var) -> Result<()> {
        self.declare(&var.name);
        if let Some(expr) = &var.initialiser {
            self.resolve_expr(expr)?;
        }
        self.define(&var.name);
        Ok(())
    }

    fn resolve_stmt_function(&mut self, function: &stmt::Function) -> Result<()> {
        self.declare(&function.name);
        self.define(&function.name);

        self.resolve_function(function)?;
        Ok(())
    }

    fn resolve_function(&mut self, function: &stmt::Function) -> Result<()> {
        self.begin_scope();
        for param in &function.params {
            self.declare(param);
            self.define(param);
        }
        self.resolve(&function.body)?;
        self.end_scope();
        Ok(())
    }

    fn resolve_stmt_expression(&mut self, expression: &stmt::Expression) -> Result<()> {
        self.resolve_expr(&expression.expression)?;
        Ok(())
    }

    fn resolve_stmt_if(&mut self, if_stmt: &stmt::If) -> Result<()> {
        self.resolve_expr(&if_stmt.condition)?;
        self.resolve_stmt(&if_stmt.body)?;
        if let Some(else_stmt) = &if_stmt.else_stmt {
            self.resolve_stmt(else_stmt)?;
        }
        Ok(())
    }

    fn resolve_stmt_print(&mut self, print: &stmt::Print) -> Result<()> {
        self.resolve_expr(&print.expression)?;
        Ok(())
    }

    fn resolve_stmt_return(&mut self, return_stmt: &stmt::Return) -> Result<()> {
        self.resolve_expr(&return_stmt.value)?;
        Ok(())
    }

    fn resolve_stmt_while(&mut self, while_stmt: &stmt::While) -> Result<()> {
        self.resolve_expr(&while_stmt.condition)?;
        self.resolve_stmt(&while_stmt.body)?;
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result<()> {
        match &expr.content {
            ExprContent::Variable(variable) => {
                if let Some(scope) = self.scopes.peek() {
                    if scope
                        .get(&variable.name.lexeme)
                        .is_some_and(VarStatus::is_undefined)
                    {
                        return Err(ResolutionError::new(
                            variable.name.clone(),
                            "Can't read local variable in its own initialiser.",
                        ));
                    }
                }

                self.resolve_expr_local(expr, &variable.name)
            }
            ExprContent::Assign(assign) => {
                self.resolve_expr(&assign.value)?;
                self.resolve_expr_local(expr, &assign.name)?;
                Ok(())
            }
            ExprContent::Binary(binary) => {
                self.resolve_expr(&binary.left)?;
                self.resolve_expr(&binary.right)?;
                Ok(())
            }
            ExprContent::Grouping(grouping) => self.resolve_expr(&grouping.expr),
            ExprContent::Literal(_) => Ok(()),
            ExprContent::Unary(unary) => self.resolve_expr(&unary.expr),
            ExprContent::Logical(logical) => {
                self.resolve_expr(&logical.left)?;
                self.resolve_expr(&logical.right)?;
                Ok(())
            }
            ExprContent::Call(call) => {
                self.resolve_expr(&call.callee)?;
                for arg in &call.args {
                    self.resolve_expr(arg)?;
                }
                Ok(())
            }
        }
    }

    fn resolve_expr_local(&mut self, expr: &Expr, name: &Token) -> Result<()> {
        for (i, scope) in self.scopes.0.iter_mut().rev().enumerate() {
            if scope.contains_key(&name.lexeme) {
                self.interpreter.resolve_at_depth(expr, i)?;
            }
        }
        // We can kick the can down the road here since a final check of the global namespace will catch any
        // undeclared variables.
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.peek_mut() {
            scope.insert(name.lexeme.clone(), VarStatus::Undefined);
        }
    }

    fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.peek_mut() {
            scope.insert(name.lexeme.clone(), VarStatus::Defined);
        }
    }
}

#[derive(Clone, Debug)]
pub struct ResolutionError {
    token: Token,
    message: String,
}

impl std::fmt::Display for ResolutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[line {}] ResolutionError at '{}': {}",
            self.token.line, self.token.lexeme, self.message
        )
    }
}

impl ResolutionError {
    fn new(token: Token, message: &str) -> Self {
        ResolutionError {
            token,
            message: message.to_string(),
        }
    }
}

impl std::error::Error for ResolutionError {}

pub type Result<T> = std::result::Result<T, ResolutionError>;

impl From<RuntimeException> for ResolutionError {
    fn from(value: RuntimeException) -> Self {
        match value {
            RuntimeException::RuntimeError(runtime_error) => ResolutionError {
                token: runtime_error.token,
                message: runtime_error.message,
            },
            RuntimeException::ReturnValue(_) => {
                unreachable!("Never convert return value to a ResolutionError.")
            }
        }
    }
}
