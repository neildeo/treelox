use crate::expr::{
    Assign, Binary, Call, Expr, ExprContent, ExprIdList, Get, Grouping, Literal, Logical, Set,
    This, Unary, Variable,
};
use crate::stmt::{Block, Class, Expression, Function, If, Print, Return, Stmt, Var, While};
use crate::token::{Token, TokenType};
use crate::value::Value;
use std::error::Error;
use std::fmt::Display;
use std::iter::Peekable;

#[derive(Clone, Debug)]
pub struct ParseError {
    token: Token,
    message: String,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[line {}] ParseError at '{}': {}",
            self.token.line, self.token.lexeme, self.message
        )
    }
}

impl ParseError {
    fn new(token: Token, message: &str) -> Self {
        ParseError {
            token,
            message: message.to_string(),
        }
    }

    fn unexpected_eof() -> Self {
        ParseError {
            token: Token::new(
                TokenType::EOF,
                String::from(""),
                crate::token::LiteralValue::Null,
                0,
            ),
            message: "Unexpected EOF.".to_string(),
        }
    }
}

impl Error for ParseError {}

pub type Result<T> = std::result::Result<T, ParseError>;

pub fn parse(tokens: Vec<Token>) -> (Vec<Stmt>, bool) {
    let mut tokens = tokens.into_iter().peekable();
    let mut statements = Vec::new();
    let mut expr_id_list = ExprIdList::new();
    let mut had_error = false;

    while let Some(token) = tokens.peek() {
        if token.token_type == TokenType::EOF {
            break;
        }

        match declaration(&mut tokens, &mut expr_id_list) {
            Ok(stmt) => statements.push(stmt),
            Err(e) => {
                eprintln!("{}", e);
                synchronise(&mut tokens);
                had_error = true;
            }
        }
    }

    (statements, had_error)
}

pub fn consume(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    token_type: TokenType,
    message: &str,
) -> Result<Token> {
    let token = tokens.next().unwrap();
    if TokenType::from(&token) != token_type {
        return Err(ParseError::new(token, message));
    }

    Ok(token)
}

pub fn check_next(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    token_type: TokenType,
) -> bool {
    tokens.peek().is_some_and(|t| t.token_type == token_type)
}

fn primary(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Expr> {
    let t = tokens.next().ok_or(ParseError::unexpected_eof())?;
    let result: Result<Expr> = match t.token_type {
        TokenType::True => Ok(Expr::new(
            ExprContent::Literal(Literal::new(Value::True)),
            expr_id_list,
        )),
        TokenType::False => Ok(Expr::new(
            ExprContent::Literal(Literal::new(Value::False)),
            expr_id_list,
        )),
        TokenType::Nil => Ok(Expr::new(
            ExprContent::Literal(Literal::new(Value::Nil)),
            expr_id_list,
        )),
        TokenType::String => match String::try_from(t.literal.clone()) {
            Ok(s) => Ok(Expr::new(
                ExprContent::Literal(Literal::new(Value::String(s))),
                expr_id_list,
            )),
            Err(e) => Err(ParseError::new(t, &e)),
        },
        TokenType::Number => match f64::try_from(t.literal.clone()) {
            Ok(x) => Ok(Expr::new(
                ExprContent::Literal(Literal::new(Value::Number(x))),
                expr_id_list,
            )),
            Err(e) => Err(ParseError::new(t, &e)),
        },
        TokenType::LeftParen => {
            let expr = expression(tokens, expr_id_list)?;
            match tokens.next() {
                Some(t) if t.token_type == TokenType::RightParen => Ok(Expr::new(
                    ExprContent::Grouping(Grouping::new(expr)),
                    expr_id_list,
                )),
                Some(t) => Err(ParseError::new(t, "Expected closing parenthesis.")),
                None => Err(ParseError::unexpected_eof()),
            }
        }
        TokenType::Identifier => Ok(Expr::new(
            ExprContent::Variable(Variable::new(t)),
            expr_id_list,
        )),
        TokenType::This => Ok(Expr::new(ExprContent::This(This::new(t)), expr_id_list)),
        _ => Err(ParseError::new(t, "Unexpected token in expression")),
    };

    if let Err(e) = &result {
        eprintln!("{}", e.clone());
    }

    result
}

fn call(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Expr> {
    let mut expr = primary(tokens, expr_id_list)?;

    loop {
        if check_next(tokens, TokenType::LeftParen) {
            // Consume the left paren
            tokens.next();
            expr = finish_call(tokens, expr_id_list, expr)?;
        } else if check_next(tokens, TokenType::Dot) {
            // Consume the dot
            tokens.next();
            let name = consume(
                tokens,
                TokenType::Identifier,
                "Expect property name after '.'.",
            )?;
            expr = Expr::new(ExprContent::Get(Get::new(name, expr)), expr_id_list);
        } else {
            break;
        }
    }

    Ok(expr)
}

fn finish_call(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
    callee: Expr,
) -> Result<Expr> {
    let mut args = Vec::new();

    if tokens
        .peek()
        .is_some_and(|t| t.token_type != TokenType::RightParen)
    {
        args.push(expression(tokens, expr_id_list)?);
        while check_next(tokens, TokenType::Comma) {
            if args.len() >= 255 {
                return Err(ParseError::new(
                    tokens.next().unwrap(),
                    "Can't have more than 255 arguments.",
                ));
            }
            tokens.next();
            args.push(expression(tokens, expr_id_list)?);
        }
    }

    let paren = consume(tokens, TokenType::RightParen, "Expect ')' after arguments.")?;

    Ok(Expr::new(
        ExprContent::Call(Call::new(callee, paren, args)),
        expr_id_list,
    ))
}

fn unary(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Expr> {
    if tokens
        .peek()
        .is_some_and(|t| t.token_type == TokenType::Bang || t.token_type == TokenType::Minus)
    {
        let operator = tokens.next().unwrap();
        let expr = unary(tokens, expr_id_list)?;
        return Ok(Expr::new(
            ExprContent::Unary(Unary::new(operator, expr)),
            expr_id_list,
        ));
    }

    call(tokens, expr_id_list)
}

fn factor(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Expr> {
    let mut left = unary(tokens, expr_id_list)?;

    while tokens
        .peek()
        .is_some_and(|t| t.token_type == TokenType::Star || t.token_type == TokenType::Slash)
    {
        let operator = tokens.next().unwrap();
        let right = unary(tokens, expr_id_list)?;
        left = Expr::new(
            ExprContent::Binary(Binary::new(left, operator, right)),
            expr_id_list,
        );
    }

    Ok(left)
}

fn term(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Expr> {
    let mut left = factor(tokens, expr_id_list)?;

    while tokens
        .peek()
        .is_some_and(|t| t.token_type == TokenType::Plus || t.token_type == TokenType::Minus)
    {
        let operator = tokens.next().unwrap();
        let right = factor(tokens, expr_id_list)?;
        left = Expr::new(
            ExprContent::Binary(Binary::new(left, operator, right)),
            expr_id_list,
        );
    }

    Ok(left)
}

fn comparison(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Expr> {
    let mut left = term(tokens, expr_id_list)?;

    while tokens.peek().is_some_and(|t| {
        t.token_type == TokenType::Greater
            || t.token_type == TokenType::GreaterEqual
            || t.token_type == TokenType::Less
            || t.token_type == TokenType::LessEqual
    }) {
        let operator = tokens.next().unwrap();
        let right = term(tokens, expr_id_list)?;
        left = Expr::new(
            ExprContent::Binary(Binary::new(left, operator, right)),
            expr_id_list,
        );
    }

    Ok(left)
}

fn equality(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Expr> {
    let mut left = comparison(tokens, expr_id_list)?;

    while tokens.peek().is_some_and(|t| {
        t.token_type == TokenType::BangEqual || t.token_type == TokenType::EqualEqual
    }) {
        let operator = tokens.next().unwrap();
        let right = comparison(tokens, expr_id_list)?;
        left = Expr::new(
            ExprContent::Binary(Binary::new(left, operator, right)),
            expr_id_list,
        );
    }

    Ok(left)
}

fn assignment(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Expr> {
    let expr = or(tokens, expr_id_list)?;

    if check_next(tokens, TokenType::Equal) {
        let equals = tokens.next().unwrap();
        let value = assignment(tokens, expr_id_list)?;

        if let Some(name) = expr.content.get_name() {
            return Ok(Expr::new(
                ExprContent::Assign(Assign::new(name, value)),
                expr_id_list,
            ));
        } else if let ExprContent::Get(get) = expr.content {
            return Ok(Expr::new(
                ExprContent::Set(Set::new(get.name, *get.object, value)),
                expr_id_list,
            ));
        }

        return Err(ParseError::new(equals, "Invalid assignment target."));
    }

    Ok(expr)
}

fn or(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Expr> {
    let mut expr = and(tokens, expr_id_list)?;

    while check_next(tokens, TokenType::Or) {
        let operator = tokens.next().unwrap();
        let right = comparison(tokens, expr_id_list)?;
        expr = Expr::new(
            ExprContent::Logical(Logical::new(expr, operator, right)),
            expr_id_list,
        );
    }

    Ok(expr)
}

fn and(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Expr> {
    let mut expr = equality(tokens, expr_id_list)?;

    while check_next(tokens, TokenType::And) {
        let operator = tokens.next().unwrap();
        let right = comparison(tokens, expr_id_list)?;
        expr = Expr::new(
            ExprContent::Logical(Logical::new(expr, operator, right)),
            expr_id_list,
        );
    }

    Ok(expr)
}

fn expression(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Expr> {
    assignment(tokens, expr_id_list)
}

fn statement(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Stmt> {
    let t = tokens.peek().ok_or(ParseError::unexpected_eof())?;
    match t.token_type {
        TokenType::Print => {
            // Consume PRINT
            tokens.next();
            print_statement(tokens, expr_id_list)
        }
        TokenType::LeftBrace => {
            // Consume LEFTBRACE
            tokens.next();
            Ok(Stmt::Block(Block::new(block(tokens, expr_id_list)?)))
        }
        TokenType::If => {
            // Consume IF
            tokens.next();
            if_statement(tokens, expr_id_list)
        }
        TokenType::While => {
            // Consume WHILE
            tokens.next();
            while_statement(tokens, expr_id_list)
        }
        TokenType::For => {
            // Consume FOR
            tokens.next();
            for_statement(tokens, expr_id_list)
        }
        TokenType::Return => return_statement(tokens, expr_id_list),
        _ => expression_statement(tokens, expr_id_list),
    }
}

fn for_statement(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Stmt> {
    consume(tokens, TokenType::LeftParen, "Expect '(' after 'for'.")?;

    let initialiser = if tokens
        .peek()
        .is_some_and(|t| t.token_type == TokenType::Semicolon)
    {
        tokens.next();
        None
    } else if check_next(tokens, TokenType::Var) {
        Some(var_declaration(tokens, expr_id_list)?)
    } else {
        Some(expression_statement(tokens, expr_id_list)?)
    };

    let condition = if check_next(tokens, TokenType::Semicolon) {
        None
    } else {
        Some(expression(tokens, expr_id_list)?)
    };
    consume(
        tokens,
        TokenType::Semicolon,
        "Expect ';' after loop condition",
    )?;

    let increment = if check_next(tokens, TokenType::RightParen) {
        None
    } else {
        Some(expression(tokens, expr_id_list)?)
    };
    consume(
        tokens,
        TokenType::RightParen,
        "Expect ')' after for clauses.",
    )?;

    let mut body = statement(tokens, expr_id_list)?;

    if let Some(incr) = increment {
        body = Stmt::Block(Block::new(vec![
            body,
            Stmt::Expression(Expression::new(incr)),
        ]));
    }

    let condition = condition.unwrap_or(Expr::new(
        ExprContent::Literal(Literal::new(Value::True)),
        expr_id_list,
    ));
    body = Stmt::While(While::new(condition, body));

    if let Some(init) = initialiser {
        body = Stmt::Block(Block::new(vec![init, body]));
    }

    Ok(body)
}

fn while_statement(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Stmt> {
    consume(tokens, TokenType::LeftParen, "Expect '(' after 'while'.")?;
    let condition = expression(tokens, expr_id_list)?;
    consume(tokens, TokenType::RightParen, "Expect ')' after 'while'.")?;
    let body = statement(tokens, expr_id_list)?;

    Ok(Stmt::While(While::new(condition, body)))
}

fn if_statement(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Stmt> {
    consume(tokens, TokenType::LeftParen, "Expect '(' after 'if'.")?;
    let condition = expression(tokens, expr_id_list)?;
    consume(
        tokens,
        TokenType::RightParen,
        "Expect ')' after if condition.",
    )?;

    let body = statement(tokens, expr_id_list)?;
    let else_stmt = if check_next(tokens, TokenType::Else) {
        tokens.next();
        Some(statement(tokens, expr_id_list)?)
    } else {
        None
    };

    Ok(Stmt::If(If::new(condition, body, else_stmt)))
}

fn expression_statement(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Stmt> {
    let expr = expression(tokens, expr_id_list)?;
    consume(tokens, TokenType::Semicolon, "Expect ';' after expression.")?;

    Ok(Stmt::Expression(Expression::new(expr)))
}

/// NOTE: this returns the raw list of statements, and delegates wrapping into a Block struct
/// to the calling method. This is so this function can also be used to parse function bodies.
fn block(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Vec<Stmt>> {
    let mut statements = Vec::new();

    while tokens
        .peek()
        .is_some_and(|t| !matches!(t.token_type, TokenType::RightBrace | TokenType::EOF))
    {
        statements.push(declaration(tokens, expr_id_list)?);
    }

    consume(tokens, TokenType::RightBrace, "Expect '}' after block.")?;

    Ok(statements)
}

fn print_statement(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Stmt> {
    let expr = expression(tokens, expr_id_list)?;
    consume(tokens, TokenType::Semicolon, "Expect ';' after expression.")?;

    Ok(Stmt::Print(Print::new(expr)))
}

fn return_statement(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Stmt> {
    // Unwrap is safe since this function is called only when "RETURN" has
    // been detected and not yet consumed
    let token = tokens.next().unwrap();
    let value = if tokens
        .peek()
        .is_some_and(|t| t.token_type != TokenType::Semicolon)
    {
        expression(tokens, expr_id_list)?
    } else {
        Expr::new(ExprContent::Literal(Literal::new(Value::Nil)), expr_id_list)
    };

    consume(
        tokens,
        TokenType::Semicolon,
        "Expect ';' after return value.",
    )?;

    Ok(Stmt::Return(Return::new(token, value)))
}

fn declaration(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Stmt> {
    if check_next(tokens, TokenType::Var) {
        var_declaration(tokens, expr_id_list)
    } else if check_next(tokens, TokenType::Fun) {
        // Consume FUN
        tokens.next();
        function_declaration(tokens, expr_id_list, "function")
    } else if check_next(tokens, TokenType::Class) {
        class_declaration(tokens, expr_id_list)
    } else {
        statement(tokens, expr_id_list)
    }
}

fn var_declaration(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Stmt> {
    consume(tokens, TokenType::Var, "Expect VAR keyword.")?;

    let name = consume(tokens, TokenType::Identifier, "Expect variable name")?;
    let initialiser = if check_next(tokens, TokenType::Equal) {
        tokens.next();
        Some(expression(tokens, expr_id_list)?)
    } else {
        None
    };

    consume(
        tokens,
        TokenType::Semicolon,
        "Expect ';' after variable declaration.",
    )?;
    Ok(Stmt::Var(Var::new(name, initialiser)))
}

fn function_declaration(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
    kind: &'static str,
) -> Result<Stmt> {
    let name = consume(
        tokens,
        TokenType::Identifier,
        &format!("Expect {} name.", kind),
    )?;
    consume(
        tokens,
        TokenType::LeftParen,
        &format!("Expect '(' after {} name.", kind),
    )?;

    let mut params = Vec::new();
    if !check_next(tokens, TokenType::RightParen) {
        params.push(consume(
            tokens,
            TokenType::Identifier,
            "Expect parameter name.",
        )?);
        while check_next(tokens, TokenType::Comma) {
            tokens.next();
            if params.len() >= 255 {
                return Err(ParseError::new(
                    tokens.next().unwrap(),
                    "Can't have more than 255 parameters.",
                ));
            }
            params.push(consume(
                tokens,
                TokenType::Identifier,
                "Expect parameter name.",
            )?);
        }
    }
    consume(
        tokens,
        TokenType::RightParen,
        "Expect ')' after parameters.",
    )?;

    consume(
        tokens,
        TokenType::LeftBrace,
        &format!("Expect '{{' before {} body.", kind),
    )?;
    let body = block(tokens, expr_id_list)?;

    Ok(Stmt::Function(Function::new(name, params, body)))
}

fn class_declaration(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expr_id_list: &mut ExprIdList,
) -> Result<Stmt> {
    // Consume CLASS
    tokens.next();

    let name = consume(tokens, TokenType::Identifier, "Expect class name.")?;
    consume(
        tokens,
        TokenType::LeftBrace,
        "Expect '{' before class body.",
    )?;

    let mut methods = Vec::new();
    while !check_next(tokens, TokenType::RightBrace) && !check_next(tokens, TokenType::EOF) {
        methods.push(function_declaration(tokens, expr_id_list, "method")?);
    }

    consume(
        tokens,
        TokenType::RightBrace,
        "Expect '}' after class body.",
    )?;

    Ok(Stmt::Class(Class::new(name, methods)))
}

fn synchronise(tokens: &mut Peekable<impl Iterator<Item = Token>>) {
    while let Some(t) = tokens.peek() {
        if t.token_type == TokenType::Semicolon {
            tokens.next();
            return;
        }

        if matches!(
            t.token_type,
            TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return
        ) {
            return;
        }

        tokens.next();
    }
}
