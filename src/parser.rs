use crate::expr::{Assign, Binary, Call, Expr, Grouping, Literal, Logical, Unary, Variable};
use crate::stmt::{Block, Expression, Function, If, Print, Return, Stmt, Var, While};
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

pub fn parse(tokens: Vec<Token>) -> Vec<Stmt> {
    let mut tokens = tokens.into_iter().peekable();
    let mut statements = Vec::new();

    while let Some(token) = tokens.peek() {
        if token.token_type == TokenType::EOF {
            break;
        }

        match declaration(&mut tokens) {
            Ok(stmt) => statements.push(stmt),
            Err(e) => eprintln!("{}", e.clone()),
        }
    }

    statements
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

fn primary(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr> {
    let t = tokens.next().ok_or(ParseError::unexpected_eof())?;
    let result: Result<Expr> = match t.token_type {
        TokenType::True => Ok(Expr::Literal(Literal::new(Value::True))),
        TokenType::False => Ok(Expr::Literal(Literal::new(Value::False))),
        TokenType::Nil => Ok(Expr::Literal(Literal::new(Value::Nil))),
        TokenType::String => match String::try_from(t.literal.clone()) {
            Ok(s) => Ok(Expr::Literal(Literal::new(Value::String(s)))),
            Err(e) => Err(ParseError::new(t, &e)),
        },
        TokenType::Number => match f64::try_from(t.literal.clone()) {
            Ok(x) => Ok(Expr::Literal(Literal::new(Value::Number(x)))),
            Err(e) => Err(ParseError::new(t, &e)),
        },
        TokenType::LeftParen => {
            let expr = expression(tokens)?;
            match tokens.next() {
                Some(t) if t.token_type == TokenType::RightParen => {
                    Ok(Expr::Grouping(Grouping::new(expr)))
                }
                Some(t) => Err(ParseError::new(t, "Expected closing parenthesis.")),
                None => Err(ParseError::unexpected_eof()),
            }
        }
        TokenType::Identifier => Ok(Expr::Variable(Variable::new(t))),
        _ => Err(ParseError::new(t, "Unexpected token in expression")),
    };

    if let Err(e) = &result {
        eprintln!("{}", e.clone());
    }

    result
}

fn call(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr> {
    let mut expr = primary(tokens)?;

    loop {
        if check_next(tokens, TokenType::LeftParen) {
            // Consume the left paren
            tokens.next();
            expr = finish_call(tokens, expr)?;
        } else {
            break;
        }
    }

    Ok(expr)
}

fn finish_call(tokens: &mut Peekable<impl Iterator<Item = Token>>, callee: Expr) -> Result<Expr> {
    let mut args = Vec::new();

    if tokens
        .peek()
        .is_some_and(|t| t.token_type != TokenType::RightParen)
    {
        args.push(expression(tokens)?);
        while check_next(tokens, TokenType::Comma) {
            if args.len() >= 255 {
                return Err(ParseError::new(
                    tokens.next().unwrap(),
                    "Can't have more than 255 arguments.",
                ));
            }
            tokens.next();
            args.push(expression(tokens)?);
        }
    }

    let paren = consume(tokens, TokenType::RightParen, "Expect ')' after arguments.")?;

    Ok(Expr::Call(Call::new(callee, paren, args)))
}

fn unary(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr> {
    if tokens
        .peek()
        .is_some_and(|t| t.token_type == TokenType::Bang || t.token_type == TokenType::Minus)
    {
        let operator = tokens.next().unwrap();
        let expr = unary(tokens)?;
        return Ok(Expr::Unary(Unary::new(operator, expr)));
    }

    call(tokens)
}

fn factor(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr> {
    let mut left = unary(tokens)?;

    while tokens
        .peek()
        .is_some_and(|t| t.token_type == TokenType::Star || t.token_type == TokenType::Slash)
    {
        let operator = tokens.next().unwrap();
        let right = unary(tokens)?;
        left = Expr::Binary(Binary::new(left, operator, right))
    }

    Ok(left)
}

fn term(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr> {
    let mut left = factor(tokens)?;

    while tokens
        .peek()
        .is_some_and(|t| t.token_type == TokenType::Plus || t.token_type == TokenType::Minus)
    {
        let operator = tokens.next().unwrap();
        let right = factor(tokens)?;
        left = Expr::Binary(Binary::new(left, operator, right))
    }

    Ok(left)
}

fn comparison(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr> {
    let mut left = term(tokens)?;

    while tokens.peek().is_some_and(|t| {
        t.token_type == TokenType::Greater
            || t.token_type == TokenType::GreaterEqual
            || t.token_type == TokenType::Less
            || t.token_type == TokenType::LessEqual
    }) {
        let operator = tokens.next().unwrap();
        let right = term(tokens)?;
        left = Expr::Binary(Binary::new(left, operator, right))
    }

    Ok(left)
}

fn equality(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr> {
    let mut left = comparison(tokens)?;

    while tokens.peek().is_some_and(|t| {
        t.token_type == TokenType::BangEqual || t.token_type == TokenType::EqualEqual
    }) {
        let operator = tokens.next().unwrap();
        let right = comparison(tokens)?;
        left = Expr::Binary(Binary::new(left, operator, right));
    }

    Ok(left)
}

fn assignment(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr> {
    let expr = or(tokens)?;

    if check_next(tokens, TokenType::Equal) {
        let equals = tokens.next().unwrap();
        let value = assignment(tokens)?;

        if let Some(name) = expr.get_name() {
            return Ok(Expr::Assign(Assign::new(name, value)));
        }

        return Err(ParseError::new(equals, "Invalid assignment target."));
    }

    Ok(expr)
}

fn or(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr> {
    let mut expr = and(tokens)?;

    while check_next(tokens, TokenType::Or) {
        let operator = tokens.next().unwrap();
        let right = comparison(tokens)?;
        expr = Expr::Logical(Logical::new(expr, operator, right));
    }

    Ok(expr)
}

fn and(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr> {
    let mut expr = equality(tokens)?;

    while check_next(tokens, TokenType::And) {
        let operator = tokens.next().unwrap();
        let right = comparison(tokens)?;
        expr = Expr::Logical(Logical::new(expr, operator, right));
    }

    Ok(expr)
}

fn expression(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr> {
    assignment(tokens)
}

fn statement(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Stmt> {
    let t = tokens.peek().ok_or(ParseError::unexpected_eof())?;
    match t.token_type {
        TokenType::Print => {
            // Consume PRINT
            tokens.next();
            print_statement(tokens)
        }
        TokenType::LeftBrace => {
            // Consume LEFTBRACE
            tokens.next();
            Ok(Stmt::Block(Block::new(block(tokens)?)))
        }
        TokenType::If => {
            // Consume IF
            tokens.next();
            if_statement(tokens)
        }
        TokenType::While => {
            // Consume WHILE
            tokens.next();
            while_statement(tokens)
        }
        TokenType::For => {
            // Consume FOR
            tokens.next();
            for_statement(tokens)
        }
        TokenType::Return => return_statement(tokens),
        _ => expression_statement(tokens),
    }
}

fn for_statement(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Stmt> {
    consume(tokens, TokenType::LeftParen, "Expect '(' after 'for'.")?;

    let initialiser = if tokens
        .peek()
        .is_some_and(|t| t.token_type == TokenType::Semicolon)
    {
        tokens.next();
        None
    } else if check_next(tokens, TokenType::Var) {
        Some(var_declaration(tokens)?)
    } else {
        Some(expression_statement(tokens)?)
    };

    let condition = if check_next(tokens, TokenType::Semicolon) {
        None
    } else {
        Some(expression(tokens)?)
    };
    consume(
        tokens,
        TokenType::Semicolon,
        "Expect ';' after loop condition",
    )?;

    let increment = if check_next(tokens, TokenType::RightParen) {
        None
    } else {
        Some(expression(tokens)?)
    };
    consume(
        tokens,
        TokenType::RightParen,
        "Expect ')' after for clauses.",
    )?;

    let mut body = statement(tokens)?;

    if let Some(incr) = increment {
        body = Stmt::Block(Block::new(vec![
            body,
            Stmt::Expression(Expression::new(incr)),
        ]));
    }

    let condition = condition.unwrap_or(Expr::Literal(Literal::new(Value::True)));
    body = Stmt::While(While::new(condition, body));

    if let Some(init) = initialiser {
        body = Stmt::Block(Block::new(vec![init, body]));
    }

    Ok(body)
}

fn while_statement(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Stmt> {
    consume(tokens, TokenType::LeftParen, "Expect '(' after 'while'.")?;
    let condition = expression(tokens)?;
    consume(tokens, TokenType::RightParen, "Expect ')' after 'while'.")?;
    let body = statement(tokens)?;

    Ok(Stmt::While(While::new(condition, body)))
}

fn if_statement(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Stmt> {
    consume(tokens, TokenType::LeftParen, "Expect '(' after 'if'.")?;
    let condition = expression(tokens)?;
    consume(
        tokens,
        TokenType::RightParen,
        "Expect ')' after if condition.",
    )?;

    let body = statement(tokens)?;
    let else_stmt = if check_next(tokens, TokenType::Else) {
        tokens.next();
        Some(statement(tokens)?)
    } else {
        None
    };

    Ok(Stmt::If(If::new(condition, body, else_stmt)))
}

fn expression_statement(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Stmt> {
    let expr = expression(tokens)?;
    consume(tokens, TokenType::Semicolon, "Expect ';' after expression.")?;

    Ok(Stmt::Expression(Expression::new(expr)))
}

/// NOTE: this returns the raw list of statements, and delegates wrapping into a Block struct
/// to the calling method. This is so this function can also be used to parse function bodies.
fn block(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Vec<Stmt>> {
    let mut statements = Vec::new();

    while tokens
        .peek()
        .is_some_and(|t| !matches!(t.token_type, TokenType::RightBrace | TokenType::EOF))
    {
        statements.push(declaration(tokens)?);
    }

    consume(tokens, TokenType::RightBrace, "Expect '}' after block.")?;

    Ok(statements)
}

fn print_statement(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Stmt> {
    let expr = expression(tokens)?;
    consume(tokens, TokenType::Semicolon, "Expect ';' after expression.")?;

    Ok(Stmt::Print(Print::new(expr)))
}

fn return_statement(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Stmt> {
    //   private Stmt returnStatement() {
    //     Token keyword = previous();
    //     Expr value = null;
    //     if (!check(SEMICOLON)) {
    //       value = expression();
    //     }

    //     consume(SEMICOLON, "Expect ';' after return value.");
    //     return new Stmt.Return(keyword, value);
    //   }

    // Unwrap is safe since this function is called only when "RETURN" has
    // been detected and not yet consumed
    let token = tokens.next().unwrap();
    let value = if tokens
        .peek()
        .is_some_and(|t| t.token_type != TokenType::Semicolon)
    {
        expression(tokens)?
    } else {
        Expr::Literal(Literal::new(Value::Nil))
    };

    consume(
        tokens,
        TokenType::Semicolon,
        "Expect ';' after return value.",
    )?;

    Ok(Stmt::Return(Return::new(token, value)))
}

fn declaration(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Stmt> {
    if check_next(tokens, TokenType::Var) {
        match var_declaration(tokens) {
            Ok(v) => Ok(v),
            Err(e) => {
                println!("{}", &e);
                synchronise(tokens);
                Err(e)
            }
        }
    } else if check_next(tokens, TokenType::Fun) {
        match function_declaration(tokens, "function") {
            Ok(v) => Ok(v),
            Err(e) => {
                println!("{}", &e);
                synchronise(tokens);
                Err(e)
            }
        }
    } else {
        statement(tokens)
    }
}

fn var_declaration(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Stmt> {
    consume(tokens, TokenType::Var, "Expect VAR keyword.")?;

    let name = consume(tokens, TokenType::Identifier, "Expect variable name")?;
    let initialiser = if check_next(tokens, TokenType::Equal) {
        tokens.next();
        Some(expression(tokens)?)
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
    kind: &'static str,
) -> Result<Stmt> {
    // Consume FUN
    tokens.next();
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
    let body = block(tokens)?;

    Ok(Stmt::Function(Function::new(name, params, body)))
}

fn synchronise(tokens: &mut Peekable<impl Iterator<Item = Token>>) {
    tokens.next();
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
    }
}
