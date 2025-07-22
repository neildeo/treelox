use std::collections::HashMap;

use crate::token::{LiteralValue, Token, TokenType};
use itertools::Itertools;

/// Scan source string
///
/// # Returns
/// A tuple where the first item is the vector of scanned tokens, and the second
/// is a bool saying whether any errors were encountered.
pub fn scan(source: String) -> (Vec<Token>, bool) {
    let mut tokens = Vec::new();
    let mut source_stream = source.chars().multipeek();
    let mut line = 1usize;
    let mut had_error = false;

    let mut keywords = HashMap::new();
    keywords.insert("and", TokenType::And);
    keywords.insert("class", TokenType::Class);
    keywords.insert("else", TokenType::Else);
    keywords.insert("false", TokenType::False);
    keywords.insert("for", TokenType::For);
    keywords.insert("fun", TokenType::Fun);
    keywords.insert("if", TokenType::If);
    keywords.insert("nil", TokenType::Nil);
    keywords.insert("or", TokenType::Or);
    keywords.insert("print", TokenType::Print);
    keywords.insert("return", TokenType::Return);
    keywords.insert("super", TokenType::Super);
    keywords.insert("this", TokenType::This);
    keywords.insert("true", TokenType::True);
    keywords.insert("var", TokenType::Var);
    keywords.insert("while", TokenType::While);

    while let Some(c) = source_stream.next() {
        match c {
            // Simple single-character tokens
            '(' => tokens.push(Token::new(
                TokenType::LeftParen,
                String::from('('),
                LiteralValue::Null,
                line,
            )),
            ')' => tokens.push(Token::new(
                TokenType::RightParen,
                String::from(')'),
                LiteralValue::Null,
                line,
            )),
            '{' => tokens.push(Token::new(
                TokenType::LeftBrace,
                String::from('{'),
                LiteralValue::Null,
                line,
            )),
            '}' => tokens.push(Token::new(
                TokenType::RightBrace,
                String::from('}'),
                LiteralValue::Null,
                line,
            )),
            ',' => tokens.push(Token::new(
                TokenType::Comma,
                String::from(','),
                LiteralValue::Null,
                line,
            )),
            '.' => tokens.push(Token::new(
                TokenType::Dot,
                String::from('.'),
                LiteralValue::Null,
                line,
            )),
            '-' => tokens.push(Token::new(
                TokenType::Minus,
                String::from('-'),
                LiteralValue::Null,
                line,
            )),
            '+' => tokens.push(Token::new(
                TokenType::Plus,
                String::from('+'),
                LiteralValue::Null,
                line,
            )),
            ';' => tokens.push(Token::new(
                TokenType::Semicolon,
                String::from(';'),
                LiteralValue::Null,
                line,
            )),
            '*' => tokens.push(Token::new(
                TokenType::Star,
                String::from('*'),
                LiteralValue::Null,
                line,
            )),

            // Single- or double-character tokens, inc. comments
            '!' => match source_stream.peek() {
                Some('=') => {
                    source_stream.next();
                    tokens.push(Token::new(
                        TokenType::BangEqual,
                        String::from("!="),
                        LiteralValue::Null,
                        line,
                    ));
                }
                _ => {
                    tokens.push(Token::new(
                        TokenType::Bang,
                        String::from("!"),
                        LiteralValue::Null,
                        line,
                    ));
                }
            },
            '=' => match source_stream.peek() {
                Some('=') => {
                    source_stream.next();
                    tokens.push(Token::new(
                        TokenType::EqualEqual,
                        String::from("=="),
                        LiteralValue::Null,
                        line,
                    ));
                }
                _ => {
                    tokens.push(Token::new(
                        TokenType::Equal,
                        String::from("="),
                        LiteralValue::Null,
                        line,
                    ));
                }
            },
            '<' => match source_stream.peek() {
                Some('=') => {
                    source_stream.next();
                    tokens.push(Token::new(
                        TokenType::LessEqual,
                        String::from("<="),
                        LiteralValue::Null,
                        line,
                    ));
                }
                _ => {
                    tokens.push(Token::new(
                        TokenType::Less,
                        String::from("<"),
                        LiteralValue::Null,
                        line,
                    ));
                }
            },
            '>' => match source_stream.peek() {
                Some('=') => {
                    source_stream.next();
                    tokens.push(Token::new(
                        TokenType::GreaterEqual,
                        String::from(">="),
                        LiteralValue::Null,
                        line,
                    ));
                }
                _ => {
                    tokens.push(Token::new(
                        TokenType::Greater,
                        String::from(">"),
                        LiteralValue::Null,
                        line,
                    ));
                }
            },
            '/' => match source_stream.peek() {
                Some('/') => {
                    source_stream.reset_peek();
                    // Comment: consume chars until newline
                    while source_stream.peek().is_some_and(|c| *c != '\n') {
                        source_stream.next();
                    }
                    source_stream.reset_peek();
                }
                _ => {
                    tokens.push(Token::new(
                        TokenType::Slash,
                        String::from("/"),
                        LiteralValue::Null,
                        line,
                    ));
                }
            },

            // Whitespace
            c if c.is_whitespace() => {
                if c == '\n' {
                    line += 1;
                }
            }

            // String literals
            '"' => {
                let mut lexeme = String::from('"');
                while source_stream.peek().is_some_and(|c| *c != '"') {
                    let next_ch = source_stream.next().unwrap();
                    if next_ch == '\n' {
                        line += 1;
                    }
                    lexeme.push(next_ch);
                }

                source_stream.reset_peek();

                if source_stream.peek().is_none() {
                    eprintln!("[line {line}] Error: Unexpected EOF.");
                    had_error = true;
                } else {
                    // Consume the last '"'
                    source_stream.next();
                    lexeme.push('"');
                    let literal = lexeme.trim_matches('"').to_string();
                    // Push token
                    tokens.push(Token::new(
                        TokenType::String,
                        lexeme,
                        LiteralValue::from(literal),
                        line,
                    ));
                }
            }

            // Number literals
            c if c.is_ascii_digit() => {
                let mut lexeme = String::from(c);
                while source_stream.peek().is_some_and(|c| c.is_ascii_digit()) {
                    lexeme.push(source_stream.next().unwrap());
                }

                source_stream.reset_peek();

                if source_stream.peek() == Some(&'.')
                    && source_stream.peek().is_some_and(|c| c.is_ascii_digit())
                {
                    lexeme.push(source_stream.next().unwrap());
                    while source_stream.peek().is_some_and(|c| c.is_ascii_digit()) {
                        lexeme.push(source_stream.next().unwrap());
                    }
                }

                let literal = match lexeme.parse::<f64>() {
                    Ok(x) => x,
                    Err(e) => {
                        eprintln!("[line {line}] Error: {e}");
                        had_error = true;
                        continue;
                    }
                };

                tokens.push(Token::new(
                    TokenType::Number,
                    lexeme,
                    LiteralValue::from(literal),
                    line,
                ));
            }

            // Identifiers and reserved words
            c if c.is_ascii_alphabetic() || c == '_' => {
                let mut lexeme = String::from(c);
                while source_stream.peek().is_some_and(is_ident_char) {
                    lexeme.push(source_stream.next().unwrap());
                }
                source_stream.reset_peek();

                if keywords.contains_key(lexeme.as_str()) {
                    let token_type = keywords.get(lexeme.as_str()).unwrap().clone();
                    tokens.push(Token::new(token_type, lexeme, LiteralValue::Null, line));
                } else {
                    tokens.push(Token::new(
                        TokenType::Identifier,
                        lexeme,
                        LiteralValue::Null,
                        line,
                    ));
                }
            }

            // Unrecognised
            _ => {
                eprintln!("[line {line}] Error: Unexpected character: {c}");
                had_error = true;
            }
        }
    }

    tokens.push(Token::new(
        TokenType::EOF,
        String::new(),
        LiteralValue::Null,
        line,
    ));

    (tokens, had_error)
}

fn is_ident_char(c: &char) -> bool {
    c.is_ascii_alphanumeric() || *c == '_'
}
