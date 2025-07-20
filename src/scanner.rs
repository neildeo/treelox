use std::collections::HashMap;

use crate::token::Token;
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
    keywords.insert("and", Token::And);
    keywords.insert("class", Token::Class);
    keywords.insert("else", Token::Else);
    keywords.insert("false", Token::False);
    keywords.insert("for", Token::For);
    keywords.insert("fun", Token::Fun);
    keywords.insert("if", Token::If);
    keywords.insert("nil", Token::Nil);
    keywords.insert("or", Token::Or);
    keywords.insert("print", Token::Print);
    keywords.insert("return", Token::Return);
    keywords.insert("super", Token::Super);
    keywords.insert("this", Token::This);
    keywords.insert("true", Token::True);
    keywords.insert("var", Token::Var);
    keywords.insert("while", Token::While);

    while let Some(c) = source_stream.next() {
        match c {
            // Simple single-character tokens
            '(' => tokens.push(Token::LeftParen),
            ')' => tokens.push(Token::RightParen),
            '{' => tokens.push(Token::LeftBrace),
            '}' => tokens.push(Token::RightBrace),
            ',' => tokens.push(Token::Comma),
            '.' => tokens.push(Token::Dot),
            '-' => tokens.push(Token::Minus),
            '+' => tokens.push(Token::Plus),
            ';' => tokens.push(Token::Semicolon),
            '*' => tokens.push(Token::Star),

            // Single- or double-character tokens, inc. comments
            '!' => match source_stream.peek() {
                Some('=') => {
                    source_stream.next();
                    tokens.push(Token::BangEqual);
                }
                _ => {
                    tokens.push(Token::Bang);
                }
            },
            '=' => match source_stream.peek() {
                Some('=') => {
                    source_stream.next();
                    tokens.push(Token::EqualEqual);
                }
                _ => {
                    tokens.push(Token::Equal);
                }
            },
            '<' => match source_stream.peek() {
                Some('=') => {
                    source_stream.next();
                    tokens.push(Token::LessEqual);
                }
                _ => {
                    tokens.push(Token::Less);
                }
            },
            '>' => match source_stream.peek() {
                Some('=') => {
                    source_stream.next();
                    tokens.push(Token::GreaterEqual);
                }
                _ => {
                    tokens.push(Token::Greater);
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
                    tokens.push(Token::Slash);
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
                    eprintln!("[line {line}] Error: Unterminated string.");
                    had_error = true;
                } else {
                    // Consume the last '"'
                    source_stream.next();
                    lexeme.push('"');
                    let literal = lexeme.trim_matches('"').to_string();
                    // Push token
                    tokens.push(Token::String(lexeme, literal));
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

                let literal = lexeme.parse::<f64>().unwrap();

                tokens.push(Token::Number(lexeme, literal));
            }

            // Identifiers and reserved words
            c if c.is_ascii_alphabetic() || c == '_' => {
                let mut lexeme = String::from(c);
                while source_stream.peek().is_some_and(is_ident_char) {
                    lexeme.push(source_stream.next().unwrap());
                }
                source_stream.reset_peek();

                if keywords.contains_key(lexeme.as_str()) {
                    tokens.push(keywords.get(lexeme.as_str()).unwrap().clone());
                } else {
                    tokens.push(Token::Identifier(lexeme));
                }
            }

            // Unrecognised
            _ => {
                eprintln!("[line {line}] Error: Unexpected character: {c}");
                had_error = true;
            }
        }
    }

    tokens.push(Token::EOF);

    (tokens, had_error)
}

fn is_ident_char(c: &char) -> bool {
    c.is_ascii_alphanumeric() || *c == '_'
}
