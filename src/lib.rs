mod environment;
mod expr;
mod interpreter;
mod lox_callable;
mod parser;
mod resolver;
mod scanner;
mod stmt;
mod token;
mod value;

use std::{
    fs,
    io::{self, Write},
    path::Path,
    process::ExitCode,
};

use crate::{interpreter::Interpreter, resolver::Resolver};

pub fn repl() -> ExitCode {
    let mut interpreter = Interpreter::new();

    loop {
        let mut line = String::new();
        io::stdout().write_all("> ".as_bytes()).unwrap();
        io::stdout().flush().unwrap();
        match io::stdin().read_line(&mut line) {
            Ok(_) => {
                if line.is_empty() {
                    println!("Exiting REPL...");
                    break;
                }

                let (tokens, had_error) = scanner::scan(line);
                if had_error {
                    continue;
                }

                let (stmts, had_error) = parser::parse(tokens);
                if had_error || stmts.is_empty() {
                    continue;
                }

                let mut resolver = Resolver::new(&mut interpreter);
                let had_error = resolver.resolve(&stmts);
                if had_error {
                    continue;
                }

                match interpreter.interpret(&stmts) {
                    Ok(maybe_val) => {
                        if let Some(val) = maybe_val {
                            println!("{val}");
                        }
                    }
                    Err(e) => {
                        println!("{}", e);
                        continue;
                    }
                }
            }
            Err(_) => {
                println!("Error in REPL read_line");
                return ExitCode::from(74);
            }
        }
    }

    ExitCode::SUCCESS
}

pub fn interpret_file(filename: &Path) -> ExitCode {
    let mut interpreter = Interpreter::new();

    let file_contents = match fs::read_to_string(filename) {
        Ok(s) => s,
        Err(_) => {
            eprintln!("Failed to read file {}", filename.display());
            return ExitCode::from(65);
        }
    };

    let (tokens, had_error) = scanner::scan(file_contents);
    if had_error {
        return ExitCode::from(65);
    }

    let (stmts, had_error) = parser::parse(tokens);
    if had_error || stmts.is_empty() {
        return ExitCode::from(65);
    }

    let mut resolver = Resolver::new(&mut interpreter);
    let had_error = resolver.resolve(&stmts);
    if had_error {
        return ExitCode::from(70);
    }

    match interpreter.interpret(&stmts) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{}", e);
            return ExitCode::from(70);
        }
    }

    ExitCode::SUCCESS
}
