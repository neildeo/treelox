mod expr;
mod parser;
mod scanner;
mod token;

use std::{fs, io, path::Path, process::ExitCode};

pub fn repl() -> ExitCode {
    loop {
        let mut line = String::new();
        match io::stdin().read_line(&mut line) {
            Ok(_) => {
                if line.is_empty() {
                    break;
                }

                let (tokens, _) = scanner::scan(line);
                for token in tokens {
                    println!("{token:?}");
                }
            }
            Err(_) => {
                eprintln!("Error in REPL read_line");
                return ExitCode::from(74);
            }
        }
    }

    ExitCode::SUCCESS
}

/// Scan source file
///
/// # Returns
/// bool which is true if any lexical errors were encountered
pub fn tokenize(filename: &Path) -> bool {
    let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        eprintln!("Failed to read file {}", filename.display());
        String::new()
    });

    let (tokens, had_error) = scanner::scan(file_contents);
    for token in tokens {
        println!("{token:?}");
    }

    had_error
}

pub fn parse(filename: &Path) -> bool {
    let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        eprintln!("Failed to read file {}", filename.display());
        String::new()
    });

    let (tokens, mut had_error) = scanner::scan(file_contents);

    match parser::parse(tokens) {
        Ok(expr) => println!("{expr}"),
        Err(e) => {
            eprintln!("{e}");
            had_error = true;
        }
    }

    had_error
}
