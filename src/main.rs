use std::env;
use std::path::Path;
use std::process::ExitCode;

use treelox::{parse, repl, tokenize};

// Book uses sysexits.h header exit codes
fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        return repl();
    } else if args.len() >= 3 {
        let command = &args[1];
        let filename = &args[2];

        match command.as_str() {
            "tokenize" => {
                let had_error = tokenize(Path::new(filename));
                if had_error {
                    return ExitCode::from(65);
                }
            }
            "parse" => {
                let had_error = parse(Path::new(filename));
                if had_error {
                    return ExitCode::from(65);
                }
            }
            _ => {
                eprintln!("Unknown command: {}", command);
            }
        }
    } else {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}
