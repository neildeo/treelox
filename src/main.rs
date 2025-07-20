use std::env;
use std::path::Path;
use std::process::ExitCode;

use treelox::interpret_file;
use treelox::repl;

// Book uses sysexits.h header exit codes
fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        // REPL mode
        repl()
    } else if args.len() == 2 {
        // Source file mode
        let filename = &args[1];
        interpret_file(Path::new(filename))
    } else {
        eprintln!("Usage: {} <filename>", args[0]);
        ExitCode::from(64)
    }
}
