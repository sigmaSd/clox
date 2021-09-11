mod chunk;
use std::io::Write;
use std::path::Path;
use std::process;

use vm::InterpretError;
mod debug;
pub mod memory;
mod value;
mod vm;
use vm::VM;
mod compile;
mod scanner;

fn main() {
    unsafe {
        VM.init();
        let args: Vec<String> = std::env::args().skip(1).collect();
        match args.len() {
            0 => repl(),
            1 => run_file(&args[0]),
            _ => {
                eprintln!("Usage: clox [path]");
                process::exit(64);
            }
        }
        VM.free();
    }
}

unsafe fn run_file<P: AsRef<Path>>(path: P) {
    let path = path.as_ref();
    let source = std::fs::read_to_string(path).unwrap();
    let result = VM.interpret(&source);
    match result {
        Ok(()) => (),
        Err(InterpretError::CompileError) => process::exit(65),
        Err(InterpretError::RuntimError) => process::exit(70),
    }
}

unsafe fn repl() {
    let mut line = String::new();
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_line(&mut line).unwrap();
        let input = line.trim();
        if input.is_empty() {
            println!();
            break;
        }
        let _ = VM.interpret(input);
        line.clear();
    }
}
