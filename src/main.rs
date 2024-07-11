pub mod compiler;
pub mod errors;
pub mod parser;
pub mod position;
pub mod scanner;
pub mod vm;
pub mod vmtypes;

use compiler::Compiler;
use errors::ZeptwoError;
use parser::Parser;
use std::io::{self, Write};
use vm::VM;

fn main() {
    std::env::set_var("RUST_BACKTRACE", "1");

    let args: Vec<String> = std::env::args().collect();
    match match args.len() {
        1 => repl(),
        2 => run_file(&args[1]),
        _ => Ok(eprintln!("Too many arguments provided.")),
    } {
        Ok(_) => {}
        Err(e) => eprintln!("{e}"),
    };
}

fn repl() -> Result<(), ZeptwoError> {
    loop {
        let mut line = String::new();
        print!(">");
        io::stdout().flush().unwrap();
        io::stdin()
            .read_line(&mut line)
            .expect("Something went wrong when reading input.");
        if line.is_empty() {
            break Ok(());
        }
        let mut parser = Parser::new(Box::leak(line.into_boxed_str()));
        parser.parse()?;
        let mut compiler = Compiler::new(parser.lookup);
        let bytecode = compiler.compile()?;
        bytecode.disassemble();
        let mut vm = VM::new(bytecode);
        vm.run()?;
    }
}

fn read_file_as_static(path: &str) -> Option<&'static str> {
    match std::fs::read_to_string(path) {
        Ok(contents) => Some(Box::leak(contents.into_boxed_str())),
        Err(_) => None,
    }
}

fn run_file(path: &str) -> Result<(), ZeptwoError> {
    let source: &'static str = match read_file_as_static(path) {
        Some(o) => o,
        None => {
            Err(ZeptwoError::Scanner("Could not read file.".to_string()))?
        }
    };
    let mut parser = Parser::new(source);
    parser.parse()?;
    let mut compiler = Compiler::new(parser.lookup);
    let bytecode = compiler.compile()?;
    bytecode.disassemble();
    let mut vm = VM::new(bytecode);
    vm.run()?;
    Ok(())
}

