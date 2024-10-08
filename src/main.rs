use std::fs;

use clap::Parser;

use crate::parser::Compiler;

mod ast;
mod parser;
mod bytecode;
mod bytecode_compiler;
mod chunk;
mod debug;
mod scanner;
mod utils;
mod value;

#[derive(Parser, Debug)]
struct Args {
    input: String,
}

fn read_file(file_name: &String) -> String {
    match fs::read_to_string(file_name) {
        Ok(input) => input,
        Err(err) => {
            panic!("Error reading {}: {}", file_name, err);
        }
    }
}

fn main() {
    let args = Args::parse();

    let file_name = args.input;

    let source = read_file(&file_name);

    let mut compiler = Compiler::default();

    let _ = compiler.compile(&source);
}
