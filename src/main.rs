use std::fs;

use clap::Parser;

mod scanner;

#[derive(Parser, Debug)]
struct Args {
    input: String,
}

fn read_file(file_name: &String) -> String{
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

    let compiled = scanner::scan_tokens(&source);

    println!("{:?}", compiled);
}
