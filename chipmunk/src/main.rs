use std::{env::args, iter};

use squirrel_lang::context::IntoSquirrelErrorContext;

fn main() {
    let file_name = args()
        .nth(1)
        .expect("Must pass input filename as first argument");
    let contents = std::fs::read_to_string(&file_name).expect("Unable to read source file");
    let ast = squirrel_lang::parser::parse(&contents, file_name.clone());
    let ast = match ast {
        Ok(ast) => ast,
        Err(e) => {
            println!("Failed to parse file:\n{}", e);
            std::process::exit(1);
        }
    };
    match squirrel_lang::walker::runtime::run(&ast, &file_name, None, iter::empty()) {
        Ok(()) => {}
        Err(err) => {
            println!("Error: {}", err.with_context(&contents));
        }
    }
}
