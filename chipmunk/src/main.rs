use std::{env::args, iter};

use squirrel_lang::context::IntoSquirrelErrorContext;

const ENABLE_WALKER: bool = false;

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
    if ENABLE_WALKER {
        match squirrel_lang::walker::runtime::run(&ast, &file_name, None, iter::empty()) {
            Ok(()) => {}
            Err(err) => {
                println!("Error: {}", err.with_context(&contents));
            }
        }
    } else {
        let code = squirrel_lang::vm::compiler::compile_function(&ast);

        println!("{:#?}", code);
    }
}
