use std::env::args;

fn main() {
    let file_name = args().nth(1).expect("Must pass input filename as first argument");
    println!("Parsing file: {}", file_name);
    let contents = std::fs::read_to_string(&file_name).expect("Unable to read source file");
    let ast = squirrel_lang::parser::parse(&contents, file_name);
    match ast {
        Ok(ast) => println!("Successfully parsed file: {:#?}", ast),
        Err(e) => {
            println!("Failed to parse file:\n{}", e);
            std::process::exit(1);
        }
    }
}
