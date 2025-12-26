mod bytecode;
mod compiler;
mod lexer;
mod parser;
mod utils;
mod value;
mod vm;

use std::env;
use std::fs;
use compiler::Compiler;
use vm::VM;

use crate::bytecode::OpCode;
use crate::parser::Statement;
use crate::value::Value;
use crate::{lexer::{Lexer, Token}, parser::Parser};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        println!("Usage:");
        println!("  ngn run <file.ngn>    - Compile and run immediately");
        println!("  ngn build <file.ngn>  - Compile to bytecode file");
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    if filename.ends_with(".mod") {
        let bytes = fs::read(filename).expect("Could not read binary file");
        
        // Deserialize the (Instructions, Constants) tuple we saved earlier
        let (instructions, constants): (Vec<OpCode>, Vec<Value>) = 
            bincode::deserialize(&bytes).expect("Failed to deserialize bytecode");

        let mut vm = VM::new(instructions, constants);
        vm.run();
        return;
    }

    // 1. Load the source code
    let source = fs::read_to_string(filename)
        .expect(&format!("Could not read file: {}", filename));

    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer);
    let mut compiler = Compiler::new();

    // 1. COLLECT all statements into a list first
    let mut statements = Vec::new();
    while parser.current_token != Token::EOF {
        if parser.current_token == Token::Newline {
            parser.advance();
            continue;
        }
        statements.push(parser.parse_statement());
    }

    // 2. PASS ONE: Register all function names in the symbol table
    // (We don't compile them yet, just reserve their slots)
    for stmt in &statements {
        if let Statement::Function { name, .. } = stmt {
            let var_idx = compiler.next_index;
            compiler.symbol_table.insert(name.clone(), var_idx);
            compiler.next_index += 1;
        }
    }

    // 3. PASS TWO: Now compile the actual code
    // The compiler will now find 'greet' in the table even if it's called in 'main'
    for stmt in statements {
        compiler.compile_statement(stmt);
    }

    // 3. Command Logic
    match command.as_str() {
        "run" => {
            let mut final_instructions = compiler.instructions.clone();
            if let Some(&main_idx) = compiler.symbol_table.get("main") {
                final_instructions.push(bytecode::OpCode::Call(main_idx));
                final_instructions.push(bytecode::OpCode::Halt);
            } else {
                panic!("ngn Error: No main() function defined!");
            }
            
            let mut my_vm = VM::new(final_instructions, compiler.constants);
            my_vm.run();
        }
        "build" => {
            let output_filename = format!("{}.mod", filename.replace(".ngn", ""));
            let mut final_instructions = compiler.instructions.clone();
            
            // Add the bootstrap call to main
            if let Some(&main_idx) = compiler.symbol_table.get("main") {
                final_instructions.push(bytecode::OpCode::Call(main_idx));
                final_instructions.push(bytecode::OpCode::Halt);
            }

            // Package everything together
            let package = (final_instructions, compiler.constants);
            let bytes = bincode::serialize(&package).unwrap();
            fs::write(&output_filename, bytes).expect("Failed to write bytecode file");
            
            println!("Successfully built: {}", output_filename);
        }
        _ => println!("Unknown command: {}", command),
    }
}
