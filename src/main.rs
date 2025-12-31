use ngn::compiler::Compiler;
use ngn::vm::VM;
use ngn::analyzer::Analyzer;
use ngn::lexer::{Lexer, Token};
use ngn::parser::Parser;
use ngn::parser::Statement;
use ngn::bytecode::OpCode;
use ngn::value::Value;
use std::env;
use std::fs;
use std::io::{Read, Seek, SeekFrom};

fn main() {
    // Check for "Self-Running" mode
    if let Some((instructions, constants)) = check_for_embedded_bytecode() {
        let mut vm = VM::new(instructions, constants, 0);
        vm.run();
        return;
    }

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

        let mut vm = VM::new(instructions, constants, 0);
        vm.run();
        return;
    }

    // 1. Load the source code
    let source = fs::read_to_string(filename)
        .expect(&format!("Could not read file: {}", filename));

    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer);
    let mut compiler = Compiler::new();
    compiler.inject_builtins();

    // 1. COLLECT all statements into a list first
    let mut statements = Vec::new();
    while parser.current_token != Token::EOF {
        if parser.current_token == Token::Newline {
            parser.advance();
            continue;
        }
        statements.push(parser.parse_statement());
    }
    
    // 2. Semantic Analysis (Static Type Checking)
    let mut analyzer = Analyzer::new();
    if let Err(errors) = analyzer.analyze(&statements) {
        for err in errors {
            eprintln!("{}", err);
        }
        std::process::exit(1);
    }

    // 2. PASS ONE: Register all function names in the symbol table
    // (We don't compile them yet, just reserve their slots)
    for stmt in &statements {
        if let Statement::Function { name, params, .. } = stmt {
            let var_idx = compiler.next_index;
            compiler.global_table.insert(name.clone(), var_idx);
            
            // Register function signature for ownership checks
            let ownership: Vec<bool> = params.iter().map(|p| p.is_owned).collect();
            compiler.signatures.insert(name.clone(), ownership);

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
            if let Some(&main_idx) = compiler.global_table.get("main") {
                final_instructions.push(OpCode::CallGlobal(0, main_idx, 0, 0));
                final_instructions.push(OpCode::Halt);
            } else {
                panic!("ngn Error: No main() function defined!");
            }
            
            let mut my_vm = VM::new(final_instructions, compiler.constants, compiler.next_index);
            my_vm.run();
        }
        "build" => {
            let output_name = filename.replace(".ngn", "");
            let mut final_instructions = compiler.instructions.clone();
            
            // Add the bootstrap call to main
            if let Some(&main_idx) = compiler.global_table.get("main") {
                final_instructions.push(OpCode::CallGlobal(0, main_idx, 0, 0));
                final_instructions.push(OpCode::Halt);
            }

            // Serialize to bytes
            let payload = (final_instructions, compiler.constants);
            let bytecode_bytes = bincode::serialize(&payload).unwrap();
            let payload_len = bytecode_bytes.len() as u64;
            let magic: u64 = 0x4E474E20;

            // Get the ngn compiler itself
            let compiler_path = std::env::current_exe().expect("Failed to find compiler");
            let mut compiler_bytes = std::fs::read(compiler_path).expect("Failed to read compiler");

            // Stitch them together
            compiler_bytes.extend(bytecode_bytes);
            compiler_bytes.extend(magic.to_le_bytes());
            compiler_bytes.extend(payload_len.to_le_bytes());

            // Write the final binary
            std::fs::write(&output_name, compiler_bytes).expect("Failed to write binary");
            
            // Make it executable (On Linux/Mac)
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                let mut perms = std::fs::metadata(&output_name).unwrap().permissions();
                perms.set_mode(0o755);
                std::fs::set_permissions(&output_name, perms).unwrap();
            }
            
            println!("Built ngn: {}", output_name);
        }
        _ => println!("Unknown command: {}", command),
    }
}

fn check_for_embedded_bytecode() -> Option<(Vec<OpCode>, Vec<Value>)> {
    let path = std::env::current_exe().ok()?;
    let mut file = std::fs::File::open(path).ok()?;
    let file_len = file.metadata().ok()?.len();

    // We need at least 16 bytes: 8 for a 'Magic Number' and 8 for the Length
    if file_len < 16 { return None; }

    // Read the last 16 bytes
    file.seek(SeekFrom::End(-16)).ok()?;
    let mut footer = [0u8; 16];
    file.read_exact(&mut footer).ok()?;

    let magic = u64::from_le_bytes(footer[0..8].try_into().ok()?);
    let size = u64::from_le_bytes(footer[8..16].try_into().ok()?);

    // Use a unique number to identify ngn binaries (e.g., 0x4E474E20 / "NGN ")
    if magic != 0x4E474E20 {
        return None;
    }

    // Seek back to where the bytecode starts
    file.seek(SeekFrom::End(-(16 + size as i64))).ok()?;
    let mut buffer = vec![0u8; size as usize];
    file.read_exact(&mut buffer).ok()?;

    bincode::deserialize(&buffer).ok()
}
