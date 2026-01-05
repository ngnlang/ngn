use ngn::analyzer::{Analyzer, Symbol};
use ngn::bytecode::OpCode;
use ngn::compiler::Compiler;
use ngn::lexer::{Lexer, Span, Token};
use ngn::parser::{Expr, Parser, Statement, StatementKind, Type};
use ngn::value::Value;
use ngn::vm::VM;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::io::{Read, Seek, SeekFrom};
use std::path::PathBuf;
use std::sync::Arc;
use std::time::SystemTime;

// Cached compiled exports from a module (with module's globals Arc for home_globals)
type ModuleExports = HashMap<String, Value>;
type ModuleGlobals = Arc<Vec<Value>>;
type ModuleCacheEntry = (ModuleExports, ModuleGlobals);
type ModuleCache = HashMap<PathBuf, (ModuleCacheEntry, SystemTime)>;

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
    let source = fs::read_to_string(filename).expect(&format!("Could not read file: {}", filename));

    // Pre-check: Ensure fn main() exists before full parsing
    // This gives a clearer error than "const can only be used inside functions"
    {
        let mut check_lexer = Lexer::new(&source);
        let mut found_main = false;
        let mut current = check_lexer.next_token();

        while current != Token::EOF {
            if current == Token::Fn {
                current = check_lexer.next_token();
                if let Token::Identifier(name) = &current {
                    if name == "main" {
                        found_main = true;
                        break;
                    }
                }
            }
            current = check_lexer.next_token();
        }

        if !found_main {
            eprintln!("ngn Error: Entry point files must define a fn main() function");
            eprintln!("  Hint: Wrap your code in 'fn main() {{ ... }}'");
            std::process::exit(1);
        }
    }

    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer);
    let mut compiler = Compiler::new(None);
    compiler.inject_builtins();

    // 1. Collect all statements into a list
    let mut statements = Vec::new();
    while parser.current_token != Token::EOF {
        if parser.current_token == Token::Newline {
            parser.advance();
            continue;
        }
        statements.push(parser.parse_statement());
    }

    // 2. Process file imports first (so Analyzer knows about them)
    let mut analyzer = Analyzer::new();
    let base_path = PathBuf::from(filename);
    let mut module_cache: ModuleCache = HashMap::new();

    for stmt in &statements {
        if let StatementKind::Import { names, source } = &stmt.kind {
            // Skip toolbox imports (handled by compiler)
            if source.starts_with("tbx::") {
                continue;
            }

            // Load the module
            let exports = load_module(source, &base_path, &mut module_cache);

            // Inject each requested export into the compiler
            for (name, alias) in names {
                if let Some(val) = exports.get(name) {
                    // Add the exported Value to constants pool directly
                    // It's already a runtime Value (Closure, String, etc.)
                    let const_idx = compiler.add_constant(val.clone());

                    // Register in global table
                    let var_idx = compiler.next_index;
                    let bind_name = alias.as_ref().unwrap_or(name);
                    compiler.global_table.insert(bind_name.clone(), var_idx);
                    compiler.next_index += 1;

                    // If it's a Closure/Function, we should register signatures for call checks if possible
                    if let Value::Function(f) = val {
                        let ownership: Vec<bool> = f.param_ownership.iter().cloned().collect();
                        compiler.signatures.insert(bind_name.clone(), ownership);
                    } else if let Value::Closure(c) = val {
                        let ownership: Vec<bool> =
                            c.function.param_ownership.iter().cloned().collect();
                        compiler.signatures.insert(bind_name.clone(), ownership);
                    }

                    // Emit load and assign at global scope
                    let reg = compiler.alloc_reg();
                    compiler
                        .instructions
                        .push(OpCode::LoadConst(reg, const_idx));
                    compiler
                        .instructions
                        .push(OpCode::DefGlobal(var_idx, false));
                    compiler
                        .instructions
                        .push(OpCode::AssignGlobal(var_idx, reg));
                    compiler.reg_top = compiler.temp_start;

                    // Register with Semantic Analyzer
                    // We need to reconstruct a basic Symbol from the Value
                    let symbol = if let Value::Function(f) = &val {
                        let params = f.param_ownership.iter().map(|_| Type::Any).collect();
                        Symbol {
                            ty: Type::Function {
                                params,
                                return_type: Box::new(Type::Void), // Unknown
                            },
                            is_mutable: false,
                        }
                    } else if let Value::Closure(c) = &val {
                        let params = c
                            .function
                            .param_ownership
                            .iter()
                            .map(|_| Type::Any)
                            .collect();
                        Symbol {
                            ty: Type::Function {
                                params,
                                return_type: Box::new(Type::Void), // Unknown
                            },
                            is_mutable: false,
                        }
                    } else {
                        Symbol {
                            is_mutable: false,
                            ty: Type::Void, // Unknown
                        }
                    };
                    analyzer.define_global(bind_name.clone(), symbol);
                    // For now, analyzer might be blind to types unless we return them too.
                    // This task focuses on Runtime Correctness (Global Collision).
                } else {
                    panic!("Import Error: '{}' is not exported from '{}'", name, source);
                }
            }
        } else if let StatementKind::ImportDefault { name, source } = &stmt.kind {
            // Load the module
            let exports = load_module(source, &base_path, &mut module_cache);

            if let Some(val) = exports.get("default") {
                let const_idx = compiler.add_constant(val.clone());
                let var_idx = compiler.next_index;
                compiler.global_table.insert(name.clone(), var_idx);
                compiler.next_index += 1;

                // Register signature
                if let Value::Function(f) = val {
                    let ownership: Vec<bool> = f.param_ownership.iter().cloned().collect();
                    compiler.signatures.insert(name.clone(), ownership);
                } else if let Value::Closure(c) = val {
                    let ownership: Vec<bool> = c.function.param_ownership.iter().cloned().collect();
                    compiler.signatures.insert(name.clone(), ownership);
                }

                let reg = compiler.alloc_reg();
                compiler
                    .instructions
                    .push(OpCode::LoadConst(reg, const_idx));
                compiler
                    .instructions
                    .push(OpCode::DefGlobal(var_idx, false));
                compiler
                    .instructions
                    .push(OpCode::AssignGlobal(var_idx, reg));
                compiler.reg_top = compiler.temp_start;

                let symbol = if let Value::Function(f) = &val {
                    let params = f.param_ownership.iter().map(|_| Type::Any).collect();
                    Symbol {
                        ty: Type::Function {
                            params,
                            return_type: Box::new(Type::Void),
                        },
                        is_mutable: false,
                    }
                } else if let Value::Closure(c) = &val {
                    let params = c
                        .function
                        .param_ownership
                        .iter()
                        .map(|_| Type::Any)
                        .collect();
                    Symbol {
                        ty: Type::Function {
                            params,
                            return_type: Box::new(Type::Void),
                        },
                        is_mutable: false,
                    }
                } else {
                    Symbol {
                        is_mutable: false,
                        ty: Type::Void,
                    }
                };
                analyzer.define_global(name.clone(), symbol);
            } else {
                panic!("Import Error: Module '{}' has no default export", source);
            }
        } else if let StatementKind::ImportModule { alias, source } = &stmt.kind {
            // import * as alias from "source"
            eprintln!(
                "Warning: Module imports ('import * as {} from \"{}\"') are not fully supported yet.",
                alias, source
            );
        }
    }

    // 3. Semantic Analysis (Static Type Checking)
    if let Err(errors) = analyzer.analyze(&statements) {
        for err in errors {
            eprintln!("{}", err);
        }
        std::process::exit(1);
    }

    // 3. PASS ONE: Register all function names in the symbol table
    // (We don't compile them yet, just reserve their slots)
    for stmt in &statements {
        if let StatementKind::Function { name, params, .. } = &stmt.kind {
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

            // Serialize bytecode
            let payload = (final_instructions, compiler.constants);
            let bytecode_bytes = bincode::serialize(&payload).unwrap();
            let payload_len = bytecode_bytes.len() as u64;
            let magic: u64 = 0x4E474E20;

            // Use embedded runtime binary (VM-only, much smaller than full ngn)
            let runtime_bytes = include_bytes!(concat!(env!("OUT_DIR"), "/runtime_binary"));
            let mut output_bytes = runtime_bytes.to_vec();

            // Append bytecode and footer
            output_bytes.extend(bytecode_bytes);
            output_bytes.extend(magic.to_le_bytes());
            output_bytes.extend(payload_len.to_le_bytes());

            // Write the final binary
            std::fs::write(&output_name, output_bytes).expect("Failed to write binary");

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
    if file_len < 16 {
        return None;
    }

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

/// Load and compile a module, returning its exported functions (with home_globals injected)
fn load_module(module_path: &str, base_path: &PathBuf, cache: &mut ModuleCache) -> ModuleExports {
    // Resolve relative path
    let resolved_path = if module_path.starts_with("./") || module_path.starts_with("../") {
        let parent = base_path.parent().unwrap_or(base_path);
        parent.join(module_path)
    } else {
        PathBuf::from(module_path)
    };

    // Check cache
    if let Some(entry) = cache.get(&resolved_path) {
        return entry.0.0.clone(); // Return cached exports
    }

    // Load source
    let source = fs::read_to_string(&resolved_path)
        .unwrap_or_else(|_| panic!("Could not read module: {}", resolved_path.display()));

    // Parse
    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer);
    let mut statements = Vec::new();
    while parser.current_token != Token::EOF {
        if parser.current_token == Token::Newline {
            parser.advance();
            continue;
        }
        statements.push(parser.parse_statement());
    }

    // 1. Identify exports and handle ExportDefault
    let mut export_names: Vec<String> = Vec::new();
    let mut default_export_expr: Option<Expr> = None;

    for stmt in &statements {
        match &stmt.kind {
            StatementKind::Function {
                name,
                is_exported: true,
                ..
            } => {
                export_names.push(name.clone());
            }
            StatementKind::ExportDefault(expr) => {
                default_export_expr = Some(expr.clone());
            }
            _ => {}
        }
    }

    // 2. Transform statements: remove ExportDefault, add __default__ declaration if needed
    let mut processed_statements: Vec<Statement> = statements
        .into_iter()
        .filter(|s| !matches!(s.kind, StatementKind::ExportDefault(_)))
        .collect();

    if let Some(expr) = default_export_expr {
        processed_statements.push(Statement {
            kind: StatementKind::Declaration {
                name: "__default__".to_string(),
                is_mutable: false,
                is_static: true, // Global scope
                value: expr,
                declared_type: None,
            },
            span: Span::default(),
        });
        export_names.push("__default__".to_string());
    }

    // 3. Compile at global scope (like main entrypoint)
    let mut module_compiler = Compiler::new(None);
    module_compiler.inject_builtins();

    for stmt in processed_statements {
        module_compiler.compile_statement(stmt);
    }

    // Add Halt to end execution cleanly
    module_compiler.instructions.push(OpCode::Halt);

    // 4. Execute module in isolated VM
    let mut vm = VM::new(
        module_compiler.instructions.clone(),
        module_compiler.constants.clone(),
        module_compiler.max_reg as usize,
    );
    vm.run();

    // 5. Create Arc for module's globals (to be shared by all functions from this module)
    let module_globals: ModuleGlobals = Arc::new(vm.globals.clone());

    // 6. Extract exports from VM globals and inject home_globals
    let mut exports: ModuleExports = HashMap::new();

    for name in &export_names {
        let export_key = if name == "__default__" {
            "default".to_string()
        } else {
            name.clone()
        };

        if let Some(&idx) = module_compiler.global_table.get(name) {
            let value = vm.globals[idx].clone();

            // Inject home_globals into Function/Closure values
            let value = match value {
                Value::Function(mut func) => {
                    func.home_globals = Some(module_globals.clone());
                    Value::Function(func)
                }
                Value::Closure(mut closure) => {
                    closure.function.home_globals = Some(module_globals.clone());
                    Value::Closure(closure)
                }
                other => other,
            };

            exports.insert(export_key, value);
        }
    }

    cache.insert(
        resolved_path.clone(),
        ((exports.clone(), module_globals), SystemTime::now()),
    );
    exports
}
