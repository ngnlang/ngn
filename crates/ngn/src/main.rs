use ngn::analyzer::{Analyzer, Symbol};
use ngn::bytecode::OpCode;
use ngn::compiler::Compiler;
use ngn::lexer::{Lexer, Span, Token};
use ngn::parser::{Expr, ExprKind, Parser, Statement, StatementKind, Type};
use ngn::value::{ObjectData, Value};
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
        // Initialize env from current working directory
        ngn::env::init(std::path::Path::new("."));
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

    // Pre-check: Ensure either fn main() OR export default exists
    // This gives a clearer error than later compile errors
    let (found_main, found_export_default) = {
        let mut check_lexer = Lexer::new(&source);
        let mut found_main = false;
        let mut found_export_default = false;
        let mut current = check_lexer.next_token();

        while current != Token::EOF {
            if current == Token::Fn {
                current = check_lexer.next_token();
                if let Token::Identifier(name) = &current {
                    if name == "main" {
                        found_main = true;
                    }
                }
            }
            if current == Token::Export {
                current = check_lexer.next_token();
                if current == Token::Default {
                    found_export_default = true;
                }
            }
            current = check_lexer.next_token();
        }
        (found_main, found_export_default)
    };

    if found_main && found_export_default {
        eprintln!("ngn Error: Cannot have both fn main() and export default");
        eprintln!("  Use fn main() for normal apps, or export default for HTTP servers");
        std::process::exit(1);
    }

    if !found_main && !found_export_default {
        eprintln!("ngn Error: Entry point files must define fn main() or export default");
        eprintln!("  Hint: For HTTP servers, use: export default api");
        std::process::exit(1);
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
                    // Use the stored type info from the Function
                    let symbol = if let Value::Function(f) = &val {
                        Symbol {
                            ty: Type::Function {
                                params: f.param_types.clone(),
                                optional_count: 0,
                                return_type: Box::new(f.return_type.clone()),
                            },
                            is_mutable: false,
                        }
                    } else if let Value::Closure(c) = &val {
                        Symbol {
                            ty: Type::Function {
                                params: c.function.param_types.clone(),
                                optional_count: 0,
                                return_type: Box::new(c.function.return_type.clone()),
                            },
                            is_mutable: false,
                        }
                    } else {
                        Symbol {
                            is_mutable: false,
                            ty: Type::Any, // Unknown type for non-function values
                        }
                    };
                    analyzer.define_global(bind_name.clone(), symbol);
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
                    Symbol {
                        ty: Type::Function {
                            params: f.param_types.clone(),
                            optional_count: 0,
                            return_type: Box::new(f.return_type.clone()),
                        },
                        is_mutable: false,
                    }
                } else if let Value::Closure(c) = &val {
                    Symbol {
                        ty: Type::Function {
                            params: c.function.param_types.clone(),
                            optional_count: 0,
                            return_type: Box::new(c.function.return_type.clone()),
                        },
                        is_mutable: false,
                    }
                } else {
                    Symbol {
                        is_mutable: false,
                        ty: Type::Any, // Unknown type for non-function values
                    }
                };
                analyzer.define_global(name.clone(), symbol);
            } else {
                panic!("Import Error: Module '{}' has no default export", source);
            }
        } else if let StatementKind::ImportModule { alias, source } = &stmt.kind {
            // import * as alias from "source"
            if source.starts_with("tbx::") {
                continue;
            }

            let exports = load_module(source, &base_path, &mut module_cache);
            let (module_value, symbol) = if exports.len() == 1 {
                if let Some(val) = exports.get("default") {
                    let symbol = if let Value::Function(f) = val {
                        Symbol {
                            ty: Type::Function {
                                params: f.param_types.clone(),
                                optional_count: 0,
                                return_type: Box::new(f.return_type.clone()),
                            },
                            is_mutable: false,
                        }
                    } else if let Value::Closure(c) = val {
                        Symbol {
                            ty: Type::Function {
                                params: c.function.param_types.clone(),
                                optional_count: 0,
                                return_type: Box::new(c.function.return_type.clone()),
                            },
                            is_mutable: false,
                        }
                    } else {
                        Symbol {
                            ty: Type::Any,
                            is_mutable: false,
                        }
                    };
                    (val.clone(), symbol)
                } else {
                    let module_obj = ObjectData::into_value("Module".to_string(), exports);
                    (
                        module_obj,
                        Symbol {
                            ty: Type::Any,
                            is_mutable: false,
                        },
                    )
                }
            } else {
                let module_obj = ObjectData::into_value("Module".to_string(), exports);
                (
                    module_obj,
                    Symbol {
                        ty: Type::Any,
                        is_mutable: false,
                    },
                )
            };

            let const_idx = compiler.add_constant(module_value.clone());
            let var_idx = compiler.next_index;
            compiler.global_table.insert(alias.clone(), var_idx);
            compiler.next_index += 1;

            if let Value::Function(f) = &module_value {
                let ownership: Vec<bool> = f.param_ownership.iter().cloned().collect();
                compiler.signatures.insert(alias.clone(), ownership);
            } else if let Value::Closure(c) = &module_value {
                let ownership: Vec<bool> = c.function.param_ownership.iter().cloned().collect();
                compiler.signatures.insert(alias.clone(), ownership);
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

            analyzer.define_global(alias.clone(), symbol);
        }
    }

    // 2a. Handle export default (transform to __default__ declaration)
    let mut default_export_expr_kind: Option<ExprKind> = None;
    let mut processed_statements: Vec<Statement> = Vec::new();

    for stmt in statements {
        if let StatementKind::ExportDefault(expr) = &stmt.kind {
            // Track the expression kind for later type resolution
            default_export_expr_kind = Some(expr.kind.clone());

            // Transform to __default__ declaration
            processed_statements.push(Statement {
                kind: StatementKind::Declaration {
                    name: "__default__".to_string(),
                    is_mutable: false,
                    is_global: true,
                    value: expr.clone(),
                    declared_type: None,
                },
                span: stmt.span,
            });
        } else {
            processed_statements.push(stmt);
        }
    }
    let statements = processed_statements;

    // 3. Semantic Analysis (Static Type Checking)
    if let Err(errors) = analyzer.analyze(&statements) {
        for err in errors {
            eprintln!(
                "{}",
                err.span.format_diagnostic(&source, filename, &err.message)
            );
        }
        std::process::exit(1);
    }

    // Print warnings (non-blocking)
    for warning in &analyzer.warnings {
        eprintln!(
            "{}",
            warning
                .span
                .format_diagnostic(&source, filename, &warning.message)
        );
    }

    // 3a. Now resolve the export type and check for fetch method
    let is_http_server = if let Some(expr_kind) = &default_export_expr_kind {
        match expr_kind {
            ExprKind::Variable(name) => analyzer
                .lookup_variable_type(name)
                .map(|ty| analyzer.has_method(&ty, "fetch"))
                .unwrap_or(false),
            ExprKind::ModelInstance { name, .. } => {
                analyzer.has_method(&Type::Model(name.clone()), "fetch")
            }
            ExprKind::Object(fields) => fields.iter().any(|(key, _)| key == "fetch"),
            _ => false,
        }
    } else {
        false
    };

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
            // Initialize env from the source file's directory
            let working_dir = std::path::Path::new(filename)
                .parent()
                .unwrap_or(std::path::Path::new("."));
            ngn::env::init(working_dir);

            let mut final_instructions = compiler.instructions.clone();

            if is_http_server {
                // HTTP server mode: emit ServeHttp with __default__ handler
                if let Some(&default_idx) = compiler.global_table.get("__default__") {
                    final_instructions.push(OpCode::ServeHttp(default_idx));
                    final_instructions.push(OpCode::Halt);
                } else {
                    panic!("ngn Error: export default not compiled correctly");
                }
            } else if let Some(&main_idx) = compiler.global_table.get("main") {
                final_instructions.push(OpCode::CallGlobal(0, main_idx, 0, 0));
                final_instructions.push(OpCode::Halt);
            } else {
                panic!("ngn Error: No main() function or HTTP handler defined!");
            }

            let mut my_vm = VM::new(final_instructions, compiler.constants, compiler.next_index);
            my_vm.run();
        }
        "build" => {
            let output_name = filename.replace(".ngn", "");
            let mut final_instructions = compiler.instructions.clone();

            // Add the bootstrap call (main or ServeHttp)
            if is_http_server {
                if let Some(&default_idx) = compiler.global_table.get("__default__") {
                    final_instructions.push(OpCode::ServeHttp(default_idx));
                    final_instructions.push(OpCode::Halt);
                }
            } else if let Some(&main_idx) = compiler.global_table.get("main") {
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
        return entry.0 .0.clone(); // Return cached exports
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
                is_global: true, // Global scope
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
