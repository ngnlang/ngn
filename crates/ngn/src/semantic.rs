use std::collections::HashMap;
use crate::lexer::tokenize;
use crate::runtime::ExportKind;
use crate::runtime::ImportKind;
use crate::toolbox::ImportSource;
use crate::toolbox::parse_import_source;
use crate::toolbox::signatures::get_builtin_signature;
use crate::types::*;
use crate::ast::*;
use crate::utils::parse_type;
use crate::utils::resolve_module_path;

#[derive(Debug)]
pub struct Analyzer {
    scopes: Vec<HashMap<String, Symbol>>,
    models: HashMap<String, ModelDef>,
    model_methods: HashMap<(String, String), FnDef>,
    loop_depth: usize,
    match_depth: usize,
    current_return_type: Option<Type>,
    enums: HashMap<String, EnumDef>,
    module_exports: HashMap<String, AnalyzedModule>,
    current_file: String,
    module_cache: HashMap<String, AnalyzedModule>,
    exports: AnalyzedModule,
    pub errors: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub ty: Type,
    pub kind: AssignKind,
    pub ownership: Ownership,
    pub defined_at: usize, // scope depth
    pub fn_def: Option<FnDef>,
    pub module_def: Option<AnalyzedModule>,
}

#[derive(Debug, Clone, Default)]
pub struct AnalyzedModule {
    functions: HashMap<String, FnDef>,
    models: HashMap<String, ModelDef>,
    enums: HashMap<String, EnumDef>,
    default: Option<String>,
}

pub fn parse_builtin_type(s: &str) -> Type {
    if s == "T" {
        return Type::Generic("T".to_string());
    }

    let tokens = tokenize(s);
    let mut peekable = tokens.into_iter().peekable();
    
    // We unwrap here because builtin signatures must be valid syntax.
    // If they aren't, it's a compiler bug, so panic is acceptable.
    match parse_type(&mut peekable) {
        Ok((ty, _)) => ty,
        Err(e) => panic!("Invalid builtin type signature '{}': {}", s, e),
    }
}

impl Analyzer {
    pub fn new(current_file: String) -> Self {
        let mut enums = HashMap::new();
        let global_scope = HashMap::new();
    
        // Built-in Result<T, E>
        enums.insert("Result".to_string(), EnumDef {
            name: "Result".to_string(),
            type_params: vec!["T".to_string(), "E".to_string()],
            variants: vec![
                EnumVariant { name: "Ok".to_string(), data_type: Some(Type::Generic("T".to_string())) },
                EnumVariant { name: "Error".to_string(), data_type: Some(Type::Generic("E".to_string())) },
            ],
        });
        
        // Built-in Maybe<T>
        enums.insert("Maybe".to_string(), EnumDef {
            name: "Maybe".to_string(),
            type_params: vec!["T".to_string()],
            variants: vec![
                EnumVariant { name: "Value".to_string(), data_type: Some(Type::Generic("T".to_string())) },
                EnumVariant { name: "Null".to_string(), data_type: None },
            ],
        });

        Self {
            scopes: vec![global_scope],
            models: HashMap::new(),
            model_methods: HashMap::new(),
            loop_depth: 0,
            match_depth: 0,
            current_return_type: None,
            enums,
            module_exports: HashMap::new(),
            current_file,
            module_cache: HashMap::new(),
            exports: AnalyzedModule::default(),
            errors: Vec::new(),
        }
    }

    pub fn with_cache(current_file: String, cache: HashMap<String, AnalyzedModule>) -> Self {
        let mut analyzer = Self::new(current_file);
        analyzer.module_cache = cache;
        analyzer
    }

    pub fn register_builtin(&mut self, name: &str, alias: &str) {
        if let Some((_, params_def, return_def)) = get_builtin_signature(name) {
            let params = params_def.iter()
                .map(|(p_name, p_type_str)| {
                    (p_name.to_string(), Some(parse_builtin_type(p_type_str)), Ownership::Borrowed)
                })
                .collect();

            let return_type = parse_builtin_type(return_def);
            let fn_def = FnDef {
                name: alias.to_string(),
                params,
                body: None,
                return_type: Some(return_type.clone()),
            };

            let ty = Type::Function {
                params: fn_def.params.iter()
                    .map(|(_, t, _)| t.clone().unwrap_or(Type::Void))
                    .collect(),
                return_type: Box::new(return_type),
            };

            // Insert into GLOBAL SCOPE (index 0)
            if let Some(global_scope) = self.scopes.first_mut() {
                global_scope.insert(alias.to_string(), Symbol {
                    ty,
                    kind: AssignKind::Const,
                    ownership: Ownership::Owned,
                    defined_at: 0,
                    fn_def: Some(fn_def),
                    module_def: None,
                });
            }
        }
    }

    pub fn analyze(&mut self, stmts: &[Stmt]) -> Result<(), Vec<String>> {
        // Pass 1: collect function signatures
        for stmt in stmts {
            match stmt {
                Stmt::FnDef(fn_def) => {
                    let ty = Type::Function {
                        params: fn_def.params.iter().map(|(_, t, _)| t.clone().unwrap_or(Type::Void)).collect(),
                        return_type: Box::new(fn_def.return_type.clone().unwrap_or(Type::Void))
                    };

                    self.scopes[0].insert(fn_def.name.clone(), Symbol {
                        ty,
                        kind: AssignKind::Const,
                        ownership: Ownership::Owned,
                        defined_at: 0,
                        fn_def: Some(fn_def.clone()),
                        module_def: None,
                    });
                }
                Stmt::ModelDef(model_def) => {
                    self.models.insert(model_def.name.clone(), model_def.clone());
                }
                Stmt::EnumDef(enum_def) => {
                    self.enums.insert(enum_def.name.clone(), enum_def.clone());
                }
                Stmt::ExtendModel { model_name, methods, .. } => {
                    for method in methods {
                        self.model_methods.insert(
                            (model_name.clone(), method.name.clone()),
                            method.clone()
                        );
                    }
                }
                _ => {}
            }
        }

        // Pass 2: check everything
        for stmt in stmts {
            self.check_stmt(stmt);
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn analyze_module(&mut self, source: &str) -> AnalyzedModule {
        let file_path = resolve_module_path(source, &self.current_file);

        // Check the persistent cache first (from previous compilations or other analyzers)
        if let Some(cached) = self.module_cache.get(&file_path) {
            return cached.clone();
        }
        
        // Check if we've already analyzed it in this session (handles circular imports)
        if let Some(cached) = self.module_exports.get(&file_path) {
            return cached.clone();
        }
        
        // Insert placeholder to prevent infinite recursion on circular imports
        self.module_exports.insert(file_path.clone(), AnalyzedModule::default());
        
        let source_code = match std::fs::read_to_string(&file_path) {
            Ok(s) => s,
            Err(_) => {
                self.error(format!("Could not read module '{}'", file_path));
                return AnalyzedModule::default();
            }
        };
        
        let tokens = crate::lexer::tokenize(&source_code);
        let mut parser = crate::parser::Parser::new(tokens, HashMap::new(), file_path.clone());
        
        let stmts = match parser.parse_program() {
            Ok(s) => s,
            Err(e) => {
                self.error(format!("Parse error in '{}': {}", file_path, e));
                return AnalyzedModule::default();
            }
        };
        
        // Create child analyzer with BOTH caches
        let mut module_analyzer = Analyzer::new(file_path.clone());
        module_analyzer.module_cache = self.module_cache.clone();
        module_analyzer.module_exports = std::mem::take(&mut self.module_exports);

        let _ = module_analyzer.analyze(&stmts);
        
        // Merge caches back
        self.module_exports = module_analyzer.module_exports;
        let exports = module_analyzer.exports;
        
        // Store in both caches
        self.module_exports.insert(file_path.clone(), exports.clone());
        self.module_cache.insert(file_path, exports.clone());

        exports
    }

    fn validate_args(
        &mut self,
        fn_name: &str,
        params: &[(String, Option<Type>, Ownership)],
        arg_types: &[Type],
    ) {
        // Check arity
        if params.len() != arg_types.len() {
            self.error(format!(
                "'{}' expects {} arguments, got {}",
                fn_name, params.len(), arg_types.len()
            ));
            return;
        }

        // Check each argument type
        for (i, ((param_name, param_type, _), arg_type)) in 
            params.iter().zip(arg_types.iter()).enumerate() 
        {
            if let Some(expected) = param_type {
                if !types_compatible(expected, arg_type) {
                    self.error(format!(
                        "'{}' argument {} ('{}'): expected {:?}, got {:?}",
                        fn_name, i + 1, param_name, expected, arg_type
                    ));
                }
            }
            // If param_type is None, it's untyped — accept anything
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn define(&mut self, name: &str, sym: Symbol) {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(name) {
                self.error(format!("'{}' already defined in this scope", name));
                return;
            }
            scope.insert(name.to_string(), sym);
        }
    }

    fn current_depth(&self) -> usize {
        self.scopes.len() - 1
    }

    fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.scopes.iter().rev().find_map(|s| s.get(name))
    }

    fn error(&mut self, msg: String) {
        self.errors.push(msg);
    }

    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Echo(expr) | Stmt::Print(expr) | Stmt::ExprStmt(expr) => {
                self.check_expr(expr);
            }

            Stmt::Assign { name, value, kind, declared_type, ownership } => {
                let inferred = self.check_expr(value);

                if let Some(declared) = declared_type {
                    if !types_compatible(declared, &inferred) {
                        self.error(format!(
                            "'{}': expected {:?}, got {:?}", name, declared, inferred
                        ));
                    }
                }

                self.define(name, Symbol {
                    ty: declared_type.clone().unwrap_or(inferred),
                    kind: kind.clone(),
                    ownership: ownership.clone(),
                    defined_at: self.current_depth(),
                    fn_def: None,
                    module_def: None,
                });
            }

            Stmt::Reassign { name, value } => {
                let val_type = self.check_expr(value);

                match self.lookup(name) {
                    None => self.error(format!("'{}' not defined", name)),
                    Some(sym) if sym.kind == AssignKind::Const => {
                        self.error(format!("cannot reassign const '{}'", name));
                    }
                    Some(sym) if !types_compatible(&sym.ty, &val_type) => {
                        self.error(format!(
                            "'{}': cannot assign {:?} to {:?}", name, val_type, sym.ty
                        ));
                    }
                    _ => {}
                }
            }
            Stmt::If { condition, then_block, else_ifs, else_block } => {
                let cond_type = self.check_expr(condition);

                if cond_type != Type::Bool {
                    self.error(format!("if condition must be bool, got {:?}", cond_type));
                }

                self.enter_scope();
                for s in then_block { self.check_stmt(s); }
                self.exit_scope();

                for (cond, block) in else_ifs {
                    let t = self.check_expr(cond);
                    if t != Type::Bool {
                        self.error(format!("else if condition must be bool, got {:?}", t));
                    }
                    self.enter_scope();
                    for s in block { self.check_stmt(s); }
                    self.exit_scope();
                }

                if let Some(block) = else_block {
                    self.enter_scope();
                    for s in block { self.check_stmt(s); }
                    self.exit_scope();
                }
            }

            Stmt::While { condition, body } | Stmt::WhileOnce { condition, body } |
            Stmt::Until { condition, body } | Stmt::UntilOnce { condition, body } => {
                let cond_type = self.check_expr(condition);

                if cond_type != Type::Bool {
                    self.error(format!("loop condition must be bool, got {:?}", cond_type));
                }

                self.enter_scope();
                self.loop_depth += 1;

                for s in body { self.check_stmt(s); }

                self.loop_depth -= 1;
                self.exit_scope();
            }

            Stmt::Break => {
                if self.loop_depth == 0 && self.match_depth == 0 {
                    self.error("break outside of loop".to_string());
                }
            }

            Stmt::Next => {
                if self.loop_depth == 0 && self.match_depth == 0 {
                    self.error("next outside of loop".to_string());
                }
            }

            Stmt::Return(expr) => {
                let expected = self.current_return_type.clone();

                match (&expected, expr) {
                    // Function expects a value, got one
                    (Some(expected), Some(e)) => {
                        let actual = self.check_expr(e);

                        if !types_compatible(expected, &actual) {
                            self.error(format!(
                                "return type mismatch: expected {:?}, got {:?}", expected, actual
                            ));
                        }
                    }
                    // Function expects a value, got nothing
                    (Some(expected), None) => {
                        if *expected != Type::Void {
                            self.error(format!("expected return value of type {:?}", expected));
                        }
                    }
                    // No declared return type, infer it
                    (None, Some(e)) => {
                        let actual = self.check_expr(e);
                        self.current_return_type = Some(actual);
                        // if actual != Type::Void {
                        //     self.error(format!("unexpected return value: {:?}", actual));
                        // }
                    }
                    // Function expects nothing, got nothing
                    (None, None) => {}
                }
            }

            Stmt::FnDef(fn_def) => {
                // Register nested functions
                if self.current_depth() > 0 {
                    let ty = Type::Function {
                        params: fn_def.params.iter().map(|(_, t, _)| t.clone().unwrap_or(Type::Void)).collect(),
                        return_type: Box::new(fn_def.return_type.clone().unwrap_or(Type::Void))
                    };
                    
                    self.define(&fn_def.name, Symbol {
                        ty,
                        kind: AssignKind::Const,
                        ownership: Ownership::Owned,
                        defined_at: self.current_depth(),
                        fn_def: Some(fn_def.clone()),
                        module_def: None,
                    });
                }

                // Save previous (for nested functions/closures)
                let prev_return_type = self.current_return_type.clone();
                
                self.current_return_type = fn_def.return_type.clone();
                
                self.enter_scope();
                
                // Define parameters
                for (param_name, param_type, ownership) in &fn_def.params {
                    let ty = match param_type {
                        Some(t) => t.clone(),
                        None => {
                            self.error(format!(
                                "parameter '{}' in function '{}' requires a type annotation",
                                param_name, fn_def.name
                            ));
                            Type::Void
                        }
                    };

                    self.define(param_name, Symbol {
                        ty,
                        kind: AssignKind::Var,
                        ownership: ownership.clone(),
                        defined_at: self.current_depth(),
                        fn_def: None,
                        module_def: None,
                    });
                }
                
                // Check body
                if let Some(body) = &fn_def.body {
                    for s in body {
                        self.check_stmt(s);
                    }
                }
                
                self.exit_scope();
                
                // Restore
                self.current_return_type = prev_return_type;
            }

            Stmt::ModelDef(model_def) => {
                // Check for duplicate field names
                let mut seen_fields = std::collections::HashSet::new();
                for (field_name, _field_type) in &model_def.fields {
                    if !seen_fields.insert(field_name) {
                        self.error(format!(
                            "duplicate field '{}' in model '{}'", field_name, model_def.name
                        ));
                    }
                }

                self.models.insert(model_def.name.clone(), model_def.clone());

                // Also make the model name available for static method calls
                self.define(&model_def.name, Symbol {
                    ty: Type::Model(model_def.name.clone()),
                    kind: AssignKind::Const,
                    ownership: Ownership::Owned,
                    defined_at: 0,
                    fn_def: None,
                    module_def: None,
                });
            }

            Stmt::EnumDef(enum_def) => {
                // Check for duplicate variant names
                let mut seen_variants = std::collections::HashSet::new();
                for variant in &enum_def.variants {
                    if !seen_variants.insert(&variant.name) {
                        self.error(format!(
                            "duplicate variant '{}' in enum '{}'", variant.name, enum_def.name
                        ));
                    }
                }
            }

            Stmt::RoleDef(role_def) => {
                // Check each method signature (bodies are usually None for roles/interfaces)
                let mut seen_methods = std::collections::HashSet::new();
                for method in &role_def.methods {
                    if !seen_methods.insert(&method.name) {
                        self.error(format!(
                            "duplicate method '{}' in role '{}'", method.name, role_def.name
                        ));
                    }
                }
            }

            Stmt::ExtendModel { model_name, role_name: _, methods } => {
                // Verify model exists
                if !self.models.contains_key(model_name) {
                    self.error(format!("cannot extend unknown model '{}'", model_name));
                    return;
                }

                // Check each method body
                for method in methods {
                    let prev_return_type = self.current_return_type.clone();
                    self.current_return_type = method.return_type.clone();

                    self.enter_scope();

                    // Define `this` as the model type
                    self.define("this", Symbol {
                        ty: Type::Model(model_name.clone()),
                        kind: AssignKind::Const,
                        ownership: Ownership::Borrowed,
                        defined_at: self.current_depth(),
                        fn_def: None,
                        module_def: None,
                    });

                    // Define parameters
                    for (param_name, param_type, ownership) in &method.params {
                        self.define(param_name, Symbol {
                            ty: param_type.clone().unwrap_or(Type::Void),
                            kind: AssignKind::Var,
                            ownership: ownership.clone(),
                            defined_at: self.current_depth(),
                            fn_def: None,
                            module_def: None,
                        });
                    }

                    // Check body
                    if let Some(body) = &method.body {
                        for s in body {
                            self.check_stmt(s);
                        }
                    }

                    self.exit_scope();
                    self.current_return_type = prev_return_type;
                }
            }

            Stmt::Import(import_stmt) => {
                match parse_import_source(&import_stmt.source) {
                    ImportSource::Toolbox { module } => {
                        match &import_stmt.kind {
                            ImportKind::Named(names) => {
                                let _module_name = match &module {
                                    Some(m) => m.clone(),
                                    None => {
                                        self.error("Named imports from 'tbx' require a module, e.g., 'tbx::math'".to_string());
                                        return;
                                    }
                                };
                                
                                // Register each imported function
                                // Note: We don't have full type info for builtins
                                for (original, alias) in names {
                                    self.register_builtin(original, alias);

                                    // If register_builtin failed to find the signature, we fallback
                                    if self.lookup(alias).is_none() {
                                        // self.error(format!("Unknown builtin '{}'", original)); // Optional: strict mode
                                        
                                        // Insert fallback into CURRENT SCOPE (likely global scope)
                                        // This prevents "unknown function" crashes but will likely warn about 0 args later
                                        let fallback_def = FnDef {
                                            name: alias.clone(),
                                            params: vec![],
                                            body: None,
                                            return_type: None,
                                        };

                                        let ty = Type::Function { 
                                            params: vec![], 
                                            return_type: Box::new(Type::Void) 
                                        };

                                        self.define(alias, Symbol {
                                            ty,
                                            kind: AssignKind::Const,
                                            ownership: Ownership::Owned,
                                            defined_at: self.current_depth(),
                                            fn_def: Some(fallback_def),
                                            module_def: None,
                                        });
                                    }
                                }
                            }
                            
                            ImportKind::Namespace(namespace) => {
                                let module_name = match &module {
                                    Some(m) => m.clone(),
                                    None => {
                                        self.error("Namespace imports from 'tbx' require a module".to_string());
                                        return;
                                    }
                                };

                                // Create a "virtual" Module for the toolbox
                                let mut virtual_module = AnalyzedModule::default();

                                // Populate virtual module with the builtins for this module (e.g., "math")
                                // We scan our builtin signatures to find ones that belong to this module.
                                // (Assuming you have a way to know which builtins are in "math". 
                                // If not, you might have to register ALL builtins or add a mapping)
                                
                                // Simple approach: predefined list for known toolboxes
                                let builtins = match module_name.as_str() {
                                    "math" => vec!["abs", "round", "floor", "ceil", "sin"],
                                    // Add other modules here
                                    _ => vec![],
                                };

                                for name in builtins {
                                    if let Some((_, params_def, return_def)) = get_builtin_signature(name) {
                                        let params = params_def.iter()
                                            .map(|(p_name, p_type_str)| {
                                                (p_name.to_string(), Some(parse_builtin_type(p_type_str)), Ownership::Borrowed)
                                            })
                                            .collect();

                                        let fn_def = FnDef {
                                            name: name.to_string(),
                                            params,
                                            body: None,
                                            return_type: Some(parse_builtin_type(return_def)),
                                        };
                                        
                                        virtual_module.functions.insert(name.to_string(), fn_def);
                                    }
                                }
                                
                                self.define(namespace, Symbol {
                                    ty: Type::Namespace(namespace.clone()),
                                    kind: AssignKind::Const,
                                    ownership: Ownership::Borrowed,
                                    defined_at: self.current_depth(),
                                    fn_def: None,
                                    module_def: Some(virtual_module),
                                });
                            }
                            
                            ImportKind::Default(local_name) => {
                                let _module_name = match &module {
                                    Some(m) => m.clone(),
                                    None => {
                                        self.error("Default imports from 'tbx' require a module".to_string());
                                        return;
                                    }
                                };
                                
                                self.define(local_name, Symbol {
                                    ty: Type::Namespace(local_name.clone()),
                                    kind: AssignKind::Const,
                                    ownership: Ownership::Borrowed,
                                    defined_at: self.current_depth(),
                                    fn_def: None,
                                    module_def: None,
                                });
                            }
                        }
                    }
                    
                    ImportSource::File { path: _ } => {
                        // Analyze the imported module
                        let module_exports = self.analyze_module(&import_stmt.source);
                        
                        match &import_stmt.kind {
                            ImportKind::Named(names) => {
                                for (original, local) in names {
                                    if let Some(fn_def) = module_exports.functions.get(original) {
                                        let mut imported_fn = fn_def.clone();
                                        imported_fn.name = local.clone();
                                        
                                        let ty = Type::Function {
                                            params: imported_fn.params.iter()
                                                .map(|(_, t, _)| t.clone().unwrap_or(Type::Void))
                                                .collect(),
                                            return_type: Box::new(imported_fn.return_type.clone().unwrap_or(Type::Void)),
                                        };

                                        self.define(local, Symbol {
                                            ty,
                                            kind: AssignKind::Const,
                                            ownership: Ownership::Owned,
                                            defined_at: self.current_depth(),
                                            fn_def: Some(imported_fn),
                                            module_def: None,
                                        });
                                    } else if let Some(model_def) = module_exports.models.get(original) {
                                        self.models.insert(local.clone(), model_def.clone());
                                    } else if let Some(enum_def) = module_exports.enums.get(original) {
                                        self.enums.insert(local.clone(), enum_def.clone());
                                    } else {
                                        self.error(format!(
                                            "'{}' is not exported from '{}'", 
                                            original, import_stmt.source
                                        ));
                                    }
                                }
                            }
                            
                            ImportKind::Default(local_name) => {
                                let default_name = match &module_exports.default {
                                    Some(name) => name.clone(),
                                    None => {
                                        self.error(format!(
                                            "Module '{}' has no default export", 
                                            import_stmt.source
                                        ));
                                        return;
                                    }
                                };
                                
                                if let Some(fn_def) = module_exports.functions.get(&default_name) {
                                    let mut renamed = fn_def.clone();
                                    renamed.name = local_name.clone();
                                    
                                    let ty = Type::Function {
                                        params: renamed.params.iter()
                                            .map(|(_, t, _)| t.clone().unwrap_or(Type::Void))
                                            .collect(),
                                        return_type: Box::new(renamed.return_type.clone().unwrap_or(Type::Void)),
                                    };

                                    self.define(local_name, Symbol {
                                        ty,
                                        kind: AssignKind::Const,
                                        ownership: Ownership::Owned,
                                        defined_at: self.current_depth(),
                                        fn_def: Some(renamed),
                                        module_def: None,
                                    });
                                } else {
                                    self.error(format!(
                                        "Default export '{}' not found in '{}'", 
                                        default_name, import_stmt.source
                                    ));
                                }
                            }
                            
                            ImportKind::Namespace(namespace) => {
                                self.define(namespace, Symbol {
                                    ty: Type::Namespace(namespace.clone()),
                                    kind: AssignKind::Const,
                                    ownership: Ownership::Borrowed,
                                    defined_at: self.current_depth(),
                                    fn_def: None,
                                    module_def: None,
                                });
                                
                                // Register qualified functions
                                for (name, fn_def) in &module_exports.functions {
                                    let qualified_name = format!("{}.{}", namespace, name);
        
                                    let ty = Type::Function {
                                        params: fn_def.params.iter().map(|(_, t, _)| t.clone().unwrap_or(Type::Void)).collect(),
                                        return_type: Box::new(fn_def.return_type.clone().unwrap_or(Type::Void))
                                    };

                                    self.define(&qualified_name, Symbol {
                                        ty,
                                        kind: AssignKind::Const,
                                        ownership: Ownership::Owned,
                                        defined_at: self.current_depth(),
                                        fn_def: Some(fn_def.clone()),
                                        module_def: None,
                                    });
                                }
                                
                                // Register qualified models
                                for (name, model_def) in &module_exports.models {
                                    self.models.insert(
                                        format!("{}.{}", namespace, name),
                                        model_def.clone(),
                                    );
                                }
                                
                                // Register qualified enums
                                for (name, enum_def) in &module_exports.enums {
                                    self.enums.insert(
                                        format!("{}.{}", namespace, name),
                                        enum_def.clone(),
                                    );
                                }
                            }
                        }
                    }
                }
            }

            Stmt::Export(export_kind) => {
                match export_kind {
                    ExportKind::Named(inner_stmt) => {
                        // First, process the inner statement normally
                        self.check_stmt(inner_stmt);
                        
                        // Then record the export
                        match inner_stmt.as_ref() {
                            Stmt::FnDef(fn_def) => {
                                self.exports.functions.insert(fn_def.name.clone(), fn_def.clone());
                            }
                            Stmt::ModelDef(model_def) => {
                                self.exports.models.insert(model_def.name.clone(), model_def.clone());
                            }
                            Stmt::EnumDef(enum_def) => {
                                self.exports.enums.insert(enum_def.name.clone(), enum_def.clone());
                            }
                            _ => {
                                self.error("Cannot export this statement type".to_string());
                            }
                        }
                    }
                    ExportKind::Default(name) => {
                        if self.exports.default.is_some() {
                            self.error("Module already has a default export".to_string());
                        } else {
                            self.exports.default = Some(name.clone());
                        }
                    }
                }
            }

            Stmt::For { binding, index_binding, iterable, body } => {
                // Determine element type from iterable
                let element_type = match iterable {
                    ForIterable::Collection(expr) => {
                        let iter_type = self.check_expr(expr);
                        match iter_type {
                            Type::Array(inner) => *inner,
                            Type::String | Type::Str => Type::String,  // iterating chars
                            Type::Map(key_type, _) => *key_type,       // iterating keys
                            Type::Set(inner) => *inner,
                            _ => {
                                self.error(format!("cannot iterate over {:?}", iter_type));
                                Type::Void
                            }
                        }
                    }
                    ForIterable::CountReceive(chan_expr, count_expr) => {
                        self.check_expr(count_expr);  // validate count
                        let chan_type = self.check_expr(chan_expr);
                        match chan_type {
                            Type::Channel(inner) => *inner,
                            _ => {
                                self.error("count receive requires a channel".to_string());
                                Type::Void
                            }
                        }
                    }
                    ForIterable::MaybeReceive(chan_expr) => {
                        let chan_type = self.check_expr(chan_expr);
                        match chan_type {
                            Type::Channel(inner) => *inner,
                            _ => {
                                self.error("maybe receive requires a channel".to_string());
                                Type::Void
                            }
                        }
                    }
                };

                self.enter_scope();
                self.loop_depth += 1;

                // Define loop binding
                self.define(binding, Symbol {
                    ty: element_type,
                    kind: AssignKind::Const,  // loop var typically immutable
                    ownership: Ownership::Borrowed,
                    defined_at: self.current_depth(),
                    fn_def: None,
                    module_def: None,
                });

                // Define index if present
                if let Some(idx) = index_binding {
                    self.define(idx, Symbol {
                        ty: Type::I64,
                        kind: AssignKind::Const,
                        ownership: Ownership::Owned,
                        defined_at: self.current_depth(),
                        fn_def: None,
                        module_def: None,
                    });
                }

                for s in body {
                    self.check_stmt(s);
                }

                self.loop_depth -= 1;
                self.exit_scope();
            }

            Stmt::Match { expr, cases, default, match_type: _ } => {
                let matched_type = self.check_expr(expr);

                self.match_depth += 1;

                for (patterns, body) in cases {
                    self.enter_scope();

                    for pattern in patterns {
                        match pattern {
                            Pattern::Literal(lit_expr) => {
                                let lit_type = self.check_expr(lit_expr);
                                if !types_compatible(&matched_type, &lit_type) {
                                    self.error(format!(
                                        "pattern type {:?} doesn't match {:?}", lit_type, matched_type
                                    ));
                                }
                            }
                            Pattern::EnumVariant { enum_name, variant, binding } => {
                                // Verify enum exists
                                if let Some(enum_def) = self.enums.get(enum_name).cloned() {
                                    // Verify variant exists
                                    let variant_def = enum_def.variants.iter()
                                        .find(|v| &v.name == variant);
                                    
                                    match variant_def {
                                        Some(v) => {
                                            // If pattern has binding, define it
                                            if let Some(bind_name) = binding {
                                                let bind_type = v.data_type.clone()
                                                    .unwrap_or(Type::Void);
                                                self.define(bind_name, Symbol {
                                                    ty: bind_type,
                                                    kind: AssignKind::Const,
                                                    ownership: Ownership::Borrowed,
                                                    defined_at: self.current_depth(),
                                                    fn_def: None,
                                                    module_def: None,
                                                });
                                            }
                                        }
                                        None => {
                                            self.error(format!(
                                                "unknown variant '{}' on enum '{}'", variant, enum_name
                                            ));
                                        }
                                    }
                                } else {
                                    self.error(format!("unknown enum '{}'", enum_name));
                                }
                            }
                            Pattern::Wildcard => {
                                // Always valid
                            }
                        }
                    }

                    for s in body {
                        self.check_stmt(s);
                    }

                    self.exit_scope();
                }

                // Check default branch
                if let Some(default_body) = default {
                    self.enter_scope();
                    for s in default_body {
                        self.check_stmt(s);
                    }
                    self.exit_scope();
                }
            }
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> Type {
        match expr {
            Expr::I64(_) => Type::I64,
            Expr::I32(_) => Type::I32,
            Expr::U64(_) => Type::U64,
            Expr::U32(_) => Type::U32,
            Expr::F64(_) => Type::F64,
            Expr::F32(_) => Type::F32,
            Expr::String(_) => Type::Str,
            Expr::InterpolatedString(_) => Type::String,
            Expr::Bool(_) => Type::Bool,
            Expr::Add(a, b) => {
                let left = self.check_expr(a);
                let right = self.check_expr(b);
                
                if is_numeric_type(&left) && is_numeric_type(&right) {
                    // Both numeric—return left type (they must match due to strict typing)
                    if left == right {
                        left
                    } else {
                        self.error(format!("cannot add {:?} and {:?}", left, right));
                        left
                    }
                } else if matches!(left, Type::String) && matches!(right, Type::String) {
                    Type::String
                } else if matches!(left, Type::Str) && matches!(right, Type::Str) {
                    Type::String
                } else {
                    self.error(format!("cannot add {:?} and {:?}", left, right));
                    Type::Void
                }
            }
            Expr::Subtract(a, b) | Expr::Multiply(a, b) | Expr::Divide(a, b) | Expr::Modulo(a, b) => {
                let left = self.check_expr(a);
                let right = self.check_expr(b);
                
                if !is_numeric_type(&left) || !is_numeric_type(&right) || left != right {
                    self.error(format!("arithmetic requires matching numeric types"));
                }
                left
            }
            Expr::Power(a, b) => {
                let left = self.check_expr(a);
                let right = self.check_expr(b);
                
                if !is_numeric_type(&left) || !is_numeric_type(&right) || left != right {
                    self.error(format!("power requires matching numeric types"));
                }
                left
            }

            Expr::Negative(e) => {
                let t = self.check_expr(e);
                if !is_numeric_type(&t) {
                    self.error(format!("cannot negate non-number"));
                }
                t
            }
            Expr::Equal(_, _) | Expr::NotEqual(_, _) | Expr::LessThan(_, _) | 
            Expr::LessThanOrEqual(_, _) | Expr::GreaterThan(_, _) | Expr::GreaterThanOrEqual(_, _) => {
                Type::Bool
            }
            Expr::Array(exprs) => {
                if exprs.is_empty() {
                    Type::Array(Box::new(Type::I64))
                } else {
                    let elem_type = self.check_expr(&exprs[0]);
                    for expr in exprs.iter().skip(1) {
                        let other_type = self.check_expr(expr);
                        if !types_compatible(&elem_type, &other_type) {
                            self.error(format!("Array type error: mixed types"));
                        }
                    }
                    Type::Array(Box::new(elem_type))
                }
            }

            Expr::Not(_) => Type::Bool,
            Expr::Var(name) | Expr::Const(name) | Expr::Static(name) => {
                if let Some(sym) = self.lookup(name).cloned() {
                    sym.ty.clone()
                } else {
                    self.error(format!("'{}' not defined", name));
                    Type::Void
                }
            }
            Expr::Thread(closure_expr) => {
                let closure_type = self.check_expr(closure_expr);
                
                let inner_type = match closure_type {
                    Type::Function { return_type, .. } => *return_type,
                    other => {
                        self.error(format!("thread expects closure, got {:?}", other));
                        Type::Void
                    }
                };
                
                Type::Channel(Box::new(inner_type))
            }
            Expr::Call { name, args } => {
                // Check all args (even if function unknown, still validate args)
                for arg in args {
                    self.check_expr(arg);
                }

                if name == "sleep" {
                    if args.len() != 1 {
                        self.error(format!("sleep() takes exactly 1 argument"));
                    }
                    let arg_type = self.check_expr(&args[0]);
                    if !matches!(arg_type, Type::I64 | Type::I32 | Type::U64 | Type::U32) {
                        self.error(format!("sleep() expects an integer, got {:?}", arg_type));
                    }
                    return Type::Void;
                }

                let arg_types: Vec<Type> = args.iter().map(|a| self.check_expr(a)).collect();

                // User-defined function
                if let Some(sym) = self.lookup(name).cloned() {
                    // If the symbol has a concrete FnDef (it's a real function definition)
                    if let Some(fn_def) = &sym.fn_def {
                        // Check if the function uses a Generic "T"
                        // This looks for "T" in parameters OR return type
                        let uses_generic_t = fn_def.params.iter().any(|(_, t, _)| matches!(t, Some(Type::Generic(g)) if g == "T"))
                            || matches!(&fn_def.return_type, Some(Type::Generic(g)) if g == "T");

                        if uses_generic_t {
                            // 1. Validate Arity
                            if fn_def.params.len() != arg_types.len() {
                                self.error(format!("'{}' expects {} arguments, got {}", name, fn_def.params.len(), arg_types.len()));
                                return Type::Void;
                            }

                            // 2. Resolve "T" based on the arguments
                            let mut resolved_t: Option<Type> = None;

                            for (_, ((param_name, param_type, _), arg_type)) in fn_def.params.iter().zip(arg_types.iter()).enumerate() {
                                match param_type {
                                    Some(Type::Generic(g)) if g == "T" => {
                                        // We found an argument that maps to "T".
                                        
                                        // Check if arg is numeric (constraint for math funcs)
                                        if !is_numeric_type(arg_type) {
                                            self.error(format!("Argument '{}' must be numeric, got {:?}", param_name, arg_type));
                                            return Type::Void;
                                        }

                                        match &resolved_t {
                                            None => {
                                                // First time seeing T, lock it in
                                                resolved_t = Some(arg_type.clone());
                                            }
                                            Some(prev) => {
                                                // If T appears multiple times, subsequent args must match the first one
                                                // e.g. min(i64, f64) might be invalid if you enforce strict matching
                                                if prev != arg_type {
                                                    self.error(format!("Type mismatch for generic argument '{}'. Expected {:?}, got {:?}", param_name, prev, arg_type));
                                                }
                                            }
                                        }
                                    }
                                    Some(other) => {
                                        // Regular concrete type check
                                        if !types_compatible(other, arg_type) {
                                            self.error(format!("Argument '{}' expected {:?}, got {:?}", param_name, other, arg_type));
                                        }
                                    }
                                    None => {}
                                }
                            }

                            // 3. Determine Return Type
                            if let Some(Type::Generic(g)) = &fn_def.return_type {
                                if g == "T" {
                                    // Return the resolved type (e.g., if input was i64, return i64)
                                    return resolved_t.unwrap_or(Type::Void);
                                }
                            }
                            
                            // Fallback for concrete returns (though unlikely in this context)
                            return fn_def.return_type.clone().unwrap_or(Type::Void);

                        } else {
                            // Standard validation for non-generic functions
                            self.validate_args(name, &fn_def.params, &arg_types);
                            return fn_def.return_type.clone().unwrap_or(Type::Void);
                        }
                    }

                    // Variable that might be a function/closure
                    match &sym.ty {
                        Type::Function { params, return_type } => {
                            self.validate_args(name, 
                                &params.iter()
                                    .enumerate()
                                    .map(|(i, t)| (format!("arg{}", i), Some(t.clone()), Ownership::Owned))
                                    .collect::<Vec<_>>(),
                                &arg_types
                            );
                            return *return_type.clone();
                        }
                        Type::Void => return Type::Void, // untyped param
                        other => {
                            self.error(format!("'{}' is not callable (type: {:?})", name, other));
                            return Type::Void;
                        }
                    }
                }

                self.error(format!("unknown function: {}", name));
                Type::Void
            }
            Expr::Assign { value, .. } => self.check_expr(value),
            Expr::CompoundAssign { value, .. } => self.check_expr(value),
            Expr::Regex(_) => Type::Regex,
            Expr::ModelInstance { name, .. } => Type::Model(name.clone()),
            Expr::FieldAccess { object, field, value: _ } => {
                let obj_type = self.check_expr(object);
                match obj_type {
                    Type::Model(model_name) => {
                        if let Some(model_def) = self.models.get(&model_name) {
                            // Find field in model definition
                            model_def.fields.iter()
                                .find(|(name, _)| name == field)
                                .map(|(_, field_type)| field_type.clone())
                                .unwrap_or_else(|| {
                                    self.error(format!("field '{}' not found on model '{}'", field, model_name));
                                    Type::Void
                                })
                        } else {
                            self.error(format!("unknown model: {}", model_name));
                            Type::Void
                        }
                    }
                    _ => {
                        self.error(format!("cannot access field on type {:?}", obj_type));
                        Type::Void
                    }
                }
            }
            Expr::MethodCall { object, method, args } => {
                let obj_type = self.check_expr(object);

                // Check args (even if we error, validate them)
                let arg_types: Vec<Type> = args.iter().map(|a| self.check_expr(a)).collect();

                match obj_type {
                    Type::I64 | Type::I32 | Type::U64 | Type::U32 | Type::F64 | Type::F32 => {
                        match method.as_str() {
                            _ => {
                                self.error(format!("Unknown number method: {}", method));
                                Type::Void
                            }
                        }
                    }
                    Type::Array(inner_type) => {
                        match method.as_str() {
                            "push" | "size" | "splice" => Type::I64,
                            "pull" => *inner_type,
                            "slice" | "copy" => Type::Array(inner_type),
                            "each" => Type::Void,
                            _ => {
                                self.error(format!("Unknown array method: {}", method));
                                Type::Void
                            }
                        }
                    }
                    Type::Str | Type::String => {
                        match method.as_str() {
                            "includes" | "starts" | "ends" => Type::Bool,
                            "replace" | "slice" | "copy" | "upper" | "lower" | "trim" | "repeat" => Type::String,
                            "split" => Type::Array(Box::new(Type::String)),
                            "index" | "length" => Type::I64,
                            _ => {
                                self.error(format!("Unknown string method: {}", method));
                                Type::Void
                            }
                        }
                    }
                    Type::Map(key_type, val_type) => {
                        match method.as_str() {
                            "set" => Type::Map(key_type.clone(), val_type.clone()),
                            "get" => (*val_type).clone(),
                            "has" => Type::Bool,
                            "remove" => (*val_type).clone(),
                            "size" => Type::I64,
                            _ => {
                                self.error(format!("Unknown map method: {}", method));
                                Type::Void
                            }
                        }
                    }
                    Type::Set(val_type) => {
                        match method.as_str() {
                            "add" => Type::Set(val_type.clone()),
                            "get" => (*val_type).clone(),
                            "has" => Type::Bool,
                            "remove" => (*val_type).clone(),
                            "size" => Type::I64,
                            _ => {
                                self.error(format!("Unknown set method: {}", method));
                                Type::Void
                            }
                        }
                    }
                    Type::Model(model_name) => {
                        if let Some(fn_def) = self.model_methods.get(&(model_name.clone(), method.clone())) {
                            fn_def.return_type.clone().unwrap_or(Type::Void)
                        } else {
                            self.error(format!("unknown method '{}' on '{}'", method, model_name));
                            Type::Void
                        }
                    }
                    Type::Namespace(ns) => {
                        let qualified_name = format!("{}.{}", ns, method);
                        
                        if let Some(sym) = self.lookup(&qualified_name).cloned() {
                            if let Some(fn_def) = &sym.fn_def {
                                self.validate_args(&qualified_name, &fn_def.params, &arg_types);
                                return fn_def.return_type.clone().unwrap_or(Type::Void);
                            }
                        }

                        self.error(format!("unknown function '{}' in namespace '{}'", method, ns));
                        return Type::Void;
                    }
                    Type::StateActor(inner_type) => {
                        return match method.as_str() {
                            "read" => *inner_type.clone(),
                            "write" => Type::Void,
                            "update" => *inner_type.clone(),
                            _ => {
                                self.error(format!("unknown StateActor method: {}", method));
                                Type::Void
                            }
                        };
                    }
                    Type::Channel(inner_type) => {
                        return match method.as_str() {
                            "send" => Type::Void,
                            "recv" => *inner_type.clone(),
                            "close" => Type::Void,
                            _ => {
                                self.error(format!("unknown Channel method: {}", method));
                                Type::Void
                            }
                        };
                    }
                    _ => {
                        self.error(format!("Cannot call method on type {:?}", obj_type));
                        Type::Void
                    }
                }
            }
            Expr::Closure(closure_def) => {
                let param_types: Vec<Type> = closure_def.params
                    .iter()
                    .map(|(_, ty, _)| ty.clone().unwrap_or(Type::Void))
                    .collect();
                
                let return_type = closure_def.return_type.clone().unwrap_or(Type::Void);
                
                Type::Function {
                    params: param_types,
                    return_type: Box::new(return_type),
                }
            }
            Expr::EnumVariant { enum_name, variant, data } => {
                match enum_name.as_str() {
                    "Result" => {
                        if let Some(expr) = data {
                            let data_type = self.check_expr(expr);
                            let wildcard = Type::Generic("_".to_string());

                            if variant == "Ok" {
                                Type::Enum("Result".to_string(), vec![data_type, wildcard])
                            } else {
                                Type::Enum("Result".to_string(), vec![wildcard, data_type])
                            }
                        } else {
                            Type::Enum("Result".to_string(), vec![])
                        }
                    }
                    "Maybe" => {
                        if let Some(expr) = data {
                            let data_type = self.check_expr(expr);
                            Type::Enum("Maybe".to_string(), vec![data_type])
                        } else {
                            Type::Enum("Maybe".to_string(), vec![])
                        }
                    }
                    _ => {
                        if let Some(expr) = data {
                            let data_type = self.check_expr(expr);
                            Type::Enum(enum_name.clone(), vec![data_type])
                        } else {
                            Type::Enum(enum_name.clone(), vec![])
                        }
                    }
                }
            },

            Expr::CreateChannel(hint) => {
                match hint {
                    Some(inner_type) => Type::Channel(Box::new(inner_type.clone())),
                    None => {
                        self.error(format!("channel() requires a type annotation, e.g., channel(): string"));
                        Type::Channel(Box::new(Type::Void))
                    }
                }
            }

            Expr::Send(chan_expr, val_expr) => {
                let chan_type = self.check_expr(chan_expr);
                let val_type = self.check_expr(val_expr);

                if let Type::Channel(inner) = chan_type {
                    if !types_compatible(&inner, &val_type) {
                        self.error(format!("cannot send {:?} to channel of {:?}", val_type, inner));
                    }
                } else {
                    self.error("left side of <- must be a channel".to_string());
                }
                Type::Void  // always void
            }

            Expr::Receive(chan_expr) => {
                let chan_type = self.check_expr(chan_expr);
                match chan_type {
                    Type::Channel(inner) => *inner,
                    _ => {
                        self.error(format!("right side of <- must be a channel"));
                        Type::Void
                    }
                }
            }

            Expr::CountReceive(chan_expr, message_count) => {
                self.check_expr(message_count);

                let chan_type = self.check_expr(chan_expr);
                match chan_type {
                    Type::Channel(inner) => *inner,
                    _ => {
                        self.error(format!("right side of <-n must be a channel"));
                        Type::Void
                    }
                }
            }

            Expr::MaybeReceive(chan_expr) => {
                let chan_type = self.check_expr(chan_expr);
                if let Type::Channel(inner) = chan_type {
                    // Wrap inner type in Maybe
                    Type::Enum("Maybe".to_string(), vec![*inner])
                } else {
                    self.error(format!("right side of <-? must be a channel"));
                    Type::Void
                }
            }
            Expr::CreateState(initial_expr) => {
                let inner_type = self.check_expr(initial_expr);
                Type::StateActor(Box::new(inner_type))
            }
            Expr::CreateMap(pairs, key_type, val_type) => {
                if pairs.is_empty() {
                    Type::Map(Box::new(key_type.clone()), Box::new(val_type.clone()))
                } else {
                    // Infer from first pair
                    let (first_key, first_value) = &pairs[0];
                    let key_type = self.check_expr(first_key);
                    let val_type = self.check_expr(first_value);
                    
                    // Validate all pairs match
                    for (k_expr, v_expr) in pairs.iter().skip(1) {
                        let k_type = self.check_expr(k_expr);
                        let v_type = self.check_expr(v_expr);
                        
                        if !types_compatible(&key_type, &k_type) {
                            self.error(format!("map key type mismatch: expected {:?}, got {:?}", key_type, k_type));
                        }
                        if !types_compatible(&val_type, &v_type) {
                            self.error(format!("map value type mismatch: expected {:?}, got {:?}", val_type, v_type));
                        }
                    }
                    
                    Type::Map(Box::new(key_type), Box::new(val_type))
                }
            }
            Expr::CreateSet(values, val_type) => {
                if values.is_empty() {
                    Type::Set(Box::new(val_type.clone()))
                } else {
                    // Infer from first pair
                    let val_type = self.check_expr(&values[0]);
                    
                    // Validate all values match
                    for v_expr in values.iter().skip(1) {
                        let v_type = self.check_expr(v_expr);
                        
                        if !types_compatible(&val_type, &v_type) {
                            self.error(format!("set type mismatch: expected {:?}, got {:?}", val_type, v_type));
                        }
                    }
                    
                    Type::Set(Box::new(val_type))
                }
            }
        }
    }
}