use crate::parser::{Statement, Expr, Type, EnumDef, EnumVariantDef, Pattern, ModelDef, RoleDef};
use crate::lexer::Token;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct Symbol {
    pub ty: Type,
    pub is_mutable: bool,
}

pub struct Analyzer {
    scopes: Vec<HashMap<String, Symbol>>,
    errors: Vec<String>,
    current_return_type: Option<Type>,
    infer_stack: Vec<Option<Type>>,
    enums: HashMap<String, EnumDef>,
    models: HashMap<String, ModelDef>,
    roles: HashMap<String, RoleDef>,
    custom_methods: HashMap<Type, HashMap<String, Type>>,
    model_roles: HashMap<Type, HashSet<String>>,
}

impl Analyzer {
    pub fn new() -> Self {
        let mut global_scope = HashMap::new();
        
        // Register Built-ins
        global_scope.insert("print".to_string(), Symbol {
            ty: Type::Function {
                params: vec![Type::Any],
                return_type: Box::new(Type::Void),
            },
            is_mutable: false,
        });

        global_scope.insert("echo".to_string(), Symbol {
            ty: Type::Function {
                params: vec![Type::Any],
                return_type: Box::new(Type::Void),
            },
            is_mutable: false,
        });

        global_scope.insert("sleep".to_string(), Symbol {
            ty: Type::Function {
                params: vec![Type::I64],
                return_type: Box::new(Type::Void),
            },
            is_mutable: false,
        });

        // Built-in Result
        let result_enum = EnumDef {
            name: "Result".to_string(),
            variants: vec![
                EnumVariantDef { name: "Ok".to_string(), data_type: Some(Type::Any) },
                EnumVariantDef { name: "Error".to_string(), data_type: Some(Type::Any) },
            ],
        };
        
        // Built-in Maybe
        let maybe_enum = EnumDef {
            name: "Maybe".to_string(),
            variants: vec![
                EnumVariantDef { name: "Value".to_string(), data_type: Some(Type::Any) },
                EnumVariantDef { name: "Null".to_string(), data_type: None },
            ],
        };

        let mut analyzer = Self {
            scopes: vec![global_scope],
            errors: Vec::new(),
            current_return_type: None,
            infer_stack: Vec::new(),
            enums: HashMap::new(),
            models: HashMap::new(),
            roles: HashMap::new(),
            custom_methods: HashMap::new(),
            model_roles: HashMap::new(),
        };

        analyzer.register_enum(&result_enum);
        analyzer.register_enum(&maybe_enum);

        analyzer
    }

    pub fn analyze(&mut self, statements: &[Statement]) -> Result<(), Vec<String>> {
        // Pass 1a: Collect all top-level types (Enums, Models, Roles)
        for stmt in statements {
            match stmt {
                Statement::Enum(enum_def) => {
                    self.register_enum(enum_def);
                }
                Statement::Model(model_def) => {
                    self.models.insert(model_def.name.clone(), model_def.clone());
                }
                Statement::Role(role_def) => {
                    self.roles.insert(role_def.name.clone(), role_def.clone());
                }
                _ => {}
            }
        }

        // Pass 1b: Collect function signatures and extensions with normalized types
        for stmt in statements {
            match stmt {
                Statement::Function { name, params, return_type, .. } => {
                    let mut param_types = Vec::new();
                    for p in params {
                        let ty = p.ty.clone().unwrap_or(Type::Any);
                        param_types.push(self.normalize_type(ty));
                    }
                    let ret_ty = return_type.clone().unwrap_or(Type::Void);
                    let ret_ty = self.normalize_type(ret_ty);
                    self.define(name, Type::Function {
                        params: param_types,
                        return_type: Box::new(ret_ty),
                    }, false); 
                }
                Statement::Extend { target, role, methods } => {
                    let target = self.normalize_type(target.clone());
                    if let Some(role_name) = role {
                        self.model_roles.entry(target.clone()).or_insert_with(HashSet::new).insert(role_name.clone());
                    }
                    
                    let mut methods_to_add = Vec::new();
                    for m in methods {
                        if let Statement::Function { name, params, return_type, .. } = m {
                            let mut param_types = Vec::new();
                            for p in params {
                                let ty = p.ty.clone().unwrap_or(Type::Any);
                                param_types.push(self.normalize_type(ty));
                            }
                            let ret_ty = return_type.clone().unwrap_or(Type::Void);
                            let ret_ty = self.normalize_type(ret_ty);
                            methods_to_add.push((name.clone(), Type::Function {
                                params: param_types,
                                return_type: Box::new(ret_ty),
                            }));
                        }
                    }

                    let methods_map = self.custom_methods.entry(target).or_insert_with(HashMap::new);
                    for (name, ty) in methods_to_add {
                        methods_map.insert(name, ty);
                    }
                }
                _ => {}
            }
        }

        for stmt in statements {
            self.check_statement(stmt);
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn define(&mut self, name: &str, ty: Type, is_mutable: bool) {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(name) {
                if let Some(existing) = scope.get(name) {
                     if existing.ty == ty { return; }
                }
                self.errors.push(format!("Error: '{}' already defined in this scope", name));
                return;
            }
            scope.insert(name.to_string(), Symbol { ty, is_mutable });
        }
    }

    fn register_enum(&mut self, enum_def: &EnumDef) {
        self.enums.insert(enum_def.name.clone(), enum_def.clone());
        // Register variants in global scope (accessible as bare names if unambiguous)
        for v in &enum_def.variants {
            self.define(&v.name, Type::Any, false);
        }
    }

    fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.scopes.iter().rev().find_map(|s| s.get(name))
    }

    fn check_statement(&mut self, stmt: &Statement) -> Type {
        match stmt {
            Statement::Declaration { name, is_mutable, value, declared_type, .. } => {
                let inferred = self.check_expression(value);
                
                if let Type::Array(inner) = &inferred {
                    if matches!(**inner, Type::Any) && declared_type.is_none() {
                        self.errors.push(format!("Type Error: Cannot infer type for empty array variable '{}'. Please provide an explicit type annotation.", name));
                    }
                }

                let ty = if let Some(declared) = declared_type {
                    if !self.types_compatible(declared, &inferred) {
                        self.errors.push(format!("Type Error: '{}' expected {:?}, but got {:?}", name, declared, inferred));
                    }
                    declared.clone()
                } else {
                    inferred
                };

                if let Type::Channel(_) = &ty {
                    if *is_mutable {
                        self.errors.push(format!("Type Error: Channel '{}' must be declared as 'const' or 'static', not 'var'.", name));
                    }
                }

                // Closure
                if let Type::Function { .. } = &ty {
                    if *is_mutable {
                        self.errors.push(format!("Type Error: Closure '{}' must be declared as 'const' or 'static', not 'var'.", name));
                    }
                }

                self.define(name, ty, *is_mutable);
                Type::Void
            }
            Statement::Expression(expr) => self.check_expression(expr),
            Statement::Function { name, params, body, return_type, .. } => {
                // Register local function in outer scope first
                let mut param_types = Vec::new();
                for p in params {
                    if let Some(ty) = &p.ty {
                        param_types.push(ty.clone());
                    } else {
                        param_types.push(Type::Any);
                    }
                }
                let actual_return_type = return_type.clone().unwrap_or(Type::Void);
                
                // Check if function is already defined (from Pass 1 or previous statement)
                // If this is a nested function, we still need to define it.
                // If it's top-level, it's already defined in Pass 1.
                let mut already_defined = false;
                if let Some(scope) = self.scopes.last() {
                    if scope.contains_key(name) {
                        already_defined = true;
                    }
                }

                if !already_defined {
                     self.define(name, Type::Function {
                        params: param_types.clone(),
                        return_type: Box::new(actual_return_type.clone()),
                    }, false);
                }

                let prev_return = self.current_return_type.clone();
                self.current_return_type = Some(actual_return_type);

                self.enter_scope();
                for param in params {
                    if let Some(ty) = &param.ty {
                        self.define(&param.name, ty.clone(), true);
                    } else {
                        self.define(&param.name, Type::Any, true);
                    }
                }

                for s in body {
                    self.check_statement(s);
                }

                self.exit_scope();
                self.current_return_type = prev_return;
                Type::Void
            }
            Statement::If { condition, then_branch, else_branch } => {
                self.check_expression(condition);
                self.check_statement(then_branch);
                if let Some(eb) = else_branch {
                    self.check_statement(eb);
                }
                Type::Void
            }
            Statement::While { condition, body, .. } => {
                self.check_expression(condition);
                self.check_statement(body);
                Type::Void
            }
            Statement::For { binding, index_binding, iterable, body } => {
                let iter_ty = self.check_expression(iterable);
                let element_ty = match iter_ty {
                    Type::Array(inner) => *inner,
                    Type::Tuple(_) => Type::Any,
                    Type::Enum(ref name) if name == "Maybe" => Type::Any,
                    Type::Channel(inner) => *inner,
                    Type::Any => Type::Any,
                    _ => {
                        self.errors.push(format!("Type Error: Cannot iterate over type {:?}", iter_ty));
                        Type::Any
                    }
                };

                self.enter_scope();
                self.define(binding, element_ty, false);
                if let Some(idx_name) = index_binding {
                    self.define(idx_name, Type::I64, false);
                }
                self.check_statement(body);
                self.exit_scope();
                Type::Void
            }
            Statement::Match { condition, arms, .. } => {
                let cond_ty = self.check_expression(condition);
                for arm in arms {
                    self.enter_scope();
                    for pattern in &arm.patterns {
                        self.check_pattern(pattern, &cond_ty);
                    }
                    self.check_statement(&arm.body);
                    self.exit_scope();
                }
                Type::Void
            }
            Statement::Enum(_) => Type::Void,
            Statement::Next => Type::Void,
            Statement::Model(_) => Type::Void,
            Statement::Role(_) => Type::Void,
            Statement::Extend { target, role, methods } => {
                // If role is provided, verify it exists and methods match
                if let Some(role_name) = role {
                    if let Some(role_def) = self.roles.get(role_name).cloned() {
                        for rm in &role_def.methods {
                            if let Statement::Function { name: rm_name, params: rm_params, return_type: rm_ret, .. } = rm {
                                if let Some(m) = methods.iter().find(|m| {
                                    if let Statement::Function { name: m_name, .. } = m {
                                         m_name == rm_name
                                    } else { false }
                                }) {
                                     if let Statement::Function { params: m_params, return_type: m_ret, .. } = m {
                                         if m_params.len() != rm_params.len() {
                                             self.errors.push(format!("Type Error: Method '{}' in extend of '{:?}' with role '{}' has wrong number of parameters", rm_name, target, role_name));
                                         }
                                     }
                                } else {
                                    self.errors.push(format!("Type Error: Target '{:?}' does not implement method '{}' required by role '{}'", target, rm_name, role_name));
                                }
                            }
                        }
                    } else {
                        self.errors.push(format!("Type Error: Cannot use unknown role '{}' in extend", role_name));
                    }
                }

                // Check method bodies
                for method in methods {
                    if let Statement::Function { name, params, body, return_type, .. } = method {
                        let actual_return_type = return_type.clone().unwrap_or(Type::Void);
                        let prev_return = self.current_return_type.clone();
                        self.current_return_type = Some(actual_return_type);

                        self.enter_scope();
                        self.define("this", target.clone(), false);

                        for param in params {
                            self.define(&param.name, param.ty.clone().unwrap_or(Type::Any), true);
                        }

                        for s in body {
                            self.check_statement(s);
                        }

                        self.exit_scope();
                        self.current_return_type = prev_return;
                    }
                }
                Type::Void
            }
            Statement::Block(stmts) => {
                self.enter_scope();
                for s in stmts { self.check_statement(s); }
                self.exit_scope();
                Type::Void
            }
            Statement::Print(expr) => {
                self.check_expression(expr);
                Type::Void
            }
            Statement::Return(expr_opt) => {
                let actual = if let Some(expr) = expr_opt {
                    self.check_expression(expr)
                } else {
                    Type::Void
                };

                if let Some(expected) = &self.current_return_type {
                    if !self.types_compatible(expected, &actual) {
                        self.errors.push(format!("Type Error: Return type mismatch. Expected {:?}, got {:?}", expected, actual));
                    }
                } else if let Some(last_inferred) = self.infer_stack.last_mut() {
                    match last_inferred {
                        Some(prev) => {
                            let prev_cloned = prev.clone();
                            if !self.types_compatible(&prev_cloned, &actual) {
                                self.errors.push(format!("Type Error: Return type inconsistency. Previously inferred {:?}, but now got {:?}", prev_cloned, actual));
                            }
                        }
                        None => {
                            *last_inferred = Some(actual);
                        }
                    }
                }
                Type::Void
            }
            Statement::Break => Type::Void,
            Statement::Import { names, source } => {
                // Handle toolbox imports by registering functions in scope
                if source.starts_with("tbx::") {
                    let module = source.strip_prefix("tbx::").unwrap();
                    for name in names {
                        // Determine the function type based on known toolbox functions
                        let fn_type = match (module, name.as_str()) {
                            ("test", "assert") => Type::Function {
                                params: vec![Type::Bool],
                                return_type: Box::new(Type::Void),
                            },
                            ("math", "abs") | ("math", "round") | ("math", "floor") | ("math", "ceil") => Type::Function {
                                params: vec![Type::Any],
                                return_type: Box::new(Type::Any),
                            },
                            ("math", "sin") => Type::Function {
                                params: vec![Type::F64],
                                return_type: Box::new(Type::F64),
                            },
                            _ => Type::Function {
                                params: vec![Type::Any],
                                return_type: Box::new(Type::Any),
                            },
                        };
                        self.define(name, fn_type, false);
                    }
                }
                Type::Void
            }
            _ => Type::Void,
        }
    }

    fn check_expression(&mut self, expr: &Expr) -> Type {
        match expr {
            Expr::Number(_) => Type::I64,
            Expr::Float(_) => Type::F64,
            Expr::String(_) => Type::String,
            Expr::InterpolatedString(parts) => {
                for part in parts {
                    self.check_expression(part);
                }
                Type::String
            }
            Expr::Bool(_) => Type::Bool,
            Expr::Closure { params, body, return_type } => {
                self.enter_scope();
                let mut param_types = Vec::new();
                for param in params {
                    let ty = if let Some(t) = &param.ty {
                         t.clone()
                    } else {
                         Type::Any 
                    };
                    self.define(&param.name, ty.clone(), false);
                    param_types.push(ty);
                }
                
                let previous_return_type = self.current_return_type.clone();
                self.current_return_type = return_type.clone();
                
                if return_type.is_none() {
                    self.infer_stack.push(None);
                }

                self.check_statement(body);
                
                let actual_ret = if return_type.is_none() {
                    self.infer_stack.pop().unwrap().unwrap_or(Type::Void)
                } else {
                    return_type.clone().unwrap()
                };

                self.current_return_type = previous_return_type;
                self.exit_scope();
                
                Type::Function {
                    params: param_types,
                    return_type: Box::new(actual_ret),
                }
            }
            Expr::Variable(name) => {
                if let Some(sym) = self.lookup(name) {
                    sym.ty.clone()
                } else {
                    self.errors.push(format!("Error: Undefined variable '{}'", name));
                    Type::Void
                }
            }
            Expr::Binary { left, op, right } => {
                let l_ty = self.check_expression(left);
                let r_ty = self.check_expression(right);
                
                match op {
                    Token::EqualEqual | Token::NotEqual => {
                        if !self.types_compatible(&l_ty, &r_ty) && !self.types_compatible(&r_ty, &l_ty) {
                             self.errors.push(format!("Type Error: Cannot compare {:?} and {:?}", l_ty, r_ty));
                        }
                        return Type::Bool;
                    }
                    Token::LessThan | Token::GreaterThan | Token::LessThanEqual | Token::GreaterThanEqual => {
                        if !self.is_numeric(&l_ty) || !self.is_numeric(&r_ty) {
                             self.errors.push(format!("Type Error: Comparison requires numeric types, got {:?} and {:?}", l_ty, r_ty));
                        }
                        return Type::Bool;
                    }
                    _ => {}
                }

                if self.is_numeric(&l_ty) && self.is_numeric(&r_ty) {
                    if matches!(l_ty, Type::F64) || matches!(r_ty, Type::F64) {
                        Type::F64
                    } else {
                        l_ty
                    }
                } else if (l_ty == Type::String || r_ty == Type::String) && matches!(op, Token::Plus) {
                    // Allow String + String, String + Any, or Any + String
                    if l_ty != Type::String && l_ty != Type::Any {
                         self.errors.push(format!("Type Error: Cannot perform binary operation on {:?} and {:?}", l_ty, r_ty));
                    }
                    if r_ty != Type::String && r_ty != Type::Any {
                         self.errors.push(format!("Type Error: Cannot perform binary operation on {:?} and {:?}", l_ty, r_ty));
                    }
                    Type::String
                } else {
                    self.errors.push(format!("Type Error: Cannot perform binary operation on {:?} and {:?}", l_ty, r_ty));
                    Type::Void
                }
            }
            Expr::Assign { name, value } => {
                let val_type = self.check_expression(value);
                let sym_info = self.lookup(name).cloned();
                if let Some(sym) = sym_info {
                    if !sym.is_mutable {
                        self.errors.push(format!("Error: Cannot reassign immutable variable '{}'", name));
                    }
                    if !self.types_compatible(&sym.ty, &val_type) {
                        self.errors.push(format!("Type Error: Cannot assign {:?} to variable '{}' of type {:?}", val_type, name, sym.ty));
                    }
                } else {
                    self.errors.push(format!("Error: Undefined variable '{}'", name));
                }
                val_type
            }
            Expr::Call { name, args } => {
                let arg_types: Vec<Type> = args.iter().map(|a| self.check_expression(a)).collect();
                
                // Check if it's an enum variant constructor
                if let Some(e) = self.enums.values().find(|e| e.variants.iter().any(|v| &v.name == name)) {
                    let variant = e.variants.iter().find(|v| &v.name == name).unwrap();
                    if let Some(expected_ty) = &variant.data_type {
                        if args.len() != 1 {
                             self.errors.push(format!("Error: Enum variant '{}' expects 1 argument, got {}", name, args.len()));
                        } else if !self.types_compatible(expected_ty, &arg_types[0]) {
                             self.errors.push(format!("Type Error: Enum variant '{}' expected {:?}, got {:?}", name, expected_ty, arg_types[0]));
                        }
                    } else if !args.is_empty() {
                        self.errors.push(format!("Error: Enum variant '{}' does not take any arguments", name));
                    }
                    return Type::Enum(e.name.clone());
                }

                let sym_info = self.lookup(name).cloned();
                
                if let Some(sym) = sym_info {
                    if let Type::Function { params, return_type } = sym.ty {
                        if params.len() != arg_types.len() {
                            self.errors.push(format!("Error: '{}' expects {} arguments, got {}", name, params.len(), arg_types.len()));
                        } else {
                            for (i, (expected, actual)) in params.iter().zip(arg_types.iter()).enumerate() {
                                if !self.types_compatible(expected, actual) {
                                    self.errors.push(format!("Type Error: '{}' argument {} expected {:?}, got {:?}", name, i + 1, expected, actual));
                                }
                            }
                        }
                        return *return_type;
                    } else {
                        self.errors.push(format!("Error: '{}' is not a function", name));
                    }
                } else {
                    self.errors.push(format!("Error: Undefined function '{}'", name));
                }
                Type::Void
            }
            Expr::EnumVariant { enum_name, variant_name, args } => {
                let arg_types: Vec<Type> = args.iter().map(|a| self.check_expression(a)).collect();
                
                let enum_def = if let Some(e_name) = enum_name {
                    self.enums.get(e_name)
                } else {
                    self.enums.values().find(|e| e.variants.iter().any(|v| &v.name == variant_name))
                };

                if let Some(e) = enum_def {
                    let variant = e.variants.iter().find(|v| &v.name == variant_name).unwrap();
                    if let Some(expected_ty) = &variant.data_type {
                        if args.len() != 1 {
                            self.errors.push(format!("Error: Enum variant '{}' expects 1 argument, got {}", variant_name, args.len()));
                        } else if !self.types_compatible(expected_ty, &arg_types[0]) {
                            self.errors.push(format!("Type Error: Enum variant '{}' expected {:?}, got {:?}", variant_name, expected_ty, arg_types[0]));
                        }
                    } else if !args.is_empty() {
                         self.errors.push(format!("Error: Enum variant '{}' does not take any arguments", variant_name));
                    }
                    Type::Enum(e.name.clone())
                } else {
                    self.errors.push(format!("Type Error: Unknown enum variant '{}'", variant_name));
                    Type::Any
                }
            }
            Expr::Array(elements) => {
                if elements.is_empty() {
                    Type::Array(Box::new(Type::Any))
                } else {
                    let first_ty = self.check_expression(&elements[0]);
                    for el in elements.iter().skip(1) {
                        let ty = self.check_expression(el);
                        if !self.types_compatible(&first_ty, &ty) {
                            self.errors.push(format!("Type Error: Array contains mixed types. Expected {:?}, got {:?}", first_ty, ty));
                        }
                    }
                    Type::Array(Box::new(first_ty))
                }
            }
            Expr::Tuple(elements) => {
                let types = elements.iter().map(|e| self.check_expression(e)).collect();
                Type::Tuple(types)
            }
            Expr::Thread(expr) => {
                let closure_ty = self.check_expression(expr);
                if let Type::Function { return_type, .. } = closure_ty {
                    Type::Channel(return_type)
                } else {
                    self.errors.push("Type Error: thread() expects a closure".to_string());
                    Type::Void
                }
            }
            Expr::Send(chan_expr, val_expr) => {
                let chan_ty = self.check_expression(chan_expr);
                let val_ty = self.check_expression(val_expr);
                if let Type::Channel(inner) = chan_ty {
                    if !self.types_compatible(&inner, &val_ty) {
                        self.errors.push(format!("Type Error: Cannot send {:?} to channel of type {:?}", val_ty, *inner));
                    }
                } else {
                    self.errors.push(format!("Type Error: Cannot send to non-channel type {:?}", chan_ty));
                }
                Type::Void
            }
            Expr::Receive(chan_expr) => {
                let chan_ty = self.check_expression(chan_expr);
                if let Type::Channel(inner) = chan_ty {
                    *inner
                } else {
                    self.errors.push(format!("Type Error: Cannot receive from non-channel type {:?}", chan_ty));
                    Type::Any
                }
            }
            Expr::Channel(inner_ty) => {
                if inner_ty.is_none() {
                    self.errors.push(format!("Type Error: channel() requires a data type suffix (e.g. channel(): i64)"));
                }
                Type::Channel(Box::new(inner_ty.clone().unwrap_or(Type::Any)))
            }
            Expr::ReceiveCount(chan_expr, count_expr) => {
                let chan_ty = self.check_expression(chan_expr);
                let _count_ty = self.check_expression(count_expr);
                // TODO: verify count is numeric
                if let Type::Channel(inner) = chan_ty {
                    Type::Array(inner)
                } else {
                    self.errors.push(format!("Type Error: Cannot receive from non-channel type {:?}", chan_ty));
                    Type::Any
                }
            }
            Expr::ReceiveMaybe(chan_expr) => {
                let chan_ty = self.check_expression(chan_expr);
                if let Type::Channel(_) = chan_ty {
                    Type::Enum("Maybe".to_string())
                } else {
                    self.errors.push(format!("Type Error: Cannot receive from non-channel type {:?}", chan_ty));
                    Type::Enum("Maybe".to_string())
                }
            }
            Expr::State(initial_expr) => {
                let initial_ty = self.check_expression(initial_expr);
                Type::State(Box::new(initial_ty))
            }
            Expr::ModelInstance { name, fields } => {
                if let Some(model_def) = self.models.get(name).cloned() {
                    for (f_name, f_val_expr) in fields {
                        if let Some((_, f_ty)) = model_def.fields.iter().find(|(n, _)| n == f_name) {
                            let val_ty = self.check_expression(f_val_expr);
                            if !self.types_compatible(f_ty, &val_ty) {
                                self.errors.push(format!("Type Error: Field '{}' in model '{}' expected {:?}, got {:?}", f_name, name, f_ty, val_ty));
                            }
                        } else {
                            self.errors.push(format!("Type Error: Model '{}' has no field named '{}'", name, f_name));
                        }
                    }
                    for (f_name, _) in &model_def.fields {
                        if !fields.iter().any(|(n, _)| n == f_name) {
                            self.errors.push(format!("Type Error: Missing field '{}' in instantiation of model '{}'", f_name, name));
                        }
                    }
                    Type::Model(name.clone())
                } else {
                    self.errors.push(format!("Type Error: Undefined model '{}'", name));
                    Type::Any
                }
            }
            Expr::FieldAccess { object, field } => {
                let obj_ty = self.check_expression(object);
                match obj_ty {
                    Type::Model(name) => {
                        if let Some(model_def) = self.models.get(&name) {
                            if let Some((_, f_ty)) = model_def.fields.iter().find(|(f, _)| f == field) {
                                f_ty.clone()
                            } else {
                                self.errors.push(format!("Type Error: Model '{}' has no field named '{}'", name, field));
                                Type::Any
                            }
                        } else {
                            self.errors.push(format!("Type Error: Undefined model '{}'", name));
                            Type::Any
                        }
                    }
                    _ => {
                        self.errors.push(format!("Type Error: Cannot access field '{}' on non-model type {:?}", field, obj_ty));
                        Type::Any
                    }
                }
            }
            Expr::This => {
                if let Some(sym) = self.lookup("this") {
                    sym.ty.clone()
                } else {
                    self.errors.push("Type Error: 'this' can only be used inside model methods".to_string());
                    Type::Any
                }
            }
            Expr::MethodCall(obj_expr, method, args) => {
                let obj_ty = self.check_expression(obj_expr);
                let obj_ty = self.normalize_type(obj_ty);
                
                // Check custom methods from 'extend'
                let mut custom_method_ty = None;
                if let Some(methods_map) = self.custom_methods.get(&obj_ty) {
                    if let Some(method_ty) = methods_map.get(method) {
                        custom_method_ty = Some(method_ty.clone());
                    }
                }

                if let Some(Type::Function { params, return_type }) = custom_method_ty {
                    if args.len() != params.len() {
                        self.errors.push(format!("Type Error: Method '{}' expected {} arguments, got {}", method, params.len(), args.len()));
                    }
                    for (p_ty, arg_expr) in params.iter().zip(args) {
                        let arg_ty = self.check_expression(arg_expr);
                        if !self.types_compatible(p_ty, &arg_ty) {
                            self.errors.push(format!("Type Error: Method '{}' parameter mismatch, expected {:?}, got {:?}", method, p_ty, arg_ty));
                        }
                    }
                    return *return_type;
                }
                match obj_ty.clone() {
                    Type::Model(name) => {
                         self.errors.push(format!("Type Error: Method '{}' not found on model '{}'", method, name));
                         Type::Any
                    }
                    Type::Role(role_name) => {
                        if let Some(role_def) = self.roles.get(&role_name) {
                            let mut found_method = None;
                            for m in &role_def.methods {
                                if let Statement::Function { name, params, return_type, .. } = m {
                                    if name == method {
                                        let mut param_types = Vec::new();
                                        for p in params {
                                            param_types.push(p.ty.clone().unwrap_or(Type::Any));
                                        }
                                        let ret_ty = return_type.clone().unwrap_or(Type::Void);
                                        found_method = Some((param_types, ret_ty));
                                        break;
                                    }
                                }
                            }
                            
                            if let Some((params, ret_ty)) = found_method {
                                if args.len() != params.len() {
                                    self.errors.push(format!("Type Error: Role method '{}' expected {} arguments, got {}", method, params.len(), args.len()));
                                }
                                for (p_ty, arg_expr) in params.iter().zip(args) {
                                    let arg_ty = self.check_expression(arg_expr);
                                    if !self.types_compatible(p_ty, &arg_ty) {
                                        self.errors.push(format!("Type Error: Role method '{}' parameter mismatch, expected {:?}, got {:?}", method, p_ty, arg_ty));
                                    }
                                }
                                return ret_ty;
                            } else {
                                self.errors.push(format!("Type Error: Method '{}' not found in role '{}'", method, role_name));
                                Type::Any
                            }
                        } else {
                            self.errors.push(format!("Type Error: Unknown role '{}'", role_name));
                            Type::Any
                        }
                    }
                    // Array methods
                    Type::Array(inner) => {
                        match method.as_str() {
                            "size" => {
                                if !args.is_empty() { self.errors.push("Type Error: .size() takes no arguments".to_string()); }
                                Type::I64
                            }
                            "push" => {
                                if args.is_empty() { 
                                    self.errors.push("Type Error: .push() requires at least 1 argument".to_string()); 
                                } else {
                                    let arg_ty = self.check_expression(&args[0]);
                                    if !self.types_compatible(&inner, &arg_ty) {
                                        self.errors.push(format!("Type Error: Cannot push {:?} to array of {:?}", arg_ty, *inner));
                                    }
                                    if args.len() > 1 {
                                        let idx_ty = self.check_expression(&args[1]);
                                        if !self.types_compatible(&Type::I64, &idx_ty) {
                                            self.errors.push(format!("Type Error: push() index must be I64, got {:?}", idx_ty));
                                        }
                                    }
                                }
                                Type::I64 // Returns new size
                            }
                            "pull" => {
                                if args.len() > 1 { self.errors.push("Type Error: .pull() takes at most 1 argument".to_string()); }
                                if !args.is_empty() {
                                    let idx_ty = self.check_expression(&args[0]);
                                    if !self.types_compatible(&Type::I64, &idx_ty) {
                                        self.errors.push(format!("Type Error: pull() index must be I64, got {:?}", idx_ty));
                                    }
                                }
                                *inner
                            }
                            "splice" => {
                                if args.len() < 1 { 
                                    self.errors.push("Type Error: .splice() requires an array as the first argument".to_string()); 
                                } else {
                                    let items_ty = self.check_expression(&args[0]);
                                    if !self.types_compatible(&Type::Array(inner.clone()), &items_ty) {
                                        self.errors.push(format!("Type Error: .splice() first argument must be array of {:?}, got {:?}", *inner, items_ty));
                                    }
                                    if args.len() > 1 {
                                        let idx_ty = self.check_expression(&args[1]);
                                        if !self.types_compatible(&Type::I64, &idx_ty) {
                                            self.errors.push(format!("Type Error: splice() index must be I64, got {:?}", idx_ty));
                                        }
                                    }
                                }
                                Type::I64 // Returns new size
                            }
                            "slice" | "copy" => {
                                // args: start, optional end
                                for arg in args {
                                    let arg_ty = self.check_expression(arg);
                                    if !self.types_compatible(&Type::I64, &arg_ty) {
                                        self.errors.push(format!("Type Error: {} index must be I64, got {:?}", method, arg_ty));
                                    }
                                }
                                Type::Array(inner)
                            }
                            "each" => {
                                if args.len() != 1 { 
                                    self.errors.push("Type Error: .each() takes 1 argument".to_string()); 
                                } else {
                                    let closure_ty = self.check_expression(&args[0]);
                                    if let Type::Function { params, .. } = closure_ty {
                                        if params.is_empty() || !self.types_compatible(&params[0], &inner) {
                                             self.errors.push(format!("Type Error: .each() closure must accept item of type {:?}", *inner));
                                        }
                                    } else {
                                        self.errors.push("Type Error: .each() expects a closure".to_string());
                                    }
                                }
                                Type::Void
                            }
                            _ => {
                                self.errors.push(format!("Type Error: Unknown array method '{}'", method));
                                Type::Any
                            }
                        }
                    }
                    
                    // String methods
                    Type::String => {
                        match method.as_str() {
                            "length" => {
                                if !args.is_empty() { self.errors.push("Type Error: .length() takes no arguments".to_string()); }
                                Type::I64
                            }
                            "index" => {
                                if args.is_empty() { self.errors.push("Type Error: .index() requires at least 1 argument".to_string()); }
                                for (i, arg) in args.iter().enumerate() {
                                    let arg_ty = self.check_expression(arg);
                                    if i == 0 {
                                        if !self.types_compatible(&Type::String, &arg_ty) {
                                            self.errors.push(format!("Type Error: index() pattern must be string, got {:?}", arg_ty));
                                        }
                                    } else if !self.types_compatible(&Type::I64, &arg_ty) {
                                        self.errors.push(format!("Type Error: index() start index must be I64, got {:?}", arg_ty));
                                    }
                                }
                                Type::I64
                            }
                            "includes" | "starts" | "ends" => {
                                if args.len() != 1 { self.errors.push(format!("Type Error: .{}() takes 1 argument", method)); }
                                else {
                                    let arg_ty = self.check_expression(&args[0]);
                                    if !self.types_compatible(&Type::String, &arg_ty) {
                                        self.errors.push(format!("Type Error: .{}() argument must be string", method));
                                    }
                                }
                                Type::Bool
                            }
                            "upper" | "lower" | "trim" => {
                                if !args.is_empty() { self.errors.push(format!("Type Error: .{}() takes no arguments", method)); }
                                Type::String
                            }
                            "replace" => {
                                if args.len() != 2 { self.errors.push("Type Error: .replace() takes 2 arguments".to_string()); }
                                for arg in args {
                                    let arg_ty = self.check_expression(arg);
                                    if !self.types_compatible(&Type::String, &arg_ty) {
                                        self.errors.push(format!("Type Error: replace() arguments must be strings, got {:?}", arg_ty));
                                    }
                                }
                                Type::String
                            }
                            "repeat" => {
                                if args.len() != 1 { self.errors.push("Type Error: .repeat() takes 1 argument".to_string()); }
                                else {
                                    let arg_ty = self.check_expression(&args[0]);
                                    if !self.types_compatible(&Type::I64, &arg_ty) {
                                        self.errors.push(format!("Type Error: repeat() count must be I64, got {:?}", arg_ty));
                                    }
                                }
                                Type::String
                            }
                            "copy" | "slice" => {
                                for arg in args {
                                    let arg_ty = self.check_expression(arg);
                                    if !self.types_compatible(&Type::I64, &arg_ty) {
                                        self.errors.push(format!("Type Error: {} index must be I64, got {:?}", method, arg_ty));
                                    }
                                }
                                Type::String
                            }
                            "split" => {
                                if args.len() > 1 { self.errors.push("Type Error: .split() takes at most 1 argument".to_string()); }
                                if !args.is_empty() {
                                    let arg_ty = self.check_expression(&args[0]);
                                    if !self.types_compatible(&Type::String, &arg_ty) {
                                        self.errors.push(format!("Type Error: split() delimiter must be string, got {:?}", arg_ty));
                                    }
                                }
                                Type::Array(Box::new(Type::String))
                            }
                            _ => {
                                self.errors.push(format!("Type Error: Unknown string method '{}'", method));
                                Type::Any
                            }
                        }
                    }
                    
                    // Tuple methods
                    Type::Tuple(_) => {
                        match method.as_str() {
                            "size" => {
                                if !args.is_empty() { self.errors.push("Type Error: .size() takes no arguments".to_string()); }
                                Type::I64
                            }
                            "toArray" => {
                                if !args.is_empty() { self.errors.push("Type Error: .toArray() takes no arguments".to_string()); }
                                Type::Array(Box::new(Type::Any))
                            }
                            "includes" => {
                                if args.len() != 1 { self.errors.push("Type Error: .includes() takes 1 argument".to_string()); }
                                else { self.check_expression(&args[0]); }
                                Type::Bool
                            }
                            "index" => {
                                if args.len() != 1 { self.errors.push("Type Error: .index() takes 1 argument".to_string()); }
                                else { self.check_expression(&args[0]); }
                                Type::I64
                            }
                            "copy" => {
                                if args.len() > 2 {
                                    self.errors.push("Type Error: .copy() takes at most 2 arguments".to_string());
                                }
                                for arg in args {
                                    let arg_ty = self.check_expression(arg);
                                    if !self.types_compatible(&Type::I64, &arg_ty) {
                                        self.errors.push(format!("Type Error: .copy() arguments must be I64, got {:?}", arg_ty));
                                    }
                                }
                                Type::Any // Hard to determine tuple size/types at compile time without constant folding
                            }
                            "join" => {
                                if args.len() != 1 { self.errors.push("Type Error: .join() takes 1 argument".to_string()); }
                                else {
                                    let arg_ty = self.check_expression(&args[0]);
                                    if !self.types_compatible(&Type::String, &arg_ty) {
                                        self.errors.push(format!("Type Error: .join() argument must be string, got {:?}", arg_ty));
                                    }
                                }
                                Type::String
                            }
                            _ => {
                                self.errors.push(format!("Type Error: Unknown tuple method '{}'", method));
                                Type::Any
                            }
                        }
                    }
                    
                    // State methods
                    Type::State(inner) => {
                        match method.as_str() {
                            "read" => {
                                if !args.is_empty() { self.errors.push("Type Error: .read() takes no arguments".to_string()); }
                                *inner
                            }
                            "write" => {
                                if args.len() != 1 { 
                                    self.errors.push("Type Error: .write() takes 1 argument".to_string()); 
                                } else {
                                    // Note: args were already checked at the top of this match
                                    // Type compatibility check for write value
                                    let arg_ty = self.check_expression(&args[0]);
                                    if !self.types_compatible(&inner, &arg_ty) {
                                        self.errors.push(format!("Type Error: Cannot write {:?} to state of type {:?}", arg_ty, *inner));
                                    }
                                }
                                Type::Void
                            }
                            "update" => {
                                if args.len() != 1 { 
                                    self.errors.push("Type Error: .update() takes 1 argument".to_string()); 
                                } else {
                                    // Type check: closure should be |T| -> T
                                    let closure_ty = self.check_expression(&args[0]);
                                    if let Type::Function { params, return_type } = closure_ty {
                                        if params.len() != 1 || !self.types_compatible(&params[0], &inner) || !self.types_compatible(&inner, &return_type) {
                                            self.errors.push(format!("Type Error: .update() expects closure |{:?}| -> {:?}", *inner, *inner));
                                        }
                                    } else {
                                        self.errors.push("Type Error: .update() expects a closure".to_string());
                                    }
                                }
                                Type::Void
                            }
                            _ => {
                                self.errors.push(format!("Type Error: Unknown method '{}' for State type", method));
                                Type::Any
                            }
                        }
                    }
                    
                    // Channel methods
                    Type::Channel(_) => {
                        match method.as_str() {
                            "close" => {
                                if !args.is_empty() { self.errors.push("Type Error: .close() takes no arguments".to_string()); }
                                Type::Void
                            }
                            _ => {
                                self.errors.push(format!("Type Error: Unknown method '{}' for Channel type", method));
                                Type::Any
                            }
                        }
                    }
                    
                    _ => {
                         self.errors.push(format!("Type Error: Methods not supported for type {:?}", obj_ty));
                         Type::Any
                    }
                }
            }
            Expr::Index(obj, index) => {
                let obj_ty = self.check_expression(obj);
                let index_ty = self.check_expression(index);
                
                if !self.types_compatible(&Type::I64, &index_ty) {
                    self.errors.push(format!("Type Error: Array index must be I64, got {:?}", index_ty));
                }
                
                match obj_ty {
                    Type::Array(inner) => *inner,
                    Type::String => Type::String,
                    Type::Any => Type::Any,
                    _ => {
                        self.errors.push(format!("Type Error: Type {:?} does not support indexing", obj_ty));
                        Type::Any
                    }
                }
            }
            Expr::Unary { op, right } => {
                let right_ty = self.check_expression(right);
                match op {
                    Token::Minus => {
                        if !self.is_numeric(&right_ty) {
                            self.errors.push(format!("Type Error: Cannot negate type {:?}", right_ty));
                        }
                        right_ty
                    }
                    Token::Bang => {
                        if !self.types_compatible(&Type::Bool, &right_ty) {
                            self.errors.push(format!("Type Error: Cannot apply ! to type {:?}", right_ty));
                        }
                        Type::Bool
                    }
                    _ => {
                        self.errors.push(format!("Type Error: Unknown unary operator {:?}", op));
                        Type::Any
                    }
                }
            }
        }
    }

    fn is_numeric(&self, ty: &Type) -> bool {
        matches!(ty, Type::I64 | Type::I32 | Type::I16 | Type::I8 | Type::U64 | Type::U32 | Type::U16 | Type::U8 | Type::F64 | Type::F32 | Type::Any)
    }

    fn normalize_type(&self, ty: Type) -> Type {
        match ty {
            Type::Model(name) => {
                if self.roles.contains_key(&name) {
                    Type::Role(name)
                } else {
                    Type::Model(name)
                }
            }
            Type::Array(inner) => Type::Array(Box::new(self.normalize_type(*inner))),
            Type::Tuple(elements) => Type::Tuple(elements.into_iter().map(|t| self.normalize_type(t)).collect()),
            Type::Channel(inner) => Type::Channel(Box::new(self.normalize_type(*inner))),
            Type::State(inner) => Type::State(Box::new(self.normalize_type(*inner))),
            Type::Function { params, return_type } => Type::Function {
                params: params.into_iter().map(|p| self.normalize_type(p)).collect(),
                return_type: Box::new(self.normalize_type(*return_type)),
            },
            _ => ty,
        }
    }

    fn types_compatible(&self, expected: &Type, actual: &Type) -> bool {
        let expected = self.normalize_type(expected.clone());
        let actual = self.normalize_type(actual.clone());
        
        if matches!(expected, Type::Any) || matches!(actual, Type::Any) { return true; }
        if expected == actual { return true; }
        
        if self.is_numeric(&expected) && self.is_numeric(&actual) { return true; }

        match (&expected, &actual) {
            (Type::Array(e_inner), Type::Array(a_inner)) => {
                if matches!(a_inner.as_ref(), Type::Any) { return true; }
                self.types_compatible(e_inner.as_ref(), a_inner.as_ref())
            }
            (Type::Tuple(e_els), Type::Tuple(a_els)) => {
                if e_els.len() != a_els.len() { return false; }
                e_els.iter().zip(a_els.iter()).all(|(e, a)| self.types_compatible(e, a))
            }
            (Type::Channel(e_inner), Type::Channel(a_inner)) => {
                self.types_compatible(e_inner.as_ref(), a_inner.as_ref())
            }
            (Type::State(e_inner), Type::State(a_inner)) => {
                self.types_compatible(e_inner.as_ref(), a_inner.as_ref())
            }
            (Type::Role(expected_role_name), _) => {
                if let Some(roles) = self.model_roles.get(&actual) {
                    roles.contains(expected_role_name)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn check_pattern(&mut self, pattern: &Pattern, matched_type: &Type) {
        match pattern {
            Pattern::Literal(expr) => {
                let ty = self.check_expression(expr);
                if !self.types_compatible(matched_type, &ty) {
                    self.errors.push(format!("Type Error: Match pattern type {:?} doesn't match condition type {:?}", ty, matched_type));
                }
            }
            Pattern::EnumVariant { enum_name, variant_name, binding } => {
                let enum_def = if let Some(e_name) = enum_name {
                    self.enums.get(e_name)
                } else {
                    self.enums.values().find(|e| e.variants.iter().any(|v| &v.name == variant_name))
                };

                if let Some(e) = enum_def {
                    let variant = e.variants.iter().find(|v| &v.name == variant_name).unwrap();
                    if let Some(bind_name) = binding {
                        if let Some(v_ty) = &variant.data_type {
                            self.define(bind_name, v_ty.clone(), false);
                        } else {
                            self.errors.push(format!("Type Error: Enum variant '{}' does not take any data", variant_name));
                        }
                    }
                } else {
                    self.errors.push(format!("Type Error: Unknown enum variant '{}'", variant_name));
                }
            }
            Pattern::Wildcard => {}
        }
    }
}
