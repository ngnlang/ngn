use crate::lexer::{Span, Token};
use crate::parser::{
    EnumDef, EnumVariantDef, Expr, ExprKind, ModelDef, Pattern, RoleDef, Statement, StatementKind,
    Type,
};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct Symbol {
    pub ty: Type,
    pub is_mutable: bool,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub message: String,
    pub span: Span,
}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {:?}", self.message, self.span)
    }
}

pub struct Analyzer {
    scopes: Vec<HashMap<String, Symbol>>,
    pub diagnostics: Vec<Diagnostic>,
    pub warnings: Vec<Diagnostic>,
    errors: Vec<String>, // Deprecated, removing incrementally
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

        // Register core globals from toolbox
        use crate::toolbox::core::{GLOBAL_NAMES, get_type};
        for name in GLOBAL_NAMES {
            if let Some(def) = get_type(name) {
                global_scope.insert(
                    name.to_string(),
                    Symbol {
                        ty: def.ty,
                        is_mutable: def.is_mutable,
                    },
                );
            }
        }

        // Built-in Result
        let result_enum = EnumDef {
            name: "Result".to_string(),
            variants: vec![
                EnumVariantDef {
                    name: "Ok".to_string(),
                    data_type: Some(Type::Any),
                },
                EnumVariantDef {
                    name: "Error".to_string(),
                    data_type: Some(Type::Any),
                },
            ],
        };

        // Built-in Maybe
        let maybe_enum = EnumDef {
            name: "Maybe".to_string(),
            variants: vec![
                EnumVariantDef {
                    name: "Value".to_string(),
                    data_type: Some(Type::Any),
                },
                EnumVariantDef {
                    name: "Null".to_string(),
                    data_type: None,
                },
            ],
        };

        let mut analyzer = Self {
            scopes: vec![global_scope],
            diagnostics: Vec::new(),
            warnings: Vec::new(),
            errors: Vec::new(),
            current_return_type: None,
            infer_stack: Vec::new(),
            enums: HashMap::new(),
            models: HashMap::new(),
            roles: HashMap::new(),
            custom_methods: HashMap::new(),
            model_roles: HashMap::new(),
        };

        analyzer.register_enum(&result_enum, Span::default());
        analyzer.register_enum(&maybe_enum, Span::default());

        // Built-in Request model for HTTP
        analyzer.models.insert(
            "Request".to_string(),
            ModelDef {
                name: "Request".to_string(),
                fields: vec![
                    ("method".to_string(), Type::String),
                    ("path".to_string(), Type::String),
                    ("query".to_string(), Type::String),
                    ("headers".to_string(), Type::Any),
                    ("body".to_string(), Type::String),
                    // New fields
                    (
                        "params".to_string(),
                        Type::Map(Box::new(Type::String), Box::new(Type::String)),
                    ),
                    ("ip".to_string(), Type::String),
                    ("url".to_string(), Type::String),
                    (
                        "cookies".to_string(),
                        Type::Map(Box::new(Type::String), Box::new(Type::String)),
                    ),
                    ("protocol".to_string(), Type::String),
                    ("host".to_string(), Type::String),
                ],
            },
        );

        // Built-in Response model for HTTP
        analyzer.models.insert(
            "Response".to_string(),
            ModelDef {
                name: "Response".to_string(),
                fields: vec![
                    ("status".to_string(), Type::I64),
                    ("statusText".to_string(), Type::String),
                    ("ok".to_string(), Type::Bool),
                    (
                        "headers".to_string(),
                        Type::Map(Box::new(Type::String), Box::new(Type::String)),
                    ),
                    ("body".to_string(), Type::String),
                ],
            },
        );

        // Built-in FetchOptions model for fetch()
        analyzer.models.insert(
            "FetchOptions".to_string(),
            ModelDef {
                name: "FetchOptions".to_string(),
                fields: vec![
                    ("method".to_string(), Type::String),
                    ("headers".to_string(), Type::Any), // accepts map or object literal
                    ("body".to_string(), Type::String),
                    ("timeout".to_string(), Type::I64),
                ],
            },
        );

        analyzer
    }

    pub fn define_global(&mut self, name: String, sym: Symbol) {
        if let Some(scope) = self.scopes.first_mut() {
            scope.insert(name, sym);
        }
    }

    /// Check if a type has a method with the given name
    pub fn has_method(&self, ty: &Type, method_name: &str) -> bool {
        // Check custom_methods for both exact type and generic type
        let generic_ty = self.generic_type(ty);

        if let Some(methods) = self.custom_methods.get(&generic_ty) {
            if methods.contains_key(method_name) {
                return true;
            }
        }
        if let Some(methods) = self.custom_methods.get(ty) {
            if methods.contains_key(method_name) {
                return true;
            }
        }
        false
    }

    /// Look up a variable's type from the symbol table
    pub fn lookup_variable_type(&self, name: &str) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(sym) = scope.get(name) {
                return Some(sym.ty.clone());
            }
        }
        None
    }

    pub fn analyze(&mut self, statements: &[Statement]) -> Result<(), Vec<Diagnostic>> {
        // Pass 1a: Collect all top-level types (Enums, Models, Roles)
        for stmt in statements {
            match &stmt.kind {
                StatementKind::Enum(enum_def) => {
                    self.register_enum(enum_def, stmt.span);
                }
                StatementKind::Model(model_def) => {
                    self.models
                        .insert(model_def.name.clone(), model_def.clone());
                }
                StatementKind::Role(role_def) => {
                    self.roles.insert(role_def.name.clone(), role_def.clone());
                }
                _ => {}
            }
        }

        // Pass 1b: Collect function signatures and extensions with normalized types
        for stmt in statements {
            match &stmt.kind {
                StatementKind::Function {
                    name,
                    params,
                    return_type,
                    ..
                } => {
                    let mut param_types = Vec::new();
                    for p in params {
                        let ty = p.ty.clone().unwrap_or(Type::Any);
                        param_types.push(self.normalize_type(ty));
                    }
                    let optional_count = params
                        .iter()
                        .filter(|p| p.is_optional || p.default_value.is_some())
                        .count();
                    let ret_ty = return_type.clone().unwrap_or(Type::Void);
                    let ret_ty = self.normalize_type(ret_ty);
                    self.define(
                        name,
                        Type::Function {
                            params: param_types,
                            optional_count,
                            return_type: Box::new(ret_ty),
                        },
                        false,
                        stmt.span,
                    );
                }
                StatementKind::Extend {
                    target,
                    role,
                    methods,
                } => {
                    let target = self.normalize_type(target.clone());
                    // Use generic type for tuple/array so that extend tuple/array applies to all
                    let target_key = self.generic_type(&target);

                    if let Some(role_name) = role {
                        self.model_roles
                            .entry(target_key.clone())
                            .or_insert_with(HashSet::new)
                            .insert(role_name.clone());
                    }

                    let mut methods_to_add = Vec::new();
                    for m in methods {
                        if let Statement {
                            kind:
                                StatementKind::Function {
                                    name,
                                    params,
                                    return_type,
                                    ..
                                },
                            ..
                        } = m
                        {
                            let mut param_types = Vec::new();
                            for p in params {
                                let ty = p.ty.clone().unwrap_or(Type::Any);
                                param_types.push(self.normalize_type(ty));
                            }
                            let ret_ty = return_type.clone().unwrap_or(Type::Void);
                            let ret_ty = self.normalize_type(ret_ty);
                            methods_to_add.push((
                                name.clone(),
                                Type::Function {
                                    params: param_types,
                                    optional_count: 0,
                                    return_type: Box::new(ret_ty),
                                },
                            ));
                        }
                    }

                    let methods_map = self
                        .custom_methods
                        .entry(target_key)
                        .or_insert_with(HashMap::new);
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

        // Merge legacy errors
        for msg in self.errors.drain(..) {
            self.diagnostics.push(Diagnostic {
                message: msg,
                span: Span::default(),
            });
        }

        if self.diagnostics.is_empty() {
            Ok(())
        } else {
            Err(self.diagnostics.clone())
        }
    }

    fn add_error(&mut self, message: String, span: Span) {
        self.diagnostics.push(Diagnostic { message, span });
    }

    fn add_warning(&mut self, message: String, span: Span) {
        self.warnings.push(Diagnostic { message, span });
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn define(&mut self, name: &str, ty: Type, is_mutable: bool, span: Span) {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(name) {
                if let Some(existing) = scope.get(name) {
                    if existing.ty == ty {
                        return;
                    }
                }
                self.add_error(
                    format!("Error: '{}' already defined in this scope", name),
                    span,
                );
                return;
            }
            scope.insert(name.to_string(), Symbol { ty, is_mutable });
        }
    }

    fn register_enum(&mut self, enum_def: &EnumDef, span: Span) {
        self.enums.insert(enum_def.name.clone(), enum_def.clone());
        // Register variants in global scope (accessible as bare names if unambiguous)
        for v in &enum_def.variants {
            self.define(&v.name, Type::Any, false, span);
        }
    }

    fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.scopes.iter().rev().find_map(|s| s.get(name))
    }

    fn check_statement(&mut self, stmt: &Statement) -> Type {
        match &stmt.kind {
            StatementKind::Declaration {
                name,
                is_mutable,
                value,
                declared_type,
                ..
            } => {
                let inferred = self.check_expression(value);

                if let Type::Array(inner) = &inferred {
                    if matches!(**inner, Type::Any) && declared_type.is_none() {
                        self.add_error(format!("Type Error: Cannot infer type for empty array variable '{}'. Please provide an explicit type annotation.", name), stmt.span);
                    }
                }

                let ty = if let Some(declared) = declared_type {
                    if !self.types_compatible(declared, &inferred) {
                        self.add_error(
                            format!(
                                "Type Error: '{}' expected {:?}, but got {:?}",
                                name, declared, inferred
                            ),
                            stmt.span,
                        );
                    }
                    declared.clone()
                } else {
                    inferred
                };

                if let Type::Channel(_) = &ty {
                    if *is_mutable {
                        self.add_error(format!("Type Error: Channel '{}' must be declared as 'const' or 'static', not 'var'.", name), stmt.span);
                    }
                }

                // Closure
                if let Type::Function { .. } = &ty {
                    if *is_mutable {
                        self.add_error(format!("Type Error: Closure '{}' must be declared as 'const' or 'static', not 'var'.", name), stmt.span);
                    }
                }

                self.define(name, ty, *is_mutable, stmt.span);
                Type::Void
            }
            StatementKind::Expression(expr) => self.check_expression(expr),
            StatementKind::Function {
                name,
                params,
                body,
                return_type,
                ..
            } => {
                // Register local function in outer scope first
                let mut param_types = Vec::new();
                for p in params {
                    if let Some(ty) = &p.ty {
                        param_types.push(ty.clone());
                    } else {
                        param_types.push(Type::Any);
                    }
                }
                let optional_count = params
                    .iter()
                    .filter(|p| p.is_optional || p.default_value.is_some())
                    .count();
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
                    self.define(
                        name,
                        Type::Function {
                            params: param_types.clone(),
                            optional_count,
                            return_type: Box::new(actual_return_type.clone()),
                        },
                        false,
                        stmt.span,
                    );
                }

                let prev_return = self.current_return_type.clone();
                self.current_return_type = Some(actual_return_type);

                self.enter_scope();
                for param in params {
                    if let Some(ty) = &param.ty {
                        self.define(&param.name, ty.clone(), true, param.span);
                    } else {
                        self.add_warning(
                            format!("Warning: Parameter '{}' has no type annotation", param.name),
                            param.span,
                        );
                        self.define(&param.name, Type::Any, true, param.span);
                    }
                }

                for s in body {
                    self.check_statement(s);
                }

                self.exit_scope();
                self.current_return_type = prev_return;
                Type::Void
            }
            StatementKind::If {
                condition,
                binding,
                then_branch,
                else_branch,
            } => {
                let condition_type = self.check_expression(condition);

                if let Some(bind_name) = &binding {
                    // For binding pattern: if (var n = x), use the expression's type
                    // Since optional params are stored with their base type, this is already unwrapped
                    let unwrapped_type = condition_type;

                    // Create scope for then-branch with binding defined
                    self.enter_scope();
                    self.define(bind_name, unwrapped_type, false, condition.span);
                    self.check_statement(then_branch);
                    self.exit_scope();
                } else {
                    self.check_statement(then_branch);
                }

                if let Some(eb) = else_branch {
                    self.check_statement(eb);
                }
                Type::Void
            }
            StatementKind::Check {
                binding,
                source,
                failure_block,
            } => {
                // Get the source expression's type (already unwrapped for optional params)
                let source_type = self.check_expression(source);

                // Check failure block
                // TODO: validate failure_block contains return/break/continue
                self.check_statement(failure_block);

                // Define binding in current scope with the source's type
                self.define(&binding, source_type, false, source.span);

                Type::Void
            }
            StatementKind::While {
                condition, body, ..
            } => {
                self.check_expression(condition);
                self.check_statement(body);
                Type::Void
            }
            StatementKind::For {
                binding,
                index_binding,
                iterable,
                body,
            } => {
                let iter_ty = self.check_expression(iterable);
                let element_ty = match iter_ty {
                    Type::Array(inner) => *inner,
                    Type::Tuple(_) => Type::Any,
                    Type::Enum(ref name) if name == "Maybe" => Type::Any,
                    Type::Channel(inner) => *inner,
                    Type::Any => Type::Any,
                    _ => {
                        self.add_error(
                            format!("Type Error: Cannot iterate over type {:?}", iter_ty),
                            stmt.span,
                        );
                        Type::Any
                    }
                };

                self.enter_scope();
                self.define(binding, element_ty, false, stmt.span);
                if let Some(idx_name) = index_binding {
                    self.define(idx_name, Type::I64, false, stmt.span);
                }
                self.check_statement(body);
                self.exit_scope();
                Type::Void
            }
            StatementKind::Match {
                condition, arms, ..
            } => {
                let cond_ty = self.check_expression(condition);
                for arm in arms {
                    self.enter_scope();
                    for pattern in &arm.patterns {
                        self.check_pattern(pattern, &cond_ty, stmt.span);
                    }
                    self.check_statement(&arm.body);
                    self.exit_scope();
                }
                Type::Void
            }
            StatementKind::Enum(_) => Type::Void,
            StatementKind::Next => Type::Void,
            StatementKind::Model(_) => Type::Void,
            StatementKind::Role(_) => Type::Void,
            StatementKind::Extend {
                target,
                role,
                methods,
            } => {
                // If role is provided, verify it exists and methods match
                if let Some(role_name) = role {
                    self.validate_role_impl(target, role_name, methods, stmt.span);
                }

                // Check method bodies
                for method in methods {
                    if let Statement {
                        kind:
                            StatementKind::Function {
                                params,
                                body,
                                return_type,
                                ..
                            },
                        span: method_span,
                    } = method
                    {
                        let actual_return_type = return_type.clone().unwrap_or(Type::Void);
                        let prev_return = self.current_return_type.clone();
                        self.current_return_type = Some(actual_return_type);

                        self.enter_scope();
                        self.define("this", target.clone(), false, *method_span);

                        for param in params {
                            self.define(
                                &param.name,
                                param.ty.clone().unwrap_or(Type::Any),
                                true,
                                param.span,
                            );
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
            StatementKind::Block(stmts) => {
                self.enter_scope();
                for s in stmts {
                    self.check_statement(s);
                }
                self.exit_scope();
                Type::Void
            }
            StatementKind::Print(expr) => {
                self.check_expression(expr);
                Type::Void
            }
            StatementKind::Return(expr_opt) => {
                let actual = if let Some(expr) = expr_opt {
                    self.check_expression(expr)
                } else {
                    Type::Void
                };

                if let Some(expected) = &self.current_return_type {
                    if !self.types_compatible(expected, &actual) {
                        self.add_error(
                            format!(
                                "Type Error: Return type mismatch. Expected {:?}, got {:?}",
                                expected, actual
                            ),
                            stmt.span,
                        );
                    }
                } else if let Some(last_inferred) = self.infer_stack.last_mut() {
                    match last_inferred {
                        Some(prev) => {
                            let prev_cloned = prev.clone();
                            if !self.types_compatible(&prev_cloned, &actual) {
                                self.add_error(format!("Type Error: Return type inconsistency. Previously inferred {:?}, but now got {:?}", prev_cloned, actual), stmt.span);
                            }
                        }
                        None => {
                            *last_inferred = Some(actual);
                        }
                    }
                }
                Type::Void
            }
            StatementKind::Break => Type::Void,
            StatementKind::Import { names, source } => {
                // Handle toolbox imports by registering functions in scope
                if source.starts_with("tbx::") {
                    let module = source.strip_prefix("tbx::").unwrap();
                    for (name, alias) in names {
                        // Determine the function type based on known toolbox functions
                        let fn_type = match (module, name.as_str()) {
                            ("test", "assert") => Type::Function {
                                params: vec![Type::Bool],
                                optional_count: 0,
                                return_type: Box::new(Type::Void),
                            },
                            ("math", "abs")
                            | ("math", "round")
                            | ("math", "floor")
                            | ("math", "ceil") => Type::Function {
                                params: vec![Type::Any],
                                optional_count: 0,
                                return_type: Box::new(Type::Any),
                            },
                            ("math", "sin") => Type::Function {
                                params: vec![Type::F64],
                                optional_count: 0,
                                return_type: Box::new(Type::F64),
                            },
                            ("http", "serve") => Type::Function {
                                params: vec![Type::I64, Type::Any], // port, handler
                                optional_count: 0,
                                return_type: Box::new(Type::Void),
                            },
                            ("http", "serve_tls") => Type::Function {
                                params: vec![Type::I64, Type::Any, Type::String, Type::String], // port, handler, cert, key
                                optional_count: 0,
                                return_type: Box::new(Type::Void),
                            },
                            _ => Type::Function {
                                params: vec![Type::Any],
                                optional_count: 0,
                                return_type: Box::new(Type::Any),
                            },
                        };
                        let bind_name = alias.as_ref().unwrap_or(name);
                        self.define(bind_name, fn_type, false, stmt.span);
                    }
                } else {
                    // For local/other modules, we blindly trust the import for now (LSP context)
                    // In a full compiler, these are resolved by the main driver.
                    for (name, alias) in names {
                        let bind_name = alias.as_ref().unwrap_or(name);
                        if self.lookup(bind_name).is_none() {
                            self.define(bind_name, Type::Any, false, stmt.span);
                        }
                    }
                }
                Type::Void
            }
            StatementKind::Error(msg) => {
                self.add_error(msg.clone(), stmt.span);
                Type::Void
            }
            _ => Type::Void,
        }
    }

    fn check_expression(&mut self, expr: &Expr) -> Type {
        match &expr.kind {
            ExprKind::Number(_) => Type::I64,
            ExprKind::Float(_) => Type::F64,
            ExprKind::String(_) => Type::String,
            ExprKind::Regex(_) => Type::Regex,
            ExprKind::InterpolatedString(parts) => {
                for part in parts {
                    self.check_expression(part);
                }
                Type::String
            }
            ExprKind::Bool(_) => Type::Bool,
            ExprKind::Closure {
                params,
                body,
                return_type,
            } => {
                self.enter_scope();
                let mut param_types = Vec::new();
                for param in params {
                    let ty = if let Some(t) = &param.ty {
                        t.clone()
                    } else {
                        Type::Any
                    };
                    self.define(&param.name, ty.clone(), false, param.span);
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
                    optional_count: 0,
                    return_type: Box::new(actual_ret),
                }
            }
            ExprKind::Variable(name) => {
                if let Some(sym) = self.lookup(name) {
                    sym.ty.clone()
                } else {
                    self.add_error(format!("Error: Undefined variable '{}'", name), expr.span);
                    Type::Void
                }
            }
            ExprKind::Binary { left, op, right } => {
                let l_ty = self.check_expression(left);
                let r_ty = self.check_expression(right);

                match op {
                    Token::EqualEqual | Token::NotEqual => {
                        if !self.types_compatible(&l_ty, &r_ty)
                            && !self.types_compatible(&r_ty, &l_ty)
                        {
                            self.add_error(
                                format!("Type Error: Cannot compare {:?} and {:?}", l_ty, r_ty),
                                expr.span,
                            );
                        }
                        return Type::Bool;
                    }
                    Token::LessThan
                    | Token::GreaterThan
                    | Token::LessThanEqual
                    | Token::GreaterThanEqual => {
                        if !self.is_numeric(&l_ty) || !self.is_numeric(&r_ty) {
                            self.add_error(format!(
                                "Type Error: Comparison requires numeric types, got {:?} and {:?}",
                                l_ty, r_ty
                            ), expr.span);
                        }
                        return Type::Bool;
                    }
                    Token::QuestionQuestion => {
                        // Null-coalescing: left should be Maybe<T>, right should be T, result is T
                        // Extract inner type from Maybe<T>
                        let inner_ty = match &l_ty {
                            Type::Generic(name, args) if name == "Maybe" && args.len() == 1 => {
                                args[0].clone()
                            }
                            _ => {
                                self.add_error(
                                    format!(
                                        "Type Error: Left side of ?? must be Maybe<T>, got {:?}",
                                        l_ty
                                    ),
                                    expr.span,
                                );
                                return r_ty;
                            }
                        };
                        // Check that right type is compatible with inner type
                        if !self.types_compatible(&inner_ty, &r_ty) {
                            self.add_error(
                                format!("Type Error: Right side of ?? ({:?}) must be compatible with {:?}", r_ty, inner_ty),
                                expr.span,
                            );
                        }
                        return inner_ty;
                    }
                    _ => {}
                }

                if self.is_numeric(&l_ty) && self.is_numeric(&r_ty) {
                    if matches!(l_ty, Type::F64) || matches!(r_ty, Type::F64) {
                        Type::F64
                    } else {
                        l_ty
                    }
                } else if (l_ty == Type::String || r_ty == Type::String)
                    && matches!(op, Token::Plus)
                {
                    // Allow String + String, String + Any, or Any + String
                    if l_ty != Type::String && l_ty != Type::Any {
                        self.add_error(
                            format!(
                                "Type Error: Cannot perform binary operation on {:?} and {:?}",
                                l_ty, r_ty
                            ),
                            expr.span,
                        );
                    }
                    if r_ty != Type::String && r_ty != Type::Any {
                        self.add_error(
                            format!(
                                "Type Error: Cannot perform binary operation on {:?} and {:?}",
                                l_ty, r_ty
                            ),
                            expr.span,
                        );
                    }
                    Type::String
                } else {
                    self.add_error(
                        format!(
                            "Type Error: Cannot perform binary operation on {:?} and {:?}",
                            l_ty, r_ty
                        ),
                        expr.span,
                    );
                    Type::Void
                }
            }
            ExprKind::Assign { name, value } => {
                let val_type = self.check_expression(value);
                let sym_info = self.lookup(name).cloned();
                if let Some(sym) = sym_info {
                    if !sym.is_mutable {
                        self.add_error(
                            format!("Error: Cannot reassign immutable variable '{}'", name),
                            expr.span,
                        );
                    }
                    if !self.types_compatible(&sym.ty, &val_type) {
                        self.add_error(
                            format!(
                                "Type Error: Cannot assign {:?} to variable '{}' of type {:?}",
                                val_type, name, sym.ty
                            ),
                            expr.span,
                        );
                    }
                } else {
                    self.add_error(format!("Error: Undefined variable '{}'", name), expr.span);
                }
                val_type
            }
            ExprKind::FieldAssign {
                object,
                field,
                value,
            } => {
                // Check the object and value expressions
                let _obj_type = self.check_expression(object);
                let val_type = self.check_expression(value);

                // Trace back to the root variable to check mutability
                fn get_root_var(e: &Expr) -> Option<&str> {
                    match &e.kind {
                        ExprKind::Variable(name) => Some(name.as_str()),
                        ExprKind::FieldAccess { object, .. } => get_root_var(object),
                        _ => None,
                    }
                }

                if let Some(root_name) = get_root_var(object) {
                    if let Some(sym) = self.lookup(root_name) {
                        if !sym.is_mutable {
                            self.add_error(
                                format!(
                                    "Error: Cannot assign to field '{}' of immutable variable '{}'",
                                    field, root_name
                                ),
                                expr.span,
                            );
                        }
                    }
                }

                val_type
            }
            ExprKind::Call { name, args } => {
                let arg_types: Vec<Type> = args.iter().map(|a| self.check_expression(a)).collect();

                // Check if it's an enum variant constructor
                let enum_info = self
                    .enums
                    .values()
                    .find(|e| e.variants.iter().any(|v| &v.name == name))
                    .map(|e| {
                        let variant = e.variants.iter().find(|v| &v.name == name).unwrap();
                        (e.name.clone(), variant.clone())
                    });

                if let Some((enum_name, variant)) = enum_info {
                    if let Some(expected_ty) = &variant.data_type {
                        if args.len() != 1 {
                            self.add_error(
                                format!(
                                    "Error: Enum variant '{}' expects 1 argument, got {}",
                                    name,
                                    args.len()
                                ),
                                expr.span,
                            );
                        } else if !self.types_compatible(expected_ty, &arg_types[0]) {
                            self.add_error(
                                format!(
                                    "Type Error: Enum variant '{}' expected {:?}, got {:?}",
                                    name, expected_ty, arg_types[0]
                                ),
                                expr.span,
                            );
                        }
                    } else if !args.is_empty() {
                        self.add_error(
                            format!("Error: Enum variant '{}' does not take any arguments", name),
                            expr.span,
                        );
                    }
                    return Type::Enum(enum_name);
                }

                let sym_info = self.lookup(name).cloned();

                if let Some(sym) = sym_info {
                    match sym.ty {
                        Type::Function {
                            params,
                            optional_count,
                            return_type,
                        } => {
                            let required_count = params.len() - optional_count;
                            let args_provided = arg_types.len();

                            if args_provided < required_count || args_provided > params.len() {
                                let msg = if optional_count > 0 {
                                    format!(
                                        "Error: '{}' expects {}-{} arguments, got {}",
                                        name,
                                        required_count,
                                        params.len(),
                                        args_provided
                                    )
                                } else {
                                    format!(
                                        "Error: '{}' expects {} arguments, got {}",
                                        name,
                                        params.len(),
                                        args_provided
                                    )
                                };
                                self.add_error(msg, expr.span);
                            } else {
                                // Type-check only the args that were provided
                                for (i, (expected, actual)) in
                                    params.iter().zip(arg_types.iter()).enumerate()
                                {
                                    if !self.types_compatible(expected, actual) {
                                        self.add_error(format!("Type Error: Argument {} mismatch. Expected {:?}, got {:?}", i + 1, expected, actual), expr.span);
                                    }
                                }
                            }

                            return *return_type;
                        }
                        Type::Any => {
                            // Support calling Any (e.g. imported functions)
                            return Type::Any;
                        }
                        _ => {
                            self.add_error(
                                format!("Error: '{}' is not a function", name),
                                expr.span,
                            );
                            // Return Any to prevent cascading errors
                            return Type::Any;
                        }
                    }
                } else {
                    self.add_error(format!("Error: Undefined function '{}'", name), expr.span);
                }
                Type::Void
            }
            ExprKind::EnumVariant {
                enum_name,
                variant_name,
                args,
            } => {
                let arg_types: Vec<Type> = args.iter().map(|a| self.check_expression(a)).collect();

                let enum_data = if let Some(e_name) = enum_name {
                    self.enums.get(e_name)
                } else {
                    self.enums
                        .values()
                        .find(|e| e.variants.iter().any(|v| &v.name == variant_name))
                }
                .map(|e| {
                    let variant = e.variants.iter().find(|v| &v.name == variant_name).unwrap();
                    (e.name.clone(), variant.clone())
                });

                if let Some((e_name, variant)) = enum_data {
                    if let Some(expected_ty) = &variant.data_type {
                        if args.len() != 1 {
                            self.add_error(
                                format!(
                                    "Error: Enum variant '{}' expects 1 argument, got {}",
                                    variant_name,
                                    args.len()
                                ),
                                expr.span,
                            );
                        } else if !self.types_compatible(expected_ty, &arg_types[0]) {
                            self.add_error(
                                format!(
                                    "Type Error: Enum variant '{}' expected {:?}, got {:?}",
                                    variant_name, expected_ty, arg_types[0]
                                ),
                                expr.span,
                            );
                        }
                    } else if !args.is_empty() {
                        self.add_error(
                            format!(
                                "Error: Enum variant '{}' does not take any arguments",
                                variant_name
                            ),
                            expr.span,
                        );
                    }
                    Type::Enum(e_name)
                } else {
                    self.add_error(
                        format!("Type Error: Unknown enum variant '{}'", variant_name),
                        expr.span,
                    );
                    Type::Any
                }
            }
            ExprKind::Array(elements) => {
                if elements.is_empty() {
                    Type::Array(Box::new(Type::Any))
                } else {
                    let first_ty = self.check_expression(&elements[0]);
                    for el in elements.iter().skip(1) {
                        let ty = self.check_expression(el);
                        if !self.types_compatible(&first_ty, &ty) {
                            self.add_error(format!(
                                "Type Error: Array contains mixed types. Expected {:?}, got {:?}",
                                first_ty, ty
                            ), expr.span);
                        }
                    }
                    Type::Array(Box::new(first_ty))
                }
            }
            ExprKind::Tuple(elements) => {
                let types = elements.iter().map(|e| self.check_expression(e)).collect();
                Type::Tuple(types)
            }
            ExprKind::Thread(expr_inner) => {
                let closure_ty = self.check_expression(expr_inner);
                if let Type::Function { return_type, .. } = closure_ty {
                    Type::Channel(return_type)
                } else {
                    self.add_error(
                        "Type Error: thread() expects a closure".to_string(),
                        expr.span,
                    );
                    Type::Void
                }
            }
            ExprKind::Send(chan_expr, val_expr) => {
                let chan_ty = self.check_expression(chan_expr);
                let val_ty = self.check_expression(val_expr);
                if let Type::Channel(inner) = chan_ty {
                    if !self.types_compatible(&inner, &val_ty) {
                        self.add_error(
                            format!(
                                "Type Error: Cannot send {:?} to channel of type {:?}",
                                val_ty, *inner
                            ),
                            expr.span,
                        );
                    }
                } else {
                    self.add_error(
                        format!("Type Error: Cannot send to non-channel type {:?}", chan_ty),
                        expr.span,
                    );
                }
                Type::Void
            }
            ExprKind::Receive(chan_expr) => {
                let chan_ty = self.check_expression(chan_expr);
                if let Type::Channel(inner) = chan_ty {
                    *inner
                } else {
                    self.add_error(
                        format!(
                            "Type Error: Cannot receive from non-channel type {:?}",
                            chan_ty
                        ),
                        expr.span,
                    );
                    Type::Any
                }
            }
            ExprKind::Channel(inner_ty) => {
                if inner_ty.is_none() {
                    self.add_error(format!(
                        "Type Error: channel() requires a data type suffix (e.g. channel(): i64)"
                    ), expr.span);
                }
                Type::Channel(Box::new(inner_ty.clone().unwrap_or(Type::Any)))
            }
            ExprKind::ReceiveCount(chan_expr, count_expr) => {
                let chan_ty = self.check_expression(chan_expr);
                let _count_ty = self.check_expression(count_expr);
                // TODO: verify count is numeric
                if let Type::Channel(inner) = chan_ty {
                    Type::Array(inner)
                } else {
                    self.add_error(
                        format!(
                            "Type Error: Cannot receive from non-channel type {:?}",
                            chan_ty
                        ),
                        expr.span,
                    );
                    Type::Any
                }
            }
            ExprKind::ReceiveMaybe(chan_expr) => {
                let chan_ty = self.check_expression(chan_expr);
                if let Type::Channel(_) = chan_ty {
                    Type::Enum("Maybe".to_string())
                } else {
                    self.add_error(
                        format!(
                            "Type Error: Cannot receive from non-channel type {:?}",
                            chan_ty
                        ),
                        expr.span,
                    );
                    Type::Enum("Maybe".to_string())
                }
            }
            ExprKind::State(initial_expr) => {
                let initial_ty = self.check_expression(initial_expr);
                Type::State(Box::new(initial_ty))
            }
            ExprKind::ModelInstance { name, fields } => {
                if let Some(model_def) = self.models.get(name).cloned() {
                    for (f_name, f_val_expr) in fields {
                        if let Some((_, f_ty)) = model_def.fields.iter().find(|(n, _)| n == f_name)
                        {
                            let val_ty = self.check_expression(f_val_expr);
                            if !self.types_compatible(f_ty, &val_ty) {
                                self.add_error(format!(
                                    "Type Error: Field '{}' in model '{}' expected {:?}, got {:?}",
                                    f_name, name, f_ty, val_ty
                                ), expr.span);
                            }
                        } else {
                            self.add_error(
                                format!(
                                    "Type Error: Model '{}' has no field named '{}'",
                                    name, f_name
                                ),
                                expr.span,
                            );
                        }
                    }
                    // Check for missing fields (skip for Response where all fields are optional)
                    if name != "Response" {
                        for (f_name, _) in &model_def.fields {
                            if !fields.iter().any(|(n, _)| n == f_name) {
                                self.add_error(
                                    format!(
                                        "Type Error: Missing field '{}' in instantiation of model '{}'",
                                        f_name, name
                                    ),
                                    expr.span,
                                );
                            }
                        }
                    }
                    Type::Model(name.clone())
                } else {
                    self.add_error(format!("Type Error: Undefined model '{}'", name), expr.span);
                    Type::Any
                }
            }
            ExprKind::Object(fields) => {
                // Anonymous object literal - just check field expressions
                for (_, f_val_expr) in fields {
                    self.check_expression(f_val_expr);
                }
                Type::Any
            }
            ExprKind::FieldAccess { object, field } => {
                let obj_ty = self.check_expression(object);
                match obj_ty {
                    Type::Model(name) => {
                        if let Some(model_def) = self.models.get(&name) {
                            if let Some((_, f_ty)) =
                                model_def.fields.iter().find(|(f, _)| f == field)
                            {
                                f_ty.clone()
                            } else {
                                self.add_error(
                                    format!(
                                        "Type Error: Model '{}' has no field named '{}'",
                                        name, field
                                    ),
                                    expr.span,
                                );
                                Type::Any
                            }
                        } else {
                            self.add_error(
                                format!("Type Error: Undefined model '{}'", name),
                                expr.span,
                            );
                            Type::Any
                        }
                    }
                    // Allow field access on Type::Any (e.g., json.parse results)
                    Type::Any => Type::Any,
                    _ => {
                        self.add_error(
                            format!(
                                "Type Error: Cannot access field '{}' on non-model type {:?}",
                                field, obj_ty
                            ),
                            expr.span,
                        );
                        Type::Any
                    }
                }
            }
            ExprKind::Error(msg) => {
                self.add_error(msg.clone(), expr.span);
                Type::Any
            }
            ExprKind::This => {
                if let Some(sym) = self.lookup("this") {
                    sym.ty.clone()
                } else {
                    self.add_error(
                        "Type Error: 'this' can only be used inside model methods".to_string(),
                        expr.span,
                    );
                    Type::Any
                }
            }
            ExprKind::MethodCall(obj_expr, method, args) => {
                // Check for immutability
                if matches!(
                    method.as_str(),
                    "push" | "pull" | "slice" | "splice" |  // array mutating methods
                    "set" | "remove" |  // map mutating methods (remove is shared with set)
                    "add" // set mutating methods
                ) {
                    if let ExprKind::Variable(name) = &obj_expr.kind {
                        if let Some(sym) = self.lookup(name) {
                            if !sym.is_mutable {
                                self.add_error(format!("Type Error: Cannot call mutating method '{}' on immutable variable '{}'", method, name), expr.span);
                            }
                        }
                    }
                }

                let obj_ty = self.check_expression(obj_expr);
                let obj_ty = self.normalize_type(obj_ty);

                // Check custom methods from 'extend'
                // First try exact type, then try generic type (for extend tuple, extend array, etc.)
                let mut custom_method_ty = None;
                if let Some(methods_map) = self.custom_methods.get(&obj_ty) {
                    if let Some(method_ty) = methods_map.get(method) {
                        custom_method_ty = Some(method_ty.clone());
                    }
                }
                // Fallback to generic type (e.g., Type::Tuple(vec![]) for any tuple)
                if custom_method_ty.is_none() {
                    let generic = self.generic_type(&obj_ty);
                    if let Some(methods_map) = self.custom_methods.get(&generic) {
                        if let Some(method_ty) = methods_map.get(method) {
                            custom_method_ty = Some(method_ty.clone());
                        }
                    }
                }

                if let Some(Type::Function {
                    params,
                    return_type,
                    ..
                }) = custom_method_ty
                {
                    if args.len() != params.len() {
                        self.add_error(
                            format!(
                                "Type Error: Method '{}' expected {} arguments, got {}",
                                method,
                                params.len(),
                                args.len()
                            ),
                            expr.span,
                        );
                    }
                    for (p_ty, arg_expr) in params.iter().zip(args) {
                        let arg_ty = self.check_expression(arg_expr);
                        if !self.types_compatible(p_ty, &arg_ty) {
                            self.add_error(format!("Type Error: Method '{}' parameter mismatch, expected {:?}, got {:?}", method, p_ty, arg_ty), expr.span);
                        }
                    }
                    return *return_type;
                }
                match obj_ty.clone() {
                    Type::Model(name) => {
                        // Special handling for built-in Response type methods
                        if name == "Response" {
                            match method.as_str() {
                                "text" => return Type::String,
                                "json" => {
                                    // Returns Result<Any, String> - use Generic type
                                    return Type::Generic(
                                        "Result".to_string(),
                                        vec![Type::Any, Type::String],
                                    );
                                }
                                _ => {}
                            }
                        }
                        // Special handling for built-in Request type methods
                        if name == "Request" {
                            match method.as_str() {
                                "clone" => return Type::Model("Request".to_string()),
                                "text" => return Type::String,
                                "json" => {
                                    return Type::Generic(
                                        "Result".to_string(),
                                        vec![Type::Any, Type::String],
                                    );
                                }
                                "formData" => {
                                    return Type::Map(
                                        Box::new(Type::String),
                                        Box::new(Type::String),
                                    );
                                }
                                _ => {}
                            }
                        }
                        self.add_error(
                            format!(
                                "Type Error: Method '{}' not found on model '{}'",
                                method, name
                            ),
                            expr.span,
                        );
                        Type::Any
                    }
                    Type::Role(role_name) => {
                        if let Some(role_def) = self.roles.get(&role_name) {
                            let mut found_method = None;
                            for m in &role_def.methods {
                                if let StatementKind::Function {
                                    name,
                                    params,
                                    return_type,
                                    ..
                                } = &m.kind
                                {
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
                                    self.add_error(format!("Type Error: Role method '{}' expected {} arguments, got {}", method, params.len(), args.len()), expr.span);
                                }
                                for (p_ty, arg_expr) in params.iter().zip(args) {
                                    let arg_ty = self.check_expression(arg_expr);
                                    if !self.types_compatible(p_ty, &arg_ty) {
                                        self.add_error(format!("Type Error: Role method '{}' parameter mismatch, expected {:?}, got {:?}", method, p_ty, arg_ty), expr.span);
                                    }
                                }
                                return ret_ty;
                            } else {
                                self.add_error(
                                    format!(
                                        "Type Error: Method '{}' not found in role '{}'",
                                        method, role_name
                                    ),
                                    expr.span,
                                );
                                Type::Any
                            }
                        } else {
                            self.add_error(
                                format!("Type Error: Unknown role '{}'", role_name),
                                expr.span,
                            );
                            Type::Any
                        }
                    }
                    // Array methods
                    Type::Array(inner) => {
                        match method.as_str() {
                            "size" => {
                                if !args.is_empty() {
                                    self.add_error(
                                        "Type Error: .size() takes no arguments".to_string(),
                                        expr.span,
                                    );
                                }
                                Type::I64
                            }
                            "push" => {
                                if args.is_empty() {
                                    self.add_error(
                                        "Type Error: .push() requires at least 1 argument"
                                            .to_string(),
                                        expr.span,
                                    );
                                } else {
                                    let arg_ty = self.check_expression(&args[0]);
                                    if !self.types_compatible(&inner, &arg_ty) {
                                        self.add_error(
                                            format!(
                                                "Type Error: Cannot push {:?} to array of {:?}",
                                                arg_ty, *inner
                                            ),
                                            expr.span,
                                        );
                                    }
                                    if args.len() > 1 {
                                        let idx_ty = self.check_expression(&args[1]);
                                        if !self.types_compatible(&Type::I64, &idx_ty) {
                                            self.add_error(format!(
                                                "Type Error: push() index must be I64, got {:?}",
                                                idx_ty
                                            ), expr.span);
                                        }
                                    }
                                }
                                Type::I64
                            }
                            "pull" => {
                                if args.len() > 1 {
                                    self.add_error(
                                        "Type Error: .pull() takes at most 1 argument".to_string(),
                                        expr.span,
                                    );
                                }
                                if !args.is_empty() {
                                    let idx_ty = self.check_expression(&args[0]);
                                    if !self.types_compatible(&Type::I64, &idx_ty) {
                                        self.add_error(
                                            format!(
                                                "Type Error: pull() index must be I64, got {:?}",
                                                idx_ty
                                            ),
                                            expr.span,
                                        );
                                    }
                                }
                                *inner
                            }
                            "splice" => {
                                if args.len() < 1 {
                                    self.add_error("Type Error: .splice() requires an array as the first argument".to_string(), expr.span);
                                } else {
                                    let items_ty = self.check_expression(&args[0]);
                                    if !self
                                        .types_compatible(&Type::Array(inner.clone()), &items_ty)
                                    {
                                        self.add_error(format!("Type Error: .splice() first argument must be array of {:?}, got {:?}", *inner, items_ty), expr.span);
                                    }
                                    if args.len() > 1 {
                                        let idx_ty = self.check_expression(&args[1]);
                                        if !self.types_compatible(&Type::I64, &idx_ty) {
                                            self.add_error(format!(
                                                "Type Error: splice() index must be I64, got {:?}",
                                                idx_ty
                                            ), expr.span);
                                        }
                                    }
                                }
                                Type::I64
                            }
                            "slice" | "copy" => {
                                for arg in args {
                                    let arg_ty = self.check_expression(arg);
                                    if !self.types_compatible(&Type::I64, &arg_ty) {
                                        self.add_error(
                                            format!(
                                                "Type Error: {} index must be I64, got {:?}",
                                                method, arg_ty
                                            ),
                                            expr.span,
                                        );
                                    }
                                }
                                Type::Array(inner)
                            }
                            "each" => {
                                if args.len() != 1 {
                                    self.add_error(
                                        "Type Error: .each() takes 1 argument".to_string(),
                                        expr.span,
                                    );
                                } else {
                                    let closure_ty = self.check_expression(&args[0]);
                                    if let Type::Function { params, .. } = closure_ty {
                                        if params.is_empty()
                                            || !self.types_compatible(&params[0], &inner)
                                        {
                                            self.add_error(format!("Type Error: .each() closure must accept item of type {:?}", *inner), expr.span);
                                        }
                                    } else {
                                        self.add_error(
                                            "Type Error: .each() expects a closure".to_string(),
                                            expr.span,
                                        );
                                    }
                                }
                                Type::Void
                            }
                            _ => {
                                self.add_error(
                                    format!("Type Error: Unknown array method '{}'", method),
                                    expr.span,
                                );
                                Type::Any
                            }
                        }
                    }

                    // String methods
                    Type::String => {
                        match method.as_str() {
                            "length" => {
                                if !args.is_empty() {
                                    self.add_error(
                                        "Type Error: .length() takes no arguments".to_string(),
                                        expr.span,
                                    );
                                }
                                Type::I64
                            }
                            "index" => {
                                if args.is_empty() {
                                    self.add_error(
                                        "Type Error: .index() requires at least 1 argument"
                                            .to_string(),
                                        expr.span,
                                    );
                                }
                                for (i, arg) in args.iter().enumerate() {
                                    let arg_ty = self.check_expression(arg);
                                    if i == 0 {
                                        if !self.types_compatible(&Type::String, &arg_ty) {
                                            self.add_error(format!(
                                            "Type Error: index() pattern must be string, got {:?}",
                                            arg_ty
                                        ), expr.span);
                                        }
                                    } else if !self.types_compatible(&Type::I64, &arg_ty) {
                                        self.add_error(format!(
                                        "Type Error: index() start index must be I64, got {:?}",
                                        arg_ty
                                    ), expr.span);
                                    }
                                }
                                Type::I64
                            }
                            "includes" | "starts" | "ends" => {
                                if args.len() != 1 {
                                    self.add_error(
                                        format!("Type Error: .{}() takes 1 argument", method),
                                        expr.span,
                                    );
                                } else {
                                    let arg_ty = self.check_expression(&args[0]);
                                    if !self.types_compatible(&Type::String, &arg_ty) {
                                        self.add_error(
                                            format!(
                                                "Type Error: .{}() argument must be string",
                                                method
                                            ),
                                            expr.span,
                                        );
                                    }
                                }
                                Type::Bool
                            }
                            "upper" | "lower" | "trim" => {
                                if !args.is_empty() {
                                    self.add_error(
                                        format!("Type Error: .{}() takes no arguments", method),
                                        expr.span,
                                    );
                                }
                                Type::String
                            }
                            "replace" => {
                                if args.len() != 2 {
                                    self.add_error(
                                        "Type Error: .replace() takes 2 arguments".to_string(),
                                        expr.span,
                                    );
                                }
                                if args.len() >= 1 {
                                    let search_ty = self.check_expression(&args[0]);
                                    if !matches!(search_ty, Type::String | Type::Regex) {
                                        self.add_error(format!("Type Error: replace() search pattern must be String or Regex, got {:?}", search_ty), expr.span);
                                    }
                                }
                                if args.len() >= 2 {
                                    let repl_ty = self.check_expression(&args[1]);
                                    if !self.types_compatible(&Type::String, &repl_ty) {
                                        self.add_error(format!("Type Error: replace() replacement must be String, got {:?}", repl_ty), expr.span);
                                    }
                                }
                                Type::String
                            }
                            "repeat" => {
                                if args.len() != 1 {
                                    self.add_error(
                                        "Type Error: .repeat() takes 1 argument".to_string(),
                                        expr.span,
                                    );
                                } else {
                                    let arg_ty = self.check_expression(&args[0]);
                                    if !self.types_compatible(&Type::I64, &arg_ty) {
                                        self.add_error(
                                            format!(
                                                "Type Error: repeat() count must be I64, got {:?}",
                                                arg_ty
                                            ),
                                            expr.span,
                                        );
                                    }
                                }
                                Type::String
                            }
                            "copy" | "slice" => {
                                for arg in args {
                                    let arg_ty = self.check_expression(arg);
                                    if !self.types_compatible(&Type::I64, &arg_ty) {
                                        self.add_error(
                                            format!(
                                                "Type Error: {} index must be I64, got {:?}",
                                                method, arg_ty
                                            ),
                                            expr.span,
                                        );
                                    }
                                }
                                Type::String
                            }
                            "split" => {
                                if args.len() > 1 {
                                    self.add_error(
                                        "Type Error: .split() takes at most 1 argument".to_string(),
                                        expr.span,
                                    );
                                }
                                if !args.is_empty() {
                                    let arg_ty = self.check_expression(&args[0]);
                                    if !self.types_compatible(&Type::String, &arg_ty) {
                                        self.add_error(format!(
                                        "Type Error: split() delimiter must be string, got {:?}",
                                        arg_ty
                                    ), expr.span);
                                    }
                                }
                                Type::Array(Box::new(Type::String))
                            }
                            _ => {
                                self.add_error(
                                    format!("Type Error: Unknown string method '{}'", method),
                                    expr.span,
                                );
                                Type::Any
                            }
                        }
                    }

                    // Tuple methods
                    Type::Tuple(_) => {
                        match method.as_str() {
                            "size" => {
                                if !args.is_empty() {
                                    self.add_error(
                                        "Type Error: .size() takes no arguments".to_string(),
                                        expr.span,
                                    );
                                }
                                Type::I64
                            }
                            "toArray" => {
                                if !args.is_empty() {
                                    self.add_error(
                                        "Type Error: .toArray() takes no arguments".to_string(),
                                        expr.span,
                                    );
                                }
                                Type::Array(Box::new(Type::Any))
                            }
                            "includes" => {
                                if args.len() != 1 {
                                    self.add_error(
                                        "Type Error: .includes() takes 1 argument".to_string(),
                                        expr.span,
                                    );
                                } else {
                                    self.check_expression(&args[0]);
                                }
                                Type::Bool
                            }
                            "index" => {
                                if args.len() != 1 {
                                    self.add_error(
                                        "Type Error: .index() takes 1 argument".to_string(),
                                        expr.span,
                                    );
                                } else {
                                    self.check_expression(&args[0]);
                                }
                                Type::I64
                            }
                            "copy" => {
                                if args.len() > 2 {
                                    self.add_error(
                                        "Type Error: .copy() takes at most 2 arguments".to_string(),
                                        expr.span,
                                    );
                                }
                                for arg in args {
                                    let arg_ty = self.check_expression(arg);
                                    if !self.types_compatible(&Type::I64, &arg_ty) {
                                        self.add_error(format!(
                                            "Type Error: .copy() arguments must be I64, got {:?}",
                                            arg_ty
                                        ), expr.span);
                                    }
                                }
                                Type::Any
                            }
                            "join" => {
                                if args.len() != 1 {
                                    self.add_error(
                                        "Type Error: .join() takes 1 argument".to_string(),
                                        expr.span,
                                    );
                                } else {
                                    let arg_ty = self.check_expression(&args[0]);
                                    if !self.types_compatible(&Type::String, &arg_ty) {
                                        self.add_error(format!(
                                            "Type Error: .join() argument must be string, got {:?}",
                                            arg_ty
                                        ), expr.span);
                                    }
                                }
                                Type::String
                            }
                            _ => {
                                self.add_error(
                                    format!("Type Error: Unknown tuple method '{}'", method),
                                    expr.span,
                                );
                                Type::Any
                            }
                        }
                    }

                    // State methods
                    Type::State(inner) => match method.as_str() {
                        "read" => {
                            if !args.is_empty() {
                                self.add_error(
                                    "Type Error: .read() takes no arguments".to_string(),
                                    expr.span,
                                );
                            }
                            *inner
                        }
                        "write" => {
                            if args.len() != 1 {
                                self.add_error(
                                    "Type Error: .write() takes 1 argument".to_string(),
                                    expr.span,
                                );
                            } else {
                                let arg_ty = self.check_expression(&args[0]);
                                if !self.types_compatible(&inner, &arg_ty) {
                                    self.add_error(
                                        format!(
                                            "Type Error: Cannot write {:?} to state of type {:?}",
                                            arg_ty, *inner
                                        ),
                                        expr.span,
                                    );
                                }
                            }
                            Type::Void
                        }
                        "update" => {
                            if args.len() != 1 {
                                self.add_error(
                                    "Type Error: .update() takes 1 argument".to_string(),
                                    expr.span,
                                );
                            } else {
                                let closure_ty = self.check_expression(&args[0]);
                                if let Type::Function {
                                    params,
                                    return_type,
                                    ..
                                } = closure_ty
                                {
                                    if params.len() != 1
                                        || !self.types_compatible(&params[0], &inner)
                                        || !self.types_compatible(&inner, &return_type)
                                    {
                                        self.add_error(format!("Type Error: .update() expects closure |{:?}| -> {:?}", *inner, *inner), expr.span);
                                    }
                                } else {
                                    self.add_error(
                                        "Type Error: .update() expects a closure".to_string(),
                                        expr.span,
                                    );
                                }
                            }
                            Type::Void
                        }
                        _ => {
                            self.add_error(
                                format!("Type Error: Unknown method '{}' for State type", method),
                                expr.span,
                            );
                            Type::Any
                        }
                    },

                    // Channel methods
                    Type::Channel(_) => match method.as_str() {
                        "close" => {
                            if !args.is_empty() {
                                self.add_error(
                                    "Type Error: .close() takes no arguments".to_string(),
                                    expr.span,
                                );
                            }
                            Type::Void
                        }
                        _ => {
                            self.add_error(
                                format!("Type Error: Unknown method '{}' for Channel type", method),
                                expr.span,
                            );
                            Type::Any
                        }
                    },

                    // Map methods
                    Type::Map(key_type, value_type) => {
                        match method.as_str() {
                            "size" => {
                                if !args.is_empty() {
                                    self.add_error(
                                        "Type Error: .size() takes no arguments".to_string(),
                                        expr.span,
                                    );
                                }
                                Type::I64
                            }
                            "has" => {
                                if args.len() != 1 {
                                    self.add_error(
                                        "Type Error: .has() takes 1 argument".to_string(),
                                        expr.span,
                                    );
                                } else {
                                    let arg_ty = self.check_expression(&args[0]);
                                    if !self.types_compatible(&key_type, &arg_ty) {
                                        self.add_error(format!(
                                            "Type Error: .has() expects key type {:?}, got {:?}",
                                            *key_type, arg_ty
                                        ), expr.span);
                                    }
                                }
                                Type::Bool
                            }
                            "get" => {
                                if args.len() != 1 {
                                    self.add_error(
                                        "Type Error: .get() takes 1 argument".to_string(),
                                        expr.span,
                                    );
                                } else {
                                    let arg_ty = self.check_expression(&args[0]);
                                    if !self.types_compatible(&key_type, &arg_ty) {
                                        self.add_error(format!(
                                            "Type Error: .get() expects key type {:?}, got {:?}",
                                            *key_type, arg_ty
                                        ), expr.span);
                                    }
                                }
                                // Returns Maybe<value_type>
                                Type::Generic("Maybe".to_string(), vec![*value_type.clone()])
                            }
                            "set" => {
                                if args.len() != 2 {
                                    self.add_error(
                                        "Type Error: .set() takes 2 arguments".to_string(),
                                        expr.span,
                                    );
                                } else {
                                    let key_ty = self.check_expression(&args[0]);
                                    let val_ty = self.check_expression(&args[1]);
                                    if !self.types_compatible(&key_type, &key_ty) {
                                        self.add_error(format!(
                                            "Type Error: .set() expects key type {:?}, got {:?}",
                                            *key_type, key_ty
                                        ), expr.span);
                                    }
                                    if !self.types_compatible(&value_type, &val_ty) {
                                        self.add_error(format!(
                                            "Type Error: .set() expects value type {:?}, got {:?}",
                                            *value_type, val_ty
                                        ), expr.span);
                                    }
                                }
                                obj_ty.clone()
                            }
                            "remove" => {
                                if args.len() != 1 {
                                    self.add_error(
                                        "Type Error: .remove() takes 1 argument".to_string(),
                                        expr.span,
                                    );
                                } else {
                                    let arg_ty = self.check_expression(&args[0]);
                                    if !self.types_compatible(&key_type, &arg_ty) {
                                        self.add_error(format!(
                                            "Type Error: .remove() expects key type {:?}, got {:?}",
                                            *key_type, arg_ty
                                        ), expr.span);
                                    }
                                }
                                *value_type.clone()
                            }
                            _ => {
                                self.add_error(
                                    format!("Type Error: Unknown method '{}' for Map type", method),
                                    expr.span,
                                );
                                Type::Any
                            }
                        }
                    }

                    // Set methods
                    Type::Set(element_type) => match method.as_str() {
                        "size" => {
                            if !args.is_empty() {
                                self.add_error(
                                    "Type Error: .size() takes no arguments".to_string(),
                                    expr.span,
                                );
                            }
                            Type::I64
                        }
                        "has" => {
                            if args.len() != 1 {
                                self.add_error(
                                    "Type Error: .has() takes 1 argument".to_string(),
                                    expr.span,
                                );
                            } else {
                                let arg_ty = self.check_expression(&args[0]);
                                if !self.types_compatible(&element_type, &arg_ty) {
                                    self.add_error(format!("Type Error: .has() expects element type {:?}, got {:?}", *element_type, arg_ty), expr.span);
                                }
                            }
                            Type::Bool
                        }
                        "add" => {
                            if args.len() != 1 {
                                self.add_error(
                                    "Type Error: .add() takes 1 argument".to_string(),
                                    expr.span,
                                );
                            } else {
                                let arg_ty = self.check_expression(&args[0]);
                                if !self.types_compatible(&element_type, &arg_ty) {
                                    self.add_error(format!("Type Error: .add() expects element type {:?}, got {:?}", *element_type, arg_ty), expr.span);
                                }
                            }
                            obj_ty.clone()
                        }
                        "remove" => {
                            if args.len() != 1 {
                                self.add_error(
                                    "Type Error: .remove() takes 1 argument".to_string(),
                                    expr.span,
                                );
                            } else {
                                let arg_ty = self.check_expression(&args[0]);
                                if !self.types_compatible(&element_type, &arg_ty) {
                                    self.add_error(format!("Type Error: .remove() expects element type {:?}, got {:?}", *element_type, arg_ty), expr.span);
                                }
                            }
                            Type::Bool
                        }
                        _ => {
                            self.add_error(
                                format!("Type Error: Unknown method '{}' for Set type", method),
                                expr.span,
                            );
                            Type::Any
                        }
                    },

                    // Allow method calls on Type::Json (for json.parse, json.stringify)
                    Type::Json => {
                        // Check arguments but return Any since parse returns dynamic data
                        for arg in args {
                            self.check_expression(arg);
                        }
                        Type::Any
                    }

                    _ => {
                        self.add_error(
                            format!("Type Error: Methods not supported for type {:?}", obj_ty),
                            expr.span,
                        );
                        Type::Any
                    }
                }
            }
            ExprKind::Index(obj, index) => {
                let obj_ty = self.check_expression(obj);
                let index_ty = self.check_expression(index);

                if !self.types_compatible(&Type::I64, &index_ty) {
                    self.add_error(
                        format!("Type Error: Array index must be I64, got {:?}", index_ty),
                        expr.span,
                    );
                }

                match obj_ty {
                    Type::Array(inner) => *inner,
                    Type::String => Type::String,
                    Type::Any => Type::Any,
                    _ => {
                        self.add_error(
                            format!("Type Error: Type {:?} does not support indexing", obj_ty),
                            expr.span,
                        );
                        Type::Any
                    }
                }
            }
            ExprKind::Unary { op, right } => {
                let right_ty = self.check_expression(right);
                match op {
                    Token::Minus => {
                        if !self.is_numeric(&right_ty) {
                            self.add_error(
                                format!("Type Error: Cannot negate type {:?}", right_ty),
                                expr.span,
                            );
                        }
                        right_ty
                    }
                    Token::Bang => {
                        if !self.types_compatible(&Type::Bool, &right_ty) {
                            self.add_error(
                                format!("Type Error: Cannot apply ! to type {:?}", right_ty),
                                expr.span,
                            );
                        }
                        Type::Bool
                    }
                    _ => {
                        self.add_error(
                            format!("Type Error: Unknown unary operator {:?}", op),
                            expr.span,
                        );
                        Type::Any
                    }
                }
            }
            ExprKind::Map(key_type, value_type) => {
                Type::Map(Box::new(key_type.clone()), Box::new(value_type.clone()))
            }
            ExprKind::Set(element_type) => Type::Set(Box::new(element_type.clone())),
        }
    }

    fn is_numeric(&self, ty: &Type) -> bool {
        matches!(
            ty,
            Type::I64
                | Type::I32
                | Type::I16
                | Type::I8
                | Type::U64
                | Type::U32
                | Type::U16
                | Type::U8
                | Type::F64
                | Type::F32
                | Type::Number
                | Type::Any
        )
    }

    fn validate_role_impl(
        &mut self,
        target: &Type,
        role_name: &str,
        methods: &[Statement],
        span: Span,
    ) {
        let role_def = match self.roles.get(role_name).cloned() {
            Some(def) => def,
            None => {
                self.add_error(
                    format!(
                        "Type Error: Cannot use unknown role '{}' in extend",
                        role_name
                    ),
                    span,
                );
                return;
            }
        };

        for rm in &role_def.methods {
            let (rm_name, rm_params, rm_ret) = if let StatementKind::Function {
                name,
                params,
                return_type,
                ..
            } = &rm.kind
            {
                (name, params, return_type)
            } else {
                continue;
            };

            let impl_method = methods.iter().find(|m| {
                if let StatementKind::Function { name, .. } = &m.kind {
                    name == rm_name
                } else {
                    false
                }
            });

            let m = match impl_method {
                Some(m) => m,
                None => {
                    self.add_error(format!("Type Error: Target '{:?}' does not implement method '{}' required by role '{}'", target, rm_name, role_name), span);
                    continue;
                }
            };

            let (m_params, m_ret) = if let StatementKind::Function {
                params,
                return_type,
                ..
            } = &m.kind
            {
                (params, return_type)
            } else {
                continue;
            };

            if m_params.len() != rm_params.len() {
                self.add_error(format!("Type Error: Method '{}' in extend of '{:?}' with role '{}' has wrong number of parameters", rm_name, target, role_name), m.span);
                continue;
            }

            for (i, (mp, rmp)) in m_params.iter().zip(rm_params.iter()).enumerate() {
                let m_ty = mp.ty.clone().unwrap_or(Type::Any);
                let rm_ty = rmp.ty.clone().unwrap_or(Type::Any);
                if !self.types_compatible(&rm_ty, &m_ty) {
                    self.add_error(format!("Type Error: Method '{}' parameter {} type mismatch. Expected {:?}, got {:?}", rm_name, i + 1, rm_ty, m_ty), m.span);
                }
            }

            let m_ret_ty = m_ret.clone().unwrap_or(Type::Void);
            let rm_ret_ty = rm_ret.clone().unwrap_or(Type::Void);
            if !self.types_compatible(&rm_ret_ty, &m_ret_ty) {
                self.add_error(
                    format!(
                        "Type Error: Method '{}' return type mismatch. Expected {:?}, got {:?}",
                        rm_name, rm_ret_ty, m_ret_ty
                    ),
                    m.span,
                );
            }
        }
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
            Type::Tuple(elements) => Type::Tuple(
                elements
                    .into_iter()
                    .map(|t| self.normalize_type(t))
                    .collect(),
            ),
            Type::Channel(inner) => Type::Channel(Box::new(self.normalize_type(*inner))),
            Type::State(inner) => Type::State(Box::new(self.normalize_type(*inner))),
            Type::Function {
                params,
                optional_count,
                return_type,
            } => Type::Function {
                params: params.into_iter().map(|p| self.normalize_type(p)).collect(),
                optional_count,
                return_type: Box::new(self.normalize_type(*return_type)),
            },
            Type::Generic(name, args) => {
                if self.enums.contains_key(&name) {
                    Type::Enum(name)
                } else if self.roles.contains_key(&name) {
                    Type::Role(name)
                } else {
                    Type::Generic(
                        name,
                        args.into_iter().map(|a| self.normalize_type(a)).collect(),
                    )
                }
            }
            _ => ty,
        }
    }

    // Helper to get a generic version of a type for extend lookups
    // This allows `extend tuple { }` to apply to all tuples, `extend array { }` to all arrays, etc.
    fn generic_type(&self, ty: &Type) -> Type {
        match ty {
            Type::Tuple(_) => Type::Tuple(vec![]), // All tuples map to empty tuple
            Type::Array(_) => Type::Array(Box::new(Type::Any)), // All arrays map to array<any>
            Type::Map(_, _) => Type::Map(Box::new(Type::Any), Box::new(Type::Any)), // All maps map to map<any, any>
            Type::Set(_) => Type::Set(Box::new(Type::Any)), // All sets map to set<any>
            // All numeric types map to Number for extend number { }
            Type::I64
            | Type::I32
            | Type::I16
            | Type::I8
            | Type::U64
            | Type::U32
            | Type::U16
            | Type::U8
            | Type::F64
            | Type::F32 => Type::Number,
            _ => ty.clone(),
        }
    }

    fn types_compatible(&self, expected: &Type, actual: &Type) -> bool {
        let expected = self.normalize_type(expected.clone());
        let actual = self.normalize_type(actual.clone());

        if matches!(expected, Type::Any) || matches!(actual, Type::Any) {
            return true;
        }
        if expected == actual {
            return true;
        }

        if self.is_numeric(&expected) && self.is_numeric(&actual) {
            return true;
        }

        match (&expected, &actual) {
            (Type::Array(e_inner), Type::Array(a_inner)) => {
                if matches!(a_inner.as_ref(), Type::Any) {
                    return true;
                }
                self.types_compatible(e_inner.as_ref(), a_inner.as_ref())
            }
            (Type::Tuple(e_els), Type::Tuple(a_els)) => {
                if e_els.len() != a_els.len() {
                    return false;
                }
                e_els
                    .iter()
                    .zip(a_els.iter())
                    .all(|(e, a)| self.types_compatible(e, a))
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

    fn check_pattern(&mut self, pattern: &Pattern, matched_type: &Type, span: Span) {
        match pattern {
            Pattern::Literal(expr) => {
                let ty = self.check_expression(expr);
                if !self.types_compatible(matched_type, &ty) {
                    self.add_error(
                        format!(
                            "Type Error: Match pattern type {:?} doesn't match condition type {:?}",
                            ty, matched_type
                        ),
                        span,
                    );
                }
            }
            Pattern::EnumVariant {
                enum_name,
                variant_name,
                binding,
            } => {
                let enum_def = if let Some(e_name) = enum_name {
                    self.enums.get(e_name)
                } else {
                    self.enums
                        .values()
                        .find(|e| e.variants.iter().any(|v| &v.name == variant_name))
                };

                if let Some(e) = enum_def {
                    let variant = e.variants.iter().find(|v| &v.name == variant_name).unwrap();
                    if let Some(bind_name) = binding {
                        if let Some(v_ty) = &variant.data_type {
                            self.define(bind_name, v_ty.clone(), false, span);
                        } else {
                            self.add_error(
                                format!(
                                    "Type Error: Enum variant '{}' does not take any data",
                                    variant_name
                                ),
                                span,
                            );
                        }
                    }
                } else {
                    self.add_error(
                        format!("Type Error: Unknown enum variant '{}'", variant_name),
                        span,
                    );
                }
            }
            Pattern::Wildcard => {}
        }
    }
}
