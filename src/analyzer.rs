use crate::parser::{Statement, Expr, Type};
use crate::lexer::Token;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Symbol {
    pub ty: Type,
    pub is_mutable: bool,
}

pub struct Analyzer {
    scopes: Vec<HashMap<String, Symbol>>,
    errors: Vec<String>,
    current_return_type: Option<Type>,
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

        global_scope.insert("assert".to_string(), Symbol {
            ty: Type::Function {
                params: vec![Type::Any],
                return_type: Box::new(Type::Void),
            },
            is_mutable: false,
        });

        Self {
            scopes: vec![global_scope],
            errors: Vec::new(),
            current_return_type: None,
        }
    }

    pub fn analyze(&mut self, statements: &[Statement]) -> Result<(), Vec<String>> {
        // Pass 1: Collect top-level function signatures
        for stmt in statements {
            if let Statement::Function { name, params, return_type, .. } = stmt {
                let mut param_types = Vec::new();
                for p in params {
                    if let Some(ty) = &p.ty {
                        param_types.push(ty.clone());
                    } else {
                        self.errors.push(format!("Type Error: Missing type annotation for parameter '{}' in function '{}'", p.name, name));
                        param_types.push(Type::Any);
                    }
                }

                let ret_ty = if let Some(ty) = return_type {
                    ty.clone()
                } else {
                    self.errors.push(format!("Type Error: Missing return type annotation for function '{}'. Use ': void' for functions that return nothing.", name));
                    Type::Void
                };
                
                let ty = Type::Function {
                    params: param_types,
                    return_type: Box::new(ret_ty),
                };
                
                self.define(name, ty, false); 
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
                self.define(name, ty, *is_mutable);
                Type::Void
            }
            Statement::Expression(expr) => self.check_expression(expr),
            Statement::Function { name: _, params, body, return_type, .. } => {
                let actual_return_type = return_type.clone().unwrap_or(Type::Void);
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
                }
                Type::Void
            }
            Statement::Break => Type::Void,
            _ => Type::Void,
        }
    }

    fn check_expression(&mut self, expr: &Expr) -> Type {
        match expr {
            Expr::Number(_) => Type::I64,
            Expr::Float(_) => Type::F64,
            Expr::String(_) => Type::String,
            Expr::Bool(_) => Type::Bool,
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
                } else if matches!(l_ty, Type::String) && matches!(r_ty, Type::String) {
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
        }
    }

    fn is_numeric(&self, ty: &Type) -> bool {
        matches!(ty, Type::I64 | Type::I32 | Type::I16 | Type::I8 | Type::U64 | Type::U32 | Type::U16 | Type::U8 | Type::F64 | Type::F32 | Type::Any)
    }

    fn types_compatible(&self, expected: &Type, actual: &Type) -> bool {
        if matches!(expected, Type::Any) || matches!(actual, Type::Any) { return true; }
        if expected == actual { return true; }
        
        if self.is_numeric(expected) && self.is_numeric(actual) { return true; }
        
        if let Type::Array(expected_inner) = expected {
            if let Type::Array(actual_inner) = actual {
                if matches!(**actual_inner, Type::Any) { return true; }
                return self.types_compatible(expected_inner, actual_inner);
            }
        }
        
        if let Type::Tuple(expected_elements) = expected {
            if let Type::Tuple(actual_elements) = actual {
                if expected_elements.len() != actual_elements.len() { return false; }
                return expected_elements.iter().zip(actual_elements.iter()).all(|(e, a)| self.types_compatible(e, a));
            }
        }

        false
    }
}
