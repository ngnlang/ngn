mod ast;
mod lexer;
mod parser;
mod expr_parser;

use parser::Parser;
use expr_parser::ExprParser;

use std::fs;
use std::collections::HashSet;
use crate::ast::{ClosureValue, FnDef, InterpolationPart, ModelDef, Moved, Ownership, RoleDef, Type};

fn main() {
    let source = fs::read_to_string("main.ngn")
        .expect("Failed to read ngn file");

    let tokens = lexer::tokenize(&source);
    let mut parser = Parser::new(tokens);

    match parser.parse_program() {
        Ok(ast) => run(&ast),
        Err(e) => eprintln!("Parse error: {}", e),
    }
}

use std::collections::HashMap;
use ast::{Expr, Stmt};

use crate::ast::{AssignKind, ControlFlow, MatchType, Value};

fn format_value(v: &Value) -> String {
    match v {
        Value::Number(n) => n.to_string(),
        Value::String(s) => s.clone(),
        Value::Bool(b) => b.to_string(),
        Value::Array(arr) => format!("[{}]", arr.iter().map(format_value).collect::<Vec<_>>().join(", ")),
        Value::Function(_) => "<function>".to_string(),
        Value::Closure(_) => "<closure>".to_string(),
        Value::Void => "".to_string(),
        Value::Object(_model_name, fields) => {
            let field_strs: Vec<String> = fields
                .iter()
                .map(|(k, v)| format!("{}: {}", k, format_value(v)))
                .collect();
            format!("{{ {} }}", field_strs.join(", "))
        }
    }
}

fn parse_expression_from_string(expr_str: &str) -> Result<Expr, String> {
    let tokens = crate::lexer::tokenize(expr_str);
    
    let mut parser = ExprParser::new(tokens);
    parser.parse()
        .map_err(|e| format!("Failed to parse interpolated expression: {}", e))
}

fn parse_interpolated_string(s: &str) -> Result<Vec<InterpolationPart>, String> {
    let mut parts = Vec::new();
    let mut current = String::new();
    let mut chars = s.chars().peekable();
    
    while let Some(c) = chars.next() {
        if c == '{' {
            if !current.is_empty() {
                parts.push(InterpolationPart::Literal(current.clone()));
                current.clear();
            }
            
            let mut expr_str = String::new();
            let mut brace_depth = 1;
            while let Some(c) = chars.next() {
                if c == '{' {
                    brace_depth += 1;
                    expr_str.push(c);
                } else if c == '}' {
                    brace_depth -= 1;
                    if brace_depth == 0 { break; }
                    expr_str.push(c);
                } else {
                    expr_str.push(c);
                }
            }
            
            // Parse the expression!
            let expr = parse_expression_from_string(&expr_str)?;
            parts.push(InterpolationPart::Expression(Box::new(expr)));
        } else if c == '\\' {
            if let Some(next) = chars.next() {
                match next {
                    'n' => current.push('\n'),
                    't' => current.push('\t'),
                    'r' => current.push('\r'),
                    '\\' => current.push('\\'),
                    '"' => current.push('"'),
                    _ => current.push(next),
                }
            }
        } else {
            current.push(c);
        }
    }
    
    if !current.is_empty() {
        parts.push(InterpolationPart::Literal(current));
    }
    
    Ok(parts)
}

fn is_truthy(v: &Value) -> bool {
    match v {
        Value::Bool(b) => *b,
        Value::Number(n) => *n != 0.0,
        Value::String(s) => !s.is_empty(),
        Value::Array(arr) => !arr.is_empty(),
        Value::Function(_) => true,
        Value::Void => false,
        Value::Object(_, _) => true,
        Value::Closure(_) => true,
    }
}

fn is_literal(e: &Expr) -> bool {
    matches!(e, 
        Expr::Number(_) | 
        Expr::String(_) | 
        Expr::Bool(_) |
        Expr::Array(_)
    )
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Number(x), Value::Number(y)) => x == y,
        (Value::String(x), Value::String(y)) => x == y,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::Array(x), Value::Array(y)) => x == y,
        // Todo - models?
        _ => false,
    }
}

fn infer_value_type(v: &Value) -> Type {
    match v {
        Value::Number(_) => Type::Number,
        Value::String(_) => Type::Str,
        Value::Bool(_) => Type::Bool,
        Value::Array(arr) => {
            if arr.is_empty() {
                Type::Array(Box::new(Type::Number))
            } else {
                let elem_type = infer_value_type(&arr[0]);
                
                for elem in arr.iter().skip(1) {
                    let elem_val_type = infer_value_type(elem);
                    if !types_compatible(&elem_type, &elem_val_type) {
                        panic!("Type error: mixed types in array {:?}", arr);
                    }
                }
                
                Type::Array(Box::new(elem_type))
            }
        }
        Value::Function(_) => Type::Function,
        Value::Closure(_) => Type::Function,
        Value::Void => Type::Void,
        Value::Object(model_name, _) => Type::Model(model_name.clone()),
    }
}

fn infer_expr_type(
    e: &Expr,
    env: &HashMap<String, (AssignKind, Value, Ownership, Moved)>,
    fns: &HashMap<String, FnDef>,
    models: &HashMap<String, ModelDef>,
) -> Type {
    match e {
        Expr::Number(_) => Type::Number,
        Expr::String(_) => Type::Str,
        Expr::InterpolatedString(_) => Type::String,
        Expr::Bool(_) => Type::Bool,
        Expr::Array(exprs) => {
            if exprs.is_empty() {
                Type::Array(Box::new(Type::Number))
            } else {
                let elem_type = infer_expr_type(&exprs[0], env, fns, models);
                for expr in exprs.iter().skip(1) {
                    let other_type = infer_expr_type(expr, env, fns, models);
                    if !types_compatible(&elem_type, &other_type) {
                        panic!("Array type error: mixed types");
                    }
                }
                Type::Array(Box::new(elem_type))
            }
        }
        Expr::Add(a, b) => {
            let left = infer_expr_type(a, env, fns, models);
            let right = infer_expr_type(b, env, fns, models);
            if types_compatible(&left, &Type::Number) && types_compatible(&right, &Type::Number) {
                Type::Number
            } else if types_compatible(&left, &Type::String) && types_compatible(&right, &Type::String) {
                Type::String
            } else {
                panic!("Type error: cannot add {:?} and {:?}", left, right);
            }
        }
        Expr::Subtract(a, b) | Expr::Multiply(a, b) | Expr::Divide(a, b) | Expr::Modulo(a, b) => {
            let left = infer_expr_type(a, env, fns, models);
            let right = infer_expr_type(b, env, fns, models);
            if !types_compatible(&left, &Type::Number) || !types_compatible(&right, &Type::Number) {
                panic!("Type error: arithmetic requires numbers");
            }
            Type::Number
        }
        Expr::Power(a, b) => {
            let left = infer_expr_type(a, env, fns, models);
            let right = infer_expr_type(b, env, fns, models);
            if !types_compatible(&left, &Type::Number) || !types_compatible(&right, &Type::Number) {
                panic!("Type error: power requires numbers");
            }
            Type::Number
        }
        Expr::Negative(e) => {
            let t = infer_expr_type(e, env, fns, models);
            if !types_compatible(&t, &Type::Number) {
                panic!("Type error: cannot negate non-number");
            }
            Type::Number
        }
        Expr::Equal(_, _) | Expr::NotEqual(_, _) | Expr::LessThan(_, _) | 
        Expr::LessThanOrEqual(_, _) | Expr::GreaterThan(_, _) | Expr::GreaterThanOrEqual(_, _) => {
            Type::Bool
        }
        Expr::Not(_) => Type::Bool,
        Expr::Var(name) | Expr::Const(name) | Expr::Lit(name) | Expr::Static(name) => {
            if let Some((_, v, _ownership, _moved)) = env.get(name) {
                infer_value_type(v)
            } else if fns.contains_key(name) {
                Type::Function
            } else {
                panic!("Unknown variable: {}", name);
            }
        }
        Expr::Call { name, .. } => {
            // Check if it's a function value in the environment
            if let Some((_, Value::Function(fn_def), _ownership, _moved)) = env.get(name) {
                fn_def.return_type.clone().unwrap_or(Type::Number)
            } else if let Some((_, Value::Closure(closure), _ownership, _moved)) = env.get(name) {
                // Closure return type
                closure.def.return_type.clone().unwrap_or(Type::Number)
            } else if let Some(fn_def) = fns.get(name) {
                // Check if it's a global function
                fn_def.return_type.clone().unwrap_or(Type::Number)
            } else {
                panic!("Unknown function: {}", name);
            }
        }
        Expr::Assign { value, .. } => infer_expr_type(value, env, fns, models),
        Expr::CompoundAssign { value, .. } => infer_expr_type(value, env, fns, models),
        Expr::ModelInstance { name, .. } => Type::Model(name.clone()),
        Expr::FieldAccess { object, field } => {
            let obj_type = infer_expr_type(object, env, fns, models);
            match obj_type {
                Type::Model(model_name) => {
                    if let Some(model_def) = models.get(&model_name) {
                        // Find field in model definition
                        model_def.fields.iter()
                            .find(|(name, _)| name == field)
                            .map(|(_, field_type)| field_type.clone())
                            .unwrap_or_else(|| panic!("Field '{}' not found on model '{}'", field, model_name))
                    } else {
                        panic!("Unknown model: {}", model_name);
                    }
                }
                _ => panic!("Cannot access field on type {:?}", obj_type),
            }
        }
        Expr::MethodCall { object, method: _, args: _ } => {
            let obj_type = infer_expr_type(object, env, fns, models);
            match obj_type {
                Type::Model(_model_name) => {
                    // For now, assume method returns what we'll determine at runtime
                    // In a full implementation, you'd look up the method signature
                    Type::Void  // TODO: look up actual return type from implementations
                }
                _ => panic!("Cannot call method on type {:?}", obj_type),
            }
        }
        Expr::Closure(_) => Type::Function,
    }
}

fn types_compatible(expected: &Type, actual: &Type) -> bool {
    match (expected, actual) {
        (Type::Number, Type::Number) => true,
        (Type::Str, Type::Str) => true,
        (Type::String, Type::String) => true,
        (Type::Bool, Type::Bool) => true,
        (Type::Array(e1), Type::Array(e2)) => types_compatible(e1, e2),
        (Type::Void, Type::Void) => true,
        (Type::Model(a), Type::Model(b)) => a == b,
        _ => false,
    }
}

fn convert_ownership(t: Type) -> Type {
    match t {
        Type::Str => Type::String,
        Type::Array(inner) => Type::Array(Box::new(convert_ownership(*inner))),
        Type::Object(map) => {
            Type::Object(
                map.into_iter()
                    .map(|(k, v)| (k, convert_ownership(v)))
                    .collect()
            )
        }
        other => other,
    }
}

fn get_arg_ownership(e: &Expr, env: &HashMap<String, (AssignKind, Value, Ownership, Moved)>) -> Ownership {
    match e {
        Expr::Var(name) => {
            env.get(name).map(|(_, _, o, _)| o.clone()).unwrap_or(Ownership::Borrowed)
        }
        Expr::String(_) => Ownership::Borrowed,
        Expr::Number(_) => Ownership::Borrowed,
        Expr::Bool(_) => Ownership::Borrowed,
        Expr::Add(_, _) => Ownership::Owned,  // String concat returns owned
        _ => Ownership::Borrowed,
    }
}

fn is_copy_type(t: &Type) -> bool {
    matches!(t, Type::Number | Type::Bool)
}

fn validate_role_compliance(
    model_name: &str,
    role_name: &str,
    methods: &[FnDef],
    roles: &HashMap<String, RoleDef>,
) {
    let role = &roles[role_name];
    let method_names: HashSet<String> = methods.iter().map(|m| m.name.clone()).collect();
    
    for required_method in &role.methods {
        if !method_names.contains(&required_method.name) {
            panic!(
                "Model '{}' does not implement required method '{}' from role '{}'",
                model_name, required_method.name, role_name
            );
        }
        
        // Validate signature matches (params and return type)
        let impl_method = methods.iter()
            .find(|m| m.name == required_method.name)
            .unwrap();
        
        if impl_method.params.len() != required_method.params.len() {
            panic!(
                "Method '{}' signature mismatch: expected {} params, got {}",
                required_method.name,
                required_method.params.len(),
                impl_method.params.len()
            );
        }
        
        // TODO: Add return type validation
    }
}

fn call_closure(
    closure: &ClosureValue,
    args: &[Expr],
    outer_env: &mut HashMap<String, (AssignKind, Value, Ownership, Moved)>,
    fns: &mut HashMap<String, FnDef>,
    models: &mut HashMap<String, ModelDef>,
    roles: &mut HashMap<String, RoleDef>,
    model_methods: &mut HashMap<(String, String), FnDef>,
) -> Value {
    // Create new scope starting with captured environment
    let mut closure_env = closure.captured_env.clone();
    
    // Bind parameters
    for (i, (param_name, _param_type)) in closure.def.params.iter().enumerate() {
        if i < args.len() {
            let arg_val = eval_expr(&args[i], outer_env, fns, models, roles, model_methods);
            closure_env.insert(
                param_name.clone(),
                (AssignKind::Var, arg_val, Ownership::Borrowed, Moved::False),
            );
        } else {
            panic!("Closure expects {} arguments, got {}", closure.def.params.len(), args.len());
        }
    }
    
    // Execute closure body
    let flow = execute_block(
        &closure.def.body,
        &mut closure_env,
        fns,
        models,
        roles,
        model_methods,
        closure.def.return_type.as_ref(),
    );

    for var in &closure.vars {
        if let Some(value) = closure_env.get(var) {
            outer_env.insert(var.clone(), value.clone());
        }
    }
    
    match flow {
        ControlFlow::Return(val) => val,
        _ => Value::Void,
    }
}

fn eval_expr(
    e: &Expr,
    env: &mut HashMap<String, (AssignKind, Value, Ownership, Moved)>,
    fns: &mut HashMap<String, FnDef>,
    models: &mut HashMap<String, ModelDef>,
    roles: &mut HashMap<String, RoleDef>,
    model_methods: &mut HashMap<(String, String), FnDef>,
) -> Value {
    match e {
        Expr::Number(n) => Value::Number(*n),
        Expr::InterpolatedString(parts) => {
            let mut result = String::new();
            for part in parts {
                match part {
                    InterpolationPart::Literal(s) => result.push_str(s),
                    InterpolationPart::Expression(expr) => {
                        let val = eval_expr(expr, env, fns, models, roles, model_methods);
                        match val {
                            Value::String(s) => result.push_str(&s),
                            Value::Number(n) => result.push_str(&n.to_string()),
                            Value::Bool(b) => result.push_str(&b.to_string()),
                            _ => result.push_str(&format!("{:?}", val)),
                        }
                    }
                }
            }
            Value::String(result)
        },
        Expr::String(s) => Value::String(s.clone()),
        Expr::Bool(b) => Value::Bool(*b),
        Expr::Array(exprs) => {
            let mut values: Vec<Value> = Vec::new();
            for e in exprs {
                values.push(eval_expr(e, env, fns, models, roles, model_methods));
            }
            Value::Array(values)
        },
        Expr::Not(e) => {
            let val = eval_expr(e, env, fns, models, roles, model_methods);
            Value::Bool(!is_truthy(&val))
        },
        Expr::Add(a, b) => {
            match (eval_expr(a, env, fns, models, roles, model_methods), eval_expr(b, env, fns, models, roles, model_methods)) {
                (Value::Number(x), Value::Number(y)) => Value::Number(x + y),
                (Value::String(x), Value::String(y)) => Value::String(format!("{}{}", x, y)),
                _ => panic!("Type error: cannot add these types"),
            }
        }
        Expr::Subtract(a, b) => {
            match (eval_expr(a, env, fns, models, roles, model_methods), eval_expr(b, env, fns, models, roles, model_methods)) {
                (Value::Number(x), Value::Number(y)) => Value::Number(x - y),
                _ => panic!("Type error: cannot subtract non-numbers"),
            }
        }
        Expr::Multiply(a, b) => {
            match (eval_expr(a, env, fns, models, roles, model_methods), eval_expr(b, env, fns, models, roles, model_methods)) {
                (Value::Number(x), Value::Number(y)) => Value::Number(x * y),
                _ => panic!("Type error: cannot multiply non-numbers"),
            }
        }
        Expr::Divide(a, b) => {
            match (eval_expr(a, env, fns, models, roles, model_methods), eval_expr(b, env, fns, models, roles, model_methods)) {
                (Value::Number(x), Value::Number(y)) => Value::Number(x / y),
                _ => panic!("Type error: cannot divide non-numbers"),
            }
        }
        Expr::Negative(e) => {
            match eval_expr(e, env, fns, models, roles, model_methods) {
                Value::Number(x) => Value::Number(-x),
                _ => panic!("Type error: cannot negate non-number"),
            }
        }
        Expr::Power(a, b) => {
            match (eval_expr(a, env, fns, models, roles, model_methods), eval_expr(b, env, fns, models, roles, model_methods)) {
                (Value::Number(x), Value::Number(y)) => Value::Number(x.powf(y)),
                _ => panic!("Type error: cannot exponentiate non-numbers"),
            }
        }
        Expr::Modulo(a, b) => {
            match (eval_expr(a, env, fns, models, roles, model_methods), eval_expr(b, env, fns, models, roles, model_methods)) {
                (Value::Number(x), Value::Number(y)) => Value::Number(x % y),
                _ => panic!("Type error: cannot modulo non-numbers"),
            }
        }
        Expr::Equal(a, b) => {
            let av = eval_expr(a, env, fns, models, roles, model_methods);
            let bv = eval_expr(b, env, fns, models, roles, model_methods);
            Value::Bool(values_equal(&av, &bv))
        }
        Expr::NotEqual(a, b) => {
            let av = eval_expr(a, env, fns, models, roles, model_methods);
            let bv = eval_expr(b, env, fns, models, roles, model_methods);
            Value::Bool(!values_equal(&av, &bv))
        }
        Expr::LessThan(a, b) => {
            match (eval_expr(a, env, fns, models, roles, model_methods), eval_expr(b, env, fns, models, roles, model_methods)) {
                (Value::Number(x), Value::Number(y)) => Value::Bool(x < y),
                _ => panic!("Type error: cannot compare non-numbers"),
            }
        }
        Expr::LessThanOrEqual(a, b) => {
            match (eval_expr(a, env, fns, models, roles, model_methods), eval_expr(b, env, fns, models, roles, model_methods)) {
                (Value::Number(x), Value::Number(y)) => Value::Bool(x <= y),
                _ => panic!("Type error: cannot compare non-numbers"),
            }
        }
        Expr::GreaterThan(a, b) => {
            match (eval_expr(a, env, fns, models, roles, model_methods), eval_expr(b, env, fns, models, roles, model_methods)) {
                (Value::Number(x), Value::Number(y)) => Value::Bool(x > y),
                _ => panic!("Type error: cannot compare non-numbers"),
            }
        }
        Expr::GreaterThanOrEqual(a, b) => {
            match (eval_expr(a, env, fns, models, roles, model_methods), eval_expr(b, env, fns, models, roles, model_methods)) {
                (Value::Number(x), Value::Number(y)) => Value::Bool(x >= y),
                _ => panic!("Type error: cannot compare non-numbers"),
            }
        }
        Expr::Assign { name, value } => {
            let v = eval_expr(value, env, fns, models, roles, model_methods);
            env.insert(name.clone(), (AssignKind::Var, v.clone(), Ownership::Borrowed, Moved::False));
            v
        }
        Expr::CompoundAssign { name, op: _, value } => {
            let v = eval_expr(value, env, fns, models, roles, model_methods);
            env.insert(name.clone(), (AssignKind::Var, v.clone(), Ownership::Borrowed, Moved::False));
            v
        }
        Expr::Var(name) => {
            if let Some((_, v, _ownership, moved)) = env.get(name) {
                if let Moved::True = *moved {
                    panic!("Variable '{}' has been moved", name);
                }
                v.clone()
            } else if let Some(fn_def) = fns.get(name) {
                // If it's a function in fns, return it as a Value::Function
                Value::Function(fn_def.clone())
            } else {
                panic!("Undefined variable: {}", name);
            }
        }
        Expr::Const(name) => {
            if let Some((_, v, _ownership, moved)) = env.get(name) {
                if let Moved::True = *moved {
                    panic!("Constant '{}' has been moved", name);
                }
                v.clone()
            } else if let Some(fn_def) = fns.get(name) {
                // If it's a function in fns, return it as a Value::Function
                Value::Function(fn_def.clone())
            } else {
                panic!("Undefined constant: {}", name);
            }
        },
        Expr::Lit(name) => {
            if let Some((_, v, _ownership, _moved)) = env.get(name) {
                v.clone()
            } else {
                panic!("Undefined literal: {}", name);
            }
        },
        Expr::Static(name) => {
            if let Some((_, v, _ownership, _moved)) = env.get(name) {
                v.clone()
            } else {
                panic!("Undefined static: {}", name);
            }
        },
        Expr::Call { name, args } => {
            if let Some((_, Value::Closure(closure), _ownership, _moved)) = env.get(name) {
                let closure = closure.clone();
                return call_closure(
                    &closure,
                    args,
                    env,
                    fns,
                    models,
                    roles,
                    model_methods,
                );
            }

            let fn_def = if let Some((_, Value::Function(fn_def), _ownership, _moved)) = env.get(name) {
                fn_def.clone()
            } else if let Some(fn_def) = fns.get(name) {
                fn_def.clone()
            } else {
                panic!("Unknown function: {}", name);
            };

            // Create new scope for function
            let mut fn_env: HashMap<String, (AssignKind, Value, Ownership, Moved)> = HashMap::new();
            let fn_arg_count = fn_def.params.len();
            
            // Bind parameters
            for (i, (param_name, param_type, param_ownership)) in fn_def.params.iter().enumerate() {
                let arg_expr = &args[i];
                let ownership = get_arg_ownership(arg_expr, env);  // Helper function

                let arg_val = if i < args.len() {
                    eval_expr(arg_expr, env, fns, models, roles, model_methods)
                } else {
                    panic!("Function {} expects {} arguments, got {}", name, fn_arg_count, args.len())
                };
                
                // Only error if param expects Owned but arg is Borrowed
                if *param_ownership == Ownership::Owned && ownership == Ownership::Borrowed {
                    panic!("Function {} param '{}' expects owned, but got borrowed", name, param_name);
                }
                
                // Mark as moved if param expects Owned
                if *param_ownership == Ownership::Owned {
                    if let Expr::Var(var_name) = arg_expr {
                        if let Some((k, v, o, _)) = env.get(var_name) {
                            env.insert(var_name.clone(), (k.clone(), v.clone(), o.clone(), Moved::True));
                        }
                    }
                }

                // Validate parameter type
                if let Some(expected_type) = param_type {
                    let actual_type = infer_value_type(&arg_val);
                    if !types_compatible(expected_type, &actual_type) {
                        panic!("Type error: function param {}: expected {:?}, got {:?}", name, expected_type, actual_type);
                    }
                }

                fn_env.insert(param_name.clone(), (AssignKind::Var, arg_val, ownership, Moved::False));
            }
            
            // Execute function body
            if let Some(body) = &fn_def.body {
                let flow = execute_block(body, &mut fn_env, fns, models, roles, model_methods, fn_def.return_type.as_ref());
                match flow {
                    ControlFlow::Return(val) => val,
                    _ => Value::Void,
                }
            } else {
                panic!("Cannot call function '{}' as it has not body.", name);
            }
            
        }
        Expr::ModelInstance { name, fields } => {
            // Lookup model definition
            if let Some(_model_def) = models.get(name).cloned() {
                let mut field_values = HashMap::new();
                
                // Evaluate all field expressions
                for (field_name, field_expr) in fields {
                    let field_val = eval_expr(field_expr, env, fns, models, roles, model_methods);
                    field_values.insert(field_name.clone(), field_val);
                }
                
                Value::Object(name.clone(), field_values)
            } else {
                panic!("Unknown model: {}", name);
            }
        }
        Expr::FieldAccess { object, field } => {
            let obj_val = eval_expr(object, env, fns, models, roles, model_methods);
            match obj_val {
                Value::Object(_, fields) => {
                    fields.get(field).cloned()
                        .unwrap_or_else(|| panic!("Field '{}' not found", field))
                }
                _ => panic!("Cannot access field on non-object"),
            }
        }
        Expr::MethodCall { object, method, args } => {
            let obj_val = eval_expr(object, env, fns, models, roles, model_methods);
            match &obj_val {
                Value::Object(model_name, _fields) => {
                    // Clone the method_def first to avoid borrow issues
                    let method_def = model_methods
                        .get(&(model_name.clone(), method.clone()))
                        .cloned()
                        .unwrap_or_else(|| panic!("Method '{}' not found on model '{}'", method, model_name));

                    // Create new scope for method
                    let mut method_env: HashMap<String, (AssignKind, Value, Ownership, Moved)> = HashMap::new();

                    // Bind `this` as first implicit parameter
                    method_env.insert(
                        "this".to_string(),
                        (AssignKind::Const, obj_val.clone(), Ownership::Borrowed, Moved::False),
                    );
                    
                    // Bind explicit parameters (skip first param if it matches signature)
                    for (i, (param_name, _param_type, param_ownership)) in method_def.params.iter().enumerate() {
                        if i < args.len() {
                            let arg_val = eval_expr(&args[i], env, fns, models, roles, model_methods);
                            method_env.insert(
                                param_name.clone(),
                                (AssignKind::Var, arg_val, param_ownership.clone(), Moved::False),
                            );
                        }
                    }
                        
                    // Execute method body
                    if let Some(body) = &method_def.body {
                        let flow = execute_block(body, &mut method_env, fns, models, roles, model_methods, method_def.return_type.as_ref());
                        match flow {
                            ControlFlow::Return(val) => val,
                            _ => Value::Void,
                        }
                    } else {
                        panic!("Method '{}' has no body", method);
                    }
                }
                _ => panic!("Cannot call method on non-object"),
            }
        }
        Expr::Closure(closure_def) => {
            let vars: Vec<String> = env
                .iter()
                .filter(|(_, (kind, _, _, _))| matches!(kind, AssignKind::Var))
                .map(|(name, _)| name.clone())
                .collect();

            Value::Closure(ClosureValue {
                def: Box::new(closure_def.as_ref().clone()),
                captured_env: env.clone(),
                vars,
            })
        }
    }
}

fn execute_stmt(
    stmt: &Stmt,
    env: &mut HashMap<String, (AssignKind, Value, Ownership, Moved)>,
    fns: &mut HashMap<String, FnDef>,
    models: &mut HashMap<String, ModelDef>,
    roles: &mut HashMap<String, RoleDef>,
    model_methods: &mut HashMap<(String, String), FnDef>,
    expected_return_type: Option<&Type>,
) -> ControlFlow {
    match stmt {
        Stmt::Echo(e) => {
            let _type = infer_expr_type(e, env, fns, models);
            let v = eval_expr(e, env, fns, models, roles, model_methods);
            print!("{}", format_value(&v));
            ControlFlow::None
        }
        Stmt::Print(e) => {
            let _type = infer_expr_type(e, env, fns, models);
            let v = eval_expr(e, env, fns, models, roles, model_methods);
            println!("{}", format_value(&v));
            ControlFlow::None
        }
        Stmt::ExprStmt(e) => {
            let _type = infer_expr_type(e, env, fns, models);
            eval_expr(e, env, fns, models, roles, model_methods);
            ControlFlow::None
        }
        Stmt::Assign { kind, declared_type, name, value, ownership} => {
            // Disallow function assignments for all variable types
            if let AssignKind::Var | AssignKind::Const = kind {
                if let Expr::Var(fn_name) | Expr::Const(fn_name) = value {
                    if fns.contains_key(fn_name) {
                        panic!("Cannot assign function '{}' to variable. Use 'fn' to define functions.", fn_name);
                    }
                }
            }

            // For lit and static, only allow literals
            match kind {
                AssignKind::Lit | AssignKind::Static => {
                    if !is_literal(value) {
                        panic!("'{}' can only be used to assign literal values, not expressions or function calls", 
                            match kind {
                                AssignKind::Lit => "lit",
                                AssignKind::Static => "static",
                                _ => "<Illegal initializer>",
                            }
                        );
                    }
                }
                _ => {}
            }

            let _type = infer_expr_type(value, env, fns, models);
            let v = eval_expr(value, env, fns, models, roles, model_methods);
            let mut actual_type = infer_value_type(&v);
            let mut normalized_declared_type = declared_type.clone();

            normalized_declared_type = normalized_declared_type.map(|t| {
                match ownership {
                    Ownership::Owned => convert_ownership(t),
                    Ownership::Borrowed => t,
                }
            });

            match ownership {
                Ownership::Borrowed => {
                    // Value must be borrowable (&str, &[T], etc.)
                    if *kind == AssignKind::Var && actual_type == Type::String {
                        panic!("Cannot use '=' with String; use '<-' for owned assignment");
                    }
                }
                Ownership::Owned => {
                    actual_type = convert_ownership(actual_type);
                }
            }

            if let Some(expected_type) = normalized_declared_type {
                if !types_compatible(&expected_type, &actual_type) {
                    panic!("Type error: expected {:?}, got {:?}", expected_type, actual_type);
                }
            }

            env.insert(name.clone(), (kind.clone(), v, ownership.clone(), Moved::False));
            ControlFlow::None
        }
        Stmt::Reassign { name, value } => {
            let _type = infer_expr_type(value, env, fns, models);
            if !env.contains_key(name) {
                panic!("Variable '{}' not declared", name);
            }

            let (kind, existing_val, ownership, _moved) = env.get(name).unwrap().clone();
            let existing_type = infer_value_type(&existing_val);

            match kind {
                AssignKind::Var => {
                    let v = eval_expr(value, env, fns, models, roles, model_methods);
                    let actual_type = infer_value_type(&v);

                    // Only allow reassignment if value is mutable (owned)
                    if ownership == Ownership::Borrowed && !is_copy_type(&actual_type) {
                        panic!("Cannot reassign immutable borrowed value '{}'", name);
                    }

                    // Validate type matches original
                    if !types_compatible(&existing_type, &actual_type) {
                        panic!("Type error: variable {} is {:?}, cannot assign {:?}", name, existing_type, actual_type);
                    }

                    env.insert(name.clone(), (kind.clone(), v, ownership, _moved));
                    ControlFlow::None
                }
                kind => panic!("Cannot reassign for {}", match kind {
                    AssignKind::Const => "const",
                    AssignKind::Lit => "lit",
                    AssignKind::Static => "static",
                    _ => "an invalid initializer",
                })
            }
        }
        Stmt::Rebind { name, value } => {
            let (kind, ownership) = if let Some((k, _, o, moved)) = env.get(name) {
                if *k != AssignKind::Var {
                    panic!("Can only rebind var, not {:?}", k);
                }
                if let Moved::True = *moved {
                    panic!("Cannot rebind moved variable '{}'", name);
                }
                (k.clone(), o.clone())
            } else {
                panic!("Cannot rebind undefined variable '{}'", name);
            };
            
            let v = eval_expr(value, env, fns, models, roles, model_methods);
            env.insert(name.clone(), (kind, v, ownership, Moved::False));
            ControlFlow::None
        }
        Stmt::Break => ControlFlow::Break,
        Stmt::Next => ControlFlow::Next,
        Stmt::If { condition, then_block, else_ifs, else_block } => {
            let _cond_type = infer_expr_type(condition, env, fns, models);
            let cond_value = eval_expr(condition, env, fns, models, roles, model_methods);
            
            if is_truthy(&cond_value) {
                execute_block(then_block, env, fns, models, roles, model_methods, None)
            } else {
                for (else_if_cond, else_if_stmts) in else_ifs {
                    let _type = infer_expr_type(else_if_cond, env, fns, models);
                    let else_if_value = eval_expr(else_if_cond, env, fns, models, roles, model_methods);
                    if is_truthy(&else_if_value) {
                        return execute_block(else_if_stmts, env, fns, models, roles, model_methods, None);
                    }
                }
                
                if let Some(else_stmts) = else_block {
                    execute_block(else_stmts, env, fns, models, roles, model_methods, None)
                } else {
                    ControlFlow::None
                }
            }
        }
        Stmt::While { condition, body } => {
            loop {
                let _cond_type = infer_expr_type(condition, env, fns, models);
                let cond_value = eval_expr(condition, env, fns, models, roles, model_methods);
                if !is_truthy(&cond_value) {
                    break;
                }
                
                match execute_block(body, env, fns, models, roles, model_methods, None) {
                    ControlFlow::Break => break,
                    ControlFlow::Next => continue,
                    ControlFlow::Return(_) => return ControlFlow::None,
                    ControlFlow::None => {}
                }
            }
            ControlFlow::None
        }
        Stmt::WhileOnce { condition, body } => {
            loop {
                match execute_block(body, env, fns, models, roles, model_methods, None) {
                    ControlFlow::Break => break,
                    ControlFlow::Next => continue,
                    ControlFlow::Return(_) => return ControlFlow::None,
                    ControlFlow::None => {}
                }

                let _cond_type = infer_expr_type(condition, env, fns, models);
                let cond_value = eval_expr(condition, env, fns, models, roles, model_methods);
                if !is_truthy(&cond_value) {
                    break;
                }
            }
            ControlFlow::None
        }
        Stmt::Until { condition, body } => {
            loop {
                let _cond_type = infer_expr_type(condition, env, fns, models);
                let cond_value = eval_expr(condition, env, fns, models, roles, model_methods);
                if is_truthy(&cond_value) {
                    break;
                }
                
                match execute_block(body, env, fns, models, roles, model_methods, None) {
                    ControlFlow::Break => break,
                    ControlFlow::Next => continue,
                    ControlFlow::Return(_) => return ControlFlow::None,
                    ControlFlow::None => {}
                }
            }
            ControlFlow::None
        }
        Stmt::UntilOnce { condition, body } => {
            loop {
                match execute_block(body, env, fns, models, roles, model_methods, None) {
                    ControlFlow::Break => break,
                    ControlFlow::Next => continue,
                    ControlFlow::Return(_) => return ControlFlow::None,
                    ControlFlow::None => {}
                }

                let _cond_type = infer_expr_type(condition, env, fns, models);
                let cond_value = eval_expr(condition, env, fns, models, roles, model_methods);
                if is_truthy(&cond_value) {
                    break;
                }
            }
            ControlFlow::None
        }
        Stmt::Match { expr, cases, default, match_type } => {
            let _expr_type = infer_expr_type(expr, env, fns, models);
            let val = eval_expr(expr, env, fns, models, roles, model_methods);
            let mut matched = false;
            
            for (tests, stmts) in cases {
                for test in tests {
                    let _test_type = infer_expr_type(test, env, fns, models);
                    let test_val = eval_expr(test, env, fns, models, roles, model_methods);

                    if values_equal(&val, &test_val) {
                        matched = true;
                        let flow = execute_block(stmts, env, fns, models, roles, model_methods, None);
                        
                        match (match_type, flow) {
                            (MatchType::One, ControlFlow::Next) => continue,
                            (MatchType::One, _) => return ControlFlow::None,
                            
                            (MatchType::Any, ControlFlow::Break) => return ControlFlow::None,
                            (MatchType::Any, _) => continue,
                        }
                    }
                }
            }
            
            // No match found, execute default if it exists
            if !matched || *match_type == MatchType::Any {
                if let Some(default_stmts) = default {
                    execute_block(default_stmts, env, fns, models, roles, model_methods, None)
                } else {
                    ControlFlow::None
                }
            } else {
                ControlFlow::None
            }
        }
        Stmt::FnDef(fn_def) => {
            fns.insert(fn_def.name.clone(), FnDef {
                name: fn_def.name.clone(),
                params: fn_def.params.clone(),
                body: fn_def.body.clone(),
                return_type: fn_def.return_type.clone(),
            });
            ControlFlow::None
        }
        Stmt::ModelDef(model_def) => {
            models.insert(model_def.name.clone(), model_def.clone());
            ControlFlow::None
        }
        Stmt::RoleDef(role_def) => {
            roles.insert(role_def.name.clone(), role_def.clone());
            ControlFlow::None
        }
        Stmt::ExtendModel { .. } => { ControlFlow::None }
        Stmt::Return(expr_opt) => {
            let val = match expr_opt {
                Some(e) => {
                    let _type = infer_expr_type(e, env, fns, models);
                    eval_expr(e, env, fns, models, roles, model_methods)
                }
                None => Value::Void,
            };

            if let Some(expected_type) = expected_return_type {
                let actual_type = infer_value_type(&val);
                if !types_compatible(expected_type, &actual_type) {
                    panic!("Return type error: expected {:?}, got {:?}", expected_type, actual_type);
                }
            }

            ControlFlow::Return(val)
        }
    }
}

fn execute_block(
    stmts: &[Stmt],
    env: &mut HashMap<String, (AssignKind, Value, Ownership, Moved)>,
    fns: &mut HashMap<String, FnDef>,
    models: &mut HashMap<String, ModelDef>,
    roles: &mut HashMap<String, RoleDef>,
    model_methods: &mut HashMap<(String, String), FnDef>,
    expected_return_type: Option<&Type>,
) -> ControlFlow {
    for stmt in stmts {
        let flow = execute_stmt(stmt, env, fns, models, roles, model_methods, expected_return_type);
        match flow {
            ControlFlow::Break | ControlFlow::Next | ControlFlow::Return(_) => return flow,
            ControlFlow::None => {}
        }
    }
    ControlFlow::None
}

fn run(stmts: &[Stmt]) {
    let mut env: HashMap<String, (AssignKind, Value, Ownership, Moved)> = HashMap::new();
    let mut fns: HashMap<String, FnDef> = HashMap::new();
    let mut models: HashMap<String, ModelDef> = HashMap::new();
    let mut roles: HashMap<String, RoleDef> = HashMap::new();
    let mut model_roles: HashMap<(String, String), bool> = HashMap::new();
    let mut model_methods: HashMap<(String, String), FnDef> = HashMap::new();

    for stmt in stmts {
        match stmt {
            Stmt::FnDef(fn_def) => {
                fns.insert(fn_def.name.clone(), fn_def.clone());
            }
            Stmt::ModelDef(model_def) => {
                models.insert(model_def.name.clone(), model_def.clone());
            }
            Stmt::RoleDef(role_def) => {
                roles.insert(role_def.name.clone(), role_def.clone());
            }
            Stmt::ExtendModel { model_name, role_name, methods } => {
                // Validate model exists
                if !models.contains_key(model_name) {
                    panic!("Cannot extend unknown model '{}'", model_name);
                }
                
                // If role specified, validate it exists
                if let Some(role) = role_name {
                    if !roles.contains_key(role) {
                        panic!("Cannot extend with unknown role '{}'", role);
                    }
                    
                    // Validate role compliance
                    validate_role_compliance(
                        model_name, 
                        role, 
                        methods, 
                        &roles
                    );
                    
                    model_roles.insert(
                        (model_name.clone(), role.clone()), 
                        true
                    );
                }
                
                // Register methods
                for method in methods {
                    model_methods.insert(
                        (model_name.clone(), method.name.clone()),
                        method.clone(),
                    );
                }
            }
            Stmt::Assign { kind: AssignKind::Lit | AssignKind::Static, .. } => {
                execute_stmt(
                    stmt,
                    &mut env,
                    &mut fns,
                    &mut models,
                    &mut roles,
                    &mut model_methods,
                    None
                );
            }
            _ => {
                panic!("Top-level statements must be model/function definitions or lit/static assignments");
            }
        }
    }

    if let Some(main_fn) = fns.get("main").cloned() {
        if !main_fn.params.is_empty() {
            panic!("main() must have no parameters");
        }
        
        if let Some(body) = &main_fn.body {
            let mut main_env = env.clone();
            execute_block(
                body,
                &mut main_env,
                &mut fns,
                &mut models,
                &mut roles,
                &mut model_methods,
                main_fn.return_type.as_ref()
            );
        } else {
            panic!("The main() function must have a body.");
        }
        
    } else {
        panic!("No main() function found. Entrypoint files must define main()");
    }
}
