mod ast;
mod lexer;
mod parser;
mod expr_parser;
mod utils;

use parser::Parser;

use regex::Regex as RegexLib;

use std::fs;
use std::collections::HashSet;
use crate::ast::{
    ClosureValue, EnumDef, FnDef, InterpolationPart, MethodMutationType, ModelDef, Moved, Ownership, Pattern, RoleDef, Type
};

use async_recursion::async_recursion;
use tokio::sync::mpsc;
use std::sync::Arc;
use tokio::sync::Mutex;

#[tokio::main]
async fn main() {
    let source = fs::read_to_string("main.ngn")
        .expect("Failed to read ngn file");

    let tokens = lexer::tokenize(&source);

    // Preliminary parse to extract enum definitions
    let mut preliminary_parser = Parser::new(tokens.clone(), HashMap::new());
    let preliminary_ast = match preliminary_parser.parse_program() {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("Parse error: {}", e);
            return;
        }
    };
    
    // Extract enums from the AST
    let mut enums: HashMap<String, EnumDef> = HashMap::new();
    for stmt in &preliminary_ast {
        if let Stmt::EnumDef(enum_def) = stmt {
            enums.insert(enum_def.name.clone(), enum_def.clone());
        }
    }

    let mut parser = Parser::new(tokens, enums.clone());

    match parser.parse_program() {
        Ok(ast) => run(&ast).await,
        Err(e) => eprintln!("Parse error: {}", e),
    }
}

use std::collections::HashMap;
use ast::{Expr, Stmt};

use crate::ast::{AssignKind, ControlFlow, MatchType, Value};

fn format_type_for_error(t: &Type) -> String {
    match t {
        Type::I64 => "i64".to_string(),
        Type::I32 => "i32".to_string(),
        Type::U64 => "u64".to_string(),
        Type::U32 => "u32".to_string(),
        Type::F64 => "f64".to_string(),
        Type::F32 => "f32".to_string(),
        Type::String => "string".to_string(),
        Type::Str => "string".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Array(inner) => format!("array<{}>", format_type_for_error(inner)),
        Type::Object(map) => {
            let fields = map.iter()
                .map(|(k, v)| format!("{}: {}", k, format_type_for_error(v)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{ {} }}", fields)
        }
        Type::Function => "function".to_string(),
        Type::Void => "void".to_string(),
        Type::Model(name) => name.clone(),
        Type::Regex => "regex".to_string(),
        Type::Generic(name) => name.clone(),
        Type::Enum(name, type_args) => {
            // Format as EnumName<TypeArgs>
            if type_args.is_empty() {
                name.clone()
            } else {
                let args = type_args.iter()
                    .map(|t| format_type_for_error(t))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}<{}>", name, args)
            }
        }
        Type::Channel(inner) => format!("channel<{}>", format_type_for_error(inner)),
    }
}

fn format_value(v: &Value) -> String {
    match v {
        Value::I64(n) => n.to_string(),
        Value::I32(n) => n.to_string(),
        Value::U64(n) => n.to_string(),
        Value::U32(n) => n.to_string(),
        Value::F64(n) => n.to_string(),
        Value::F32(n) => n.to_string(),
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
        },
        Value::Regex(pattern) => format!("/{}/", pattern),
        Value::EnumValue(enum_name, variant, data) => {
            if let Some(val) = data {
                format!("{}::{} ({})", enum_name, variant, format_value(val))
            } else {
                format!("{}::{}", enum_name, variant)
            }
        },
        Value::Channel(_, _, _) => "<channel>".to_string(),
    }
}

fn method_mutation_type(body: &[Stmt]) -> MethodMutationType {
    let mut has_direct = false;
    let mut has_rebind = false;

    for stmt in body {
        match stmt {
            // Direct field assignment: this.field = value
            Stmt::ExprStmt(Expr::FieldAccess { object, value, .. }) => {
                if let Expr::Var(name) = object.as_ref() {
                    if name == "this" && value.is_some() {
                        has_direct = true;
                    }
                }
            }
            // Rebind on this
            Stmt::RebindField { object, .. } if object == "this" => {
                has_rebind = true;
            }
            // Check nested blocks
            Stmt::If { then_block, else_ifs, else_block, .. } => {
                let mut_type = method_mutation_type(then_block);
                if mut_type != MethodMutationType::None {
                    match mut_type {
                        MethodMutationType::DirectAssignment => has_direct = true,
                        MethodMutationType::Rebind => has_rebind = true,
                        MethodMutationType::Mixed => return MethodMutationType::Mixed,
                        MethodMutationType::None => {}
                    }
                }
                
                for (_, block) in else_ifs {
                    let mut_type = method_mutation_type(block);
                    if mut_type != MethodMutationType::None {
                        match mut_type {
                            MethodMutationType::DirectAssignment => has_direct = true,
                            MethodMutationType::Rebind => has_rebind = true,
                            MethodMutationType::Mixed => return MethodMutationType::Mixed,
                            MethodMutationType::None => {}
                        }
                    }
                }
                
                if let Some(block) = else_block {
                    let mut_type = method_mutation_type(block);
                    if mut_type != MethodMutationType::None {
                        match mut_type {
                            MethodMutationType::DirectAssignment => has_direct = true,
                            MethodMutationType::Rebind => has_rebind = true,
                            MethodMutationType::Mixed => return MethodMutationType::Mixed,
                            MethodMutationType::None => {}
                        }
                    }
                }
            }
            Stmt::While { body, .. } | Stmt::WhileOnce { body, .. } |
            Stmt::Until { body, .. } | Stmt::UntilOnce { body, .. } => {
                let mut_type = method_mutation_type(body);
                if mut_type != MethodMutationType::None {
                    match mut_type {
                        MethodMutationType::DirectAssignment => has_direct = true,
                        MethodMutationType::Rebind => has_rebind = true,
                        MethodMutationType::Mixed => return MethodMutationType::Mixed,
                        MethodMutationType::None => {}
                    }
                }
            }
            Stmt::Match { cases, default, .. } => {
                for (_, block) in cases {
                    let mut_type = method_mutation_type(block);
                    if mut_type != MethodMutationType::None {
                        match mut_type {
                            MethodMutationType::DirectAssignment => has_direct = true,
                            MethodMutationType::Rebind => has_rebind = true,
                            MethodMutationType::Mixed => return MethodMutationType::Mixed,
                            MethodMutationType::None => {}
                        }
                    }
                }
                if let Some(block) = default {
                    let mut_type = method_mutation_type(block);
                    if mut_type != MethodMutationType::None {
                        match mut_type {
                            MethodMutationType::DirectAssignment => has_direct = true,
                            MethodMutationType::Rebind => has_rebind = true,
                            MethodMutationType::Mixed => return MethodMutationType::Mixed,
                            MethodMutationType::None => {}
                        }
                    }
                }
            }
            _ => {}
        }
    }
    
    match (has_direct, has_rebind) {
        (false, false) => MethodMutationType::None,
        (true, false) => MethodMutationType::DirectAssignment,
        (false, true) => MethodMutationType::Rebind,
        (true, true) => MethodMutationType::Mixed,
    }
}

fn can_mutate(var_name: &str, env: &HashMap<String, (AssignKind, Value, Ownership, Moved)>) -> bool {
    if let Some((kind, _, ownership, _)) = env.get(var_name) {
        // Only Var with Owned can mutate
        matches!(kind, AssignKind::Var) && matches!(ownership, Ownership::Owned)
    } else {
        false
    }
}

fn is_truthy(v: &Value) -> bool {
    match v {
        Value::Bool(b) => *b,
        Value::I64(n) => *n != 0,
        Value::I32(n) => *n != 0,
        Value::U64(n) => *n != 0,
        Value::U32(n) => *n != 0,
        Value::F64(n) => *n != 0.0,
        Value::F32(n) => *n != 0.0,
        Value::String(s) => !s.is_empty(),
        Value::Array(arr) => !arr.is_empty(),
        Value::Function(_) => true,
        Value::Void => false,
        Value::Object(_, _) => true,
        Value::Closure(_) => true,
        Value::Regex(_) => true,
        Value::EnumValue(_, _, _) => true,
        Value::Channel(_, _, _) => true,
    }
}

fn is_literal(e: &Expr) -> bool {
    matches!(e, 
        Expr::I64(_) |
        Expr::I32(_) |
        Expr::U64(_) |
        Expr::U32(_) |
        Expr::F64(_) |
        Expr::F32(_) |
        Expr::String(_) |
        Expr::Bool(_) |
        Expr::Array(_)
    )
}

fn is_numeric_type(t: &Type) -> bool {
    matches!(t, Type::I64 | Type::I32 | Type::U64 | Type::U32 | Type::F64 | Type::F32)
}

fn to_usize(v: &Value) -> Result<usize, String> {
    match v {
        Value::I64(n) => Ok(*n as usize),
        Value::I32(n) => Ok(*n as usize),
        Value::U64(n) => Ok(*n as usize),
        Value::U32(n) => Ok(*n as usize),
        _ => Err("Index must be an integer".to_string()),
    }
}

fn normalize_regex_pattern(pattern: &str) -> String {
    // Remove 'g' flag (Rust regex doesn't support it)
    let mut normal = pattern.replace("g", "");
    
    // Remove empty flag groups like (?)
    normal = normal.replace("(?)", "");
    
    normal
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::I64(x), Value::I64(y)) => x == y,
        (Value::I32(x), Value::I32(y)) => x == y,
        (Value::U64(x), Value::U64(y)) => x == y,
        (Value::U32(x), Value::U32(y)) => x == y,
        (Value::F64(x), Value::F64(y)) => x == y,
        (Value::F32(x), Value::F32(y)) => x == y,
        (Value::String(x), Value::String(y)) => x == y,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::Array(x), Value::Array(y)) => x == y,
        // Todo - models?
        _ => false,
    }
}

fn infer_value_type(v: &Value) -> Type {
    match v {
        Value::I64(_) => Type::I64,
        Value::I32(_) => Type::I32,
        Value::U64(_) => Type::U64,
        Value::U32(_) => Type::U32,
        Value::F64(_) => Type::F64,
        Value::F32(_) => Type::F32,
        Value::String(_) => Type::Str,
        Value::Bool(_) => Type::Bool,
        Value::Array(arr) => {
            if arr.is_empty() {
                Type::Array(Box::new(Type::I64))
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
        Value::Regex(_) => Type::Regex,
        Value::EnumValue(enum_name, variant, data) => {
            match enum_name.as_str() {
                "Result" => {
                    if let Some(val) = data {
                        let data_type = infer_value_type(val);
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
                    if let Some(val) = data {
                        let data_type = infer_value_type(val);
                        Type::Enum("Maybe".to_string(), vec![data_type])
                    } else {
                        Type::Enum("Maybe".to_string(), vec![])
                    }
                }
                _ => {
                    if let Some(val) = data {
                        let data_type = infer_value_type(val);
                        Type::Enum(enum_name.clone(), vec![data_type])
                    } else {
                        Type::Enum(enum_name.clone(), vec![])
                    }
                }
            }
        },
        Value::Channel(_, _, inner_type) => {
            // Runtime channels are type-erased (they hold `Value`), 
            // so we can't know the strict inner type without storing extra metadata.
            // For now, return a generic channel.
            Type::Channel(Box::new(inner_type.clone()))
        }
    }
}

fn infer_expr_type(
    e: &Expr,
    env: &HashMap<String, (AssignKind, Value, Ownership, Moved)>,
    fns: &HashMap<String, FnDef>,
    models: &HashMap<String, ModelDef>,
    model_methods: &mut HashMap<(String, String), FnDef>,
    enums: &HashMap<String, EnumDef>,
) -> Type {
    match e {
        Expr::I64(_) => Type::I64,
        Expr::I32(_) => Type::I32,
        Expr::U64(_) => Type::U64,
        Expr::U32(_) => Type::U32,
        Expr::F64(_) => Type::F64,
        Expr::F32(_) => Type::F32,
        Expr::String(_) => Type::Str,
        Expr::InterpolatedString(_) => Type::String,
        Expr::Bool(_) => Type::Bool,
        Expr::Array(exprs) => {
            if exprs.is_empty() {
                Type::Array(Box::new(Type::I64))
            } else {
                let elem_type = infer_expr_type(&exprs[0], env, fns, models, model_methods, enums);
                for expr in exprs.iter().skip(1) {
                    let other_type = infer_expr_type(expr, env, fns, models, model_methods, enums);
                    if !types_compatible(&elem_type, &other_type) {
                        panic!("Array type error: mixed types");
                    }
                }
                Type::Array(Box::new(elem_type))
            }
        }
        Expr::Add(a, b) => {
            let left = infer_expr_type(a, env, fns, models, model_methods, enums);
            let right = infer_expr_type(b, env, fns, models, model_methods, enums);
            if is_numeric_type(&left) && is_numeric_type(&right) {
                // Both numeric—return left type (they must match due to strict typing)
                if left == right {
                    left
                } else {
                    panic!("Type error: cannot add {:?} and {:?}", left, right);
                }
            } else if matches!(left, Type::String) && matches!(right, Type::String) {
                Type::String
            } else {
                panic!("Type error: cannot add {:?} and {:?}", left, right);
            }
        }
        Expr::Subtract(a, b) | Expr::Multiply(a, b) | Expr::Divide(a, b) | Expr::Modulo(a, b) => {
            let left = infer_expr_type(a, env, fns, models, model_methods, enums);
            let right = infer_expr_type(b, env, fns, models, model_methods, enums);
            
            if !is_numeric_type(&left) || !is_numeric_type(&right) || left != right {
                panic!("Type error: arithmetic requires matching numeric types");
            }
            left
        }
        Expr::Power(a, b) => {
            let left = infer_expr_type(a, env, fns, models, model_methods, enums);
            let right = infer_expr_type(b, env, fns, models, model_methods, enums);
            
            if !is_numeric_type(&left) || !is_numeric_type(&right) || left != right {
                panic!("Type error: power requires matching numeric types");
            }
            left
        }

        Expr::Negative(e) => {
            let t = infer_expr_type(e, env, fns, models, model_methods, enums);
            if !is_numeric_type(&t) {
                panic!("Type error: cannot negate non-number");
            }
            t
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
        Expr::Thread(_) => Type::Void, // must be before Expr::Call
        Expr::Call { name, args } => {
            if name == "close" {
                if args.len() != 1 {
                    panic!("Type Error: close() takes exactly 1 argument");
                }
                let arg_type = infer_expr_type(&args[0], env, fns, models, model_methods, enums);
                if !matches!(arg_type, Type::Channel(_)) {
                    panic!("Type Error: close() expects a channel, got {:?}", arg_type);
                }
                return Type::Void;
            }

            // Check if it's a function value in the environment
            if let Some((_, Value::Function(fn_def), _ownership, _moved)) = env.get(name) {
                fn_def.return_type.clone().unwrap_or(Type::Void)
            } else if let Some((_, Value::Closure(closure), _ownership, _moved)) = env.get(name) {
                // Closure return type
                closure.def.return_type.clone().unwrap_or(Type::Void)
            } else if let Some(fn_def) = fns.get(name) {
                // Check if it's a global function
                fn_def.return_type.clone().unwrap_or(Type::Void)
            } else {
                panic!("Unknown function: {}", name);
            }
        }
        Expr::Assign { value, .. } => infer_expr_type(value, env, fns, models, model_methods, enums),
        Expr::CompoundAssign { value, .. } => infer_expr_type(value, env, fns, models, model_methods, enums),
        Expr::Regex(_) => Type::Regex,
        Expr::ModelInstance { name, .. } => Type::Model(name.clone()),
        Expr::FieldAccess { object, field, value: _ } => {
            let obj_type = infer_expr_type(object, env, fns, models, model_methods, enums);
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
        Expr::MethodCall { object, method, args: _ } => {
            // Check for static method on model
            if let Expr::Var(model_name) = &**object {
                if models.contains_key(model_name) {
                    if let Some(method_def) = model_methods.get(&(model_name.clone(), method.clone())) {
                        return method_def.return_type.clone().unwrap_or(Type::Void);
                    }
                }
            }

            let obj_type = infer_expr_type(object, env, fns, models, model_methods, enums);
            match obj_type {
                Type::I64 | Type::I32 | Type::U64 | Type::U32 | Type::F64 | Type::F32 => {
                    match method.as_str() {
                        "abs" => obj_type.clone(),  // Returns same type as input
                        "round" | "floor" | "ceil" => Type::I64,  // These convert to integer
                        _ => panic!("Unknown number method: {}", method),
                    }
                }
                Type::Array(inner_type) => {
                    match method.as_str() {
                        "push" | "size" | "splice" => Type::I64,
                        "pull" => *inner_type,
                        "slice" | "copy" => Type::Array(inner_type),
                        _ => panic!("Unknown array method: {}", method),
                    }
                }
                Type::Str | Type::String => {
                    match method.as_str() {
                        "includes" | "starts" | "ends" => Type::Bool,
                        "replace" | "slice" | "copy" | "upper" | "lower" | "trim" | "repeat" => Type::String,
                        "split" => Type::Array(Box::new(Type::String)),
                        "index" | "length" => Type::I64,
                        _ => panic!("Unknown string method: {}", method),
                    }
                }
                Type::Model(_model_name) => {
                    // For now, assume method returns what we'll determine at runtime
                    // In a full implementation, you'd look up the method signature
                    Type::Void  // TODO: look up actual return type from implementations
                }
                _ => panic!("Cannot call method on type {:?}", obj_type),
            }
        }
        Expr::Closure(_) => Type::Function,
        Expr::EnumVariant { enum_name, variant, data } => {
            match enum_name.as_str() {
                "Result" => {
                    if let Some(expr) = data {
                        let data_type = infer_expr_type(expr, env, fns, models, model_methods, enums);
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
                        let data_type = infer_expr_type(expr, env, fns, models, model_methods, enums);
                        Type::Enum("Maybe".to_string(), vec![data_type])
                    } else {
                        Type::Enum("Maybe".to_string(), vec![])
                    }
                }
                _ => {
                    if let Some(expr) = data {
                        let data_type = infer_expr_type(expr, env, fns, models, model_methods, enums);
                        Type::Enum(enum_name.clone(), vec![data_type])
                    } else {
                        Type::Enum(enum_name.clone(), vec![])
                    }
                }
            }
        },

        Expr::MakeChannel(hint) => {
            if let Some(t) = hint {
                Type::Channel(Box::new(t.clone()))
            } else {
                panic!("Type Error: channel() requires a type annotation, e.g., channel(): string");
            }
        }

        Expr::Send(chan_expr, val_expr) => {
            let chan_type = infer_expr_type(chan_expr, env, fns, models, model_methods, enums);
            let val_type = infer_expr_type(val_expr, env, fns, models, model_methods, enums);

            match chan_type {
                Type::Channel(inner) => {
                    if !types_compatible(&inner, &val_type) {
                        panic!("Type Error: Cannot send {:?} to channel of type {:?}", val_type, inner);
                    }
                },
                _ => panic!("Type Error: Left side of <- must be a channel"),
            }
            Type::Void
        }

        Expr::Receive(chan_expr) => {
            let chan_type = infer_expr_type(chan_expr, env, fns, models, model_methods, enums);
            match chan_type {
                Type::Channel(inner) => *inner,
                _ => panic!("Type Error: Right side of <- must be a channel"),
            }
        }

        Expr::MaybeReceive(chan_expr) => {
            let chan_type = infer_expr_type(chan_expr, env, fns, models, model_methods, enums);
            if let Type::Channel(inner) = chan_type {
                // Wrap inner type in Maybe
                Type::Enum("Maybe".to_string(), vec![*inner])
            } else {
                panic!("Type Error: Right side of <-? must be a channel");
            }
        }
    }
}

fn types_compatible(expected: &Type, actual: &Type) -> bool {
    if let Type::Generic(name) = actual {
        if name == "_" { return true; }
    }
    if let Type::Generic(name) = expected {
        if name == "_" { return true; }
    }
    match (expected, actual) {
        (Type::I64, Type::I64) => true,
        (Type::I32, Type::I32) => true,
        (Type::U64, Type::U64) => true,
        (Type::U32, Type::U32) => true,
        (Type::F64, Type::F64) => true,
        (Type::F32, Type::F32) => true,
        (Type::Str, Type::Str) => true,
        (Type::String, Type::String) => true,
        (Type::Bool, Type::Bool) => true,
        (Type::Array(e1), Type::Array(e2)) => types_compatible(e1, e2),
        (Type::Channel(t1), Type::Channel(t2)) => types_compatible(t1, t2),
        (Type::Function, Type::Function) => true,
        (Type::Void, Type::Void) => true,
        (Type::Model(a), Type::Model(b)) => a == b,
        (Type::Regex, Type::Regex) => true,
        (Type::Enum(name1, args1), Type::Enum(name2, args2)) => {
            // Both must be the same enum
            if name1 != name2 {
                return false;
            }

            // For built-in enums, allow empty args to match anything
            if (name1 == "Maybe" || name1 == "Result") && args2.is_empty() {
                return true;
            }

            // Check all type arguments match
            if args1.len() != args2.len() {
                return false;
            }

            // Check all type arguments match
            args1.iter().zip(args2.iter()).all(|(a, b)| types_compatible(a, b))
        }
        _ => false,
    }
}

fn get_arg_ownership(e: &Expr, env: &HashMap<String, (AssignKind, Value, Ownership, Moved)>) -> Ownership {
    match e {
        Expr::Var(name) => {
            env.get(name).map(|(_, _, o, _)| o.clone()).unwrap_or(Ownership::Borrowed)
        }
        Expr::String(_) => Ownership::Borrowed,
        Expr::I64(_) | Expr::I32(_) | Expr::U64(_) | Expr::U32(_) | Expr::F64(_) | Expr::F32(_) => Ownership::Borrowed,
        Expr::Bool(_) => Ownership::Borrowed,
        Expr::Add(_, _) => Ownership::Owned,  // String concat returns owned
        _ => Ownership::Borrowed,
    }
}

fn is_copy_type(t: &Type) -> bool {
    matches!(t, Type::Bool) || is_numeric_type(t)
}

fn match_pattern(
    val: &Value,
    pattern: &Pattern,
    env: &HashMap<String, (AssignKind, Value, Ownership, Moved)>,
) -> Option<HashMap<String, (AssignKind, Value, Ownership, Moved)>> {
    let mut new_env = env.clone();
    
    match pattern {
        Pattern::Literal(expr) => {
            // Evaluate the pattern expression and compare
            match expr {
                Expr::I64(n) => {
                    if let Value::I64(val_n) = val {
                        if val_n == n {
                            return Some(new_env);
                        }
                    }
                }
                Expr::I32(n) => {
                    if let Value::I32(val_n) = val {
                        if val_n == n {
                            return Some(new_env);
                        }
                    }
                }
                Expr::U64(n) => {
                    if let Value::U64(val_n) = val {
                        if val_n == n {
                            return Some(new_env);
                        }
                    }
                }
                Expr::U32(n) => {
                    if let Value::U32(val_n) = val {
                        if val_n == n {
                            return Some(new_env);
                        }
                    }
                }
                Expr::F64(n) => {
                    if let Value::F64(val_n) = val {
                        if (val_n - n).abs() < f64::EPSILON {
                            return Some(new_env);
                        }
                    }
                }
                Expr::F32(n) => {
                    if let Value::F32(val_n) = val {
                        if (val_n - n).abs() < f32::EPSILON {
                            return Some(new_env);
                        }
                    }
                }
                Expr::String(s) => {
                    if let Value::String(val_s) = val {
                        if val_s == s {
                            return Some(new_env);
                        }
                    }
                }
                Expr::Bool(b) => {
                    if let Value::Bool(val_b) = val {
                        if val_b == b {
                            return Some(new_env);
                        }
                    }
                }
                _ => {}
            }
            None
        }
        Pattern::EnumVariant { enum_name, variant, binding } => {
            if let Value::EnumValue(val_enum, val_variant, data) = val {
                if val_enum == enum_name && val_variant == variant {
                    // Pattern matches!
                    if let Some(binding_name) = binding {
                        // Bind the data if present
                        if let Some(data_val) = data {
                            new_env.insert(
                                binding_name.clone(),
                                (AssignKind::Var, (**data_val).clone(), Ownership::Borrowed, Moved::False),
                            );
                        }
                    }
                    return Some(new_env);
                }
            }
            None
        }
        Pattern::Wildcard => {
            // Always matches
            Some(new_env)
        }
    }
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

#[async_recursion]
async fn call_closure(
    closure: &ClosureValue,
    args: &[Expr],
    outer_env: &mut HashMap<String, (AssignKind, Value, Ownership, Moved)>,
    fns: &mut HashMap<String, FnDef>,
    models: &mut HashMap<String, ModelDef>,
    roles: &mut HashMap<String, RoleDef>,
    model_methods: &mut HashMap<(String, String), FnDef>,
    enums: &HashMap<String, EnumDef>,
) -> Value {
    // Create new scope starting with captured environment
    let mut closure_env = closure.captured_env.clone();

    // For vars, get current values from outer_env
    // (in case they've been updated since closure creation)
    for var_name in &closure.live_vars {
        if let Some(current_value) = outer_env.get(var_name) {
            closure_env.insert(var_name.clone(), current_value.clone());
        }
    }
    
    // Bind parameters
    for (i, (param_name, param_type)) in closure.def.params.iter().enumerate() {
        if i < args.len() {
            let arg_val = eval_expr(&args[i], outer_env, fns, models, roles, model_methods, enums).await;

            // Type check if parameter has a type annotation
            if let Some(expected_type) = param_type {
                let actual_type = infer_value_type(&arg_val);
                if !types_compatible(expected_type, &actual_type) {
                    panic!(
                        "Closure param '{}' expects {}, got {}",
                        param_name,
                        format_type_for_error(expected_type),
                        format_type_for_error(&actual_type)
                    );
                }
            }

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
        enums,
        closure.def.return_type.as_ref(),
    ).await;

    // Sync all vars back (they're live references now)
    for rebound_var in &closure.live_vars {
        if let Some(value) = closure_env.get(rebound_var) {
            outer_env.insert(rebound_var.clone(), value.clone());
        }
    }
    
    match flow {
        ControlFlow::Return(val) => {
            // Type check return value if return type is specified
            if let Some(expected_return_type) = &closure.def.return_type {
                let actual_type = infer_value_type(&val);
                if !types_compatible(expected_return_type, &actual_type) {
                    panic!(
                        "Closure return type mismatch: expected {}, got {}",
                        format_type_for_error(expected_return_type),
                        format_type_for_error(&actual_type)
                    );
                }
            }
            val
        },
        _ => Value::Void,
    }
}

fn find_rebound_vars(stmts: &[Stmt]) -> Vec<String> {
    let mut rebound = Vec::new();
    
    for stmt in stmts {
        match stmt {
            Stmt::Rebind { name, .. } => {
                if !rebound.contains(name) {
                    rebound.push(name.clone());
                }
            }
            Stmt::RebindField { object, .. } => {
                if !rebound.contains(object) {
                    rebound.push(object.clone());
                }
            }
            Stmt::Echo(e) | Stmt::Print(e) => {
                collect_vars_from_expr(e, &mut rebound);
            }
            Stmt::If { then_block, else_ifs, else_block, .. } => {
                rebound.extend(find_rebound_vars(then_block));
                for (_, block) in else_ifs {
                    rebound.extend(find_rebound_vars(block));
                }
                if let Some(block) = else_block {
                    rebound.extend(find_rebound_vars(block));
                }
            }
            Stmt::While { body, .. } | Stmt::WhileOnce { body, .. } |
            Stmt::Until { body, .. } | Stmt::UntilOnce { body, .. } => {
                rebound.extend(find_rebound_vars(body));
            }
            Stmt::Match { cases, default, .. } => {
                for (_, block) in cases {
                    rebound.extend(find_rebound_vars(block));
                }
                if let Some(block) = default {
                    rebound.extend(find_rebound_vars(block));
                }
            }
            _ => {}
        }
    }
    
    rebound
}

fn find_referenced_vars(stmts: &[Stmt]) -> Vec<String> {
    let mut vars = Vec::new();
    
    for stmt in stmts {
        match stmt {
            Stmt::Echo(e) | Stmt::Print(e) | Stmt::ExprStmt(e) => {
                collect_vars_from_expr(e, &mut vars);
            }
            Stmt::Return(Some(e)) => {
                collect_vars_from_expr(e, &mut vars);
            }
            Stmt::Assign { value, .. } | Stmt::Reassign { value, .. } | Stmt::Rebind { value, .. } => {
                collect_vars_from_expr(value, &mut vars);
            }
            Stmt::If { condition, then_block, else_ifs, else_block, .. } => {
                collect_vars_from_expr(condition, &mut vars);
                vars.extend(find_referenced_vars(then_block));
                for (cond, block) in else_ifs {
                    collect_vars_from_expr(cond, &mut vars);
                    vars.extend(find_referenced_vars(block));
                }
                if let Some(block) = else_block {
                    vars.extend(find_referenced_vars(block));
                }
            }
            Stmt::While { condition, body, .. } | Stmt::WhileOnce { condition, body, .. } |
            Stmt::Until { condition, body, .. } | Stmt::UntilOnce { condition, body, .. } => {
                collect_vars_from_expr(condition, &mut vars);
                vars.extend(find_referenced_vars(body));
            }
            Stmt::Match { expr, cases, default, .. } => {
                collect_vars_from_expr(expr, &mut vars);
                for (_, block) in cases {
                    vars.extend(find_referenced_vars(block));
                }
                if let Some(block) = default {
                    vars.extend(find_referenced_vars(block));
                }
            }
            _ => {}
        }
    }
    
    vars
}

fn collect_vars_from_expr(e: &Expr, vars: &mut Vec<String>) {
    match e {
        Expr::Var(name) | Expr::Const(name) | Expr::Lit(name) | Expr::Static(name) => {
            if !vars.contains(name) {
                vars.push(name.clone());
            }
        }
        Expr::Add(a, b) | Expr::Subtract(a, b) | Expr::Multiply(a, b) | 
        Expr::Divide(a, b) | Expr::Modulo(a, b) | Expr::Power(a, b) |
        Expr::Equal(a, b) | Expr::NotEqual(a, b) | Expr::LessThan(a, b) |
        Expr::LessThanOrEqual(a, b) | Expr::GreaterThan(a, b) | Expr::GreaterThanOrEqual(a, b) => {
            collect_vars_from_expr(a, vars);
            collect_vars_from_expr(b, vars);
        }
        Expr::Not(e) | Expr::Negative(e) => collect_vars_from_expr(e, vars),
        Expr::Array(exprs) => {
            for expr in exprs {
                collect_vars_from_expr(expr, vars);
            }
        }
        Expr::Call { args, .. } => {
            for arg in args {
                collect_vars_from_expr(arg, vars);
            }
        }
        Expr::MethodCall { object, args, .. } => {
            collect_vars_from_expr(object, vars);
            for arg in args {
                collect_vars_from_expr(arg, vars);
            }
        }
        Expr::FieldAccess { object, value: opt_val, .. } => {
            collect_vars_from_expr(object, vars);
            if let Some(val) = opt_val {
                collect_vars_from_expr(val, vars);
            }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let InterpolationPart::Expression(expr) = part {
                    collect_vars_from_expr(expr, vars);
                }
            }
        }
        Expr::Send(target, value) => {
            collect_vars_from_expr(target, vars);
            collect_vars_from_expr(value, vars);
        }
        Expr::Receive(target) => {
            collect_vars_from_expr(target, vars);
        }
        Expr::MaybeReceive(target) => {
            collect_vars_from_expr(target, vars);
        }
        Expr::Closure(def) => {
            for stmt in &def.body {
                // Recursively find vars in nested closures
                match stmt {
                    Stmt::ExprStmt(e) | Stmt::Echo(e) | Stmt::Print(e) => collect_vars_from_expr(e, vars),
                    _ => {}
                }
            }
        }
        _ => {}
    }
}

#[async_recursion]
async fn eval_expr(
    e: &Expr,
    env: &mut HashMap<String, (AssignKind, Value, Ownership, Moved)>,
    fns: &mut HashMap<String, FnDef>,
    models: &mut HashMap<String, ModelDef>,
    roles: &mut HashMap<String, RoleDef>,
    model_methods: &mut HashMap<(String, String), FnDef>,
    enums: &HashMap<String, EnumDef>,
) -> Value {
    match e {
        Expr::I64(n) => Value::I64(*n),
        Expr::I32(n) => Value::I32(*n),
        Expr::U64(n) => Value::U64(*n),
        Expr::U32(n) => Value::U32(*n),
        Expr::F64(n) => Value::F64(*n),
        Expr::F32(n) => Value::F32(*n),
        Expr::InterpolatedString(parts) => {
            let mut result = String::new();
            for part in parts {
                match part {
                    InterpolationPart::Literal(s) => result.push_str(s),
                    InterpolationPart::Expression(expr) => {
                        let val = eval_expr(expr, env, fns, models, roles, model_methods, enums).await;
                        result.push_str(&format_value(&val));
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
                values.push(eval_expr(e, env, fns, models, roles, model_methods, enums).await);
            }
            Value::Array(values)
        },
        Expr::Not(e) => {
            let val = eval_expr(e, env, fns, models, roles, model_methods, enums).await;
            Value::Bool(!is_truthy(&val))
        },
        Expr::Add(a, b) => {
            match (eval_expr(a, env, fns, models, roles, model_methods, enums).await, eval_expr(b, env, fns, models, roles, model_methods, enums).await) {
                (Value::I64(x), Value::I64(y)) => Value::I64(x + y),
                (Value::I32(x), Value::I32(y)) => Value::I32(x + y),
                (Value::U64(x), Value::U64(y)) => Value::U64(x + y),
                (Value::U32(x), Value::U32(y)) => Value::U32(x + y),
                (Value::F64(x), Value::F64(y)) => Value::F64(x + y),
                (Value::F32(x), Value::F32(y)) => Value::F32(x + y),
                (Value::String(x), Value::String(y)) => Value::String(format!("{}{}", x, y)),
                _ => panic!("Type error: cannot add these types"),
            }
        }
        Expr::Subtract(a, b) => {
            match (eval_expr(a, env, fns, models, roles, model_methods, enums).await, eval_expr(b, env, fns, models, roles, model_methods, enums).await) {
                (Value::I64(x), Value::I64(y)) => Value::I64(x - y),
                (Value::I32(x), Value::I32(y)) => Value::I32(x - y),
                (Value::U64(x), Value::U64(y)) => Value::U64(x - y),
                (Value::U32(x), Value::U32(y)) => Value::U32(x - y),
                (Value::F64(x), Value::F64(y)) => Value::F64(x - y),
                (Value::F32(x), Value::F32(y)) => Value::F32(x - y),
                _ => panic!("Type error: cannot subtract non-numbers"),
            }
        }
        Expr::Multiply(a, b) => {
            match (eval_expr(a, env, fns, models, roles, model_methods, enums).await, eval_expr(b, env, fns, models, roles, model_methods, enums).await) {
                (Value::I64(x), Value::I64(y)) => Value::I64(x * y),
                (Value::I32(x), Value::I32(y)) => Value::I32(x * y),
                (Value::U64(x), Value::U64(y)) => Value::U64(x * y),
                (Value::U32(x), Value::U32(y)) => Value::U32(x * y),
                (Value::F64(x), Value::F64(y)) => Value::F64(x * y),
                (Value::F32(x), Value::F32(y)) => Value::F32(x * y),
                _ => panic!("Type error: cannot multiply non-numbers"),
            }
        }
        Expr::Divide(a, b) => {
            match (eval_expr(a, env, fns, models, roles, model_methods, enums).await, eval_expr(b, env, fns, models, roles, model_methods, enums).await) {
                (Value::I64(x), Value::I64(y)) => Value::I64(x / y),
                (Value::I32(x), Value::I32(y)) => Value::I32(x / y),
                (Value::U64(x), Value::U64(y)) => Value::U64(x / y),
                (Value::U32(x), Value::U32(y)) => Value::U32(x / y),
                (Value::F64(x), Value::F64(y)) => Value::F64(x / y),
                (Value::F32(x), Value::F32(y)) => Value::F32(x / y),
                _ => panic!("Type error: cannot divide non-numbers"),
            }
        }
        Expr::Negative(e) => {
            match eval_expr(e, env, fns, models, roles, model_methods, enums).await {
                Value::I64(x) => Value::I64(-x),
                Value::I32(x) => Value::I32(-x),
                Value::F64(x) => Value::F64(-x),
                Value::F32(x) => Value::F32(-x),
                Value::U64(_) | Value::U32(_) => {
                    panic!("Type error: cannot negate unsigned integer")
                },
                _ => panic!("Type error: cannot negate non-number"),
            }
        }
        Expr::Power(a, b) => {
            match (eval_expr(a, env, fns, models, roles, model_methods, enums).await, eval_expr(b, env, fns, models, roles, model_methods, enums).await) {
                (Value::I64(x), Value::I64(y)) => {
                    if y < 0 || y > u32::MAX as i64 {
                        panic!("Exponent out of range for i64: {}", y);
                    }
                    Value::I64(x.pow(y as u32))
                }
                (Value::I32(x), Value::I32(y)) => {
                    if y < 0 {
                        panic!("Exponent cannot be negative for i32: {}", y);
                    }
                    Value::I32(x.pow(y as u32))
                }
                (Value::U64(x), Value::U64(y)) => {
                    if y > u32::MAX as u64 {
                        panic!("Exponent out of range for u64: {}", y);
                    }
                    Value::U64(x.pow(y as u32))
                }
                (Value::U32(x), Value::U32(y)) => Value::U32(x.pow(y as u32)),
                (Value::F64(x), Value::F64(y)) => Value::F64(x.powf(y)),
                (Value::F32(x), Value::F32(y)) => Value::F32(x.powf(y)),
                _ => panic!("Type error: cannot exponentiate non-numbers"),
            }
        }
        Expr::Modulo(a, b) => {
            match (eval_expr(a, env, fns, models, roles, model_methods, enums).await, eval_expr(b, env, fns, models, roles, model_methods, enums).await) {
                (Value::I64(x), Value::I64(y)) => Value::I64(x % y),
                (Value::I32(x), Value::I32(y)) => Value::I32(x % y),
                (Value::U64(x), Value::U64(y)) => Value::U64(x % y),
                (Value::U32(x), Value::U32(y)) => Value::U32(x % y),
                (Value::F64(x), Value::F64(y)) => Value::F64(x % y),
                (Value::F32(x), Value::F32(y)) => Value::F32(x % y),
                _ => panic!("Type error: cannot modulo non-numbers"),
            }
        }
        Expr::Equal(a, b) => {
            let av = eval_expr(a, env, fns, models, roles, model_methods, enums).await;
            let bv = eval_expr(b, env, fns, models, roles, model_methods, enums).await;
            Value::Bool(values_equal(&av, &bv))
        }
        Expr::NotEqual(a, b) => {
            let av = eval_expr(a, env, fns, models, roles, model_methods, enums).await;
            let bv = eval_expr(b, env, fns, models, roles, model_methods, enums).await;
            Value::Bool(!values_equal(&av, &bv))
        }
        Expr::LessThan(a, b) => {
            match (eval_expr(a, env, fns, models, roles, model_methods, enums).await, eval_expr(b, env, fns, models, roles, model_methods, enums).await) {
                (Value::I64(x), Value::I64(y)) => Value::Bool(x < y),
                (Value::I32(x), Value::I32(y)) => Value::Bool(x < y),
                (Value::U64(x), Value::U64(y)) => Value::Bool(x < y),
                (Value::U32(x), Value::U32(y)) => Value::Bool(x < y),
                (Value::F64(x), Value::F64(y)) => Value::Bool(x < y),
                (Value::F32(x), Value::F32(y)) => Value::Bool(x < y),
                _ => panic!("Type error: cannot compare non-numbers"),
            }
        }
        Expr::LessThanOrEqual(a, b) => {
            match (eval_expr(a, env, fns, models, roles, model_methods, enums).await, eval_expr(b, env, fns, models, roles, model_methods, enums).await) {
                (Value::I64(x), Value::I64(y)) => Value::Bool(x <= y),
                (Value::I32(x), Value::I32(y)) => Value::Bool(x <= y),
                (Value::U64(x), Value::U64(y)) => Value::Bool(x <= y),
                (Value::U32(x), Value::U32(y)) => Value::Bool(x <= y),
                (Value::F64(x), Value::F64(y)) => Value::Bool(x <= y),
                (Value::F32(x), Value::F32(y)) => Value::Bool(x <= y),
                _ => panic!("Type error: cannot compare non-numbers"),
            }
        }
        Expr::GreaterThan(a, b) => {
            match (eval_expr(a, env, fns, models, roles, model_methods, enums).await, eval_expr(b, env, fns, models, roles, model_methods, enums).await) {
                (Value::I64(x), Value::I64(y)) => Value::Bool(x > y),
                (Value::I32(x), Value::I32(y)) => Value::Bool(x > y),
                (Value::U64(x), Value::U64(y)) => Value::Bool(x > y),
                (Value::U32(x), Value::U32(y)) => Value::Bool(x > y),
                (Value::F64(x), Value::F64(y)) => Value::Bool(x > y),
                (Value::F32(x), Value::F32(y)) => Value::Bool(x > y),
                _ => panic!("Type error: cannot compare non-numbers"),
            }
        }
        Expr::GreaterThanOrEqual(a, b) => {
            match (eval_expr(a, env, fns, models, roles, model_methods, enums).await, eval_expr(b, env, fns, models, roles, model_methods, enums).await) {
                (Value::I64(x), Value::I64(y)) => Value::Bool(x >= y),
                (Value::I32(x), Value::I32(y)) => Value::Bool(x >= y),
                (Value::U64(x), Value::U64(y)) => Value::Bool(x >= y),
                (Value::U32(x), Value::U32(y)) => Value::Bool(x >= y),
                (Value::F64(x), Value::F64(y)) => Value::Bool(x >= y),
                (Value::F32(x), Value::F32(y)) => Value::Bool(x >= y),
                _ => panic!("Type error: cannot compare non-numbers"),
            }
        }
        Expr::Assign { name, value } => {
            let v = eval_expr(value, env, fns, models, roles, model_methods, enums).await;
            env.insert(name.clone(), (AssignKind::Var, v.clone(), Ownership::Borrowed, Moved::False));
            v
        }
        Expr::CompoundAssign { name, op: _, value } => {
            let v = eval_expr(value, env, fns, models, roles, model_methods, enums).await;
            env.insert(name.clone(), (AssignKind::Var, v.clone(), Ownership::Borrowed, Moved::False));
            v
        }
        Expr::Regex(pattern) => {
            // Remove custom flags
            let normalized_pattern = normalize_regex_pattern(pattern);

            match RegexLib::new(&normalized_pattern) {
                Ok(_) => Value::Regex(pattern.clone()),
                Err(e) => panic!("Invalid regex pattern '{}': {}", pattern, e),
            }
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
                // Check if it's a known enum variant
                let mut found_enum = false;
                let mut enum_name = String::new();
                
                for (enum_def_name, enum_def) in enums.iter() {
                    if enum_def.variants.iter().any(|v| v.name == *name) {
                        found_enum = true;
                        enum_name = enum_def_name.clone();
                        break;
                    }
                }
                
                if found_enum {
                    Value::EnumValue(enum_name, name.clone(), None)
                } else {
                    panic!("Undefined variable: {}", name);
                }
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
        Expr::Call { name, args } if name == "thread" => {
            if args.len() != 1 {
                panic!("thread() expects exactly 1 argument (a closure)");
            }
            
            let closure_val = eval_expr(&args[0], env, fns, models, roles, model_methods, enums).await;
            
            if let Value::Closure(closure) = closure_val {
                // 1. Clone the entire environment for the new thread
                // This mimics a 'move' closure in Rust.
                let mut thread_env = env.clone();
                let mut thread_fns = fns.clone();
                let mut thread_models = models.clone();
                let mut thread_roles = roles.clone();
                let mut thread_methods = model_methods.clone();
                let thread_enums = enums.clone();
                let thread_closure = closure.clone();

                // 2. Spawn Tokio Task
                tokio::spawn(async move {
                    // Call the closure inside the new thread
                    call_closure(
                        &thread_closure,
                        &[], // No args passed to thread fn
                        &mut thread_env,
                        &mut thread_fns,
                        &mut thread_models,
                        &mut thread_roles,
                        &mut thread_methods,
                        &thread_enums
                    ).await; // call_closure must be async too!
                });
                
                Value::Void
            } else {
                panic!("thread() argument must be a closure");
            }
        }
        Expr::Thread(target) => {
            // We expect the target to be a closure
            let val = eval_expr(target, env, fns, models, roles, model_methods, enums).await;

            if let Value::Closure(closure) = val {
                // 1. Clone everything to move into the thread
                let mut thread_env = env.clone();
                let mut thread_fns = fns.clone();
                let mut thread_models = models.clone();
                let mut thread_roles = roles.clone();
                let mut thread_methods = model_methods.clone();
                let thread_enums = enums.clone();
                let thread_closure = closure.clone();

                // 2. Spawn the Tokio task
                tokio::spawn(async move {
                    // We need to call the closure.
                    call_closure(
                        &thread_closure,
                        &[], // Spawned threads take no arguments
                        &mut thread_env,
                        &mut thread_fns,
                        &mut thread_models,
                        &mut thread_roles,
                        &mut thread_methods,
                        &thread_enums
                    ).await; 
                });

                Value::Void
            } else {
                panic!("spawn/thread expects a closure");
            }
        }
        Expr::MakeChannel(hint) => {
            // We already know hint is Some(...) because of strict mode check in infer_expr_type
            // But let's be safe and unwrap or default (strict mode logic handles panic elsewhere)
            let inner_type = hint.clone().expect("Channel must be typed");

            let (tx, rx) = mpsc::channel(32);
            Value::Channel(tx, Arc::new(Mutex::new(rx)), inner_type)
        }
        Expr::Send(chan_expr, val_expr) => {
            let chan_val = eval_expr(chan_expr, env, fns, models, roles, model_methods, enums).await;
            let val = eval_expr(val_expr, env, fns, models, roles, model_methods, enums).await;

            if let Value::Channel(tx, _, _) = chan_val {
                match tx.send(val).await {
                    Ok(_) => Value::Void,
                    Err(_) => panic!("Cannot send: Channel closed"),
                }
            } else {
                panic!("Cannot send to non-channel type");
            }
        }
        Expr::Receive(chan_expr) => {
            let chan_val = eval_expr(chan_expr, env, fns, models, roles, model_methods, enums).await;

            if let Value::Channel(_, rx_mutex, _) = chan_val {
                let mut rx = rx_mutex.lock().await;
                match rx.recv().await {
                    Some(v) => v,
                    None => panic!("Channel closed and empty"),
                }
            } else {
                panic!("Cannot receive from non-channel type");
            }
        }
        Expr::MaybeReceive(chan_expr) => {
            let chan_val = eval_expr(chan_expr, env, fns, models, roles, model_methods, enums).await;

            if let Value::Channel(_, rx_arc, _) = chan_val {
                let mut rx = rx_arc.lock().await;
                match rx.recv().await {
                    Some(val) => {
                        // Maybe::Value(val)
                        Value::EnumValue("Maybe".to_string(), "Value".to_string(), Some(Box::new(val)))
                    },
                    None => {
                        // Maybe::Null
                        Value::EnumValue("Maybe".to_string(), "Null".to_string(), None)
                    }
                }
            } else {
                panic!("Runtime Error: Cannot receive from non-channel type");
            }
        }
        Expr::Call { name, args } => {
            if name == "close" {
                let arg_val = eval_expr(&args[0], env, fns, models, roles, model_methods, enums).await;
                
                if let Value::Channel(_, rx_arc, _) = arg_val {
                    let mut rx = rx_arc.lock().await;
                    rx.close(); // Tokio method: closes channel, prevents new sends
                    return Value::Void;
                } else {
                    panic!("Runtime Error: close() called on non-channel");
                }
            }

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
                    enums,
                ).await;
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
                    eval_expr(arg_expr, env, fns, models, roles, model_methods, enums).await
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
                        panic!("Type error: param for function {}: expected {:?}, got {:?}", name, expected_type, actual_type);
                    }
                }

                fn_env.insert(param_name.clone(), (AssignKind::Var, arg_val, ownership, Moved::False));
            }
            
            // Execute function body
            if let Some(body) = &fn_def.body {
                let flow = execute_block(body, &mut fn_env, fns, models, roles, model_methods, enums, fn_def.return_type.as_ref()).await;
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
            if let Some(model_def) = models.get(name).cloned() {
                let mut field_values = HashMap::new();
                
                // Evaluate all field expressions
                for (field_name, field_expr) in fields {
                    let field_val = eval_expr(field_expr, env, fns, models, roles, model_methods, enums).await;

                    // Find the expected type for this field
                    if let Some((_, expected_type)) = model_def.fields.iter().find(|(n, _)| n == field_name) {
                        let actual_type = infer_value_type(&field_val);
                        if !types_compatible(expected_type, &actual_type) {
                            panic!(
                                "Type error in model '{}': field '{}' expects {:?}, got {:?}",
                                name, field_name, expected_type, actual_type
                            );
                        }
                    } else {
                        panic!("Unknown field '{}' on model '{}'", field_name, name);
                    }

                    field_values.insert(field_name.clone(), field_val);
                }

                // Check that all required fields are provided
                // TODO some fields could be optional
                for (required_field, _) in &model_def.fields {
                    if !field_values.contains_key(required_field) {
                        panic!("Missing required field '{}' on model '{}'", required_field, name);
                    }
                }
                
                Value::Object(name.clone(), field_values)
            } else {
                panic!("Unknown model: {}", name);
            }
        }
        Expr::FieldAccess { object, field, value } => {
            let obj_val = eval_expr(object, env, fns, models, roles, model_methods, enums).await;
            
            match obj_val {
                Value::Object(model_name, mut fields) => {
                    // Only update if value is provided
                    if let Some(new_val_expr) = value {
                        let new_val = eval_expr(new_val_expr, env, fns, models, roles, model_methods, enums).await;

                        // Type check the field assignment
                        if let Some(model_def) = models.get(&model_name) {
                            if let Some((_, expected_type)) = model_def.fields.iter().find(|(n, _)| n == field) {
                                let actual_type = infer_value_type(&new_val);
                                if !types_compatible(expected_type, &actual_type) {
                                    panic!(
                                        "Type error: field '{}' on model '{}' expects {}, got {}",
                                        field, model_name,
                                        format_type_for_error(expected_type),
                                        format_type_for_error(&actual_type)
                                    );
                                }
                            } else {
                                panic!("Field '{}' not found on model '{}'", field, model_name);
                            }
                        } else {
                            panic!("Unknown model: {}", model_name);
                        }

                        fields.insert(field.clone(), new_val.clone());
                        let updated_obj = Value::Object(model_name, fields);
                        
                        // Update the original variable
                        if let Expr::Var(var_name) = object.as_ref() {
                            if let Some((kind, _, ownership, _)) = env.get(var_name) {
                                match ownership {
                                    Ownership::Owned => {
                                        env.insert(var_name.clone(), (kind.clone(), updated_obj, ownership.clone(), Moved::False));
                                    }
                                    Ownership::Borrowed => {
                                        panic!("Cannot directly assign field on borrowed instance '{}'. Use 'rebind'", var_name);
                                    }
                                }
                            }
                        } else {
                            panic!("Can only assign fields on variables");
                        }
                        
                        new_val
                    } else {
                        // Just accessing the field, return it
                        fields.get(field)
                            .cloned()
                            .unwrap_or_else(|| panic!("Field '{}' not found", field))
                    }
                }
                _ => panic!("Cannot access field on non-object"),
            }
        }
        Expr::MethodCall { object, method, args } => {
            // Check if it's a static method call on a model type
            if let Expr::Var(model_name) = &**object {
                if models.contains_key(model_name) {
                    // Static method call: Model.method(...)
                    if let Some(method_def) = model_methods.get(&(model_name.clone(), method.clone())).cloned() {
                        // Create new scope for the static method (no `this`)
                        let mut method_env: HashMap<String, (AssignKind, Value, Ownership, Moved)> = HashMap::new();
                        
                        // Bind parameters
                        for (i, (param_name, _param_type, _)) in method_def.params.iter().enumerate() {
                            if i < args.len() {
                                let arg_val = eval_expr(&args[i], env, fns, models, roles, model_methods, enums).await;
                                method_env.insert(
                                    param_name.clone(),
                                    (AssignKind::Var, arg_val, Ownership::Borrowed, Moved::False),
                                );
                            } else {
                                panic!("Method {} expects {} arguments, got {}", method, method_def.params.len(), args.len());
                            }
                        }
                        
                        // Execute method body
                        if let Some(body) = &method_def.body {
                            let flow = execute_block(body, &mut method_env, fns, models, roles, model_methods, enums, method_def.return_type.as_ref()).await;
                            match flow {
                                ControlFlow::Return(val) => return val,
                                _ => return Value::Void,
                            }
                        } else {
                            panic!("Static method '{}' has no body", method);
                        }
                    } else {
                        panic!("Static method '{}' not found on model '{}'", method, model_name);
                    }
                }
            }

            let obj_val = eval_expr(object, env, fns, models, roles, model_methods, enums).await;

            // Handle numeric methods
            match obj_val.clone() {
                Value::I64(n) => {
                    match method.as_str() {
                        "abs" => return Value::I64(n.abs()),
                        "round" | "floor" | "ceil" => return Value::I64(n),
                        _ => {}
                    }
                }
                Value::I32(n) => {
                    match method.as_str() {
                        "abs" => return Value::I32(n.abs()),
                        "round" | "floor" | "ceil" => return Value::I32(n),
                        _ => {}
                    }
                }
                Value::U64(n) => {
                    match method.as_str() {
                        "abs" => return Value::U64(n),
                        "round" | "floor" | "ceil" => return Value::U64(n),
                        _ => {}
                    }
                }
                Value::U32(n) => {
                    match method.as_str() {
                        "abs" => return Value::U32(n),
                        "round" | "floor" | "ceil" => return Value::U32(n),
                        _ => {}
                    }
                }
                Value::F64(n) => {
                    match method.as_str() {
                        "abs" => return Value::F64(n.abs()),
                        "round" => return Value::F64(n.round()),
                        "floor" => return Value::F64(n.floor()),
                        "ceil" => return Value::F64(n.ceil()),
                        _ => {}
                    }
                }
                Value::F32(n) => {
                    match method.as_str() {
                        "abs" => return Value::F32(n.abs()),
                        "round" => return Value::F32(n.round()),
                        "floor" => return Value::F32(n.floor()),
                        "ceil" => return Value::F32(n.ceil()),
                        _ => {}
                    }
                }
                _ => {} // Not a number, continue
            }

            // Handle array methods
            if let Value::Array(mut arr) = obj_val.clone() {
                match method.as_str() {
                    "size" => {
                        return Value::I64(arr.len() as i64);
                    }
                    "push" => {
                        // Check mutability
                        if let Expr::Var(var_name) = object.as_ref() {
                            if !can_mutate(var_name, env) {
                                panic!("Cannot call mutating method '{}' on immutable or borrowed array '{}'", method, var_name);
                            }
                        }
                        
                        // Handle push(item | item[])
                        if args.is_empty() {
                            panic!("push() requires at least one argument");
                        }
                        
                        let arg_val = eval_expr(&args[0], env, fns, models, roles, model_methods, enums).await;
                        let index: Option<usize> = if args.len() > 1 {
                            Some(to_usize(&eval_expr(&args[1], env, fns, models, roles, model_methods, enums).await)
                                .expect("push() index must be an integer"))
                        } else {
                            None
                        };
                        
                        match index {
                            Some(idx) => {
                                if idx > arr.len() {
                                    panic!("push() index {} out of bounds for array of length {}", idx, arr.len());
                                }
                                arr.insert(idx, arg_val);
                            }
                            None => arr.push(arg_val),
                        }

                        let size = arr.len() as i64;
                        
                        // Update the original variable
                        if let Expr::Var(var_name) = object.as_ref() {
                            if let Some((kind, _, ownership, _)) = env.get(var_name) {
                                env.insert(var_name.clone(), (kind.clone(), Value::Array(arr), ownership.clone(), Moved::False));
                            }
                        }
                        
                        return Value::I64(size);
                    }
                    "pull" => {
                        // Check mutability
                        if let Expr::Var(var_name) = object.as_ref() {
                            if !can_mutate(var_name, env) {
                                panic!("Cannot call mutating method '{}' on immutable or borrowed array '{}'", method, var_name);
                            }
                        }
                        
                        if arr.is_empty() {
                            panic!("Cannot pull from an empty array");
                        }
                        
                        let index: Option<usize> = if args.len() > 1 {
                            Some(to_usize(&eval_expr(&args[1], env, fns, models, roles, model_methods, enums).await)
                                .expect("pull() index must be an integer"))
                        } else {
                            None
                        };
                        
                        let pulled = match index {
                            Some(idx) => {
                                if idx >= arr.len() {
                                    panic!("pull() index {} out of bounds for array of length {}", idx, arr.len());
                                }
                                arr.remove(idx)
                            }
                            None => arr.pop().unwrap(),
                        };
                        
                        // Update the original variable
                        if let Expr::Var(var_name) = object.as_ref() {
                            if let Some((kind, _, ownership, _)) = env.get(var_name) {
                                env.insert(var_name.clone(), (kind.clone(), Value::Array(arr), ownership.clone(), Moved::False));
                            }
                        }
                        
                        return pulled;
                    }
                    "slice" => {
                        // Check mutability
                        if let Expr::Var(var_name) = object.as_ref() {
                            if !can_mutate(var_name, env) {
                                panic!("Cannot call mutating method '{}' on immutable or borrowed array '{}'", method, var_name);
                            }
                        }
                        
                        if args.is_empty() {
                            panic!("slice() requires at least a start index");
                        }
                        
                        let start = to_usize(&eval_expr(&args[0], env, fns, models, roles, model_methods, enums).await)
                            .expect("slice() start must be an integer");
                        
                        let stop = if args.len() > 1 {
                            to_usize(&eval_expr(&args[1], env, fns, models, roles, model_methods, enums).await)
                                .expect("slice() stop must be an integer")
                        } else {
                            arr.len()
                        };

                        if start > arr.len() || stop > arr.len() || start > stop {
                            panic!("slice() indices out of bounds: start={}, stop={}, len={}", start, stop, arr.len());
                        }
                        
                        let sliced: Vec<Value> = arr.drain(start..stop).collect();
                        
                        // Update the original variable
                        if let Expr::Var(var_name) = object.as_ref() {
                            if let Some((kind, _, ownership, _)) = env.get(var_name) {
                                env.insert(var_name.clone(), (kind.clone(), Value::Array(arr), ownership.clone(), Moved::False));
                            }
                        }
                        
                        return Value::Array(sliced);
                    }
                    "splice" => {
                        // Check mutability - only Var + Owned can mutate
                        if let Expr::Var(var_name) = object.as_ref() {
                            if !can_mutate(var_name, env) {
                                panic!("Cannot call mutating method 'splice' on immutable or borrowed array '{}'", var_name);
                            }
                        }
                        
                        if args.is_empty() {
                            panic!("splice() requires items to splice");
                        }
                        
                        let items_to_splice = match eval_expr(&args[0], env, fns, models, roles, model_methods, enums).await {
                            Value::Array(items) => items,
                            _ => panic!("splice() first argument must be an array"),
                        };
                        
                        if items_to_splice.is_empty() {
                            panic!("splice() cannot splice an empty array");
                        }
                        
                        // Type check: ensure splice items match array element type
                        if !arr.is_empty() {
                            let arr_elem_type = infer_value_type(&arr[0]);
                            for item in &items_to_splice {
                                let item_type = infer_value_type(item);
                                if !types_compatible(&arr_elem_type, &item_type) {
                                    panic!("Type error: cannot splice {:?} into array of {:?}", item_type, arr_elem_type);
                                }
                            }
                        }
                        
                        let start_pos = if args.len() > 1 {
                            to_usize(&eval_expr(&args[1], env, fns, models, roles, model_methods, enums).await)
                                .expect("splice() start position must be an integer")
                        } else {
                            arr.len()
                        };
                        
                        if start_pos > arr.len() {
                            panic!("splice() index {} out of bounds for array of length {}", start_pos, arr.len());
                        }
                        
                        // Insert items at position
                        for (i, item) in items_to_splice.into_iter().enumerate() {
                            arr.insert(start_pos + i, item);
                        }

                        let size = arr.len() as i64;
                        
                        // Update the original variable
                        if let Expr::Var(var_name) = object.as_ref() {
                            if let Some((kind, _, ownership, _)) = env.get(var_name) {
                                env.insert(var_name.clone(), (kind.clone(), Value::Array(arr.clone()), ownership.clone(), Moved::False));
                            }
                        }

                        return Value::I64(size);
                    }
                    "copy" => {
                        // copy() doesn't mutate, so no mutability check needed
                        
                        let start = if !args.is_empty() {
                            to_usize(&eval_expr(&args[0], env, fns, models, roles, model_methods, enums).await)
                                .expect("copy() start must be an integer")
                        } else {
                            0
                        };
                        
                        let stop = if args.len() > 1 {
                            to_usize(&eval_expr(&args[1], env, fns, models, roles, model_methods, enums).await)
                                .expect("copy() stop must be an integer")
                        } else {
                            arr.len()
                        };
                        
                        if start > arr.len() || stop > arr.len() || start > stop {
                            panic!("copy() indices out of bounds: start={}, stop={}, len={}", start, stop, arr.len());
                        }
                        
                        let copied: Vec<Value> = arr[start..stop].to_vec();
                        
                        return Value::Array(copied);
                    }
                    _ => {
                        // Not an array method, continue to regular method handling
                    }
                }
            }

            // Handle string methods
            if let Value::String(mut s) = obj_val.clone() {
                match method.as_str() {
                    "length" => {
                        return Value::I64(s.len() as i64);
                    }
                    "includes" => {
                        if args.is_empty() {
                            panic!("includes() requires a search argument");
                        }
                        
                        let search = match eval_expr(&args[0], env, fns, models, roles, model_methods, enums).await {
                            Value::String(search_str) => search_str,
                            _ => panic!("includes() argument must be a string"),
                        };
                        
                        if search.is_empty() {
                            return Value::Bool(false);
                        }
                        
                        return Value::Bool(s.contains(&search));
                    }
                    "starts" => {
                        if args.is_empty() {
                            panic!("starts() requires a search argument");
                        }
                        
                        let search = match eval_expr(&args[0], env, fns, models, roles, model_methods, enums).await {
                            Value::String(search_str) => search_str,
                            _ => panic!("starts() argument must be a string"),
                        };
                        
                        if search.is_empty() {
                            return Value::Bool(false);
                        }
                        
                        return Value::Bool(s.starts_with(&search));
                    }
                    "ends" => {
                        if args.is_empty() {
                            panic!("ends() requires a search argument");
                        }
                        
                        let search = match eval_expr(&args[0], env, fns, models, roles, model_methods, enums).await {
                            Value::String(search_str) => search_str,
                            _ => panic!("ends() argument must be a string"),
                        };
                        
                        if search.is_empty() {
                            return Value::Bool(false);
                        }
                        
                        return Value::Bool(s.ends_with(&search));
                    }
                    "replace" => {
                        if args.len() < 2 {
                            panic!("replace() requires search and replacement arguments");
                        }
                        
                        let search_arg = eval_expr(&args[0], env, fns, models, roles, model_methods, enums).await;
                        let replacement = match eval_expr(&args[1], env, fns, models, roles, model_methods, enums).await {
                            Value::String(replacement_str) => replacement_str,
                            _ => panic!("replace() replacement argument must be a string"),
                        };
                        
                        let result = match search_arg {
                            Value::String(search_str) => {
                                if search_str.is_empty() {
                                    panic!("replace() search string cannot be empty");
                                }
                                // String replace - only first occurrence
                                if let Some(pos) = s.find(&search_str) {
                                    let mut replaced = s.clone();
                                    replaced.replace_range(pos..pos + search_str.len(), &replacement);
                                    replaced
                                } else {
                                    s.clone()
                                }
                            }
                            Value::Regex(pattern) => {
                                // Check if pattern has custom global flag
                                let has_global = pattern.contains("g");

                                // Remove custom flags
                                let normalized_pattern = normalize_regex_pattern(&pattern);
                                
                                match RegexLib::new(&normalized_pattern) {
                                    Ok(re) => {
                                        if has_global {
                                            // Replace all occurrences
                                            re.replace_all(&s, replacement.as_str()).to_string()
                                        } else {
                                            // Replace only first occurrence
                                            re.replace(&s, replacement.as_str()).to_string()
                                        }
                                    }
                                    Err(e) => panic!("Invalid regex: {}", e),
                                }
                            }
                            _ => panic!("replace() search argument must be a string or regex"),
                        };
                        
                        return Value::String(result);
                    }
                    "slice" => {
                        // Check mutability - only Var + Owned can mutate
                        if let Expr::Var(var_name) = object.as_ref() {
                            if !can_mutate(var_name, env) {
                                panic!("Cannot call mutating method 'slice' on immutable or borrowed string '{}'", var_name);
                            }
                        }
                        
                        if args.is_empty() {
                            panic!("slice() requires at least a start index");
                        }
                        
                        let start = to_usize(&eval_expr(&args[0], env, fns, models, roles, model_methods, enums).await)
                            .expect("slice() start must be an integer");
                        
                        let stop = if args.len() > 1 {
                            to_usize(&eval_expr(&args[1], env, fns, models, roles, model_methods, enums).await)
                                .expect("slice() stop must be an integer")
                        } else {
                            s.len()
                        };
                        
                        if start > s.len() || stop > s.len() || start > stop {
                            panic!("slice() indices out of bounds: start={}, stop={}, len={}", start, stop, s.len());
                        }
                        
                        if start == stop {
                            panic!("slice() cannot slice an empty range");
                        }
                        
                        let sliced = s[start..stop].to_string();
                        s = format!("{}{}", &s[..start], &s[stop..]);
                        
                        // Update the original variable
                        if let Expr::Var(var_name) = object.as_ref() {
                            if let Some((kind, _, ownership, _)) = env.get(var_name) {
                                env.insert(var_name.clone(), (kind.clone(), Value::String(s), ownership.clone(), Moved::False));
                            }
                        }
                        
                        return Value::String(sliced);
                    }
                    "split" => {
                        let separator = if !args.is_empty() {
                            match eval_expr(&args[0], env, fns, models, roles, model_methods, enums).await {
                                Value::String(sep) => {
                                    if sep.is_empty() {
                                        panic!("split() separator cannot be empty. Use split() with no arguments to split by character");
                                    }
                                    sep
                                }
                                _ => panic!("split() argument must be a string"),
                            }
                        } else {
                            // Split by character if no argument
                            let chars: Vec<Value> = s.chars()
                                .map(|c| Value::String(c.to_string()))
                                .collect();
                            return Value::Array(chars);
                        };
                        
                        let parts: Vec<Value> = s.split(&separator)
                            .map(|part| Value::String(part.to_string()))
                            .collect();
                        
                        return Value::Array(parts);
                    }
                    "copy" => {
                        // copy() doesn't mutate, so no mutability check needed
                        
                        let start = if args.is_empty() {
                            to_usize(&eval_expr(&args[0], env, fns, models, roles, model_methods, enums).await)
                                .expect("copy() start must be an integer")
                        } else {
                            0
                        };
                        
                        let stop = if args.len() > 1 {
                            to_usize(&eval_expr(&args[1], env, fns, models, roles, model_methods, enums).await)
                                .expect("copy() stop must be an integer")
                        } else {
                            s.len()
                        };
                        
                        if start > s.len() || stop > s.len() || start > stop {
                            panic!("copy() indices out of bounds: start={}, stop={}, len={}", start, stop, s.len());
                        }
                        
                        if start == stop {
                            panic!("copy() cannot copy an empty range");
                        }
                        
                        let copied = s[start..stop].to_string();
                        return Value::String(copied);
                    }
                    "upper" => {
                        return Value::String(s.to_uppercase());
                    }
                    "lower" => {
                        return Value::String(s.to_lowercase());
                    }
                    "trim" => {
                        return Value::String(s.trim().to_string());
                    }
                    "index" => {
                        if args.is_empty() {
                            panic!("index() requires a search argument");
                        }
                        
                        let search = match eval_expr(&args[0], env, fns, models, roles, model_methods, enums).await {
                            Value::String(search_str) => search_str,
                            _ => panic!("index() search argument must be a string"),
                        };
                        
                        if search.is_empty() {
                            panic!("index() search string cannot be empty");
                        }
                        
                        let start_pos = if args.len() > 1 {
                            to_usize(&eval_expr(&args[1], env, fns, models, roles, model_methods, enums).await)
                                .expect("index() start position must be an integer")
                        } else {
                            0
                        };
                        
                        if start_pos > s.len() {
                            return Value::I64(-1);
                        }
                        
                        return match s[start_pos..].find(&search) {
                            Some(pos) => Value::I64((start_pos + pos) as i64),
                            None => Value::I64(-1),
                        };
                    }
                    "repeat" => {
                        if args.is_empty() {
                            panic!("repeat() requires a count argument");
                        }
                        
                        let count = match to_usize(&eval_expr(&args[0], env, fns, models, roles, model_methods, enums).await) {
                            Ok(n) => n,
                            _ => panic!("repeat() count argument must be an integer"),
                        };
                        
                        if count == 0 {
                            return Value::String(String::new());
                        }
                        
                        return Value::String(s.repeat(count as usize));
                    }
                    _ => {
                        // Not a string method, continue to model method handling
                    }
                }
            }

            match &obj_val {
                Value::Object(model_name, _fields) => {
                    // Clone the method_def first to avoid borrow issues
                    let method_def = model_methods
                        .get(&(model_name.clone(), method.clone()))
                        .cloned()
                        .unwrap_or_else(|| panic!("Method '{}' not found on model '{}'", method, model_name));

                    // Check if method mutates this and if caller has permission
                    if let Expr::Var(var_name) = object.as_ref() {
                        if let Some((kind, _, ownership, _)) = env.get(var_name) {
                            let mutation_type = method_def.body.as_ref()
                                .map(|body| method_mutation_type(body))
                                .unwrap_or(MethodMutationType::None);

                            match mutation_type {
                                MethodMutationType::None => {
                                    // No mutations, allow
                                }
                                MethodMutationType::DirectAssignment => {
                                    // Requires ownership
                                    match (kind, ownership) {
                                        (AssignKind::Var, Ownership::Owned) => {}
                                        (AssignKind::Var, Ownership::Borrowed) => {
                                            panic!("Cannot call method with direct field assignment on borrowed instance '{}'", var_name);
                                        }
                                        (AssignKind::Const, _) | (AssignKind::Static, _) | (AssignKind::Lit, _) => {
                                            panic!("Cannot call mutating method on immutable instance '{}'", var_name);
                                        }
                                    }
                                }
                                MethodMutationType::Rebind => {
                                    // Can work on any var
                                    match kind {
                                        AssignKind::Var => {}
                                        AssignKind::Const | AssignKind::Static | AssignKind::Lit => {
                                            panic!("Cannot call mutating method on immutable instance '{}'", var_name);
                                        }
                                    }
                                }
                                MethodMutationType::Mixed => {
                                    panic!("Method uses both direct assignment and rebind on this—pick one");
                                }
                            }
                        }
                    }

                    // Create new scope for method
                    let mut method_env: HashMap<String, (AssignKind, Value, Ownership, Moved)> = HashMap::new();

                    // Bind `this` as first implicit parameter
                    method_env.insert(
                        "this".to_string(),
                        (AssignKind::Var, obj_val.clone(), Ownership::Owned, Moved::False),
                    );
                    
                    // Bind explicit parameters (skip first param if it matches signature)
                    for (i, (param_name, _param_type, param_ownership)) in method_def.params.iter().enumerate() {
                        if i < args.len() {
                            let arg_val = eval_expr(&args[i], env, fns, models, roles, model_methods, enums).await;
                            method_env.insert(
                                param_name.clone(),
                                (AssignKind::Var, arg_val, param_ownership.clone(), Moved::False),
                            );
                        }
                    }
                        
                    // Execute method body
                    let flow = if let Some(body) = &method_def.body {
                        execute_block(body, &mut method_env, fns, models, roles, model_methods, enums, method_def.return_type.as_ref()).await
                    } else {
                        panic!("Method '{}' has no body", method);
                    };
                    
                    // Sync `this` back to the original variable
                    if let Expr::Var(var_name) = object.as_ref() {
                        if let Some((_, updated_obj, _, _)) = method_env.get("this") {
                            // Get the original ownership from the outer env
                            if let Some((kind, _, original_ownership, _)) = env.get(var_name) {
                                env.insert(var_name.clone(), (kind.clone(), updated_obj.clone(), original_ownership.clone(), Moved::False));
                            }
                        }
                    }
                    
                    match flow {
                        ControlFlow::Return(val) => val,
                        _ => Value::Void,
                    }
                }
                _ => panic!("Cannot call method on non-object"),
            }
        }
        Expr::Closure(closure_def) => {
            let param_names: Vec<String> = closure_def.params
            .iter()
            .map(|(name, _)| name.clone())
            .collect();

            let rebound_vars: Vec<String> = find_rebound_vars(&closure_def.body);
            let referenced_vars: Vec<String> = find_referenced_vars(&closure_def.body);

            let owned_vars: Vec<String> = env
                .iter()
                .filter(|(_, (kind, _, ownership, _))| {
                    matches!(kind, AssignKind::Var) && *ownership == Ownership::Owned
                })
                .map(|(name, _)| name.clone())
                .collect();

            // Referenced variables should not include ones that are rebound, owned, or defined in closure params
            let read_vars: Vec<String> = referenced_vars.clone()
                .into_iter()
                .filter(|ref_var| !rebound_vars.contains(ref_var) && !owned_vars.contains(ref_var) && !param_names.contains(ref_var))
                .collect();
            
            // Both rebound vars and owned vars should be live
            let mut live_vars = rebound_vars;
            for owned_var in owned_vars {
                if !live_vars.contains(&owned_var) && referenced_vars.contains(&owned_var) {
                    live_vars.push(owned_var);
                }
            }

            // Combine live_vars and read_vars for captured_env
            let all_captured = [live_vars.clone(), read_vars].concat();
            let captured_env = all_captured
                .into_iter()
                .filter_map(|var_name| {
                    env.get(&var_name).map(|val| (var_name, val.clone()))
                })
                .collect();

            Value::Closure(ClosureValue {
                def: Box::new(closure_def.as_ref().clone()),
                captured_env,
                live_vars,
            })
        }
        Expr::EnumVariant { enum_name, variant, data } => {
            let data_val = if let Some(expr) = data {
                Some(Box::new(eval_expr(expr, env, fns, models, roles, model_methods, enums).await))
            } else {
                None
            };
            
            Value::EnumValue(enum_name.clone(), variant.clone(), data_val)
        }
    }
}

#[async_recursion]
async fn execute_stmt(
    stmt: &Stmt,
    env: &mut HashMap<String, (AssignKind, Value, Ownership, Moved)>,
    fns: &mut HashMap<String, FnDef>,
    models: &mut HashMap<String, ModelDef>,
    roles: &mut HashMap<String, RoleDef>,
    model_methods: &mut HashMap<(String, String), FnDef>,
    enums: &HashMap<String, EnumDef>,
    expected_return_type: Option<&Type>,
) -> ControlFlow {
    match stmt {
        Stmt::Echo(e) => {
            let _type = infer_expr_type(e, env, fns, models, model_methods, enums);
            let v = eval_expr(e, env, fns, models, roles, model_methods, enums).await;
            print!("{}", format_value(&v));
            ControlFlow::None
        }
        Stmt::Print(e) => {
            let _type = infer_expr_type(e, env, fns, models, model_methods, enums);
            let v = eval_expr(e, env, fns, models, roles, model_methods, enums).await;
            println!("{}", format_value(&v));
            ControlFlow::None
        }
        Stmt::ExprStmt(e) => {
            let _type = infer_expr_type(e, env, fns, models, model_methods, enums);
            eval_expr(e, env, fns, models, roles, model_methods, enums).await;
            ControlFlow::None
        }
        Stmt::Assign { kind, declared_type, name, value, ownership} => {
            // Check if variable already declared in this scope
            if env.contains_key(name) {
                panic!("Identifier '{}' already declared in this scope", name);
            }
            
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

            let v = eval_expr(value, env, fns, models, roles, model_methods, enums).await;
            let actual_type = infer_value_type(&v);

            // Type check against declared type if provided
            if let Some(expected_type) = declared_type {
                if !types_compatible(&expected_type, &actual_type) {
                    panic!("Type error: expected {}, got {}", 
                        format_type_for_error(&expected_type),
                        format_type_for_error(&actual_type)
                    );
                }
            }

            env.insert(name.clone(), (kind.clone(), v, ownership.clone(), Moved::False));
            
            ControlFlow::None
        }
        Stmt::Reassign { name, value } => {
            let _type = infer_expr_type(value, env, fns, models, model_methods, enums);
            if !env.contains_key(name) {
                panic!("Variable '{}' not declared", name);
            }

            let (kind, existing_val, ownership, _moved) = env.get(name).unwrap().clone();
            let existing_type = infer_value_type(&existing_val);

            match kind {
                AssignKind::Var => {
                    let v = eval_expr(value, env, fns, models, roles, model_methods, enums).await;
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
            
            let v = eval_expr(value, env, fns, models, roles, model_methods, enums).await;
            env.insert(name.clone(), (kind, v, ownership, Moved::False));
            ControlFlow::None
        }
        Stmt::RebindField { object, field, value } => {
            if let Some((kind, _, _, moved)) = env.get(object) {
                if *kind != AssignKind::Var {
                    panic!("Can only rebind fields defined via var, not {:?}", format!("{:?}", kind).to_lowercase());
                }
                if let Moved::True = *moved {
                    panic!("Cannot rebind moved variable '{}'", object);
                }
                
                let (kind, Value::Object(model_name, mut fields), ownership, _) = env.get(object).unwrap().clone() else {
                    panic!("Can only rebind fields on objects");
                };
                
                // Look up the model definition to check field type
                let expected_type = if let Some(model_def) = models.get(&model_name) {
                    if let Some((_, t)) = model_def.fields.iter().find(|(n, _)| n == field) {
                        t.clone()
                    } else {
                        panic!("Field '{}' not found on model '{}'", field, model_name);
                    }
                } else {
                    panic!("Unknown model: {}", model_name);
                };

                let new_val = eval_expr(value, env, fns, models, roles, model_methods, enums).await;
                let actual_type = infer_value_type(&new_val);
                
                if !types_compatible(&expected_type, &actual_type) {
                    panic!(
                        "Type error: field '{}' on model '{}' expects {:?}, got {:?}",
                        field, model_name, expected_type, actual_type
                    );
                }
                
                fields.insert(field.clone(), new_val);
                
                let updated_obj = Value::Object(model_name, fields);
                env.insert(object.clone(), (kind, updated_obj, ownership, Moved::False));
            } else {
                panic!("Cannot rebind undefined variable '{}'", object);
            }
            
            ControlFlow::None
        }
        Stmt::Break => ControlFlow::Break,
        Stmt::Next => ControlFlow::Next,
        Stmt::If { condition, then_block, else_ifs, else_block } => {
            let _cond_type = infer_expr_type(condition, env, fns, models, model_methods, enums);
            let cond_value = eval_expr(condition, env, fns, models, roles, model_methods, enums).await;

            if is_truthy(&cond_value) {
                execute_block(then_block, env, fns, models, roles, model_methods, enums, None).await
            } else {
                for (else_if_cond, else_if_stmts) in else_ifs {
                    let _type = infer_expr_type(else_if_cond, env, fns, models, model_methods, enums);
                    let else_if_value = eval_expr(else_if_cond, env, fns, models, roles, model_methods, enums).await;
                    if is_truthy(&else_if_value) {
                        return execute_block(else_if_stmts, env, fns, models, roles, model_methods, enums, None).await;
                    }
                }
                
                if let Some(else_stmts) = else_block {
                    execute_block(else_stmts, env, fns, models, roles, model_methods, enums, None).await
                } else {
                    ControlFlow::None
                }
            }
        }
        Stmt::While { condition, body } => {
            loop {
                let _cond_type = infer_expr_type(condition, env, fns, models, model_methods, enums);
                let cond_value = eval_expr(condition, env, fns, models, roles, model_methods, enums).await;
                if !is_truthy(&cond_value) {
                    break;
                }
                
                match execute_block(body, env, fns, models, roles, model_methods, enums, None).await {
                    ControlFlow::Break => break,
                    ControlFlow::Next => continue,
                    ControlFlow::Return(v) => return ControlFlow::Return(v),
                    ControlFlow::None => {}
                }
            }
            ControlFlow::None
        }
        Stmt::WhileOnce { condition, body } => {
            loop {
                match execute_block(body, env, fns, models, roles, model_methods, enums, None).await {
                    ControlFlow::Break => break,
                    ControlFlow::Next => continue,
                    ControlFlow::Return(v) => return ControlFlow::Return(v),
                    ControlFlow::None => {}
                }

                let _cond_type = infer_expr_type(condition, env, fns, models, model_methods, enums);
                let cond_value = eval_expr(condition, env, fns, models, roles, model_methods, enums).await;
                if !is_truthy(&cond_value) {
                    break;
                }
            }
            ControlFlow::None
        }
        Stmt::Until { condition, body } => {
            loop {
                let _cond_type = infer_expr_type(condition, env, fns, models, model_methods, enums);
                let cond_value = eval_expr(condition, env, fns, models, roles, model_methods, enums).await;
                if is_truthy(&cond_value) {
                    break;
                }
                
                match execute_block(body, env, fns, models, roles, model_methods, enums, None).await {
                    ControlFlow::Break => break,
                    ControlFlow::Next => continue,
                    ControlFlow::Return(v) => return ControlFlow::Return(v),
                    ControlFlow::None => {}
                }
            }
            ControlFlow::None
        }
        Stmt::UntilOnce { condition, body } => {
            loop {
                match execute_block(body, env, fns, models, roles, model_methods, enums, None).await {
                    ControlFlow::Break => break,
                    ControlFlow::Next => continue,
                    ControlFlow::Return(v) => return ControlFlow::Return(v),
                    ControlFlow::None => {}
                }

                let _cond_type = infer_expr_type(condition, env, fns, models, model_methods, enums);
                let cond_value = eval_expr(condition, env, fns, models, roles, model_methods, enums).await;
                if is_truthy(&cond_value) {
                    break;
                }
            }
            ControlFlow::None
        }
        Stmt::Match { expr, cases, default, match_type } => {
            let _expr_type = infer_expr_type(expr, env, fns, models, model_methods, enums);
            let val = eval_expr(expr, env, fns, models, roles, model_methods, enums).await;
            let mut matched = false;
            
            for (patterns, stmts) in cases {
                for pattern in patterns {
                    if let Some(new_env) = match_pattern(&val, pattern, env) {
                        matched = true;
                        let mut pattern_env = new_env;
                        let flow = execute_block(stmts, &mut pattern_env, fns, models, roles, model_methods, enums, None).await;
                        
                        match (match_type, flow) {
                            (MatchType::One, ControlFlow::Next) => continue,
                            (MatchType::One, f) => return f,
                            (MatchType::Any, ControlFlow::None) => continue,
                            (MatchType::Any, f) => return f,
                        }
                    }
                }
            }
            
            // No match found, execute default if it exists
            if !matched || *match_type == MatchType::Any {
                if let Some(default_stmts) = default {
                    execute_block(default_stmts, env, fns, models, roles, model_methods, enums, None).await
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
        Stmt::EnumDef(_) => {
            // Enum definitions are processed at top-level in run(), not here
            panic!("Enum definitions must be at top-level, not inside functions or blocks");
        }
        Stmt::Return(expr_opt) => {
            let val = match expr_opt {
                Some(e) => {
                    let _type = infer_expr_type(e, env, fns, models, model_methods, enums);
                    eval_expr(e, env, fns, models, roles, model_methods, enums).await
                }
                None => Value::Void,
            };

            if let Some(expected_type) = expected_return_type {
                let actual_type = infer_value_type(&val);
                if !types_compatible(expected_type, &actual_type) {
                    panic!(
                        "Return type error: expected {:?}, got {:?}",
                        format_type_for_error(expected_type),
                        format_type_for_error(&actual_type)
                    );
                }
            }

            ControlFlow::Return(val)
        }
    }
}

#[async_recursion]
async fn execute_block(
    stmts: &[Stmt],
    env: &mut HashMap<String, (AssignKind, Value, Ownership, Moved)>,
    fns: &mut HashMap<String, FnDef>,
    models: &mut HashMap<String, ModelDef>,
    roles: &mut HashMap<String, RoleDef>,
    model_methods: &mut HashMap<(String, String), FnDef>,
    enums: &HashMap<String, EnumDef>,
    expected_return_type: Option<&Type>,
) -> ControlFlow {
    for stmt in stmts {
        // Check for illegal top-level assignments
        if let Stmt::Assign { kind, .. } = stmt {
            if matches!(kind, AssignKind::Lit | AssignKind::Static) {
                let kind_str = match kind {
                    AssignKind::Lit => "lit",
                    AssignKind::Static => "static",
                    _ => unreachable!(),
                };
                panic!("Can only assign with '{}' at the top-level of a file.", kind_str);
            }
        }

        let flow = execute_stmt(stmt, env, fns, models, roles, model_methods, enums, expected_return_type).await;
        match flow {
            ControlFlow::Break | ControlFlow::Next | ControlFlow::Return(_) => return flow,
            ControlFlow::None => {}
        }
    }
    ControlFlow::None
}

async fn run(stmts: &[Stmt]) {
    let mut env: HashMap<String, (AssignKind, Value, Ownership, Moved)> = HashMap::new();
    let mut fns: HashMap<String, FnDef> = HashMap::new();
    let mut models: HashMap<String, ModelDef> = HashMap::new();
    let mut roles: HashMap<String, RoleDef> = HashMap::new();
    let mut model_roles: HashMap<(String, String), bool> = HashMap::new();
    let mut model_methods: HashMap<(String, String), FnDef> = HashMap::new();
    let mut enums: HashMap<String, EnumDef> = HashMap::new();

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
            Stmt::EnumDef(enum_def) => {
                enums.insert(enum_def.name.clone(), enum_def.clone());
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
                    &enums,
                    None
                ).await;
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
                &enums,
                main_fn.return_type.as_ref()
            ).await;
        } else {
            panic!("The main() function must have a body.");
        }
        
    } else {
        panic!("No main() function found. Entrypoint files must define main()");
    }
}
