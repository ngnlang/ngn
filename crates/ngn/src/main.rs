use ngn::runtime::{Callable, ExportKind, ImportKind, ModuleExports, RuntimeContext};
use ngn::parser::Parser;

use ngn::toolbox::{ImportSource, Toolbox, parse_import_source};
use ngn::utils::resolve_module_path;
use regex::Regex as RegexLib;

use std::fs;
use std::collections::HashSet;
use std::collections::HashMap;
use ngn::ast::{
    ClosureDef, EnumDef, Expr, FnDef, ForIterable, InterpolationPart, MatchType, MethodMutationType, ModelDef, Pattern, RoleDef, Stmt,
};
use ngn::value::{ClosureValue, ControlFlow, MapKey, SetValue, Value};
use ngn::types::{AssignKind, Moved, Ownership, Type, is_numeric_type, types_compatible};

use async_recursion::async_recursion;
use tokio::sync::mpsc;
use std::sync::Arc;
use tokio::sync::Mutex;

use ngn::semantic::Analyzer;

use clap::Parser as ClapParser;
use std::path::PathBuf;

#[derive(ClapParser, Debug)]
#[command(author, version, about = "The ngn runtime", long_about = None)]
struct Cli {
    /// The path to the source file to run
    #[arg(value_name = "FILE")]
    file_path: Option<PathBuf>,

    /// Optional: Output debug info (AST)
    #[arg(short, long)]
    debug: bool,
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    let file_path = match cli.file_path {
        Some(path) => path,
        None => {
            eprintln!("Usage: ngn <file_path>");
            // TODO: Ideally, start a REPL here if no file is provided
            std::process::exit(1);
        }
    };

    if file_path.extension().and_then(|s| s.to_str()) != Some("ngn") {
        eprintln!("Error: File {:?} does not have the .ngn extension", file_path);
        std::process::exit(1);
    }

    let source = fs::read_to_string(&file_path)
        .expect("Failed to read ngn file");

    let tokens = ngn::lexer::tokenize(&source);
    let mut parser = Parser::new(tokens, HashMap::new(), file_path.clone());

    match parser.parse_program() {
        Ok(ast) => {
            if cli.debug {
                eprintln!("{:#?}", ast);
            }

            let mut analyzer = Analyzer::new(file_path.clone());
            if let Err(errors) = analyzer.analyze(&ast) {
                for e in errors {
                    eprintln!("Analysis warning: {}", e);
                    std::process::exit(1);
                }
            };
            run(&ast, &file_path).await;
        }
        Err(e) => {
            eprintln!("Parse error: {}", e);
            std::process::exit(1);
        }
    }
}

fn is_valid_type(t: &Type, models: &HashMap<String, ModelDef>, enums: &HashMap<String, EnumDef>) -> bool {
    match t {
        Type::I64 | Type::I32 | Type::I16 | Type::I8
        | Type::U64 | Type::U32 | Type::U16 | Type::U8
        | Type::F64 | Type::F32 | Type::Str | Type::Bool 
        | Type::Void | Type::Regex | Type::String => true,

        Type::Function { params, return_type } => {
            params.iter().all(|p| is_valid_type(p, models, enums))
                && is_valid_type(return_type, models, enums)
        }
        
        Type::Array(inner) => is_valid_type(inner, models, enums),
        Type::Map(key_type, val_type) => {
            is_valid_type(key_type, models, enums) && is_valid_type(val_type, models, enums)
        }
        Type::Set(val_type) => {
            is_valid_type(val_type, models, enums)
        }
        Type::Channel(inner) => is_valid_type(inner, models, enums),
        Type::StateActor(inner) => is_valid_type(inner, models, enums),
        Type::Namespace(_) => true,
        Type::Model(name) => models.contains_key(name),
        Type::Enum(name, _) => enums.contains_key(name),
        Type::Generic(_) => false, // Generics aren't valid unless resolved
        Type::Object(_) => true, // Object literals are always valid
    }
}

fn is_hashable_type(t: &Type) -> bool {
    matches!(
        t,
        Type::Generic(_) | Type::Str | Type::String | Type::I64 | Type::I32 | Type::I16 | Type::I8 | Type::U64 | Type::U32 | Type::U16 | Type::U8 | Type::Bool | Type::Enum(_, _)
    )
}

fn format_type_for_error(t: &Type) -> String {
    match t {
        Type::I64 => "i64".to_string(),
        Type::I32 => "i32".to_string(),
        Type::I16 => "i16".to_string(),
        Type::I8 => "i8".to_string(),
        Type::U64 => "u64".to_string(),
        Type::U32 => "u32".to_string(),
        Type::U16 => "u16".to_string(),
        Type::U8 => "u8".to_string(),
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
        Type::Function { params, return_type } => {
            let params_str = params.iter()
                .map(|p| format_type_for_error(p))
                .collect::<Vec<_>>()
                .join(", ");
            format!("fn<({}) -> {}>", params_str, format_type_for_error(return_type))
        }
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
        Type::StateActor(inner) => format!("state<{}>", format_type_for_error(inner)),
        Type::Namespace(name) => format!("namespace({})", name),
        Type::Map(key_type, val_type) => {
            format!("map<{}, {}>", 
                format_type_for_error(key_type), 
                format_type_for_error(val_type)
            )
        }
        Type::Set(val_type) => {
            format!("set<{}>",  
                format_type_for_error(val_type)
            )
        }
    }
}

fn format_value(v: &Value) -> String {
    match v {
        Value::I64(n) => n.to_string(),
        Value::I32(n) => n.to_string(),
        Value::I16(n) => n.to_string(),
        Value::I8(n) => n.to_string(),
        Value::U64(n) => n.to_string(),
        Value::U32(n) => n.to_string(),
        Value::U16(n) => n.to_string(),
        Value::U8(n) => n.to_string(),
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
        Value::StateActor(_, _, _) => "<state>".to_string(),
        Value::Namespace(name) => format!("[namespace: {}]", name),
        Value::Map(map, _, _) => {
            let entries: Vec<String> = map
                .iter()
                .map(|(k, v)| {
                    let key_str = match k {
                        MapKey::String(s) => format!("\"{}\"", s),
                        MapKey::I64(n) => n.to_string(),
                        MapKey::I32(n) => n.to_string(),
                        MapKey::I16(n) => n.to_string(),
                        MapKey::I8(n) => n.to_string(),
                        MapKey::U64(n) => n.to_string(),
                        MapKey::U32(n) => n.to_string(),
                        MapKey::U16(n) => n.to_string(),
                        MapKey::U8(n) => n.to_string(),
                        MapKey::Bool(b) => b.to_string(),
                        MapKey::Enum(enum_name, variant) => format!("{}::{}", enum_name, variant),
                    };
                    format!("{}: {}", key_str, format_value(v))
                })
                .collect();
            format!("{{ {} }}", entries.join(", "))
        },
        Value::Set(set, _) => {
            let entries: Vec<String> = set
                .iter()
                .map(|v| {
                    match v {
                        SetValue::String(s) => format!("\"{}\"", s),
                        SetValue::I64(n) => n.to_string(),
                        SetValue::I32(n) => n.to_string(),
                        SetValue::I16(n) => n.to_string(),
                        SetValue::I8(n) => n.to_string(),
                        SetValue::U64(n) => n.to_string(),
                        SetValue::U32(n) => n.to_string(),
                        SetValue::U16(n) => n.to_string(),
                        SetValue::U8(n) => n.to_string(),
                        SetValue::Bool(b) => b.to_string(),
                        SetValue::EnumValue(enum_name, variant) => format!("{}::{}", enum_name, variant),
                    }
                })
                .collect();
            format!("{{ {} }}", entries.join(", "))
        }
    }
}

fn method_mutation_type(body: &[Stmt]) -> MethodMutationType {
    let mut has_direct = false;

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
            // Check nested blocks
            Stmt::If { then_block, else_ifs, else_block, .. } => {
                let mut_type = method_mutation_type(then_block);
                if mut_type != MethodMutationType::None {
                    match mut_type {
                        MethodMutationType::DirectAssignment => has_direct = true,
                        MethodMutationType::None => {}
                    }
                }
                
                for (_, block) in else_ifs {
                    let mut_type = method_mutation_type(block);
                    if mut_type != MethodMutationType::None {
                        match mut_type {
                            MethodMutationType::DirectAssignment => has_direct = true,
                            MethodMutationType::None => {}
                        }
                    }
                }
                
                if let Some(block) = else_block {
                    let mut_type = method_mutation_type(block);
                    if mut_type != MethodMutationType::None {
                        match mut_type {
                            MethodMutationType::DirectAssignment => has_direct = true,
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
                            MethodMutationType::None => {}
                        }
                    }
                }
                if let Some(block) = default {
                    let mut_type = method_mutation_type(block);
                    if mut_type != MethodMutationType::None {
                        match mut_type {
                            MethodMutationType::DirectAssignment => has_direct = true,
                            MethodMutationType::None => {}
                        }
                    }
                }
            }
            _ => {}
        }
    }
    
    match has_direct {
        false => MethodMutationType::None,
        true => MethodMutationType::DirectAssignment,
    }
}

fn can_mutate(var_name: &str, env: &HashMap<String, (AssignKind, Value, Ownership, Moved, usize)>) -> bool {
    if let Some((kind, _, ownership, _, _)) = env.get(var_name) {
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
        Value::I16(n) => *n != 0,
        Value::I8(n) => *n != 0,
        Value::U64(n) => *n != 0,
        Value::U32(n) => *n != 0,
        Value::U16(n) => *n != 0,
        Value::U8(n) => *n != 0,
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
        Value::StateActor(_, _, _) => true,
        Value::Namespace(_) => true,
        Value::Map(_, _, _) => true,
        Value::Set(_, _,) => true,
    }
}

fn is_literal(e: &Expr) -> bool {
    matches!(e, 
        Expr::I64(_) |
        Expr::I32(_) |
        Expr::I16(_) |
        Expr::I8(_) |
        Expr::U64(_) |
        Expr::U32(_) |
        Expr::U16(_) |
        Expr::U8(_) |
        Expr::F64(_) |
        Expr::F32(_) |
        Expr::String(_) |
        Expr::Bool(_) |
        Expr::Array(_) |
        Expr::Closure(_)
    )
}

fn to_usize(v: &Value) -> Result<usize, String> {
    match v {
        Value::I64(n) => Ok(*n as usize),
        Value::I32(n) => Ok(*n as usize),
        Value::I16(n) => Ok(*n as usize),
        Value::I8(n) => Ok(*n as usize),
        Value::U64(n) => Ok(*n as usize),
        Value::U32(n) => Ok(*n as usize),
        Value::U16(n) => Ok(*n as usize),
        Value::U8(n) => Ok(*n as usize),
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

fn make_identity_closure(inner_type: &Type, ownership: &Ownership) -> Value {
    Value::Closure(ClosureValue {
        def: Box::new(ClosureDef {
            params: vec![("n".to_string(), Some(inner_type.clone()), ownership.clone())],
            body: vec![Stmt::Return(Some(Expr::Var("n".to_string())))],
            return_type: Some(inner_type.clone()),
        }),
        captured_env: HashMap::new(),
        live_vars: vec![],
    })
}

fn make_const_closure(value: Value, inner_type: &Type, ownership: &Ownership) -> Value {
    Value::Closure(ClosureValue {
        def: Box::new(ClosureDef {
            params: vec![("_".to_string(), Some(inner_type.clone()), ownership.clone())],
            body: vec![Stmt::Return(Some(value_to_expr(&value)))],
            return_type: Some(inner_type.clone()),
        }),
        captured_env: HashMap::new(),
        live_vars: vec![],
    })
}

fn value_to_expr(v: &Value) -> Expr {
    match v {
        Value::I64(n) => Expr::I64(*n),
        Value::I32(n) => Expr::I32(*n),
        Value::I16(n) => Expr::I16(*n),
        Value::I8(n) => Expr::I8(*n),
        Value::U64(n) => Expr::U64(*n),
        Value::U32(n) => Expr::U32(*n),
        Value::U16(n) => Expr::U16(*n),
        Value::U8(n) => Expr::U8(*n),
        Value::F64(n) => Expr::F64(*n),
        Value::F32(n) => Expr::F32(*n),
        Value::String(s) => Expr::String(s.clone()),
        Value::Bool(b) => Expr::Bool(*b),
        Value::Function(fn_def) => Expr::Var(fn_def.name.clone()),
        Value::Closure(closure) => Expr::Closure(closure.def.clone()),
        Value::Array(items) => {
            let exprs: Vec<Expr> = items.iter().map(|item| value_to_expr(item)).collect();
            Expr::Array(exprs)
        },
        Value::Map(map, key_type, val_type) => {
            let pairs: Vec<(Box<Expr>, Box<Expr>)> = map.iter()
                .map(|(key, val)| {
                    let key_expr = match key {
                        MapKey::String(s) => Expr::String(s.clone()),
                        MapKey::I64(n) => Expr::I64(*n),
                        MapKey::I32(n) => Expr::I32(*n),
                        MapKey::I16(n) => Expr::I16(*n),
                        MapKey::I8(n) => Expr::I8(*n),
                        MapKey::U64(n) => Expr::U64(*n),
                        MapKey::U32(n) => Expr::U32(*n),
                        MapKey::U16(n) => Expr::U16(*n),
                        MapKey::U8(n) => Expr::U8(*n),
                        MapKey::Bool(b) => Expr::Bool(*b),
                        MapKey::Enum(enum_name, variant) => {
                            Expr::EnumVariant {
                                enum_name: enum_name.clone(),
                                variant: variant.clone(),
                                data: None,
                            }
                        }
                    };
                    let val_expr = value_to_expr(val);
                    (Box::new(key_expr), Box::new(val_expr))
                })
                .collect();
            
            Expr::CreateMap(pairs, key_type.clone(), val_type.clone())
        }
        _ => panic!("Cannot convert {:?} to expr", v),
    }
}

fn value_to_map_key(val: Value) -> MapKey {
    match val {
        Value::String(s) => MapKey::String(s),
        Value::I64(n) => MapKey::I64(n),
        Value::I32(n) => MapKey::I32(n),
        Value::I16(n) => MapKey::I16(n),
        Value::I8(n) => MapKey::I8(n),
        Value::U64(n) => MapKey::U64(n),
        Value::U32(n) => MapKey::U32(n),
        Value::U16(n) => MapKey::U16(n),
        Value::U8(n) => MapKey::U8(n),
        Value::Bool(b) => MapKey::Bool(b),
        Value::EnumValue(enum_name, variant, _) => MapKey::Enum(enum_name, variant),
        _ => panic!("Invalid map key type: {:?}", val),
    }
}

fn value_to_set_value(v: &Value) -> SetValue {
    match v {
        Value::String(s) => SetValue::String(s.clone()),
        Value::I64(n) => SetValue::I64(*n),
        Value::I32(n) => SetValue::I32(*n),
        Value::I16(n) => SetValue::I16(*n),
        Value::I8(n) => SetValue::I8(*n),
        Value::U64(n) => SetValue::U64(*n),
        Value::U32(n) => SetValue::U32(*n),
        Value::U16(n) => SetValue::U16(*n),
        Value::U8(n) => SetValue::U8(*n),
        Value::Bool(b) => SetValue::Bool(*b),
        Value::EnumValue(enum_name, variant, _) => {
            SetValue::EnumValue(enum_name.clone(), variant.clone())
        }
        _ => panic!("Cannot add unhashable value to set"),
    }
}

/// For chained method calls, recurse until you get to the var
fn find_root_var(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Var(name) => Some(name.clone()),
        Expr::MethodCall { object, .. } => find_root_var(object),
        _ => None,
    }
}

fn infer_value_type(v: &Value) -> Type {
    match v {
        Value::I64(_) => Type::I64,
        Value::I32(_) => Type::I32,
        Value::I16(_) => Type::I16,
        Value::I8(_) => Type::I8,
        Value::U64(_) => Type::U64,
        Value::U32(_) => Type::U32,
        Value::U16(_) => Type::U16,
        Value::U8(_) => Type::U8,
        Value::F64(_) => Type::F64,
        Value::F32(_) => Type::F32,
        Value::String(_) => Type::Str,
        Value::Bool(_) => Type::Bool,
        Value::Array(arr) => {
            if arr.is_empty() {
                Type::Array(Box::new(Type::I64)) // default
            } else {
                Type::Array(Box::new(infer_value_type(&arr[0])))
            }
        },
        Value::Function(fn_def) => Type::Function {
            params: fn_def.params.iter()
                .map(|(_, ty, _)| ty.clone().unwrap_or(Type::Void))
                .collect(),
            return_type: Box::new(fn_def.return_type.clone().unwrap_or(Type::Void)),
        },

        Value::Closure(closure) => Type::Function {
            params: closure.def.params.iter()
                .map(|(_, ty, _)| ty.clone().unwrap_or(Type::Void))
                .collect(),
            return_type: Box::new(closure.def.return_type.clone().unwrap_or(Type::Void)),
        },
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
        Value::Channel(_, _, inner_type) => Type::Channel(Box::new(inner_type.clone())),
        Value::StateActor(_, _, inner_type) => Type::StateActor(Box::new(inner_type.clone())),
        Value::Namespace(name) => Type::Namespace(name.clone()),
        Value::Map(_, key_type, val_type) => {
            Type::Map(Box::new(key_type.clone()), Box::new(val_type.clone()))
        }
        Value::Set(_, val_type) => Type::Set(Box::new(val_type.clone())),
    }
}

fn get_ownership(e: &Expr, env: &HashMap<String, (AssignKind, Value, Ownership, Moved, usize)>) -> Ownership {
    if is_literal(e) { 
        return Ownership::Owned;
    }
    
    match e {
        Expr::Var(name) => {
            env.get(name).map(|(_, _, o, _, _)| o.clone()).unwrap_or(Ownership::Borrowed)
        }
        // Expressions that produce new values are owned
        Expr::Add(_, _) |
        Expr::Subtract(_, _) |
        Expr::Multiply(_, _) |
        Expr::Divide(_, _) |
        Expr::Negative(_) |
        Expr::Power(_, _) |
        Expr::Modulo(_, _) |
        Expr::InterpolatedString(_) |
        Expr::CreateMap( .. ) |
        Expr::CreateSet( .. ) |
        Expr::CreateChannel(_) |
        Expr::CreateState(_) |
        Expr::Call { .. } |
        Expr::MethodCall { .. } => Ownership::Owned,
        _ => Ownership::Borrowed,
    }
}

fn is_copy_type(t: &Type) -> bool {
    matches!(t, Type::Bool) || is_numeric_type(t)
}

fn match_pattern(
    val: &Value,
    pattern: &Pattern,
    env: &HashMap<String, (AssignKind, Value, Ownership, Moved, usize)>,
) -> Option<HashMap<String, (AssignKind, Value, Ownership, Moved, usize)>> {
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
                Expr::I16(n) => {
                    if let Value::I16(val_n) = val {
                        if val_n == n {
                            return Some(new_env);
                        }
                    }
                }
                Expr::I8(n) => {
                    if let Value::I8(val_n) = val {
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
                Expr::U16(n) => {
                    if let Value::U16(val_n) = val {
                        if val_n == n {
                            return Some(new_env);
                        }
                    }
                }
                Expr::U8(n) => {
                    if let Value::U8(val_n) = val {
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
                                (AssignKind::Var, (**data_val).clone(), Ownership::Owned, Moved::False, 0),
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
        
        // Validate each param
        for (i, req_param) in required_method.params.iter().enumerate() {
            let (req_name, req_type, _) = req_param;
            
            // Check if impl has this param
            let impl_param = match impl_method.params.get(i) {
                Some(p) => p,
                None => {
                    panic!(
                        "Method '{}' missing required param '{}' at position {}",
                        required_method.name, req_name, i
                    );
                }
            };
            
            let (impl_name, impl_type, _) = impl_param;
            
            // Check param name
            if req_name != impl_name {
                panic!(
                    "Method '{}' param {} name mismatch: expected '{}', got '{}'",
                    required_method.name, i, req_name, impl_name
                );
            }
            
            // Check param type (if role specifies one)
            if let Some(expected_type) = req_type {
                match impl_type {
                    Some(actual_type) => {
                        if expected_type != actual_type {
                            panic!(
                                "Method '{}' param '{}' type mismatch: expected {:?}, got {:?}",
                                required_method.name, req_name, expected_type, actual_type
                            );
                        }
                    }
                    None => {
                        panic!(
                            "Method '{}' param '{}' missing type annotation, expected {:?}",
                            required_method.name, req_name, expected_type
                        );
                    }
                }
            }
        }
        
        // Validate return type
        match (&required_method.return_type, &impl_method.return_type) {
            (Some(expected), Some(actual)) => {
                if expected != actual {
                    panic!(
                        "Method '{}' return type mismatch: expected {:?}, got {:?}",
                        required_method.name, expected, actual
                    );
                }
            }
            (Some(expected), None) => {
                panic!(
                    "Method '{}' missing return type, expected {:?}",
                    required_method.name, expected
                );
            }
            (None, _) => {
                // Role doesn't specify return type, any is fine
            }
        }
    }
}

async fn execute_callable(
    callable: Callable,
    name: &str,
    args: &[Expr],
    ctx: &mut RuntimeContext,
) -> Value {
    match callable {
        Callable::UserDefined(fn_def) => {
            // Create new scope for function
            let mut fn_env: HashMap<String, (AssignKind, Value, Ownership, Moved, usize)> = HashMap::new();
            let fn_arg_count = fn_def.params.len();
            
            // Bind parameters
            for (i, (param_name, param_type, param_ownership)) in fn_def.params.iter().enumerate() {
                let arg_expr = &args[i];
                let ownership = get_ownership(arg_expr, &ctx.env);

                let arg_val = if i < args.len() {
                    eval_expr(arg_expr, ctx).await
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
                        if let Some((k, v, o, _, sd)) = ctx.env.get(var_name) {
                            ctx.env.insert(var_name.clone(), (k.clone(), v.clone(), o.clone(), Moved::True, sd.clone()));
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

                fn_env.insert(param_name.clone(), (AssignKind::Var, arg_val, ownership, Moved::False, ctx.scope_depth + 1));
            }
            
            // Execute function body
            if let Some(body) = &fn_def.body {
                let mut fn_ctx = ctx.fork_with_env(fn_env);
                fn_ctx.scope_depth = ctx.scope_depth + 1;  // Nested function = deeper scope
                let flow = execute_block(body, &mut fn_ctx, fn_def.return_type.as_ref()).await;
                match flow {
                    ControlFlow::Return(val) => val,
                    _ => Value::Void,
                }
            } else {
                panic!("Cannot call function '{}' as it has no body.", name);
            }
        }
        Callable::Builtin(func) => {
            let mut eval_args = Vec::new();
            for arg in args {
                eval_args.push(eval_expr(arg, ctx).await);
            }
            func(eval_args).unwrap_or(Value::Void)
        }
    }
}

#[async_recursion]
async fn call_closure(
    closure: &ClosureValue,
    args: &[Expr],
    ctx: &mut RuntimeContext,
) -> Value {
    // Create new scope starting with captured environment
    let mut closure_ctx = ctx.fork_with_env(closure.captured_env.clone());

    // For vars, get current values from outer_env
    // (in case they've been updated since closure creation)
    for var_name in &closure.live_vars {
        if let Some(current_value) = ctx.env.get(var_name) {
            closure_ctx.env.insert(var_name.clone(), current_value.clone());
        }
    }
    
    // Bind parameters
    for (i, (param_name, param_type, param_ownership)) in closure.def.params.iter().enumerate() {
        if i < args.len() {
            let arg_expr = &args[i];
            let ownership = get_ownership(arg_expr, &ctx.env);
            let arg_val = eval_expr(&arg_expr, ctx).await;

            // Only error if param expects Owned but arg is Borrowed
            if *param_ownership == Ownership::Owned && ownership == Ownership::Borrowed {
                panic!("Closure param '{}' expects owned, but got borrowed", param_name);
            }
            
            // Mark as moved if param expects Owned
            if *param_ownership == Ownership::Owned {
                if let Expr::Var(var_name) = arg_expr {
                    if let Some((k, v, o, _, sd)) = ctx.env.get(var_name) {
                        ctx.env.insert(var_name.clone(), (k.clone(), v.clone(), o.clone(), Moved::True, sd.clone()));
                    }
                }
            }

            // Validate parameter type
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

            closure_ctx.env.insert(
                param_name.clone(),
                (AssignKind::Const, arg_val, ownership, Moved::False, 0),
            );
        } else {
            panic!("Closure expects {} arguments, got {}", closure.def.params.len(), args.len());
        }
    }
    
    // Execute closure body
    let flow = execute_block(
        &closure.def.body,
        &mut closure_ctx,
        closure.def.return_type.as_ref(),
    ).await;

    // Sync all vars back (they're live references now)
    for updated_var in &closure.live_vars {
        if let Some(value) = closure_ctx.env.get(updated_var) {
            ctx.env.insert(updated_var.clone(), value.clone());
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
            Stmt::Assign { value, .. } | Stmt::Reassign { value, .. } => {
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
        Expr::Var(name) | Expr::Const(name) | Expr::Static(name) => {
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
        Expr::Call { name, args } => {
            // The call target might be a variable holding a function
            if !vars.contains(name) {
                vars.push(name.clone());
            }

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
        Expr::Thread(e) => {
            collect_vars_from_expr(e, vars);
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
        Expr::CreateState(expr) => {
            collect_vars_from_expr(expr, vars);
        }
        Expr::CompoundAssign { name, op: _, value } => {
            vars.push(name.clone());
            collect_vars_from_expr(value, vars);
        }
        Expr::Assign { name, value } => {
            vars.push(name.clone());
            collect_vars_from_expr(value, vars);
        }
        _ => {}
    }
}

struct ThreadVarAnalysis {
    directly_assigned: HashSet<String>,
    state_method_calls: HashSet<String>,
}

fn analyze_thread_closure(stmts: &[Stmt]) -> ThreadVarAnalysis {
    let mut analysis = ThreadVarAnalysis {
        directly_assigned: HashSet::new(),
        state_method_calls: HashSet::new(),
    };
    for stmt in stmts {
        analyze_stmt(stmt, &mut analysis);
    }
    analysis
}

fn analyze_stmt(stmt: &Stmt, analysis: &mut ThreadVarAnalysis) {
    match stmt {
        Stmt::Reassign { name, value } => {
            analysis.directly_assigned.insert(name.clone());
            analyze_expr(value, analysis);
        }
        Stmt::Assign { value, .. } => analyze_expr(value, analysis),
        Stmt::ExprStmt(expr) | Stmt::Echo(expr) | Stmt::Print(expr) => {
            analyze_expr(expr, analysis);
        }
        Stmt::Return(Some(expr)) => analyze_expr(expr, analysis),
        Stmt::Return(None) | Stmt::Break | Stmt::Next => {}
        Stmt::If { condition, then_block, else_ifs, else_block } => {
            analyze_expr(condition, analysis);
            for s in then_block { analyze_stmt(s, analysis); }
            for (cond, block) in else_ifs {
                analyze_expr(cond, analysis);
                for s in block { analyze_stmt(s, analysis); }
            }
            if let Some(else_b) = else_block {
                for s in else_b { analyze_stmt(s, analysis); }
            }
        }
        Stmt::Match { expr, cases, default, .. } => {
            analyze_expr(expr, analysis);
            for (_, body) in cases {
                for s in body { analyze_stmt(s, analysis); }
            }
            if let Some(d) = default {
                for s in d { analyze_stmt(s, analysis); }
            }
        }
        Stmt::While { condition, body }
        | Stmt::WhileOnce { condition, body }
        | Stmt::Until { condition, body }
        | Stmt::UntilOnce { condition, body } => {
            analyze_expr(condition, analysis);
            for s in body { analyze_stmt(s, analysis); }
        }
        Stmt::For { binding: _, index_binding: _, iterable, body } => {
            match iterable {
                ForIterable::Collection(expr) => analyze_expr(expr, analysis),
                ForIterable::CountReceive(channel, count) => {
                    analyze_expr(channel, analysis);
                    analyze_expr(count, analysis);
                }
                ForIterable::MaybeReceive(channel) => analyze_expr(channel, analysis),
            }
            for s in body { analyze_stmt(s, analysis); }
        }
        Stmt::FnDef(_) | Stmt::ModelDef(_) | Stmt::RoleDef(_)
        | Stmt::ExtendModel { .. } | Stmt::EnumDef(_)
        | Stmt::Import(_) | Stmt::Export(_) => {}
    }
}

fn analyze_expr(expr: &Expr, analysis: &mut ThreadVarAnalysis) {
    match expr {
        Expr::Assign { name, value } | Expr::CompoundAssign { name, value, .. } => {
            analysis.directly_assigned.insert(name.clone());
            analyze_expr(value, analysis);
        }
        Expr::MethodCall { object, method, args } => {
            if matches!(method.as_str(), "update" | "write" | "read") {
                if let Expr::Var(var_name) = object.as_ref() {
                    analysis.state_method_calls.insert(var_name.clone());
                }
            }
            analyze_expr(object, analysis);
            for arg in args { analyze_expr(arg, analysis); }
        }
        Expr::Add(l, r) | Expr::Subtract(l, r) | Expr::Multiply(l, r)
        | Expr::Divide(l, r) | Expr::Power(l, r) | Expr::Modulo(l, r)
        | Expr::Equal(l, r) | Expr::NotEqual(l, r)
        | Expr::LessThan(l, r) | Expr::LessThanOrEqual(l, r)
        | Expr::GreaterThan(l, r) | Expr::GreaterThanOrEqual(l, r) => {
            analyze_expr(l, analysis);
            analyze_expr(r, analysis);
        }
        Expr::Not(e) | Expr::Negative(e) | Expr::Receive(e)
        | Expr::MaybeReceive(e) | Expr::Thread(e) | Expr::CreateState(e) => {
            analyze_expr(e, analysis);
        }
        Expr::CountReceive(e, _) => analyze_expr(e, analysis),
        Expr::Array(exprs) => {
            for e in exprs { analyze_expr(e, analysis); }
        }
        Expr::Call { args, .. } => {
            for arg in args { analyze_expr(arg, analysis); }
        }
        Expr::FieldAccess { object, value, .. } => {
            analyze_expr(object, analysis);
            if let Some(v) = value { analyze_expr(v, analysis); }
        }
        Expr::ModelInstance { fields, .. } => {
            for (_, e) in fields { analyze_expr(e, analysis); }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let InterpolationPart::Expression(e) = part {
                    analyze_expr(e, analysis);
                }
            }
        }
        Expr::Send(a, b) => {
            analyze_expr(a, analysis);
            analyze_expr(b, analysis);
        }
        Expr::EnumVariant { data: Some(e), .. } => analyze_expr(e, analysis),
        Expr::CreateMap(pairs, _, _) => {
            for (key_expr, val_expr) in pairs {
                analyze_expr(key_expr, analysis);
                analyze_expr(val_expr, analysis);
            }
        }
        Expr::CreateSet(values, _) => {
            for val_expr in values {
                analyze_expr(val_expr, analysis);
            }
        }
        // Do not analyze
        Expr::I64(_) | Expr::I32(_) | Expr::I16(_) | Expr::I8(_)
        | Expr::U64(_) | Expr::U32(_) | Expr::U16(_) | Expr::U8(_)
        | Expr::F64(_) | Expr::F32(_) | Expr::String(_) | Expr::Bool(_)
        | Expr::Var(_) | Expr::Const(_) | Expr::Static(_) | Expr::Regex(_)
        | Expr::CreateChannel(_) | Expr::EnumVariant { data: None, .. } 
        | Expr::Closure(_) => {}
    }
}

#[async_recursion]
async fn eval_expr(
    e: &Expr,
    ctx: &mut RuntimeContext,
) -> Value {
    match e {
        Expr::I64(n) => Value::I64(*n),
        Expr::I32(n) => Value::I32(*n),
        Expr::I16(n) => Value::I16(*n),
        Expr::I8(n) => Value::I8(*n),
        Expr::U64(n) => Value::U64(*n),
        Expr::U32(n) => Value::U32(*n),
        Expr::U16(n) => Value::U16(*n),
        Expr::U8(n) => Value::U8(*n),
        Expr::F64(n) => Value::F64(*n),
        Expr::F32(n) => Value::F32(*n),
        Expr::InterpolatedString(parts) => {
            let mut result = String::new();
            for part in parts {
                match part {
                    InterpolationPart::Literal(s) => result.push_str(s),
                    InterpolationPart::Expression(expr) => {
                        let val = eval_expr(expr, ctx).await;
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
            for ex in exprs {
                if let Expr::Var(var_name) = ex {
                    // Try to resolve as a function first
                    if let Some(callable) = ctx.fns.get(var_name) {
                        if let Callable::UserDefined(fn_def) = callable {
                            values.push(Value::Function(fn_def.clone()));
                            continue;
                        }
                    }
                }
                values.push(eval_expr(ex, ctx).await);
            }
            Value::Array(values)
        },
        Expr::Not(e) => {
            let val = eval_expr(e, ctx).await;
            Value::Bool(!is_truthy(&val))
        },
        Expr::Add(a, b) => {
            match (eval_expr(a, ctx).await, eval_expr(b, ctx).await) {
                (Value::I64(x), Value::I64(y)) => Value::I64(x + y),
                (Value::I32(x), Value::I32(y)) => Value::I32(x + y),
                (Value::I16(x), Value::I16(y)) => Value::I16(x + y),
                (Value::I8(x), Value::I8(y)) => Value::I8(x + y),
                (Value::U64(x), Value::U64(y)) => Value::U64(x + y),
                (Value::U32(x), Value::U32(y)) => Value::U32(x + y),
                (Value::U16(x), Value::U16(y)) => Value::U16(x + y),
                (Value::U8(x), Value::U8(y)) => Value::U8(x + y),
                (Value::F64(x), Value::F64(y)) => Value::F64(x + y),
                (Value::F32(x), Value::F32(y)) => Value::F32(x + y),
                (Value::String(x), Value::String(y)) => Value::String(format!("{}{}", x, y)),
                _ => panic!("Type error: cannot add these types"),
            }
        }
        Expr::Subtract(a, b) => {
            match (eval_expr(a, ctx).await, eval_expr(b, ctx).await) {
                (Value::I64(x), Value::I64(y)) => Value::I64(x - y),
                (Value::I32(x), Value::I32(y)) => Value::I32(x - y),
                (Value::I16(x), Value::I16(y)) => Value::I16(x - y),
                (Value::I8(x), Value::I8(y)) => Value::I8(x - y),
                (Value::U64(x), Value::U64(y)) => Value::U64(x - y),
                (Value::U32(x), Value::U32(y)) => Value::U32(x - y),
                (Value::U16(x), Value::U16(y)) => Value::U16(x - y),
                (Value::U8(x), Value::U8(y)) => Value::U8(x - y),
                (Value::F64(x), Value::F64(y)) => Value::F64(x - y),
                (Value::F32(x), Value::F32(y)) => Value::F32(x - y),
                _ => panic!("Type error: cannot subtract non-numbers"),
            }
        }
        Expr::Multiply(a, b) => {
            match (eval_expr(a, ctx).await, eval_expr(b, ctx).await) {
                (Value::I64(x), Value::I64(y)) => Value::I64(x * y),
                (Value::I32(x), Value::I32(y)) => Value::I32(x * y),
                (Value::I16(x), Value::I16(y)) => Value::I16(x * y),
                (Value::I8(x), Value::I8(y)) => Value::I8(x * y),
                (Value::U64(x), Value::U64(y)) => Value::U64(x * y),
                (Value::U32(x), Value::U32(y)) => Value::U32(x * y),
                (Value::U16(x), Value::U16(y)) => Value::U16(x * y),
                (Value::U8(x), Value::U8(y)) => Value::U8(x * y),
                (Value::F64(x), Value::F64(y)) => Value::F64(x * y),
                (Value::F32(x), Value::F32(y)) => Value::F32(x * y),
                _ => panic!("Type error: cannot multiply non-numbers"),
            }
        }
        Expr::Divide(a, b) => {
            match (eval_expr(a, ctx).await, eval_expr(b, ctx).await) {
                (Value::I64(x), Value::I64(y)) => Value::I64(x / y),
                (Value::I32(x), Value::I32(y)) => Value::I32(x / y),
                (Value::I16(x), Value::I16(y)) => Value::I16(x / y),
                (Value::I8(x), Value::I8(y)) => Value::I8(x / y),
                (Value::U64(x), Value::U64(y)) => Value::U64(x / y),
                (Value::U32(x), Value::U32(y)) => Value::U32(x / y),
                (Value::U16(x), Value::U16(y)) => Value::U16(x / y),
                (Value::U8(x), Value::U8(y)) => Value::U8(x / y),
                (Value::F64(x), Value::F64(y)) => Value::F64(x / y),
                (Value::F32(x), Value::F32(y)) => Value::F32(x / y),
                _ => panic!("Type error: cannot divide non-numbers"),
            }
        }
        Expr::Negative(e) => {
            match eval_expr(e, ctx).await {
                Value::I64(x) => Value::I64(-x),
                Value::I32(x) => Value::I32(-x),
                Value::I16(x) => Value::I16(-x),
                Value::I8(x) => Value::I8(-x),
                Value::F64(x) => Value::F64(-x),
                Value::F32(x) => Value::F32(-x),
                Value::U64(_) | Value::U32(_) | Value::U16(_) | Value::U8(_) => {
                    panic!("Type error: cannot negate unsigned integer")
                },
                _ => panic!("Type error: cannot negate non-number"),
            }
        }
        Expr::Power(a, b) => {
            match (eval_expr(a, ctx).await, eval_expr(b, ctx).await) {
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
                (Value::I16(x), Value::I16(y)) => {
                    if y < 0 {
                        panic!("Exponent cannot be negative for i16: {}", y);
                    }
                    Value::I16(x.pow(y as u32))
                }
                (Value::I8(x), Value::I8(y)) => {
                    if y < 0 {
                        panic!("Exponent cannot be negative for i8: {}", y);
                    }
                    Value::I8(x.pow(y as u32))
                }
                (Value::U64(x), Value::U64(y)) => {
                    if y > u32::MAX as u64 {
                        panic!("Exponent out of range for u64: {}", y);
                    }
                    Value::U64(x.pow(y as u32))
                }
                (Value::U32(x), Value::U32(y)) => Value::U32(x.pow(y as u32)),
                (Value::U16(x), Value::U16(y)) => Value::U16(x.pow(y as u32)),
                (Value::U8(x), Value::U8(y)) => Value::U8(x.pow(y as u32)),
                (Value::F64(x), Value::F64(y)) => Value::F64(x.powf(y)),
                (Value::F32(x), Value::F32(y)) => Value::F32(x.powf(y)),
                _ => panic!("Type error: cannot exponentiate non-numbers"),
            }
        }
        Expr::Modulo(a, b) => {
            match (eval_expr(a, ctx).await, eval_expr(b, ctx).await) {
                (Value::I64(x), Value::I64(y)) => Value::I64(x % y),
                (Value::I32(x), Value::I32(y)) => Value::I32(x % y),
                (Value::I16(x), Value::I16(y)) => Value::I16(x % y),
                (Value::I8(x), Value::I8(y)) => Value::I8(x % y),
                (Value::U64(x), Value::U64(y)) => Value::U64(x % y),
                (Value::U32(x), Value::U32(y)) => Value::U32(x % y),
                (Value::U16(x), Value::U16(y)) => Value::U16(x % y),
                (Value::U8(x), Value::U8(y)) => Value::U8(x % y),
                (Value::F64(x), Value::F64(y)) => Value::F64(x % y),
                (Value::F32(x), Value::F32(y)) => Value::F32(x % y),
                _ => panic!("Type error: cannot modulo non-numbers"),
            }
        }
        Expr::Equal(a, b) => {
            let av = eval_expr(a, ctx).await;
            let bv = eval_expr(b, ctx).await;
            Value::Bool(av == bv)
        }
        Expr::NotEqual(a, b) => {
            let av = eval_expr(a, ctx).await;
            let bv = eval_expr(b, ctx).await;
            Value::Bool(av != bv)
        }
        Expr::LessThan(a, b) => {
            match (eval_expr(a, ctx).await, eval_expr(b, ctx).await) {
                (Value::I64(x), Value::I64(y)) => Value::Bool(x < y),
                (Value::I32(x), Value::I32(y)) => Value::Bool(x < y),
                (Value::I16(x), Value::I16(y)) => Value::Bool(x < y),
                (Value::I8(x), Value::I8(y)) => Value::Bool(x < y),
                (Value::U64(x), Value::U64(y)) => Value::Bool(x < y),
                (Value::U32(x), Value::U32(y)) => Value::Bool(x < y),
                (Value::U16(x), Value::U16(y)) => Value::Bool(x < y),
                (Value::U8(x), Value::U8(y)) => Value::Bool(x < y),
                (Value::F64(x), Value::F64(y)) => Value::Bool(x < y),
                (Value::F32(x), Value::F32(y)) => Value::Bool(x < y),
                _ => panic!("Type error: cannot compare non-numbers"),
            }
        }
        Expr::LessThanOrEqual(a, b) => {
            match (eval_expr(a, ctx).await, eval_expr(b, ctx).await) {
                (Value::I64(x), Value::I64(y)) => Value::Bool(x <= y),
                (Value::I32(x), Value::I32(y)) => Value::Bool(x <= y),
                (Value::I16(x), Value::I16(y)) => Value::Bool(x <= y),
                (Value::I8(x), Value::I8(y)) => Value::Bool(x <= y),
                (Value::U64(x), Value::U64(y)) => Value::Bool(x <= y),
                (Value::U32(x), Value::U32(y)) => Value::Bool(x <= y),
                (Value::U16(x), Value::U16(y)) => Value::Bool(x <= y),
                (Value::U8(x), Value::U8(y)) => Value::Bool(x <= y),
                (Value::F64(x), Value::F64(y)) => Value::Bool(x <= y),
                (Value::F32(x), Value::F32(y)) => Value::Bool(x <= y),
                _ => panic!("Type error: cannot compare non-numbers"),
            }
        }
        Expr::GreaterThan(a, b) => {
            match (eval_expr(a, ctx).await, eval_expr(b, ctx).await) {
                (Value::I64(x), Value::I64(y)) => Value::Bool(x > y),
                (Value::I32(x), Value::I32(y)) => Value::Bool(x > y),
                (Value::I16(x), Value::I16(y)) => Value::Bool(x > y),
                (Value::I8(x), Value::I8(y)) => Value::Bool(x > y),
                (Value::U64(x), Value::U64(y)) => Value::Bool(x > y),
                (Value::U32(x), Value::U32(y)) => Value::Bool(x > y),
                (Value::U16(x), Value::U16(y)) => Value::Bool(x > y),
                (Value::U8(x), Value::U8(y)) => Value::Bool(x > y),
                (Value::F64(x), Value::F64(y)) => Value::Bool(x > y),
                (Value::F32(x), Value::F32(y)) => Value::Bool(x > y),
                _ => panic!("Type error: cannot compare non-numbers"),
            }
        }
        Expr::GreaterThanOrEqual(a, b) => {
            match (eval_expr(a, ctx).await, eval_expr(b, ctx).await) {
                (Value::I64(x), Value::I64(y)) => Value::Bool(x >= y),
                (Value::I32(x), Value::I32(y)) => Value::Bool(x >= y),
                (Value::I16(x), Value::I16(y)) => Value::Bool(x >= y),
                (Value::I8(x), Value::I8(y)) => Value::Bool(x >= y),
                (Value::U64(x), Value::U64(y)) => Value::Bool(x >= y),
                (Value::U32(x), Value::U32(y)) => Value::Bool(x >= y),
                (Value::U16(x), Value::U16(y)) => Value::Bool(x >= y),
                (Value::U8(x), Value::U8(y)) => Value::Bool(x >= y),
                (Value::F64(x), Value::F64(y)) => Value::Bool(x >= y),
                (Value::F32(x), Value::F32(y)) => Value::Bool(x >= y),
                _ => panic!("Type error: cannot compare non-numbers"),
            }
        }
        Expr::Assign { name, value } => {
            let v = eval_expr(value, ctx).await;
            ctx.env.insert(name.clone(), (AssignKind::Var, v.clone(), Ownership::Owned, Moved::False, 0));
            v
        }
        Expr::CompoundAssign { name, op: _, value } => {
            let v = eval_expr(value, ctx).await;
            ctx.env.insert(name.clone(), (AssignKind::Var, v.clone(), Ownership::Owned, Moved::False, 0));
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
            if let Some((_, v, ownership, moved, _)) = ctx.env.get(name) {
                if let Moved::True = *moved {
                    panic!("Variable '{}' has been moved", name);
                }
                // Auto-unwrap StateActor for reading (call get())
                if let Value::StateActor(tx, rx, inner_type) = v {
                    let identity = make_identity_closure(inner_type, ownership);
                    tx.send(identity).await.expect("State actor closed");
                    let mut rx_guard = rx.lock().await;
                    return rx_guard.recv().await.expect("State actor closed");
                }
                v.clone()
            } else if let Some(Callable::UserDefined(fn_def)) = ctx.fns.get(name) {
                // If it's a function in fns, return it as a Value::Function
                Value::Function(fn_def.clone())
            } else if let Some(Callable::Builtin(_builtin)) = ctx.fns.get(name) {
                // Decide: can builtins be passed as values?
                panic!("Cannot use builtin '{}' as a value", name)
            } else {
                // Check if it's a known enum variant
                let mut found_enum = false;
                let mut enum_name = String::new();
                
                for (enum_def_name, enum_def) in ctx.enums.iter() {
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
            if let Some((_, v, _ownership, moved, _scope_depth)) = ctx.env.get(name) {
                if let Moved::True = *moved {
                    panic!("Constant '{}' has been moved", name);
                }
                v.clone()
            } else if let Some(Callable::UserDefined(fn_def)) = ctx.fns.get(name) {
                // If it's a function in fns, return it as a Value::Function
                Value::Function(fn_def.clone())
            } else if let Some(Callable::Builtin(_builtin)) = ctx.fns.get(name) {
                // Decide: can builtins be passed as values?
                panic!("Cannot use builtin '{}' as a value", name)
            } else {
                panic!("Undefined constant: {}", name);
            }
        },
        Expr::Static(name) => {
            if let Some((_, v, _ownership, _moved, _scope_depth)) = ctx.env.get(name) {
                v.clone()
            } else {
                panic!("Undefined static: {}", name);
            }
        },
        Expr::Call { name, args } if name == "thread" => {
            // TODO - I think this is dead code
            eprintln!("!!!!!!!WRONG JASON: Expr::Call > thread IS ALIVE");
            if args.len() != 1 {
                panic!("thread() expects exactly 1 argument (a closure)");
            }
            
            let closure_val = eval_expr(&args[0], ctx).await;
            
            if let Value::Closure(closure) = closure_val {
                let mut thread_ctx = ctx.fork();
                let thread_closure = closure.clone();

                tokio::spawn(async move {
                    call_closure(
                        &thread_closure,
                        &[], // No args passed to thread fn
                        &mut thread_ctx,
                    ).await;
                });
                
                Value::Void
            } else {
                panic!("thread() argument must be a closure");
            }
        }
        Expr::Thread(target) => {
            let val = eval_expr(target, ctx).await;

            // We expect the target to be a closure
            if let Value::Closure(closure) = val {
                let analysis = analyze_thread_closure(&closure.def.body);

                // Check direct assignments
                for var_name in &analysis.directly_assigned {
                    if let Some((kind, value, _, _, _)) = ctx.env.get(var_name) {
                        if matches!(kind, AssignKind::Var) {
                            if !matches!(value, Value::StateActor(_, _, _)) {
                                panic!(
                                    "To mutate variable '{}' in thread(), use 'var {} = state(value)",
                                    var_name, var_name
                                );
                            } else {
                                panic!(
                                    "Cannot directly mutate variable '{}' in thread(); use {}.update() or {}.set()",
                                    var_name, var_name, var_name
                                );
                            }
                        }
                    }
                }

                // Check .update()/.set()/.get() calls on non-StateActor vars
                for var_name in &analysis.state_method_calls {
                    if let Some((_, value, _, _, _)) = ctx.env.get(var_name) {
                        if !matches!(value, Value::StateActor(_, _, _)) {
                            panic!(
                                "Cannot call .update()/.set()/.get() on '{}'; use 'var {} = state(value)' to create shared state",
                                var_name, var_name
                            );
                        }
                    }
                }

                // Get return type from closure, default to Void
                let return_type = closure.def.return_type.clone().unwrap_or(Type::Void);

                // Create channel for the thread's result
                let (tx, rx) = mpsc::channel::<Value>(1);
                let thread_tx = tx.clone();

                let mut thread_ctx = ctx.fork();
                let thread_closure = closure.clone();

                tokio::spawn(async move {
                    let result = call_closure(
                        &thread_closure,
                        &[], // Spawned threads take no arguments
                        &mut thread_ctx,
                    ).await;

                    // Send the closure's return value to the channel
                    let _ = thread_tx.send(result).await;
                });

                // Return channel that will receive the thread's result
                Value::Channel(tx, Arc::new(Mutex::new(rx)), return_type)
            } else {
                panic!("thread expects a closure");
            }
        }
        Expr::CreateChannel(hint) => {
            // We already know hint is Some(...) because of strict mode check in infer_expr_type
            // But let's be safe and unwrap or default (strict mode logic handles panic elsewhere)
            let inner_type = hint.clone().expect("Channel must be typed");

            let (tx, rx) = mpsc::channel(32);
            Value::Channel(tx, Arc::new(Mutex::new(rx)), inner_type)
        }
        Expr::Send(chan_expr, val_expr) => {
            let chan_val = eval_expr(chan_expr, ctx).await;
            let val = eval_expr(val_expr, ctx).await;

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
            let chan_val = eval_expr(chan_expr, ctx).await;

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
        Expr::CountReceive(chan_expr, count_expr) => {
            let count_val = eval_expr(count_expr, ctx).await;
            let count = match count_val {
                Value::I64(n) => n as usize,
                Value::U64(n) => n as usize,
                Value::I32(n) => n as usize,
                Value::U32(n) => n as usize,
                Value::I16(n) => n as usize,
                Value::U16(n) => n as usize,
                Value::I8(n) => n as usize,
                Value::U8(n) => n as usize,
                _ => panic!("Count must be an integer"),
            };

            let chan_val = eval_expr(chan_expr, ctx).await;

            if let Value::Channel(_, rx_mutex, _) = chan_val {
                let mut rx = rx_mutex.lock().await;
                let mut messages = Vec::new();

                for _ in 0..count {
                    if let Some(val) = rx.recv().await {
                        messages.push(val);
                    } else {
                        panic!("Channel closed before receiving {} values", count);
                    }
                }
                Value::Array(messages)
            } else {
                panic!("Cannot receive from non-channel type");
            }
        }
        Expr::MaybeReceive(chan_expr) => {
            let chan_val = eval_expr(chan_expr, ctx).await;

            if let Value::Channel(_, rx_arc, _) = chan_val {
                loop {
                    let result = {
                        let mut rx = rx_arc.lock().await;
                        rx.try_recv()
                    }; // Lock released here
                    
                    match result {
                        Ok(val) => {
                            return Value::EnumValue("Maybe".to_string(), "Value".to_string(), Some(Box::new(val)));
                        }
                        Err(tokio::sync::mpsc::error::TryRecvError::Empty) => {
                            // Check if channel is closed
                            let is_closed = {
                                let rx = rx_arc.lock().await;
                                rx.is_closed()
                            };
                            if is_closed {
                                return Value::EnumValue("Maybe".to_string(), "Null".to_string(), None);
                            }
                            // Yield and retry
                            tokio::task::yield_now().await;
                        }
                        Err(tokio::sync::mpsc::error::TryRecvError::Disconnected) => {
                            return Value::EnumValue("Maybe".to_string(), "Null".to_string(), None);
                        }
                    }
                }
            } else {
                panic!("Runtime Error: Cannot receive from non-channel type");
            }
        }
        Expr::CreateState(initial_expr) => {
            let initial = eval_expr(initial_expr, ctx).await;
            
            let (cmd_tx, mut cmd_rx) = mpsc::channel::<Value>(32);
            let (resp_tx, resp_rx) = mpsc::channel::<Value>(32);
            
            let actor_ctx = ctx.fork();
            let inner_type = infer_value_type(&initial);
            let inner_type_clone = inner_type.clone();
            
            tokio::spawn(async move {
                let mut current = initial;
                
                while let Some(cmd) = cmd_rx.recv().await {
                    if let Value::Closure(closure) = cmd {
                        let mut closure_env = closure.captured_env.clone();

                        let param_name = closure.def.params.first()
                            .map(|(name, _, _)| name.clone());
                        
                        if let Some(ref name) = param_name {
                            closure_env.insert(
                                name.clone(),
                                (AssignKind::Var, current.clone(), Ownership::Owned, Moved::False, 0)
                            );
                        }
                        
                        let mut fn_ctx = actor_ctx.fork_with_env(closure_env);
                        let flow = execute_block(&closure.def.body, &mut fn_ctx, None).await;
                        
                        // Use the mutated parameter value as the new state
                        let result = match flow {
                            ControlFlow::Return(val) => {
                                if !matches!(val, Value::Void) && infer_value_type(&val) == inner_type_clone {
                                    val
                                } else {
                                    param_name.as_ref()
                                        .and_then(|name| fn_ctx.env.get(name))
                                        .map(|(_, v, _, _, _)| v.clone())
                                        .unwrap_or(current.clone())
                                }
                            }
                            _ => {
                                param_name.as_ref()
                                    .and_then(|name| fn_ctx.env.get(name))
                                    .map(|(_, v, _, _,_ )| v.clone())
                                    .unwrap_or(current.clone())
                            }
                        };

                        current = result.clone();
                        let _ = resp_tx.send(result).await;
                    }
                }
            });
            
            Value::StateActor(cmd_tx, Arc::new(Mutex::new(resp_rx)), inner_type)
        }
        Expr::Call { name, args } => {
            if name == "sleep" {
                if args.is_empty() {
                    panic!("sleep() requires a duration in milliseconds");
                }
                let duration = eval_expr(&args[0], ctx).await;
                let ms = match duration {
                    Value::I64(n) => n as u64,
                    Value::I32(n) => n as u64,
                    Value::I16(n) => n as u64,
                    Value::I8(n) => n as u64,
                    Value::U64(n) => n,
                    Value::U32(n) => n as u64,
                    Value::U16(n) => n as u64,
                    Value::U8(n) => n as u64,
                    _ => panic!("sleep() requires an integer (milliseconds)"),
                };
                tokio::time::sleep(tokio::time::Duration::from_millis(ms)).await;
                return Value::Void;
            }

            // Try variable lookup first (handles function references)
            if let Some((_, value, _ownership, _moved, _)) = ctx.env.get(name) {
                if let Value::Closure(closure) = value {
                    return call_closure(&closure.clone(), args, ctx).await;
                }
                if let Value::Function(fn_def) = value {
                    return execute_callable(
                        Callable::UserDefined(fn_def.clone()),
                        name,
                        args,
                        ctx,
                    ).await;
                }
            }

            // Fall back to function name lookup
            if let Some(callable) = ctx.fns.get(name) {
                return execute_callable(callable.clone(), name, args, ctx).await;
            }

            panic!("Unknown function: {}", name);
        }
        Expr::ModelInstance { name, fields } => {
            // Lookup model definition
            if let Some(model_def) = ctx.models.get(name).cloned() {
                let mut field_values = HashMap::new();
                
                // Evaluate all field expressions
                for (field_name, field_expr) in fields {
                    let field_val = eval_expr(field_expr, ctx).await;

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
            let obj_val = eval_expr(object, ctx).await;
            
            match obj_val {
                Value::Object(model_name, mut fields) => {
                    // Only update if value is provided
                    if let Some(new_val_expr) = value {
                        let new_val = eval_expr(new_val_expr, ctx).await;

                        // Check mutability
                        if let Expr::Var(var_name) = object.as_ref() {
                            if !can_mutate(var_name, &ctx.env) {
                                panic!("Cannot mutate field {:?} on immutable {:?}", field, var_name);
                            }
                        }

                        // Type check the field assignment
                        if let Some(model_def) = ctx.models.get(&model_name) {
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
                            if let Some((kind, _, ownership, _, scope_depth)) = ctx.env.get(var_name) {
                                match ownership {
                                    Ownership::Owned => {
                                        ctx.env.insert(var_name.clone(), (kind.clone(), updated_obj, ownership.clone(), Moved::False, scope_depth.clone()));
                                    }
                                    Ownership::Borrowed => {
                                        panic!("Cannot directly assign field on borrowed instance '{}'.", var_name);
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
        Expr::CreateMap(pairs, key_type, val_type) => {
            if !is_valid_type(key_type, &ctx.models, &ctx.enums) {
                panic!("Invalid map key type: {}", format_type_for_error(key_type));
            }
            
            if !is_valid_type(val_type, &ctx.models, &ctx.enums) {
                panic!("Invalid map value type: {}", format_type_for_error(val_type));
            }
            
            if !is_hashable_type(key_type) {
                panic!("Map keys must be hashable (string, number, bool, or enum), got {}", 
                    format_type_for_error(key_type));
            }

            if pairs.is_empty() {
                return Value::Map(HashMap::new(), key_type.clone(), val_type.clone());
            }
            
            let mut map = HashMap::new();
            
            // Validate all pairs match
            for (key_expr, val_expr) in pairs {
                let key_val = eval_expr(key_expr, ctx).await;
                let val = eval_expr(val_expr, ctx).await;
                
                let k_type = infer_value_type(&key_val);
                let v_type = infer_value_type(&val);
                
                if !types_compatible(key_type, &k_type) {
                    panic!("Map key type mismatch: expected {}, got {}", format_type_for_error(key_type), format_type_for_error(&k_type));
                }
                if !types_compatible(val_type, &v_type) {
                    panic!("Map value type mismatch: expected {}, got {}", format_type_for_error(val_type), format_type_for_error(&v_type));
                }
                
                let map_key = value_to_map_key(key_val);
                map.insert(map_key, val);
            }
            
            Value::Map(map, key_type.clone(), val_type.clone())
        }
        Expr::CreateSet(values, val_type) => {
            if !is_valid_type(val_type, &ctx.models, &ctx.enums) {
                panic!("Invalid declared type for set: {}", format_type_for_error(val_type));
            }

            if !is_hashable_type(val_type) {
                panic!("Non-hashable declared type for set: {}", format_type_for_error(val_type));
            }

            if values.is_empty() {
                return Value::Set(HashSet::new(), val_type.clone());
            }
            
            let mut set = HashSet::new();
            
            // Validate all values match
            for val_expr in values {
                let val = eval_expr(val_expr, ctx).await;
                
                let v_type = infer_value_type(&val);

                if !is_hashable_type(val_type) {
                    panic!("Non-hashable inferred value type for set: {}", format_type_for_error(val_type));
                }
                
                if !types_compatible(val_type, &v_type) {
                    panic!("Set value type mismatch: expected {}, got {}", format_type_for_error(val_type), format_type_for_error(&v_type));
                }

                let set_val = value_to_set_value(&val);
                
                set.insert(set_val);
            }
            
            Value::Set(set, val_type.clone())
        }
        Expr::MethodCall { object, method, args } => {
            // Handle Namespace, StateActor and Channel methods
            if let Expr::Var(var_name) = object.as_ref() {
                let namespace_info = {
                    if let Some((_, Value::Namespace(ns), _, _, _)) = ctx.env.get(var_name) {
                        Some(ns.clone())
                    } else {
                        None
                    }
                };

                if let Some(ns) = namespace_info {
                    let qualified_name = format!("{}.{}", ns, method);
                    
                    // Evaluate arguments
                    let mut eval_args = Vec::new();
                    for arg in args {
                        eval_args.push(eval_expr(arg, ctx).await);
                    }

                    match ctx.fns.get(&qualified_name).cloned() {
                        Some(Callable::UserDefined(fn_def)) => {
                            // Build function environment
                            let mut fn_env = HashMap::new();
                            for ((param_name, param_type, ownership), arg_val) in fn_def.params.iter().zip(eval_args) {
                                let arg_type = infer_value_type(&arg_val);
                                if let Some(expected) = param_type {
                                    if !types_compatible(expected, &arg_type) {
                                        panic!(
                                            "Argument type mismatch for '{}': expected {:?}, got {:?}",
                                            param_name, expected, arg_type
                                        );
                                    }
                                }
                                fn_env.insert(
                                    param_name.clone(),
                                    (AssignKind::Var, arg_val, ownership.clone(), Moved::False, 0)
                                );
                            }
                            
                            // Execute function body
                            if let Some(body) = &fn_def.body {
                                let mut fn_ctx = ctx.fork_with_env(fn_env);
                                let flow = execute_block(body, &mut fn_ctx, fn_def.return_type.as_ref()).await;
                                
                                return match flow {
                                    ControlFlow::Return(v) => v,
                                    _ => Value::Void,
                                };
                            } else {
                                panic!("Function '{}' has no body", qualified_name);
                            }
                        }
                        Some(Callable::Builtin(func)) => {
                            return func(eval_args).unwrap_or(Value::Void);
                        }
                        None => {
                            panic!("Function '{}' not found in namespace '{}'", method, ns);
                        }
                    }
                } else if let Some((_, Value::StateActor(tx, rx, inner_type), ownership, _, _)) = ctx.env.get(var_name) {
                    let tx = tx.clone();
                    let rx = rx.clone();
                    let inner_type = inner_type.clone();
                    let ownership = ownership.clone();
                    
                    match method.as_str() {
                        "read" => {
                            let identity = make_identity_closure(&inner_type, &ownership);
                            tx.send(identity).await.expect("State actor closed");
                            let mut rx_guard = rx.lock().await;
                            return rx_guard.recv().await.expect("State actor closed");
                        }
                        "write" => {
                            if args.is_empty() {
                                panic!("state.write() requires a value argument");
                            }
                            let val = eval_expr(&args[0], ctx).await;
                            let const_closure = make_const_closure(val, &inner_type, &ownership);
                            tx.send(const_closure).await.expect("State actor closed");
                            let mut rx_guard = rx.lock().await;
                            return rx_guard.recv().await.expect("State actor closed");
                        }
                        "update" => {
                            if args.is_empty() {
                                panic!("state.update() requires a closure argument");
                            }
                            let closure = eval_expr(&args[0], ctx).await;
                            if !matches!(closure, Value::Closure(_)) {
                                panic!("state.update() requires a closure");
                            }
                            tx.send(closure).await.expect("State actor closed");
                            let mut rx_guard = rx.lock().await;
                            return rx_guard.recv().await.expect("State actor closed");
                        }
                        _ => panic!("Unknown state method: {}", method),
                    }
                } else if let Some((_, Value::Channel(_, rx_arc, _), _, _, _)) = ctx.env.get(var_name) {
                    match method.as_str() {
                        "close" => {
                            let mut rx = rx_arc.lock().await;
                            rx.close(); // Tokio method: closes channel, prevents new sends
                            return Value::Void;
                        },
                        _ => panic!("Unknown channel method: {}", method)
                    }
                }
            }

            // Check if it's a static method call on a model type
            if let Expr::Var(model_name) = &**object {
                if ctx.models.contains_key(model_name) {
                    // Static method call: Model.method(...)
                    if let Some(method_def) = ctx.model_methods.get(&(model_name.clone(), method.clone())).cloned() {
                        // Create new scope for the static method (no `this`)
                        let mut method_env: HashMap<String, (AssignKind, Value, Ownership, Moved, usize)> = HashMap::new();
                        
                        // Bind parameters
                        for (i, (param_name, param_type, param_ownership)) in method_def.params.iter().enumerate() {
                            if i < args.len() {
                                let arg_val = eval_expr(&args[i], ctx).await;

                                // Type check if parameter has a type annotation
                                let actual_type = infer_value_type(&arg_val);
                                if let Some(expected_type) = param_type {
                                    if !types_compatible(expected_type, &actual_type) {
                                        panic!(
                                            "Method param '{}' expects {}, got {}",
                                            param_name,
                                            format_type_for_error(expected_type),
                                            format_type_for_error(&actual_type)
                                        );
                                    }
                                }

                                method_env.insert(
                                    param_name.clone(),
                                    (AssignKind::Const, arg_val, param_ownership.clone(), Moved::False, 0),
                                );
                            } else {
                                panic!("Method {} expects {} arguments, got {}", method, method_def.params.len(), args.len());
                            }
                        }
                        
                        // Execute method body
                        if let Some(body) = &method_def.body {
                            let mut method_ctx = ctx.fork_with_env(method_env);
                            let flow = execute_block(body, &mut method_ctx, method_def.return_type.as_ref()).await;
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

            let obj_val = eval_expr(object, ctx).await;

            // Handle numeric methods
            match obj_val.clone() {
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
                            if !can_mutate(var_name, &ctx.env) {
                                panic!("Cannot call mutating method '{}' on immutable or borrowed array '{}'", method, var_name);
                            }
                        }
                        
                        // Handle push(item | item[])
                        if args.is_empty() {
                            panic!("push() requires at least one argument");
                        }
                        
                        let arg_val = eval_expr(&args[0], ctx).await;
                        let index: Option<usize> = if args.len() > 1 {
                            Some(to_usize(&eval_expr(&args[1], ctx).await)
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
                            if let Some((kind, _, ownership, _, scope_depth)) = ctx.env.get(var_name) {
                                ctx.env.insert(var_name.clone(), (kind.clone(), Value::Array(arr), ownership.clone(), Moved::False, scope_depth.clone()));
                            }
                        }
                        
                        return Value::I64(size);
                    }
                    "pull" => {
                        // Check mutability
                        if let Expr::Var(var_name) = object.as_ref() {
                            if !can_mutate(var_name, &ctx.env) {
                                panic!("Cannot call mutating method '{}' on immutable or borrowed array '{}'", method, var_name);
                            }
                        }
                        
                        if arr.is_empty() {
                            panic!("Cannot pull from an empty array");
                        }
                        
                        let index: Option<usize> = if args.len() > 1 {
                            Some(to_usize(&eval_expr(&args[1], ctx).await)
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
                            if let Some((kind, _, ownership, _, scope_depth)) = ctx.env.get(var_name) {
                                ctx.env.insert(var_name.clone(), (kind.clone(), Value::Array(arr), ownership.clone(), Moved::False, scope_depth.clone()));
                            }
                        }
                        
                        return pulled;
                    }
                    "slice" => {
                        // Check mutability
                        if let Expr::Var(var_name) = object.as_ref() {
                            if !can_mutate(var_name, &ctx.env) {
                                panic!("Cannot call mutating method '{}' on immutable or borrowed array '{}'", method, var_name);
                            }
                        }
                        
                        if args.is_empty() {
                            panic!("slice() requires at least a start index");
                        }
                        
                        let start = to_usize(&eval_expr(&args[0], ctx).await)
                            .expect("slice() start must be an integer");
                        
                        let stop = if args.len() > 1 {
                            to_usize(&eval_expr(&args[1], ctx).await)
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
                            if let Some((kind, _, ownership, _, scope_depth)) = ctx.env.get(var_name) {
                                ctx.env.insert(var_name.clone(), (kind.clone(), Value::Array(arr), ownership.clone(), Moved::False, scope_depth.clone()));
                            }
                        }
                        
                        return Value::Array(sliced);
                    }
                    "splice" => {
                        // Check mutability - only Var + Owned can mutate
                        if let Expr::Var(var_name) = object.as_ref() {
                            if !can_mutate(var_name, &ctx.env) {
                                panic!("Cannot call mutating method 'splice' on immutable or borrowed array '{}'", var_name);
                            }
                        }
                        
                        if args.is_empty() {
                            panic!("splice() requires items to splice");
                        }
                        
                        let items_to_splice = match eval_expr(&args[0], ctx).await {
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
                            to_usize(&eval_expr(&args[1], ctx).await)
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
                            if let Some((kind, _, ownership, _, scope_depth)) = ctx.env.get(var_name) {
                                ctx.env.insert(var_name.clone(), (kind.clone(), Value::Array(arr.clone()), ownership.clone(), Moved::False, scope_depth.clone()));
                            }
                        }

                        return Value::I64(size);
                    }
                    "copy" => {
                        // copy() doesn't mutate, so no mutability check needed
                        
                        let start = if !args.is_empty() {
                            to_usize(&eval_expr(&args[0], ctx).await)
                                .expect("copy() start must be an integer")
                        } else {
                            0
                        };
                        
                        let stop = if args.len() > 1 {
                            to_usize(&eval_expr(&args[1], ctx).await)
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
                    "each" => {
                        if args.is_empty() {
                            panic!("each() requires a closure argument");
                        }
                        
                        let closure_val = eval_expr(&args[0], ctx).await;
                        
                        if let Value::Closure(closure) = closure_val {
                            let items = arr.clone();
                            
                            for (i, item) in items.iter().enumerate() {
                                let args: Vec<Expr> = if closure.def.params.len() >= 2 {
                                    vec![value_to_expr(item), Expr::I64(i as i64)]
                                } else {
                                    vec![value_to_expr(item)]
                                };
                                
                                call_closure(&closure, &args, ctx).await;
                            }
                            
                            return Value::Void;
                        } else {
                            panic!("each() requires a closure");
                        }
                    }
                    _ => {} // Not an array method, continue to regular method handling
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
                        
                        let search = match eval_expr(&args[0], ctx).await {
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
                        
                        let search = match eval_expr(&args[0], ctx).await {
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
                        
                        let search = match eval_expr(&args[0], ctx).await {
                            Value::String(search_str) => search_str,
                            _ => panic!("ends() argument must be a string"),
                        };
                        
                        if search.is_empty() {
                            return Value::Bool(false);
                        }
                        
                        return Value::Bool(s.ends_with(&search));
                    }
                    "replace" => {
                        // Check mutability
                        if let Expr::Var(var_name) = object.as_ref() {
                            if !can_mutate(var_name, &ctx.env) {
                                panic!("Cannot call mutating method '{}' on immutable or borrowed string '{}'", method, var_name);
                            }
                        }

                        if args.len() < 2 {
                            panic!("replace() requires search and replacement arguments");
                        }
                        
                        let search_arg = eval_expr(&args[0], ctx).await;
                        let replacement = match eval_expr(&args[1], ctx).await {
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
                        // Check mutability
                        if let Expr::Var(var_name) = object.as_ref() {
                            if !can_mutate(var_name, &ctx.env) {
                                panic!("Cannot call mutating method '{}' on immutable or borrowed string '{}'", method, var_name);
                            }
                        }
                        
                        if args.is_empty() {
                            panic!("slice() requires at least a start index");
                        }
                        
                        let start = to_usize(&eval_expr(&args[0], ctx).await)
                            .expect("slice() start must be an integer");
                        
                        let stop = if args.len() > 1 {
                            to_usize(&eval_expr(&args[1], ctx).await)
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
                            if let Some((kind, _, ownership, _, scope_depth)) = ctx.env.get(var_name) {
                                ctx.env.insert(var_name.clone(), (kind.clone(), Value::String(s), ownership.clone(), Moved::False, scope_depth.clone()));
                            }
                        }
                        
                        return Value::String(sliced);
                    }
                    "split" => {
                        let separator = if !args.is_empty() {
                            match eval_expr(&args[0], ctx).await {
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
                            to_usize(&eval_expr(&args[0], ctx).await)
                                .expect("copy() start must be an integer")
                        } else {
                            0
                        };
                        
                        let stop = if args.len() > 1 {
                            to_usize(&eval_expr(&args[1], ctx).await)
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
                        
                        let search = match eval_expr(&args[0], ctx).await {
                            Value::String(search_str) => search_str,
                            _ => panic!("index() search argument must be a string"),
                        };
                        
                        if search.is_empty() {
                            panic!("index() search string cannot be empty");
                        }
                        
                        let start_pos = if args.len() > 1 {
                            to_usize(&eval_expr(&args[1], ctx).await)
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
                        
                        let count = match to_usize(&eval_expr(&args[0], ctx).await) {
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

            // Handle map methods
            if let Value::Map(mut map, key_type, val_type) = obj_val.clone() {
                match method.as_str() {
                    "set" => {
                        let root_var = find_root_var(object);

                        // Check mutability before any mutation
                        if let Some(ref var_name) = root_var {
                            if !can_mutate(&var_name, &ctx.env) {
                                panic!(
                                    "Cannot call mutating method 'set' on immutable map '{}'",
                                    var_name
                                );
                            }
                        }

                        if args.len() != 2 {
                            panic!("set() requires key and value arguments");
                        }

                        let key = eval_expr(&args[0], ctx).await;
                
                        // Type check
                        let actual_key_type = &infer_value_type(&key);
                        if !types_compatible(&key_type, actual_key_type) {
                            panic!("Key type mismatch: expected {:?} got {:?}", &key_type, actual_key_type);
                        }

                        let val = eval_expr(&args[1], ctx).await;

                        // Type check
                        let actual_val_type = &infer_value_type(&val);
                        if !types_compatible(&val_type, actual_val_type) {
                            panic!("Value type mismatch: expected {:?} got {:?}", &val_type, actual_val_type);
                        }

                        let map_key = value_to_map_key(key);
                        map.insert(map_key, val);
                        
                        // Update the original variable if it's a var
                        if let Some(var_name) = root_var {
                            if let Some((kind, _, ownership, _, scope_depth)) = ctx.env.get(&var_name) {
                                ctx.env.insert(
                                    var_name,
                                    (kind.clone(), Value::Map(map.clone(), key_type.clone(), val_type.clone()),
                                    ownership.clone(), Moved::False, scope_depth.clone())
                                );
                            }
                        }
                        
                        return Value::Map(map, key_type, val_type);
                    }
                    "get" => {
                        if args.is_empty() {
                            panic!("get() requires a key argument");
                        }
                        let key_val = eval_expr(&args[0], ctx).await;
                        let map_key = value_to_map_key(key_val);
                        
                        return map.get(&map_key)
                            .cloned()
                            .unwrap_or(Value::Void);
                    }
                    "has" => {
                        if args.is_empty() {
                            panic!("has() requires a key argument");
                        }
                        let key_val = eval_expr(&args[0], ctx).await;
                        let map_key = value_to_map_key(key_val);
                        
                        return Value::Bool(map.contains_key(&map_key));
                    }
                    "remove" => {
                        let root_var = find_root_var(object);

                        // Check mutability before any mutation
                        if let Some(ref var_name) = root_var {
                            if !can_mutate(&var_name, &ctx.env) {
                                panic!(
                                    "Cannot call mutating method 'remove' on immutable map '{}'",
                                    var_name
                                );
                            }
                        }

                        if args.is_empty() {
                            panic!("remove() requires a key argument");
                        }
                        let key_val = eval_expr(&args[0], ctx).await;
                        let map_key = value_to_map_key(key_val);
                        
                        let removed = map.remove(&map_key);
                        
                        // Update the original variable
                        if let Some(var_name) = root_var {
                            if let Some((kind, _, ownership, _, scope_depth)) = ctx.env.get(&var_name) {
                                ctx.env.insert(var_name.clone(), (kind.clone(), Value::Map(map, key_type, val_type), ownership.clone(), Moved::False, scope_depth.clone()));
                            }
                        }
                        
                        return removed.unwrap_or(Value::Void);
                    }
                    "size" => {
                        return Value::I64(map.len() as i64);
                    }
                    _ => {} // Not a map method; caught in infer_expr_type
                }
            }

            // Handle set methods
            if let Value::Set(mut set, val_type) = obj_val.clone() {
                match method.as_str() {
                    "add" => {
                        let root_var = find_root_var(object);

                        // Check mutability before any mutation
                        if let Some(ref var_name) = root_var {
                            if !can_mutate(&var_name, &ctx.env) {
                                panic!(
                                    "Cannot call mutating method 'add' on immutable set '{}'",
                                    var_name
                                );
                            }
                        }

                        if args.len() != 1 {
                            panic!("add() requires value argument");
                        }

                        let val = eval_expr(&args[0], ctx).await;

                        // Type check
                        let actual_val_type = &infer_value_type(&val);
                        if !types_compatible(&val_type, actual_val_type) {
                            panic!("Value type mismatch: expected {:?} got {:?}", &val_type, actual_val_type);
                        }
                        
                        let set_val = value_to_set_value(&val);
                        set.insert(set_val);

                        // Update the original variable if it's a var
                        if let Some(var_name) = root_var {
                            if let Some((kind, _, ownership, _, scope_depth)) = ctx.env.get(&var_name) {
                                ctx.env.insert(
                                    var_name,
                                    (kind.clone(), Value::Set(set.clone(), val_type.clone()),
                                    ownership.clone(), Moved::False, scope_depth.clone())
                                );
                            }
                        }
                        
                        return Value::Set(set, val_type);
                    }
                    "has" => {
                        if args.is_empty() {
                            panic!("has() requires a key argument");
                        }
                        let key_val = eval_expr(&args[0], ctx).await;
                        let set_val = value_to_set_value(&key_val);
                        
                        return Value::Bool(set.contains(&set_val));
                    }
                    "remove" => {
                        let root_var = find_root_var(object);

                        // Check mutability before any mutation
                        if let Some(ref var_name) = root_var {
                            if !can_mutate(&var_name, &ctx.env) {
                                panic!(
                                    "Cannot call mutating method 'remove' on immutable set '{}'",
                                    var_name
                                );
                            }
                        }

                        if args.is_empty() {
                            panic!("remove() requires a key argument");
                        }
                        let key_val = eval_expr(&args[0], ctx).await;
                        let set_val = value_to_set_value(&key_val);
                        
                        let removed = set.remove(&set_val);
                        
                        // Update the original variable
                        if let Some(var_name) = root_var {
                            if let Some((kind, _, ownership, _, scope_depth)) = ctx.env.get(&var_name) {
                                ctx.env.insert(var_name.clone(), (kind.clone(), Value::Set(set, val_type), ownership.clone(), Moved::False, scope_depth.clone()));
                            }
                        }
                        
                        return Value::Bool(removed);
                    }
                    "size" => {
                        return Value::I64(set.len() as i64);
                    }
                    _ => {} // Not a map method; (caught in infer_expr_type)
                }
            }

            match &obj_val {
                Value::Object(model_name, _fields) => {
                    // Clone the method_def first to avoid borrow issues
                    let method_def = ctx.model_methods
                        .get(&(model_name.clone(), method.clone()))
                        .cloned()
                        .unwrap_or_else(|| panic!("Method '{}' not found on model '{}'", method, model_name));

                    // Check if method mutates this and if caller has permission
                    if let Expr::Var(var_name) = object.as_ref() {
                        if let Some((kind, _, ownership, _, _)) = ctx.env.get(var_name) {
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
                                        (AssignKind::Const, _) | (AssignKind::Static, _) => {
                                            panic!("Cannot call mutating method on immutable instance '{}'", var_name);
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Create new scope for method
                    let mut method_env: HashMap<String, (AssignKind, Value, Ownership, Moved, usize)> = HashMap::new();

                    // Bind `this` as first implicit parameter
                    method_env.insert(
                        "this".to_string(),
                        (AssignKind::Var, obj_val.clone(), Ownership::Owned, Moved::False, 0),
                    );
                    
                    // Bind explicit parameters (skip first param if it matches signature)
                    for (i, (param_name, _param_type, param_ownership)) in method_def.params.iter().enumerate() {
                        if i < args.len() {
                            let arg_val = eval_expr(&args[i], ctx).await;
                            method_env.insert(
                                param_name.clone(),
                                (AssignKind::Var, arg_val, param_ownership.clone(), Moved::False, 0),
                            );
                        }
                    }

                    // Sync `this` back to the original variable
                    if let Expr::Var(var_name) = object.as_ref() {
                        if let Some((_, updated_obj, _, _, _)) = method_env.get("this") {
                            // Get the original ownership from the outer env
                            if let Some((kind, _, original_ownership, _, scope_depth)) = ctx.env.get(var_name) {
                                ctx.env.insert(var_name.clone(), (kind.clone(), updated_obj.clone(), original_ownership.clone(), Moved::False, scope_depth.clone()));
                            }
                        }
                    }
                        
                    // Execute method body
                    let flow = if let Some(body) = &method_def.body {
                        let mut method_ctx = ctx.fork_with_env(method_env);
                        execute_block(body, &mut method_ctx, method_def.return_type.as_ref()).await
                    } else {
                        panic!("Method '{}' has no body", method);
                    };
                    
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
                .map(|(name, _, _)| name.clone())
                .collect();

            let referenced_vars: Vec<String> = find_referenced_vars(&closure_def.body)
                .into_iter()
                .filter(|p| !param_names.contains(p))
                .collect();
            
            // All referenced vars (except closure params) should be captured
            let all_captured: Vec<String> = referenced_vars
                .iter()
                .filter(|v| !param_names.contains(*v))
                .cloned()
                .collect();

            // Only depth 0 owned vars should be live
            let live_vars: Vec<String> = all_captured
                .iter()
                .filter(|v| {
                    ctx.env.get(*v)
                        .map(|(kind, _, ownership, _, depth)| {
                            matches!(kind, AssignKind::Var)
                                && *ownership == Ownership::Owned
                                && *depth == 0
                        })
                        .unwrap_or(false)
                })
                .cloned()
                .collect();

            let captured_env = all_captured
                .iter()
                .filter_map(|var_name| {
                    ctx.env.get(var_name).map(|val| (var_name.clone(), val.clone()))
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
                Some(Box::new(eval_expr(expr, ctx).await))
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
    ctx: &mut RuntimeContext,
    expected_return_type: Option<&Type>,
) -> ControlFlow {
    match stmt {
        Stmt::Echo(e) => {
            let v = eval_expr(e, ctx).await;
            print!("{}", format_value(&v));
            ControlFlow::None
        }
        Stmt::Print(e) => {
            let v = eval_expr(e, ctx).await;
            println!("{}", format_value(&v));
            ControlFlow::None
        }
        Stmt::ExprStmt(e) => {
            eval_expr(e, ctx).await;
            ControlFlow::None
        }
        Stmt::Assign { kind, declared_type, name, value, ownership} => {
            // Check if variable already declared in this scope
            if ctx.env.contains_key(name) {
                panic!("Identifier '{}' already declared in this scope", name);
            }
            
            // Disallow function assignments for all variable types
            if let AssignKind::Var | AssignKind::Const = kind {
                if let Expr::Var(fn_name) | Expr::Const(fn_name) = value {
                    if ctx.fns.contains_key(fn_name) {
                        panic!("Cannot assign function '{}' to variable. Use 'fn' to define functions.", fn_name);
                    }
                }
            }

            // For static, only allow literals
            match kind {
                AssignKind::Static => {
                    if !is_literal(value) {
                        panic!("'{}' can only be used to assign literal values, not expressions or function calls", 
                            match kind {
                                AssignKind::Static => "static",
                                _ => "<Illegal initializer>",
                            }
                        );
                    }
                }
                _ => {}
            }

            let v = eval_expr(value, ctx).await;
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

            ctx.env.insert(name.clone(), (kind.clone(), v, ownership.clone(), Moved::False, 0));
            
            ControlFlow::None
        }
        Stmt::Reassign { name, value } => {
            if !ctx.env.contains_key(name) {
                panic!("Variable '{}' not declared", name);
            }

            let (kind, existing_val, ownership, moved, scope_depth) = ctx.env.get(name).unwrap().clone();
            
            // Disallow direct reassignment of StateActor
            if matches!(existing_val, Value::StateActor(_, _, _)) {
                panic!("Cannot reassign state directly. Use .set() or .update()");
            }

            let existing_type = infer_value_type(&existing_val);

            match kind {
                AssignKind::Var => {
                    let v = eval_expr(value, ctx).await;
                    let actual_type = infer_value_type(&v);

                    // Only allow reassignment if value is mutable (owned)
                    if ownership == Ownership::Borrowed && !is_copy_type(&actual_type) {
                        panic!("Cannot reassign immutable borrowed value '{}'", name);
                    }

                    // Validate type matches original
                    if !types_compatible(&existing_type, &actual_type) {
                        panic!("Type error: variable {} is {:?}, cannot assign {:?}", name, existing_type, actual_type);
                    }

                    ctx.env.insert(name.clone(), (kind.clone(), v, ownership, moved, scope_depth));
                    ControlFlow::None
                }
                kind => panic!("Cannot reassign for {}", match kind {
                    AssignKind::Const => "const",
                    AssignKind::Static => "static",
                    _ => "an invalid initializer",
                })
            }
        }
        Stmt::Break => ControlFlow::Break,
        Stmt::Next => ControlFlow::Next,
        Stmt::If { condition, then_block, else_ifs, else_block } => {
            let cond_value = eval_expr(condition, ctx).await;

            if is_truthy(&cond_value) {
                execute_block(then_block, ctx, None).await
            } else {
                for (else_if_cond, else_if_stmts) in else_ifs {
                    let else_if_value = eval_expr(else_if_cond, ctx).await;
                    if is_truthy(&else_if_value) {
                        return execute_block(else_if_stmts, ctx, None).await;
                    }
                }
                
                if let Some(else_stmts) = else_block {
                    execute_block(else_stmts, ctx, None).await
                } else {
                    ControlFlow::None
                }
            }
        }
        Stmt::While { condition, body } => {
            loop {
                let cond_value = eval_expr(condition, ctx).await;
                if !is_truthy(&cond_value) {
                    break;
                }
                
                match execute_block(body, ctx, None).await {
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
                match execute_block(body, ctx, None).await {
                    ControlFlow::Break => break,
                    ControlFlow::Next => continue,
                    ControlFlow::Return(v) => return ControlFlow::Return(v),
                    ControlFlow::None => {}
                }

                let cond_value = eval_expr(condition, ctx).await;
                if !is_truthy(&cond_value) {
                    break;
                }
            }
            ControlFlow::None
        }
        Stmt::Until { condition, body } => {
            loop {
                let cond_value = eval_expr(condition, ctx).await;
                if is_truthy(&cond_value) {
                    break;
                }
                
                match execute_block(body, ctx, None).await {
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
                match execute_block(body, ctx, None).await {
                    ControlFlow::Break => break,
                    ControlFlow::Next => continue,
                    ControlFlow::Return(v) => return ControlFlow::Return(v),
                    ControlFlow::None => {}
                }

                let cond_value = eval_expr(condition, ctx).await;
                if is_truthy(&cond_value) {
                    break;
                }
            }
            ControlFlow::None
        }
        Stmt::For { binding, index_binding, iterable, body } => {
            match iterable {
                ForIterable::Collection(expr) => {
                    let iter_val = eval_expr(expr, ctx).await;
                    
                    match iter_val {
                        Value::Array(items) => {
                            for (i, item) in items.into_iter().enumerate() {
                                ctx.env.insert(
                                    binding.clone(),
                                    (AssignKind::Var, item, Ownership::Owned, Moved::False, ctx.scope_depth)
                                );
                                
                                if let Some(ref idx_name) = index_binding {
                                    ctx.env.insert(
                                        idx_name.clone(),
                                        (AssignKind::Var, Value::I64(i as i64), Ownership::Owned, Moved::False, ctx.scope_depth)
                                    );
                                }
                                
                                match execute_block(body, ctx, None).await {
                                    ControlFlow::Break => break,
                                    ControlFlow::Next => continue,
                                    ControlFlow::Return(v) => return ControlFlow::Return(v),
                                    ControlFlow::None => {}
                                }
                            }
                        }
                        
                        Value::String(s) => {
                            for (i, c) in s.chars().enumerate() {
                                ctx.env.insert(
                                    binding.clone(),
                                    (AssignKind::Var, Value::String(c.to_string()), Ownership::Owned, Moved::False, ctx.scope_depth)
                                );
                                
                                if let Some(ref idx_name) = index_binding {
                                    ctx.env.insert(
                                        idx_name.clone(),
                                        (AssignKind::Var, Value::I64(i as i64), Ownership::Owned, Moved::False, ctx.scope_depth)
                                    );
                                }
                                
                                match execute_block(body, ctx, None).await {
                                    ControlFlow::Break => break,
                                    ControlFlow::Next => continue,
                                    ControlFlow::Return(v) => return ControlFlow::Return(v),
                                    ControlFlow::None => {}
                                }
                            }
                        }
                        
                        _ => panic!("Cannot iterate over {:?}. Use .each() for maps and sets.", iter_val),
                    }
                }
                
                ForIterable::CountReceive(channel_expr, count_expr) => {
                    let channel_val = eval_expr(channel_expr, ctx).await;
                    let count_val = eval_expr(count_expr, ctx).await;
                    
                    let limit = match count_val {
                        Value::U64(n) => n as i64,
                        Value::U32(n) => n as i64,
                        Value::U16(n) => n as i64,
                        Value::U8(n) => n as i64,
                        Value::I64(n) if n > 0 => n,
                        Value::I32(n) if n > 0 => n as i64,
                        Value::I16(n) if n > 0 => n as i64,
                        Value::I8(n) if n > 0 => n as i64,
                        _ => panic!("For loop receive count must be a positive integer"),
                    };
                    
                    if let Value::Channel(_, rx, _) = channel_val {
                        for i in 0..limit {
                            let mut rx_guard = rx.lock().await;
                            match rx_guard.recv().await {
                                Some(val) => {
                                    drop(rx_guard);
                                    
                                    ctx.env.insert(
                                        binding.clone(),
                                        (AssignKind::Var, val, Ownership::Owned, Moved::False, ctx.scope_depth)
                                    );
                                    
                                    if let Some(ref idx_name) = index_binding {
                                        ctx.env.insert(
                                            idx_name.clone(),
                                            (AssignKind::Var, Value::I64(i), Ownership::Owned, Moved::False, ctx.scope_depth)
                                        );
                                    }
                                    
                                    match execute_block(body, ctx, None).await {
                                        ControlFlow::Break => break,
                                        ControlFlow::Next => continue,
                                        ControlFlow::Return(v) => return ControlFlow::Return(v),
                                        ControlFlow::None => {}
                                    }
                                }
                                None => panic!("Channel closed before receiving {} messages", limit),
                            }
                        }
                    } else {
                        panic!("Expected channel for <- receive");
                    }
                }
                
                ForIterable::MaybeReceive(expr) => {
                    let channel_val = eval_expr(expr, ctx).await;
                    
                    if let Value::Channel(_, rx, _) = channel_val {
                        let mut i: i64 = 0;
                        
                        loop {
                            let mut rx_guard = rx.lock().await;
                            match rx_guard.recv().await {
                                Some(val) => {
                                    drop(rx_guard);
                                    
                                    ctx.env.insert(
                                        binding.clone(),
                                        (AssignKind::Var, val, Ownership::Owned, Moved::False, ctx.scope_depth)
                                    );
                                    
                                    if let Some(ref idx_name) = index_binding {
                                        ctx.env.insert(
                                            idx_name.clone(),
                                            (AssignKind::Var, Value::I64(i), Ownership::Owned, Moved::False, ctx.scope_depth)
                                        );
                                    }
                                    
                                    match execute_block(body, ctx, None).await {
                                        ControlFlow::Break => break,
                                        ControlFlow::Next => {
                                            i += 1;
                                            continue;
                                        }
                                        ControlFlow::Return(v) => return ControlFlow::Return(v),
                                        ControlFlow::None => {}
                                    }
                                    
                                    i += 1;
                                }
                                None => break, // Channel closed, exit cleanly
                            }
                        }
                    } else {
                        panic!("Expected channel for <-? receive");
                    }
                }
            }
            
            // Clean up bindings
            ctx.env.remove(binding);
            if let Some(idx_name) = index_binding {
                ctx.env.remove(idx_name);
            }
            
            ControlFlow::None
        }
        Stmt::Match { expr, cases, default, match_type } => {
            let val = eval_expr(expr, ctx).await;
            let mut matched = false;
            
            for (patterns, stmts) in cases {
                for pattern in patterns {
                    if let Some(new_env) = match_pattern(&val, pattern, &ctx.env) {
                        matched = true;
                        let pattern_env = new_env;

                        let mut pattern_ctx = ctx.fork_with_env(pattern_env);
                        let flow = execute_block(stmts, &mut pattern_ctx, None).await;

                        // Sync variables back to outer env
                        for (name, value) in &pattern_ctx.env {
                            if ctx.env.contains_key(name) {
                                ctx.env.insert(name.clone(), value.clone());
                            }
                        }

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
                    execute_block(default_stmts, ctx, None).await
                } else {
                    ControlFlow::None
                }
            } else {
                ControlFlow::None
            }
        }
        Stmt::FnDef(fn_def) => {
            ctx.fns.insert(fn_def.name.clone(), Callable::UserDefined(fn_def.clone()));
            ControlFlow::None
        }
        Stmt::ModelDef(model_def) => {
            ctx.models.insert(model_def.name.clone(), model_def.clone());
            ControlFlow::None
        }
        Stmt::RoleDef(role_def) => {
            ctx.roles.insert(role_def.name.clone(), role_def.clone());
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
                    eval_expr(e, ctx).await
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
        Stmt::Export(export_kind) => {
            match export_kind {
                ExportKind::Named(inner_stmt) => {
                    // Execute the inner statement (e.g., fn def)
                    let flow = execute_stmt(
                        inner_stmt,
                        ctx,
                        expected_return_type,
                    ).await;
                    
                    // Track the export
                    if let Stmt::FnDef(fn_def) = inner_stmt.as_ref() {
                        ctx.exports.functions.insert(fn_def.name.clone(), fn_def.clone());
                    }
                    // Add similar handling for models, roles if you want to export those
                    
                    flow
                }
                ExportKind::Default(name) => {
                    // Verify the name exists
                    if !ctx.fns.contains_key(name) {
                        panic!("Cannot export default '{}': not defined", name);
                    }
                    
                    if ctx.exports.default.is_some() {
                        panic!("Module already has a default export");
                    }
                    
                    ctx.exports.default = Some(name.clone());
                    match ctx.fns.get(name) {
                        Some(Callable::UserDefined(fn_def)) => {
                            ctx.exports.functions.insert(name.clone(), fn_def.clone());
                        }
                        Some(Callable::Builtin(_builtin)) => {
                            panic!("Cannot export builtin '{}'", name);
                        }
                        None => {
                            panic!("Cannot export '{}': not defined", name);
                        }
                    }
                    
                    ControlFlow::None
                }
            }
        }

        Stmt::Import(import_stmt) => {
            let module_exports = ctx.module_cache
                .get(&import_stmt.source)
                .unwrap_or_else(|| panic!("Module '{}' not found", import_stmt.source.display()))
                .clone();
            
            match &import_stmt.kind {
                ImportKind::Named(names) => {
                    for (original, local) in names {
                        if let Some(fn_def) = module_exports.functions.get(original) {
                            let mut imported_fn = fn_def.clone();
                            imported_fn.name = local.clone();
                            ctx.fns.insert(local.clone(), Callable::UserDefined(imported_fn.clone()));
                        } else if let Some(enum_def) = module_exports.enums.get(original) {
                            let mut imported_enum = enum_def.clone();
                            imported_enum.name = local.clone();
                            ctx.enums.insert(local.clone(), imported_enum);
                        } else if let Some(model_def) = module_exports.models.get(original) {
                            ctx.models.insert(local.clone(), model_def.clone());
                        } else {
                            panic!("'{}' is not exported from '{}'", original, import_stmt.source.display());
                        }
                    }
                }
                
                ImportKind::Default(name) => {
                    let default_name = module_exports
                        .default
                        .as_ref()
                        .unwrap_or_else(|| panic!("Module '{}' has no default export", import_stmt.source.display()));
                    
                    // Check what type the default export is
                    if let Some(fn_def) = module_exports.functions.get(default_name) {
                        let mut renamed_fn = fn_def.clone();
                        renamed_fn.name = name.clone();
                        ctx.fns.insert(name.clone(), Callable::UserDefined(renamed_fn));
                    } else if let Some(enum_def) = module_exports.enums.get(default_name) {
                        let mut renamed_enum = enum_def.clone();
                        renamed_enum.name = name.clone();
                        ctx.enums.insert(name.clone(), renamed_enum);
                    } else if let Some(model_def) = module_exports.models.get(default_name) {
                        let mut renamed_model = model_def.clone();
                        renamed_model.name = name.clone();
                        ctx.models.insert(name.clone(), renamed_model);
                    } else {
                        panic!("Default export '{}' not found in '{}'", default_name, import_stmt.source.display());
                    }
                }
                
                ImportKind::Namespace(namespace) => {
                    // Functions
                    for (fn_name, fn_def) in &module_exports.functions {
                        let qualified_name = format!("{}.{}", namespace, fn_name);
                        ctx.fns.insert(qualified_name, Callable::UserDefined(fn_def.clone()));
                    }
                    
                    // Enums
                    for (enum_name, enum_def) in &module_exports.enums {
                        let qualified_name = format!("{}.{}", namespace, enum_name);
                        ctx.enums.insert(qualified_name, enum_def.clone());
                    }
                    
                    // Models
                    for (model_name, model_def) in &module_exports.models {
                        let qualified_name = format!("{}.{}", namespace, model_name);
                        ctx.models.insert(qualified_name, model_def.clone());
                    }
                    
                    // Handle .default access
                    if let Some(default_name) = &module_exports.default {
                        let qualified_default = format!("{}.default", namespace);
                        
                        if let Some(fn_def) = module_exports.functions.get(default_name) {
                            ctx.fns.insert(qualified_default, Callable::UserDefined(fn_def.clone()));
                        } else if let Some(enum_def) = module_exports.enums.get(default_name) {
                            ctx.enums.insert(qualified_default, enum_def.clone());
                        } else if let Some(model_def) = module_exports.models.get(default_name) {
                            ctx.models.insert(qualified_default, model_def.clone());
                        }
                    }
                }
            }
            
            ControlFlow::None
        }
    }
}

#[async_recursion]
async fn execute_block(
    stmts: &[Stmt],
    ctx: &mut RuntimeContext,
    expected_return_type: Option<&Type>,
) -> ControlFlow {
    for stmt in stmts {
        // Check for illegal top-level assignments
        if let Stmt::Assign { kind, .. } = stmt {
            if matches!(kind, AssignKind::Static) {
                let kind_str = match kind {
                    AssignKind::Static => "static",
                    _ => unreachable!(),
                };
                panic!("Can only assign with '{}' at the top-level of a file.", kind_str);
            }
        }

        let flow = execute_stmt(stmt, ctx, expected_return_type).await;
        match flow {
            ControlFlow::Break | ControlFlow::Next | ControlFlow::Return(_) => return flow,
            ControlFlow::None => {}
        }
    }
    ControlFlow::None
}

async fn load_module(
    path: &PathBuf,
    current_file: &PathBuf,
    module_cache: &mut HashMap<PathBuf, ModuleExports>,
    toolbox: &Toolbox,
) -> ModuleExports {
    let file_path = resolve_module_path(path, current_file);

    if let Some(exports) = module_cache.get(&file_path) {
        return exports.clone();
    }

    let source = std::fs::read_to_string(&file_path)
        .unwrap_or_else(|_| panic!("Could not read module '{}'", file_path.display()));
    
    let tokens = ngn::lexer::tokenize(&source);
    
    // Single pass - enums collected during parse
    let mut parser = Parser::new(tokens, HashMap::new(), file_path.clone());
    let stmts = parser
        .parse_program()
        .unwrap_or_else(|e| panic!("Parse error in '{}': {}", file_path.display(), e));
    
    module_cache.insert(file_path.clone(), ModuleExports::default());
    
    let exports = Box::pin(run_module(&stmts, &file_path, module_cache, toolbox, false)).await;
    
    module_cache.insert(file_path.clone(), exports.clone());
    
    exports
}

async fn run_module(
    stmts: &[Stmt],
    current_file: &PathBuf,
    module_cache: &mut HashMap<PathBuf, ModuleExports>,
    toolbox: &Toolbox,
    is_entry: bool, // true for main file, false for imported modules
) -> ModuleExports {
    let mut ctx = RuntimeContext::with_cache(std::mem::take(module_cache));

    // First pass: collect definitions and handle imports/exports
    for stmt in stmts {
        match stmt {
            Stmt::Import(import_stmt) => {
                match parse_import_source(&import_stmt.source) {
                    ImportSource::Toolbox { module } => {
                        match &import_stmt.kind {
                            // import { abs, round } from "tbx::math"
                            ImportKind::Named(names) => {
                                let module_name = module.unwrap_or_else(|| {
                                    panic!("Named imports from 'tbx' require a module, e.g., 'tbx::math'");
                                });
                                
                                let tbx_module = toolbox.get_module(&module_name).unwrap_or_else(|| {
                                    panic!("Unknown toolbox module: tbx::{}", module_name);
                                });
                                
                                for (original, local) in names {
                                    let func = tbx_module.functions.get(original).unwrap_or_else(|| {
                                        panic!("'{}' not found in tbx::{}", original, module_name);
                                    });
                                    ctx.fns.insert(local.clone(), Callable::Builtin(*func));
                                }
                            }
                            
                            // import * as Math from "tbx::math"
                            ImportKind::Namespace(namespace) => {
                                let module_name = module.unwrap_or_else(|| {
                                    panic!("Namespace imports from 'tbx' require a module, e.g., 'tbx::math'");
                                });
                                
                                let tbx_module = toolbox.get_module(&module_name).unwrap_or_else(|| {
                                    panic!("Unknown toolbox module: tbx::{}", module_name);
                                });
                                
                                ctx.env.insert(
                                    namespace.clone(),
                                    (AssignKind::Const, Value::Namespace(namespace.clone()), Ownership::Borrowed, Moved::False, 0)
                                );
                                
                                for (name, func) in &tbx_module.functions {
                                    ctx.fns.insert(
                                        format!("{}.{}", namespace, name),
                                        Callable::Builtin(*func)
                                    );
                                }
                            }
                            
                            // import math from "tbx::math"
                            ImportKind::Default(local_name) => {
                                let module_name = module.unwrap_or_else(|| {
                                    panic!("Default imports from 'tbx' require a module, e.g., 'tbx::{}'", local_name.to_lowercase());
                                });
                                
                                let tbx_module = toolbox.get_module(&module_name).unwrap_or_else(|| {
                                    panic!("Unknown toolbox module: tbx::{}", module_name);
                                });
                                
                                ctx.env.insert(
                                    local_name.clone(),
                                    (AssignKind::Const, Value::Namespace(local_name.clone()), Ownership::Borrowed, Moved::False, 0)
                                );
                                
                                for (name, func) in &tbx_module.functions {
                                    ctx.fns.insert(
                                        format!("{}.{}", local_name, name),
                                        Callable::Builtin(*func)
                                    );
                                }
                            }
                        }
                    }
                    
                    ImportSource::File { path: _ } => {
                        let module_exports = load_module(
                            &import_stmt.source, 
                            current_file,
                            module_cache,
                            toolbox,
                        ).await;
                        
                        match &import_stmt.kind {
                            ImportKind::Named(names) => {
                                for (original, local) in names {
                                    if let Some(fn_def) = module_exports.functions.get(original) {
                                        let mut imported_fn = fn_def.clone();
                                        imported_fn.name = local.clone(); // Rename to local alias
                                        ctx.fns.insert(local.clone(), Callable::UserDefined(imported_fn.clone()));
                                    } else if let Some(model_def) = module_exports.models.get(original) {
                                        ctx.models.insert(local.clone(), model_def.clone());
                                    } else if let Some(enum_def) = module_exports.enums.get(original) {
                                        ctx.enums.insert(local.clone(), enum_def.clone());
                                    } else {
                                        panic!("'{}' is not exported from '{}'", original, import_stmt.source.display());
                                    }
                                }
                            }
                            
                            ImportKind::Default(local_name) => {
                                let default_name = module_exports
                                    .default
                                    .as_ref()
                                    .unwrap_or_else(|| {
                                        panic!("Module '{}' has no default export", import_stmt.source.display())
                                    });
                                
                                if let Some(fn_def) = module_exports.functions.get(default_name) {
                                    let mut renamed = fn_def.clone();
                                    renamed.name = local_name.clone();
                                    ctx.fns.insert(local_name.clone(), Callable::UserDefined(renamed.clone()));
                                } else {
                                    panic!("Default export '{}' not found in '{}'", default_name, import_stmt.source.display());
                                }
                            }
                            
                            ImportKind::Namespace(namespace) => {
                                // Store namespace marker in env
                                ctx.env.insert(
                                    namespace.clone(),
                                    (AssignKind::Const, Value::Namespace(namespace.clone()), Ownership::Borrowed, Moved::False, 0)
                                );

                                // Store qualified functions
                                for (name, fn_def) in &module_exports.functions {
                                    ctx.fns.insert(format!("{}.{}", namespace, name), Callable::UserDefined(fn_def.clone()));
                                }
                                for (name, model_def) in &module_exports.models {
                                    ctx.models.insert(format!("{}.{}", namespace, name), model_def.clone());
                                }
                                if let Some(default_name) = &module_exports.default {
                                    if let Some(fn_def) = module_exports.functions.get(default_name) {
                                        ctx.fns.insert(format!("{}.default", namespace), Callable::UserDefined(fn_def.clone()));
                                    }
                                }
                            }
                        }
                    }
                }
            }
            
            Stmt::Export(export_kind) => {
                match export_kind {
                    ExportKind::Named(inner_stmt) => {
                        // Process the inner statement
                        match inner_stmt.as_ref() {
                            Stmt::FnDef(fn_def) => {
                                ctx.fns.insert(fn_def.name.clone(), Callable::UserDefined(fn_def.clone()));
                                ctx.exports.functions.insert(fn_def.name.clone(), fn_def.clone());
                            }
                            Stmt::ModelDef(model_def) => {
                                ctx.models.insert(model_def.name.clone(), model_def.clone());
                                ctx.exports.models.insert(model_def.name.clone(), model_def.clone());
                            }
                            Stmt::EnumDef(enum_def) => {
                                ctx.enums.insert(enum_def.name.clone(), enum_def.clone());
                                ctx.exports.enums.insert(enum_def.name.clone(), enum_def.clone());
                            }
                            _ => panic!("Cannot export this statement type"),
                        }
                    }
                    ExportKind::Default(name) => {
                        if ctx.exports.default.is_some() {
                            panic!("Module already has a default export");
                        }
                        ctx.exports.default = Some(name.clone());
                    }
                }
            }
            Stmt::FnDef(fn_def) => {
                ctx.fns.insert(fn_def.name.clone(), Callable::UserDefined(fn_def.clone()));
            }
            Stmt::ModelDef(model_def) => {
                ctx.models.insert(model_def.name.clone(), model_def.clone());
            }
            Stmt::RoleDef(role_def) => {
                ctx.roles.insert(role_def.name.clone(), role_def.clone());
            }
            Stmt::EnumDef(enum_def) => {
                ctx.enums.insert(enum_def.name.clone(), enum_def.clone());
            }
            Stmt::ExtendModel { model_name, role_name, methods } => {
                // Validate model exists
                if !ctx.models.contains_key(model_name) {
                    panic!("Cannot extend unknown model '{}'", model_name);
                }
                
                // If role specified, validate it exists
                if let Some(role) = role_name {
                    if !ctx.roles.contains_key(role) {
                        panic!("Cannot extend with unknown role '{}'", role);
                    }
                    
                    // Validate role compliance
                    validate_role_compliance(
                        model_name, 
                        role, 
                        methods, 
                        &ctx.roles
                    );
                    
                    ctx.model_roles.insert(
                        (model_name.clone(), role.clone()), 
                        true
                    );
                }
                
                // Register methods
                for method in methods {
                    if method.name == "new" {
                        // Get the model definition
                        let model_def = ctx.models.get(model_name).unwrap_or_else(|| {
                            panic!("Model '{}' not found", model_name);
                        });
                        
                        // Build a map of method params: name -> type
                        let method_params: HashMap<String, Option<Type>> = method.params
                            .iter()
                            .map(|(name, typ, _)| (name.clone(), typ.clone()))
                            .collect();
                        
                        // Check each model field is represented in new() params
                        for (field_name, field_type) in &model_def.fields {
                            match method_params.get(field_name) {
                                None => {
                                    panic!(
                                        "Model '{}' method 'new' missing required param '{}' of type {:?}",
                                        model_name, field_name, field_type
                                    );
                                }
                                Some(param_type) => {
                                    match param_type {
                                        Some(actual_type) => {
                                            if actual_type != field_type {
                                                panic!(
                                                    "Model '{}' method 'new' param '{}' type mismatch: expected {:?}, got {:?}",
                                                    model_name, field_name, field_type, actual_type
                                                );
                                            }
                                        }
                                        None => {
                                            panic!(
                                                "Model '{}' method 'new' param '{}' missing type annotation, expected {:?}",
                                                model_name, field_name, field_type
                                            );
                                        }
                                    }
                                }
                            }
                        }

                        // Check return type against model name
                        match &method.return_type {
                            Some(Type::Model(inner_name)) if inner_name != model_name => {
                                panic!(
                                    "Model '{}' method 'new' return type mismatch: expected {:?}, got {:?}",
                                    model_name, model_name, inner_name
                                );
                            }
                            _ => {}
                        }
                        
                        // Warn about extra params not in model
                        for (param_name, _) in &method_params {
                            let field_exists = model_def.fields.iter().any(|(f, _)| f == param_name);
                            if !field_exists {
                                panic!(
                                    "Warning: Model '{}' method 'new' has param '{}' not in model fields",
                                    model_name, param_name
                                );
                            }
                        }
                    }
                    
                    
                    ctx.model_methods.insert(
                        (model_name.clone(), method.name.clone()),
                        method.clone(),
                    );
                }
            }
            Stmt::Assign { kind: AssignKind::Static, .. } => {
                execute_stmt(
                    stmt,
                    &mut ctx,
                    None,
                ).await;
            }
            _ => {
                panic!("Top-level statements must be definitions, imports, or exports");
            }
        }
    }

    // Validate default export exists
    if let Some(default_name) = &ctx.exports.default {
        match ctx.fns.get(default_name) {
            Some(Callable::UserDefined(fn_def)) => {
                ctx.exports.functions.insert(default_name.clone(), fn_def.clone());
            }
            Some(Callable::Builtin(_)) => {
                panic!("Cannot export a builtin as default");
            }
            None => {
                panic!("Default export '{}' is not defined", default_name);
            }
        }
    }

    // Only run main() for entry file
    if is_entry {
        match ctx.fns.get("main").cloned() {
            Some(Callable::UserDefined(main_fn)) => {
                if !main_fn.params.is_empty() {
                    panic!("main() must have no parameters");
                }
                
                if let Some(body) = &main_fn.body {
                    execute_block(body, &mut ctx, main_fn.return_type.as_ref()).await;
                } else {
                    panic!("The main() function must have a body.");
                }
            }
            Some(Callable::Builtin(_)) => {
                panic!("main() cannot be a builtin function");
            }
            None => {
                panic!("No main() function found. Entrypoint files must define main()");
            }
        }
    }

    // Restore cache for caller
    *module_cache = ctx.module_cache;

    ctx.exports
}

pub async fn run(stmts: &[Stmt], entry_file: &PathBuf) {
    let mut module_cache = HashMap::new();
    let toolbox = Toolbox::new();
    run_module(stmts, entry_file, &mut module_cache, &toolbox, true).await;
}