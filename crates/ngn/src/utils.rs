use crate::ast::{EnumDef};
use crate::types::{Ownership, Type};
use std::{collections::HashMap, iter::Peekable};
use crate::lexer::Token;

pub fn infer_enum_name(variant: &str, enums: &HashMap<String, EnumDef>) -> String {
    match variant {
        "Ok" | "Error" => "Result".to_string(),
        "Value" | "Null" => "Maybe".to_string(),
        _ => {
            for enum_def in enums.values() {
                if enum_def.variants.iter().any(|v| v.name == *variant) {
                    return enum_def.name.clone();
                }
            }
            panic!("Enum variant '{}' not found", variant)
        }
    }
}

pub fn parse_type<I>(tokens: &mut Peekable<I>) -> Result<(Type, Ownership), String>
where
    I: Iterator<Item = (usize, Token, usize)>, // Matches your token structure
{
    // Helper to peek without advancing
    let current_token = |t: &mut Peekable<I>| t.peek().map(|(_, token, _)| token.clone());
    
    // Helper to advance
    let advance = |t: &mut Peekable<I>| t.next();

    // Helper to expect specific token
    let expect_greater = |t: &mut Peekable<I>| -> Result<(), String> {
        match current_token(t) {
            Some(Token::Greater) => { advance(t); Ok(()) }
            other => Err(format!("Expected '>', got {:?}", other))
        }
    };
    
    let owned = if matches!(current_token(tokens), Some(Token::Less)) {
        advance(tokens);
        true
    } else {
        false
    };
    
    let type_name = match current_token(tokens) {
        Some(Token::Ident(name)) => {
            advance(tokens);
            name
        }
        Some(Token::Fn) => {
            advance(tokens);
            "fn".to_string()
        }
        Some(Token::Channel) => {
            advance(tokens);
            "channel".to_string()
        }
        other => return Err(format!("Expected type identifier, got {:?}", other)),
    };

    // Parse generic type parameters: TypeName<T, E>
    let mut type_args = Vec::new();
    
    if matches!(current_token(tokens), Some(Token::Less)) {
        advance(tokens); // consume <
        
        // Recursive call!
        let (arg_type, _) = parse_type(tokens)?;
        type_args.push(arg_type);
        
        while matches!(current_token(tokens), Some(Token::Comma)) {
            advance(tokens); // consume ,
            let (arg_type, _) = parse_type(tokens)?;
            type_args.push(arg_type);
        }
        
        expect_greater(tokens)?; // consume >
    }
    
    let base_type = match type_name.as_str() {
        "i64" => Type::I64,
        "i32" => Type::I32,
        "i16" => Type::I16,
        "i8" => Type::I8,
        "u64" => Type::U64,
        "u32" => Type::U32,
        "u16" => Type::U16,
        "u8" => Type::U8,
        "f64" => Type::F64,
        "f32" => Type::F32,
        "string" => Type::Str,
        "bool" => Type::Bool,
        "array" => {
            // Array is generic: array<T>
            if !type_args.is_empty() {
                Type::Array(Box::new(type_args[0].clone()))
            } else {
                // Default: array<number>
                Type::Array(Box::new(Type::I64))
            }
        }
        "channel" => {
            if type_args.is_empty() {
                return Err("Channel type requires a type argument, e.g. channel<string>".to_string());
            }
            Type::Channel(Box::new(type_args[0].clone()))
        },
        "fn" => {
            // Syntax: fn<(arg1, arg2) -> return_type> or fn<() -> return_type>
            // For simplicity, also allow: fn<return_type> for () -> T
            
            if type_args.is_empty() {
                // Bare `fn` with no type info
                Type::Function {
                    params: vec![],
                    return_type: Box::new(Type::Void),
                }
            } else if type_args.len() == 1 {
                // fn<return_type> means () -> return_type
                Type::Function {
                    params: vec![],
                    return_type: Box::new(type_args[0].clone()),
                }
            } else {
                // fn<arg1, arg2, ..., return_type> - last is return, rest are params
                let (param_types, return_type) = type_args.split_at(type_args.len() - 1);
                Type::Function {
                    params: param_types.to_vec(),
                    return_type: Box::new(return_type[0].clone()),
                }
            }
        },
        "void" => Type::Void,
        "Result" | "Maybe" => {
            Type::Enum(type_name.to_string(), type_args)
        }
        model_or_enum_name => {
            if !type_args.is_empty() {
                Type::Enum(model_or_enum_name.to_string(), type_args)
            } else {
                Type::Model(model_or_enum_name.to_string())
            }
        }
    };
    
    let ownership = if owned { Ownership::Owned } else { Ownership::Borrowed };
    Ok((base_type, ownership))
}

pub fn resolve_module_path(import_path: &str, current_file: &str) -> String {
    use std::path::{Path, PathBuf};
    
    let current_dir = Path::new(current_file)
        .parent()
        .unwrap_or(Path::new("."));
    
    let resolved: PathBuf = if import_path.starts_with("./") || import_path.starts_with("../") {
        // Relative import
        current_dir.join(import_path)
    } else {
        // Bare import - treat as relative to current directory
        current_dir.join(import_path)
    };
    
    // Add .ngn extension if not present
    let with_extension = if resolved.extension().is_some() {
        resolved
    } else {
        resolved.with_extension("ngn")
    };
    
    // Normalize the path (resolve .. and .)
    normalize_path(&with_extension)
}

fn normalize_path(path: &std::path::Path) -> String {
    let mut components = Vec::new();
    
    for component in path.components() {
        match component {
            std::path::Component::ParentDir => {
                match components.last() {
                    Some(std::path::Component::Normal(_)) => {
                        // If we are inside a folder, go back up
                        components.pop();
                    }
                    _ => {
                        // If stack is empty, or we already have '..', keep the '..'
                        components.push(component);
                    }
                }
            }
            std::path::Component::CurDir => {}
            c => components.push(c),
        }
    }
    
    let result: std::path::PathBuf = components.iter().collect();
    result.to_string_lossy().to_string()
}
