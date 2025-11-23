use crate::ast::{EnumDef, Ownership, Type};
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
        "u64" => Type::U64,
        "u32" => Type::U32,
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
        "fn" => Type::Function,
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