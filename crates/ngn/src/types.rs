use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum Type {
    I64,
    I32,
    U64,
    U32,
    F64,
    F32,
    String, // owned
    Str, // borrowed
    Bool,
    Array(Box<Type>),
    Object(HashMap<String, Type>), // dead code
    Function,
    Void,
    Model(String),
    Regex,
    Generic(String),
    Enum(String, Vec<Type>),
    Channel(Box<Type>),
    Namespace(String),
    StateActor(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Set(Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignKind {
    Var,
    Const,
    Static,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ownership {
    Borrowed,
    Owned,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Moved {
    True,
    False
}

pub fn is_numeric_type(t: &Type) -> bool {
    matches!(t, Type::I64 | Type::I32 | Type::U64 | Type::U32 | Type::F64 | Type::F32)
}

pub fn types_compatible(expected: &Type, actual: &Type) -> bool {
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
