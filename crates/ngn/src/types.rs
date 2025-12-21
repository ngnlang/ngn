use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum Type {
    I64,
    I32,
    I16,
    I8,
    U64,
    U32,
    U16,
    U8,
    F64,
    F32,
    String, // owned
    Str, // borrowed
    Bool,
    Array(Box<Type>),
    Object(HashMap<String, Type>), // dead code
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
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
    matches!(t, Type::I64 | Type::I32 | Type::I16 | Type::I8 | Type::U64 | Type::U32 | Type::U16 | Type::U8 | Type::F64 | Type::F32)
}

pub fn types_compatible(expected: &Type, actual: &Type) -> bool {
    // This is fine for "wildcards", but standard generics (like "T") shouldn't automatically return true
    // unless they are being resolved in a context that knows what "T" is.
    // Since types_compatible is context-free, this is probably okay for now,
    // but be aware that fn<T>(x: T) might accidentally match fn<i64>(x: i64) if you aren't careful.
    // Handle wildcards
    if let Type::Generic(name) = actual {
        if name == "_" { return true; }
    }
    if let Type::Generic(name) = expected {
        if name == "_" { return true; }
    }

    match (expected, actual) {
        (Type::I64, Type::I64) => true,
        (Type::I32, Type::I32) => true,
        (Type::I16, Type::I16) => true,
        (Type::I8, Type::I8) => true,
        (Type::U64, Type::U64) => true,
        (Type::U32, Type::U32) => true,
        (Type::U16, Type::U16) => true,
        (Type::U8, Type::U8) => true,
        (Type::F64, Type::F64) => true,
        (Type::F32, Type::F32) => true,
        (Type::Str, Type::Str) => true,
        (Type::String, Type::String) => true,
        (Type::String, Type::Str) => true,
        (Type::Str, Type::String) => true,
        (Type::Bool, Type::Bool) => true,
        (Type::Array(e1), Type::Array(e2)) => types_compatible(e1, e2),
        (Type::Channel(t1), Type::Channel(t2)) => types_compatible(t1, t2),
        (Type::Function { params: p1, return_type: r1 }, 
        Type::Function { params: p2, return_type: r2 }) => {
            if p1.len() != p2.len() {
                return false;
            }

            // Check all parameter types match
            // Note: This is "Covariance/Contravariance" territory in strict languages,
            // but for now, exact matching is the safest default.
            let params_match = p1.iter().zip(p2.iter()).all(|(t1, t2)| types_compatible(t1, t2));
            
            let return_match = types_compatible(r1, r2);

            params_match && return_match
        },
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
        (Type::Map(k1, v1), Type::Map(k2, v2)) => 
            types_compatible(k1, k2) && types_compatible(v1, v2),

        (Type::Set(v1), Type::Set(v2)) => 
            types_compatible(v1, v2),

        (Type::StateActor(inner1), Type::StateActor(inner2)) => 
            types_compatible(inner1, inner2),
        _ => false,
    }
}
