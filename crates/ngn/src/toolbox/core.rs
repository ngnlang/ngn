//! Core globals: auto-injected into every module's global scope.
//! Unlike other toolbox modules, these don't require explicit imports.

use crate::parser::Type;

/// All core global names - these are auto-injected into every module
pub const GLOBAL_NAMES: &[&str] = &[
    "print", "echo", "sleep", "thread", "state", "channel", "json", "map", "set", "fetch",
];

/// Type definition for a global symbol
pub struct GlobalDef {
    pub ty: Type,
    pub is_mutable: bool,
}

/// Get type signature for a core global (used by Analyzer).
/// Returns None for expression-level constructs (thread, state, channel, map, set)
/// which are handled directly by the parser/compiler rather than as callable symbols.
pub fn get_type(name: &str) -> Option<GlobalDef> {
    match name {
        "print" | "echo" => Some(GlobalDef {
            ty: Type::Function {
                params: vec![Type::Any],
                optional_count: 0,
                return_type: Box::new(Type::Void),
            },
            is_mutable: false,
        }),
        "sleep" => Some(GlobalDef {
            ty: Type::Function {
                params: vec![Type::I64],
                optional_count: 0,
                return_type: Box::new(Type::Void),
            },
            is_mutable: false,
        }),
        "json" => Some(GlobalDef {
            ty: Type::Json,
            is_mutable: false,
        }),
        "fetch" => Some(GlobalDef {
            ty: Type::Function {
                params: vec![Type::String, Type::Model("FetchOptions".to_string())],
                optional_count: 1, // Second arg is optional
                return_type: Box::new(Type::Channel(Box::new(Type::Model("Response".to_string())))),
            },
            is_mutable: false,
        }),
        // thread, state, channel, map, set are expression-level keywords
        // handled by the parser, not callable symbols
        _ => None,
    }
}
