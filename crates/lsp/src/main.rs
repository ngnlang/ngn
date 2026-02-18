use std::collections::{HashMap, HashSet};
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use ngn::analyzer::Analyzer;
use ngn::lexer::{Lexer, Span, Token};
use ngn::parser::{ExprKind, ModelField, ParseDiagnostic, Parser, Statement, StatementKind, Type};
use ngn::toolbox::Toolbox;
use ngn::toolbox::core::{GLOBAL_NAMES, get_type};

#[derive(Debug)]
struct Backend {
    client: Client,
    documents: Arc<RwLock<HashMap<String, DocumentState>>>,
    module_cache: Arc<RwLock<HashMap<PathBuf, ModuleExports>>>,
}

#[derive(Debug, Clone)]
struct DocumentState {
    text: String,
    analysis: Option<ngn::analyzer::Analysis>,
    tokens: Vec<(Token, Span)>,
    imported_symbols: HashMap<String, Type>,
    module_aliases: HashMap<String, ModuleExports>,
}

#[derive(Debug, Clone)]
struct ModuleExports {
    exports: HashMap<String, Type>,
    default_export: Option<Type>,
}

fn offset_to_position(text: &str, offset: usize) -> Position {
    let mut line = 0u32;
    let mut last_line_start = 0usize;
    for (i, c) in text.char_indices() {
        if i >= offset {
            break;
        }
        if c == '\n' {
            line += 1;
            last_line_start = i + 1;
        }
    }
    let slice_len = if offset <= text.len() {
        offset
    } else {
        text.len()
    };
    let character = text[last_line_start..slice_len].encode_utf16().count() as u32;
    Position { line, character }
}

fn position_to_offset(text: &str, position: Position) -> usize {
    let mut line = 0u32;
    let mut col = 0u32;
    for (i, c) in text.char_indices() {
        if line == position.line && col >= position.character {
            return i;
        }
        if c == '\n' {
            line += 1;
            col = 0;
        } else {
            col += c.len_utf16() as u32;
        }
    }
    text.len()
}

fn collect_tokens(text: &str) -> Vec<(Token, Span)> {
    let mut lexer = Lexer::new(text);
    let mut tokens = Vec::new();
    loop {
        let (token, span) = lexer.next_token_with_span();
        if token == Token::EOF {
            break;
        }
        tokens.push((token, span));
    }
    tokens
}

fn find_token_at_offset(tokens: &[(Token, Span)], offset: usize) -> Option<(usize, &Token, Span)> {
    tokens.iter().enumerate().find_map(|(idx, (token, span))| {
        if span.start <= offset && offset < span.end {
            Some((idx, token, *span))
        } else {
            None
        }
    })
}

fn find_token_before_offset(
    tokens: &[(Token, Span)],
    offset: usize,
) -> Option<(usize, &Token, Span)> {
    let mut last: Option<(usize, &Token, Span)> = None;
    for (idx, (token, span)) in tokens.iter().enumerate() {
        if span.end > offset {
            break;
        }
        last = Some((idx, token, *span));
    }
    last
}

fn find_scope_for_offset(scopes: &[ngn::analyzer::ScopeInfo], offset: usize) -> usize {
    let mut best_id = 0usize;
    let mut best_len = usize::MAX;
    for scope in scopes {
        if scope.span.start <= offset && offset <= scope.span.end {
            let len = scope.span.end.saturating_sub(scope.span.start);
            if len < best_len {
                best_len = len;
                best_id = scope.id;
            }
        }
    }
    best_id
}

fn lookup_symbol_in_scope(
    scopes: &[ngn::analyzer::ScopeInfo],
    mut scope_id: usize,
    name: &str,
) -> Option<ngn::analyzer::SymbolInfo> {
    loop {
        if let Some(scope) = scopes.get(scope_id) {
            if let Some(symbol) = scope.symbols.iter().rev().find(|s| s.name == name) {
                return Some(symbol.clone());
            }
            if let Some(parent) = scope.parent {
                scope_id = parent;
                continue;
            }
        }
        break;
    }
    None
}

fn lookup_symbol_global(scopes: &[ngn::analyzer::ScopeInfo], name: &str) -> Option<Type> {
    let global = scopes.iter().find(|s| s.parent.is_none())?;
    global
        .symbols
        .iter()
        .rev()
        .find(|s| s.name == name)
        .map(|s| s.ty.clone())
}

fn compute_module_exports(
    statements: &[Statement],
    analysis: &ngn::analyzer::Analysis,
) -> ModuleExports {
    let mut exports = HashMap::new();
    let mut default_export = None;

    for stmt in statements {
        match &stmt.kind {
            StatementKind::Function {
                name, is_exported, ..
            } => {
                if *is_exported {
                    let ty = lookup_symbol_global(&analysis.scopes, name).unwrap_or(Type::Any);
                    exports.insert(name.clone(), ty);
                }
            }
            StatementKind::TypeAlias {
                name,
                target,
                is_exported,
            } => {
                if *is_exported {
                    exports.insert(name.clone(), target.clone());
                }
            }
            StatementKind::Model { def, is_exported } => {
                if *is_exported {
                    exports.insert(def.name.clone(), Type::Model(def.name.clone()));
                }
            }
            StatementKind::Role { def, is_exported } => {
                if *is_exported {
                    exports.insert(def.name.clone(), Type::Role(def.name.clone()));
                }
            }
            StatementKind::Enum { def, is_exported } => {
                if *is_exported {
                    exports.insert(def.name.clone(), Type::Enum(def.name.clone()));
                }
            }
            StatementKind::ExportDefault(expr) => {
                let mut ty = analysis.expr_types.get(&expr.span).cloned();
                if ty.is_none() {
                    if let ExprKind::Variable(name) = &expr.kind {
                        ty = lookup_symbol_global(&analysis.scopes, name);
                    }
                }
                default_export = ty.or(Some(Type::Any));
            }
            _ => {}
        }
    }

    ModuleExports {
        exports,
        default_export,
    }
}

fn resolve_module_path(current_uri: &Url, source: &str) -> Option<PathBuf> {
    if source == "tbx" || source.starts_with("tbx::") {
        return None;
    }

    let path = Path::new(source);
    if path.extension().is_none() {
        return None;
    }

    if path.is_absolute() {
        return Some(path.to_path_buf());
    }

    let current_path = current_uri.to_file_path().ok()?;
    let base = current_path.parent()?;
    Some(base.join(path))
}

fn find_member_access<'a>(
    analysis: &'a ngn::analyzer::Analysis,
    offset: usize,
) -> Option<&'a ngn::analyzer::MemberAccessInfo> {
    let mut best: Option<&ngn::analyzer::MemberAccessInfo> = None;
    let mut best_len = usize::MAX;
    for info in &analysis.member_accesses {
        if info.span.start <= offset && offset <= info.span.end {
            let len = info.span.end.saturating_sub(info.span.start);
            if len < best_len {
                best = Some(info);
                best_len = len;
            }
        }
    }
    best
}

fn find_call_site<'a>(
    analysis: &'a ngn::analyzer::Analysis,
    offset: usize,
) -> Option<&'a ngn::analyzer::CallSiteInfo> {
    let mut best: Option<&ngn::analyzer::CallSiteInfo> = None;
    let mut best_len = usize::MAX;
    for info in &analysis.call_sites {
        if info.span.start <= offset && offset <= info.span.end {
            let len = info.span.end.saturating_sub(info.span.start);
            if len < best_len {
                best = Some(info);
                best_len = len;
            }
        }
    }
    best
}

fn unwrap_maybe(ty: &Type) -> (Type, bool) {
    match ty {
        Type::Generic(name, args) if name == "Maybe" && args.len() == 1 => (args[0].clone(), true),
        _ => (ty.clone(), false),
    }
}

fn generic_type_for_completion(ty: &Type) -> Type {
    match ty {
        Type::Tuple(_) => Type::Tuple(vec![]),
        Type::Array(_) => Type::Array(Box::new(Type::Any)),
        Type::Map(_, _) => Type::Map(Box::new(Type::Any), Box::new(Type::Any)),
        Type::Set(_) => Type::Set(Box::new(Type::Any)),
        Type::Union(members) => {
            Type::Union(members.iter().map(generic_type_for_completion).collect())
        }
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

fn substitute_type_params(ty: &Type, params: &[String], args: &[Type]) -> Type {
    match ty {
        Type::TypeParam(name) => {
            if let Some(idx) = params.iter().position(|p| p == name) {
                if idx < args.len() {
                    return args[idx].clone();
                }
            }
            ty.clone()
        }
        Type::Array(inner) => Type::Array(Box::new(substitute_type_params(inner, params, args))),
        Type::Tuple(elements) => Type::Tuple(
            elements
                .iter()
                .map(|e| substitute_type_params(e, params, args))
                .collect(),
        ),
        Type::Channel(inner) => {
            Type::Channel(Box::new(substitute_type_params(inner, params, args)))
        }
        Type::State(inner) => Type::State(Box::new(substitute_type_params(inner, params, args))),
        Type::Map(k, v) => Type::Map(
            Box::new(substitute_type_params(k, params, args)),
            Box::new(substitute_type_params(v, params, args)),
        ),
        Type::Set(inner) => Type::Set(Box::new(substitute_type_params(inner, params, args))),
        Type::Function {
            params: fn_params,
            optional_count,
            return_type,
        } => Type::Function {
            params: fn_params
                .iter()
                .map(|p| substitute_type_params(p, params, args))
                .collect(),
            optional_count: *optional_count,
            return_type: Box::new(substitute_type_params(return_type, params, args)),
        },
        Type::Generic(name, inner_args) => Type::Generic(
            name.clone(),
            inner_args
                .iter()
                .map(|t| substitute_type_params(t, params, args))
                .collect(),
        ),
        Type::Union(members) => Type::Union(
            members
                .iter()
                .map(|m| substitute_type_params(m, params, args))
                .collect(),
        ),
        _ => ty.clone(),
    }
}

fn model_fields_for_type(analysis: &ngn::analyzer::Analysis, ty: &Type) -> Vec<(String, Type)> {
    match ty {
        Type::Model(name) => analysis
            .models
            .get(name)
            .map(|m| {
                m.fields
                    .iter()
                    .map(|field| {
                        let field_ty = if field.is_optional {
                            Type::Generic("Maybe".to_string(), vec![field.field_type.clone()])
                        } else {
                            field.field_type.clone()
                        };
                        (field.name.clone(), field_ty)
                    })
                    .collect::<Vec<(String, Type)>>()
            })
            .unwrap_or_default(),
        Type::Generic(name, args) => {
            if let Some(model_def) = analysis.models.get(name) {
                let mut out = Vec::new();
                for ModelField {
                    name: field_name,
                    field_type,
                    is_optional,
                    ..
                } in &model_def.fields
                {
                    let mapped = substitute_type_params(field_type, &model_def.type_params, args);
                    let field_ty = if *is_optional {
                        Type::Generic("Maybe".to_string(), vec![mapped])
                    } else {
                        mapped
                    };
                    out.push((field_name.clone(), field_ty));
                }
                out
            } else {
                Vec::new()
            }
        }
        _ => Vec::new(),
    }
}

fn builtin_methods_for_type(ty: &Type) -> Vec<&'static str> {
    match ty {
        Type::Array(_) => vec!["size", "push", "pop", "splice", "slice", "copy", "each"],
        Type::Bytes => vec!["length", "copy", "slice", "toString", "toStringStrict"],
        Type::String => vec![
            "length", "index", "includes", "starts", "ends", "upper", "lower", "trim", "replace",
            "repeat", "copy", "slice", "split",
        ],
        Type::Tuple(_) => vec!["size", "toArray", "includes", "index", "copy", "join"],
        Type::State(_) => vec!["read", "write", "update"],
        Type::Channel(_) => vec!["close"],
        Type::Map(_, _) => vec!["size", "has", "get", "set", "remove"],
        Type::Set(_) => vec!["size", "has", "add", "remove", "clear"],
        Type::Json => vec!["parse", "stringify"],
        Type::Spawn => vec!["cpu", "block", "all", "try", "race"],
        Type::Env => vec!["get", "has"],
        Type::Time => vec!["now", "utc", "unix", "unixMs", "parse"],
        Type::Model(name) if name == "Response" => vec!["text", "json"],
        Type::Model(name) if name == "Request" => vec!["clone", "text", "json", "formData"],
        Type::Union(members) => {
            if members
                .iter()
                .all(|t| matches!(t, Type::String | Type::Bytes))
            {
                vec!["toString", "toStringStrict"]
            } else {
                Vec::new()
            }
        }
        _ => Vec::new(),
    }
}

fn builtin_method_type(ty: &Type, method: &str) -> Option<Type> {
    match ty {
        Type::Array(inner) => match method {
            "size" => Some(Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(Type::I64),
            }),
            "push" => Some(Type::Function {
                params: vec![*inner.clone(), Type::I64],
                optional_count: 1,
                return_type: Box::new(Type::I64),
            }),
            "pop" => Some(Type::Function {
                params: vec![Type::I64],
                optional_count: 1,
                return_type: Box::new(*inner.clone()),
            }),
            "splice" => Some(Type::Function {
                params: vec![Type::Array(inner.clone()), Type::I64],
                optional_count: 1,
                return_type: Box::new(Type::I64),
            }),
            "slice" | "copy" => Some(Type::Function {
                params: vec![Type::I64],
                optional_count: 1,
                return_type: Box::new(Type::Array(inner.clone())),
            }),
            "each" => Some(Type::Function {
                params: vec![Type::Function {
                    params: vec![*inner.clone()],
                    optional_count: 0,
                    return_type: Box::new(Type::Any),
                }],
                optional_count: 0,
                return_type: Box::new(Type::Void),
            }),
            _ => None,
        },
        Type::Bytes => match method {
            "length" => Some(Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(Type::I64),
            }),
            "copy" | "slice" => Some(Type::Function {
                params: vec![Type::I64],
                optional_count: 1,
                return_type: Box::new(Type::Bytes),
            }),
            "toString" | "toStringStrict" => Some(Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(Type::String),
            }),
            _ => None,
        },
        Type::String => match method {
            "length" => Some(Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(Type::I64),
            }),
            "index" => Some(Type::Function {
                params: vec![Type::String, Type::I64],
                optional_count: 1,
                return_type: Box::new(Type::I64),
            }),
            "includes" | "starts" | "ends" => Some(Type::Function {
                params: vec![Type::String],
                optional_count: 0,
                return_type: Box::new(Type::Bool),
            }),
            "upper" | "lower" | "trim" => Some(Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(Type::String),
            }),
            "replace" => Some(Type::Function {
                params: vec![Type::Union(vec![Type::String, Type::Regex]), Type::String],
                optional_count: 0,
                return_type: Box::new(Type::String),
            }),
            "repeat" => Some(Type::Function {
                params: vec![Type::I64],
                optional_count: 0,
                return_type: Box::new(Type::String),
            }),
            "copy" | "slice" => Some(Type::Function {
                params: vec![Type::I64],
                optional_count: 1,
                return_type: Box::new(Type::String),
            }),
            "split" => Some(Type::Function {
                params: vec![Type::String],
                optional_count: 1,
                return_type: Box::new(Type::Array(Box::new(Type::String))),
            }),
            _ => None,
        },
        Type::Tuple(_) => match method {
            "size" => Some(Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(Type::I64),
            }),
            "toArray" => Some(Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(Type::Array(Box::new(Type::Any))),
            }),
            "includes" => Some(Type::Function {
                params: vec![Type::Any],
                optional_count: 0,
                return_type: Box::new(Type::Bool),
            }),
            "index" => Some(Type::Function {
                params: vec![Type::Any],
                optional_count: 0,
                return_type: Box::new(Type::I64),
            }),
            "copy" => Some(Type::Function {
                params: vec![Type::I64, Type::I64],
                optional_count: 2,
                return_type: Box::new(Type::Any),
            }),
            "join" => Some(Type::Function {
                params: vec![Type::String],
                optional_count: 0,
                return_type: Box::new(Type::String),
            }),
            _ => None,
        },
        Type::State(inner) => match method {
            "read" => Some(Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(*inner.clone()),
            }),
            "write" => Some(Type::Function {
                params: vec![*inner.clone()],
                optional_count: 0,
                return_type: Box::new(Type::Void),
            }),
            "update" => Some(Type::Function {
                params: vec![Type::Function {
                    params: vec![*inner.clone()],
                    optional_count: 0,
                    return_type: Box::new(*inner.clone()),
                }],
                optional_count: 0,
                return_type: Box::new(Type::Void),
            }),
            _ => None,
        },
        Type::Channel(_) => match method {
            "close" => Some(Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(Type::Void),
            }),
            _ => None,
        },
        Type::Map(key, value) => match method {
            "size" => Some(Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(Type::I64),
            }),
            "has" => Some(Type::Function {
                params: vec![*key.clone()],
                optional_count: 0,
                return_type: Box::new(Type::Bool),
            }),
            "get" => Some(Type::Function {
                params: vec![*key.clone()],
                optional_count: 0,
                return_type: Box::new(Type::Generic("Maybe".to_string(), vec![*value.clone()])),
            }),
            "set" => Some(Type::Function {
                params: vec![*key.clone(), *value.clone()],
                optional_count: 0,
                return_type: Box::new(Type::Map(key.clone(), value.clone())),
            }),
            "remove" => Some(Type::Function {
                params: vec![*key.clone()],
                optional_count: 0,
                return_type: Box::new(*value.clone()),
            }),
            _ => None,
        },
        Type::Set(element) => match method {
            "size" => Some(Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(Type::I64),
            }),
            "has" => Some(Type::Function {
                params: vec![*element.clone()],
                optional_count: 0,
                return_type: Box::new(Type::Bool),
            }),
            "add" => Some(Type::Function {
                params: vec![*element.clone()],
                optional_count: 0,
                return_type: Box::new(Type::Set(element.clone())),
            }),
            "remove" => Some(Type::Function {
                params: vec![*element.clone()],
                optional_count: 0,
                return_type: Box::new(Type::Bool),
            }),
            "clear" => Some(Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(Type::Void),
            }),
            _ => None,
        },
        Type::Json => match method {
            "parse" => Some(Type::Function {
                params: vec![Type::String],
                optional_count: 0,
                return_type: Box::new(Type::Any),
            }),
            "stringify" => Some(Type::Function {
                params: vec![Type::Any],
                optional_count: 0,
                return_type: Box::new(Type::String),
            }),
            _ => None,
        },
        Type::Spawn => match method {
            "cpu" | "block" => Some(Type::Function {
                params: vec![Type::Any],
                optional_count: 0,
                return_type: Box::new(Type::Channel(Box::new(Type::Generic(
                    "Result".to_string(),
                    vec![Type::Any, Type::String],
                )))),
            }),
            "all" | "try" => Some(Type::Function {
                params: vec![Type::Array(Box::new(Type::Any)), Type::Any],
                optional_count: 1,
                return_type: Box::new(Type::Array(Box::new(Type::Generic(
                    "Result".to_string(),
                    vec![Type::Any, Type::String],
                )))),
            }),
            "race" => Some(Type::Function {
                params: vec![Type::Array(Box::new(Type::Any))],
                optional_count: 0,
                return_type: Box::new(Type::Generic(
                    "Result".to_string(),
                    vec![Type::Any, Type::String],
                )),
            }),
            _ => None,
        },
        Type::Env => match method {
            "get" => Some(Type::Function {
                params: vec![Type::String],
                optional_count: 0,
                return_type: Box::new(Type::Generic("Maybe".to_string(), vec![Type::String])),
            }),
            "has" => Some(Type::Function {
                params: vec![Type::String],
                optional_count: 0,
                return_type: Box::new(Type::Bool),
            }),
            _ => None,
        },
        Type::Time => match method {
            "now" | "utc" => Some(Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(Type::Model("DateTime".to_string())),
            }),
            "unix" | "unixMs" => Some(Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(Type::I64),
            }),
            "parse" => Some(Type::Function {
                params: vec![Type::String, Type::String],
                optional_count: 0,
                return_type: Box::new(Type::Generic(
                    "Result".to_string(),
                    vec![Type::Model("DateTime".to_string()), Type::String],
                )),
            }),
            _ => None,
        },
        Type::Model(name) if name == "Response" => match method {
            "text" => Some(Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(Type::String),
            }),
            "json" => Some(Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(Type::Generic(
                    "Result".to_string(),
                    vec![Type::Any, Type::String],
                )),
            }),
            _ => None,
        },
        Type::Model(name) if name == "Request" => match method {
            "clone" => Some(Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(Type::Model("Request".to_string())),
            }),
            "text" => Some(Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(Type::String),
            }),
            "json" => Some(Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(Type::Generic(
                    "Result".to_string(),
                    vec![Type::Any, Type::String],
                )),
            }),
            "formData" => Some(Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(Type::Map(Box::new(Type::String), Box::new(Type::String))),
            }),
            _ => None,
        },
        Type::Union(members) => {
            if (method == "toString" || method == "toStringStrict")
                && members
                    .iter()
                    .all(|t| matches!(t, Type::String | Type::Bytes))
            {
                Some(Type::Function {
                    params: vec![],
                    optional_count: 0,
                    return_type: Box::new(Type::String),
                })
            } else {
                None
            }
        }
        _ => None,
    }
}

fn tbx_function_type(module: &str, name: &str) -> Option<Type> {
    let ty = match (module, name) {
        ("test", "assert") => Type::Function {
            params: vec![Type::Bool, Type::String],
            optional_count: 1,
            return_type: Box::new(Type::Void),
        },
        ("math", "abs")
        | ("math", "round")
        | ("math", "floor")
        | ("math", "ceil")
        | ("math", "trunc") => Type::Function {
            params: vec![Type::Any],
            optional_count: 0,
            return_type: Box::new(Type::Any),
        },
        ("math", "sign") => Type::Function {
            params: vec![Type::Any],
            optional_count: 0,
            return_type: Box::new(Type::F64),
        },
        ("math", "sin")
        | ("math", "cos")
        | ("math", "tan")
        | ("math", "asin")
        | ("math", "acos")
        | ("math", "atan")
        | ("math", "sqrt")
        | ("math", "exp")
        | ("math", "log")
        | ("math", "log10")
        | ("math", "log2") => Type::Function {
            params: vec![Type::Any],
            optional_count: 0,
            return_type: Box::new(Type::F64),
        },
        ("math", "pow") | ("math", "atan2") => Type::Function {
            params: vec![Type::Any, Type::Any],
            optional_count: 0,
            return_type: Box::new(Type::F64),
        },
        ("math", "min") | ("math", "max") => Type::Function {
            params: vec![Type::Any, Type::Any, Type::Any, Type::Any, Type::Any],
            optional_count: 3,
            return_type: Box::new(Type::F64),
        },
        ("math", "clamp") => Type::Function {
            params: vec![Type::Any, Type::Any, Type::Any],
            optional_count: 0,
            return_type: Box::new(Type::F64),
        },
        ("math", "PI") => Type::Function {
            params: vec![],
            optional_count: 0,
            return_type: Box::new(Type::F64),
        },
        ("http", "serve") => Type::Function {
            params: vec![Type::Any, Type::Any],
            optional_count: 1,
            return_type: Box::new(Type::Void),
        },
        ("io", "read") => Type::Function {
            params: vec![Type::String],
            optional_count: 0,
            return_type: Box::new(Type::Generic(
                "Result".to_string(),
                vec![Type::String, Type::String],
            )),
        },
        ("io", "write") | ("io", "append") => Type::Function {
            params: vec![Type::String, Type::String],
            optional_count: 0,
            return_type: Box::new(Type::Generic(
                "Result".to_string(),
                vec![Type::Void, Type::String],
            )),
        },
        ("io", "exists") => Type::Function {
            params: vec![Type::String],
            optional_count: 0,
            return_type: Box::new(Type::Bool),
        },
        ("io", "delete") => Type::Function {
            params: vec![Type::String],
            optional_count: 0,
            return_type: Box::new(Type::Generic(
                "Result".to_string(),
                vec![Type::Void, Type::String],
            )),
        },
        ("encoding", "hexEncode") => Type::Function {
            params: vec![Type::Bytes],
            optional_count: 0,
            return_type: Box::new(Type::String),
        },
        ("encoding", "hexDecode") => Type::Function {
            params: vec![Type::String],
            optional_count: 0,
            return_type: Box::new(Type::Bytes),
        },
        ("encoding", "base64Encode") => Type::Function {
            params: vec![Type::Bytes],
            optional_count: 0,
            return_type: Box::new(Type::String),
        },
        ("encoding", "base64Decode") => Type::Function {
            params: vec![Type::String],
            optional_count: 0,
            return_type: Box::new(Type::Bytes),
        },
        ("process", "run") => Type::Function {
            params: vec![Type::String, Type::Any],
            optional_count: 1,
            return_type: Box::new(Type::Channel(Box::new(Type::Generic(
                "Result".to_string(),
                vec![Type::Model("ProcessOutput".to_string()), Type::String],
            )))),
        },
        ("process", "stream") | ("process", "streamRaw") => Type::Function {
            params: vec![Type::String, Type::Any],
            optional_count: 1,
            return_type: Box::new(Type::Generic(
                "Result".to_string(),
                vec![Type::Model("ProcessStream".to_string()), Type::String],
            )),
        },
        ("llm", "load") => Type::Function {
            params: vec![Type::String, Type::Any],
            optional_count: 1,
            return_type: Box::new(Type::Generic(
                "Result".to_string(),
                vec![Type::Model("LlmModel".to_string()), Type::String],
            )),
        },
        ("llm", "generate") => Type::Function {
            params: vec![Type::Model("LlmModel".to_string()), Type::String, Type::Any],
            optional_count: 1,
            return_type: Box::new(Type::Generic(
                "Result".to_string(),
                vec![Type::String, Type::String],
            )),
        },
        ("llm", "stream") => Type::Function {
            params: vec![Type::Model("LlmModel".to_string()), Type::String, Type::Any],
            optional_count: 1,
            return_type: Box::new(Type::Channel(Box::new(Type::Model("LlmChunk".to_string())))),
        },
        _ => return None,
    };
    Some(ty)
}

fn tbx_module_exports(module: &str) -> Option<ModuleExports> {
    let toolbox = Toolbox::new();
    let tbx_module = toolbox.get_module(module)?;
    let mut exports = HashMap::new();

    for name in tbx_module.functions.keys() {
        let ty = tbx_function_type(module, name).unwrap_or(Type::Function {
            params: vec![Type::Any],
            optional_count: 0,
            return_type: Box::new(Type::Any),
        });
        exports.insert(name.clone(), ty);
    }

    Some(ModuleExports {
        exports,
        default_export: None,
    })
}

fn token_builtin_name(token: &Token) -> Option<&'static str> {
    match token {
        Token::Print => Some("print"),
        Token::Echo => Some("echo"),
        Token::Sleep => Some("sleep"),
        Token::Thread => Some("thread"),
        Token::Channel => Some("channel"),
        Token::Bytes => Some("bytes"),
        Token::State => Some("state"),
        Token::Map => Some("map"),
        Token::Set => Some("set"),
        _ => None,
    }
}

impl Backend {
    async fn load_module_exports(&self, path: &Path) -> Option<ModuleExports> {
        if let Some(cached) = self.module_cache.read().await.get(path).cloned() {
            return Some(cached);
        }

        let text = std::fs::read_to_string(path).ok()?;
        let lexer = Lexer::new(&text);
        let mut parser = Parser::new(lexer);
        let mut statements = Vec::new();

        while parser.current_token != Token::EOF {
            if parser.current_token == Token::Newline {
                parser.advance();
                continue;
            }
            statements.push(parser.parse_statement());
        }

        let mut analyzer = Analyzer::new_with_index(text.len());
        let (analysis, _) = analyzer.analyze_with_index(&statements);
        let exports = compute_module_exports(&statements, &analysis);

        self.module_cache
            .write()
            .await
            .insert(path.to_path_buf(), exports.clone());

        Some(exports)
    }

    async fn validate_document(&self, uri: Url, text: &str) -> DocumentState {
        let result = catch_unwind(AssertUnwindSafe(|| {
            let lexer = Lexer::new(text);
            let mut parser = Parser::new(lexer);
            let mut statements = Vec::new();

            while parser.current_token != Token::EOF {
                if parser.current_token == Token::Newline {
                    parser.advance();
                    continue;
                }
                statements.push(parser.parse_statement());
            }

            let parser_diagnostics = parser.diagnostics;

            let mut analyzer = Analyzer::new_with_index(text.len());
            let (analysis, analyzer_result) = analyzer.analyze_with_index(&statements);
            let analyzer_diagnostics = match analyzer_result {
                Ok(_) => Vec::new(),
                Err(d) => d,
            };

            let tokens = collect_tokens(text);

            (
                parser_diagnostics,
                analyzer_diagnostics,
                analysis,
                tokens,
                statements,
            )
        }));

        let (parser_diagnostics, analyzer_diagnostics, analysis, tokens, statements) = match result
        {
            Ok((parser_diagnostics, analyzer_diagnostics, analysis, tokens, statements)) => (
                parser_diagnostics,
                analyzer_diagnostics,
                analysis,
                tokens,
                statements,
            ),
            Err(_) => {
                let diagnostic = Diagnostic {
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 0,
                        },
                        end: Position {
                            line: 0,
                            character: 1,
                        },
                    },
                    severity: Some(DiagnosticSeverity::ERROR),
                    message: "Internal error: ngn parser crashed".to_string(),
                    source: Some("ngn".to_string()),
                    ..Default::default()
                };
                self.client
                    .publish_diagnostics(uri, vec![diagnostic], None)
                    .await;
                return DocumentState {
                    text: text.to_string(),
                    analysis: None,
                    tokens: Vec::new(),
                    imported_symbols: HashMap::new(),
                    module_aliases: HashMap::new(),
                };
            }
        };

        let mut raw_diagnostics: Vec<(String, Span)> = Vec::new();
        for ParseDiagnostic { message, span } in parser_diagnostics {
            raw_diagnostics.push((message, span));
        }
        for diag in analyzer_diagnostics {
            raw_diagnostics.push((diag.message, diag.span));
        }

        let mut seen = HashSet::new();
        let mut lsp_diagnostics = Vec::new();
        for (message, span) in raw_diagnostics {
            let key = (span.start, span.end, message.clone());
            if !seen.insert(key) {
                continue;
            }
            let start = offset_to_position(text, span.start);
            let end = offset_to_position(text, span.end);
            lsp_diagnostics.push(Diagnostic {
                range: Range { start, end },
                severity: Some(DiagnosticSeverity::ERROR),
                message,
                source: Some("ngn".to_string()),
                ..Default::default()
            });
        }

        self.client
            .publish_diagnostics(uri.clone(), lsp_diagnostics, None)
            .await;

        let mut imported_symbols = HashMap::new();
        let mut module_aliases = HashMap::new();

        if let Ok(path) = uri.to_file_path() {
            let module_exports = compute_module_exports(&statements, &analysis);
            self.module_cache.write().await.insert(path, module_exports);
        }

        for stmt in &statements {
            match &stmt.kind {
                StatementKind::Import { names, source } => {
                    if source.starts_with("tbx::") {
                        let module = source.trim_start_matches("tbx::");
                        if let Some(exports) = tbx_module_exports(module) {
                            for (name, alias) in names {
                                let import_name = alias.as_ref().unwrap_or(name);
                                let ty = exports.exports.get(name).cloned().unwrap_or(Type::Any);
                                imported_symbols.insert(import_name.clone(), ty);
                            }
                        }
                        continue;
                    }

                    if let Some(path) = resolve_module_path(&uri, source) {
                        let exports = self.load_module_exports(&path).await;
                        if let Some(exports) = exports {
                            for (name, alias) in names {
                                let import_name = alias.as_ref().unwrap_or(name);
                                let ty = exports.exports.get(name).cloned().unwrap_or(Type::Any);
                                imported_symbols.insert(import_name.clone(), ty);
                            }
                        }
                    }
                }
                StatementKind::ImportDefault { name, source } => {
                    if source.starts_with("tbx::") {
                        if let Some(def) = get_type(name) {
                            imported_symbols.insert(name.clone(), def.ty);
                        } else {
                            imported_symbols.insert(name.clone(), Type::Any);
                        }
                        continue;
                    }

                    if let Some(path) = resolve_module_path(&uri, source) {
                        let exports = self.load_module_exports(&path).await;
                        if let Some(exports) = exports {
                            let ty = exports.default_export.unwrap_or(Type::Any);
                            imported_symbols.insert(name.clone(), ty);
                        }
                    }
                }
                StatementKind::ImportModule { alias, source } => {
                    if source.starts_with("tbx::") {
                        let module = source.trim_start_matches("tbx::");
                        if let Some(exports) = tbx_module_exports(module) {
                            module_aliases.insert(alias.clone(), exports);
                        }
                        continue;
                    }

                    if let Some(path) = resolve_module_path(&uri, source) {
                        let exports = self.load_module_exports(&path).await;
                        if let Some(exports) = exports {
                            module_aliases.insert(alias.clone(), exports);
                        }
                    }
                }
                _ => {}
            }
        }

        DocumentState {
            text: text.to_string(),
            analysis: Some(analysis),
            tokens,
            imported_symbols,
            module_aliases,
        }
    }
}

fn modifier_to_bit(modifier: &str) -> u32 {
    match modifier {
        "declaration" => 1 << 0,
        "readonly" => 1 << 1,
        "static" => 1 << 2,
        "defaultLibrary" => 1 << 3,
        _ => 0,
    }
}

// Helper function to map a token to type and modifiers
fn get_semantic_type(
    token: &Token,
    prev_token: Option<&Token>,
    next_token: Option<&Token>,
    is_constant: bool,
    is_default_library: bool,
) -> (u32, u32) {
    let prev_is_class_keyword = matches!(
        prev_token,
        Some(Token::Enum | Token::Model | Token::Role | Token::Extend | Token::With)
    );

    let is_likely_class = matches!(token, Token::Identifier(name)
        if name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false));

    let is_method_call =
        matches!(prev_token, Some(Token::Period)) && matches!(next_token, Some(Token::LParen));

    let mut modifiers = vec![];
    let is_map_set_type =
        matches!(token, Token::Map | Token::Set) && matches!(next_token, Some(Token::LessThan));

    // Declaration check - determine if we are in a declaration
    if let Some(Token::Const | Token::Global) = prev_token {
        if matches!(token, Token::Identifier(_)) {
            modifiers.push("declaration");
            modifiers.push("readonly");
        }
    } else if let Some(Token::Var) = prev_token {
        if matches!(token, Token::Identifier(_)) {
            modifiers.push("declaration");
        }
    }

    // Modifiers for the tokens themselves
    match token {
        Token::Var | Token::Const | Token::Global => {
            modifiers.push("declaration");
        }
        Token::Bool(_) => {
            modifiers.push("readonly");
        }
        Token::This => {
            modifiers.push("readonly");
        }
        Token::Print
        | Token::Echo
        | Token::Sleep
        | Token::Thread
        | Token::State
        | Token::Channel
        | Token::Bytes
        //| Token::Json
        //| Token::Spawn
        | Token::Map
        | Token::Set
        //| Token::Fetch
        //| Token::Panic
        //| Token::Env
        //| Token::Time
        => {
            if !matches!(prev_token, Some(Token::Period))
                && !prev_is_class_keyword
                && !is_map_set_type
            {
                modifiers.push("defaultLibrary");
            }
        }
        Token::Identifier(_) => {
            if is_constant {
                modifiers.push("readonly");
            }
            if is_default_library
                && !matches!(prev_token, Some(Token::Const | Token::Global | Token::Var))
            {
                modifiers.push("defaultLibrary");
            }
        }
        _ => {}
    }

    let token_type = match token {
        // Keywords
        Token::Var
        | Token::Const
        | Token::Global
        | Token::Break
        | Token::Enum
        | Token::Extend
        | Token::If
        | Token::Match
        | Token::Model
        | Token::Next
        | Token::Return
        | Token::Role
        | Token::While
        | Token::Loop
        | Token::For
        | Token::In
        | Token::Once
        | Token::With
        | Token::Fn
        | Token::Import
        | Token::From
        | Token::As
        | Token::Export
        | Token::Default
        | Token::This
        | Token::Check => 0, // keyword

        Token::Null => 8,

        Token::Float(_) | Token::Number(_) => 1, // number
        Token::StringStart | Token::StringEnd | Token::StringPart(_) => 2, // string
        // Punctuation
        Token::LParen
        | Token::RParen
        | Token::LBracket
        | Token::RBracket
        | Token::LBrace
        | Token::RBrace
        | Token::Colon
        | Token::Comma
        | Token::Period
        | Token::DoubleColon => 6,
        Token::Map | Token::Set if is_map_set_type => 8,
        Token::Map | Token::Set if prev_is_class_keyword => 9,
        Token::Print
        | Token::Echo
        | Token::Sleep
        | Token::Thread
        | Token::State
        | Token::Channel
        | Token::Bytes
        //| Token::Json
        //| Token::Spawn
        | Token::Map
        | Token::Set
        //| Token::Fetch
        //| Token::Panic
        //| Token::Env
        //| Token::Time
        => 4, // function
        Token::Identifier(_) => {
            if prev_is_class_keyword || is_likely_class {
                9 // class
            } else if matches!(next_token, Some(Token::LParen)) || is_method_call {
                4 // function
            } else {
                3 // variable
            }
        }
        Token::Bool(_) => 0, // keyword (for fallback)

        // Operators
        Token::Power
        | Token::EqualEqual
        | Token::NotEqual
        | Token::LessThanEqual
        | Token::GreaterThanEqual
        | Token::Plus
        | Token::Minus
        | Token::Star
        | Token::Slash
        | Token::Percent
        | Token::Equal
        | Token::LessThan
        | Token::GreaterThan
        | Token::PlusEqual
        | Token::MinusEqual
        | Token::StarEqual
        | Token::SlashEqual
        | Token::PercentEqual
        | Token::StarStarEqual
        | Token::CaretEqual
        | Token::FatArrow
        | Token::LArrow
        | Token::Pipe
        | Token::PipeForward
        | Token::Bang
        | Token::AndAnd
        | Token::OrOr => 5,

        Token::Regex(_) => 10,
        Token::InterpolationStart | Token::InterpolationEnd => 3, // variable-like for braces
        Token::Placeholder(_) | Token::PlaceholderSelf => 3,
        _ => 3,
    };

    let mod_bitset = modifiers
        .iter()
        .map(|m| modifier_to_bit(m))
        .fold(0, |acc, bit| acc | bit);

    (token_type, mod_bitset)
}

fn collect_toolbox_imports(text: &str) -> (HashSet<String>, HashSet<String>) {
    let lexer = Lexer::new(text);
    let mut parser = Parser::new(lexer);
    let mut imports = HashSet::new();
    let mut module_aliases = HashSet::new();

    while parser.current_token != Token::EOF {
        if parser.current_token == Token::Newline {
            parser.advance();
            continue;
        }

        let statement = parser.parse_statement();
        match statement.kind {
            StatementKind::Import { names, source } => {
                if source.starts_with("tbx::") {
                    for (name, alias) in names {
                        imports.insert(alias.unwrap_or(name));
                    }
                }
            }
            StatementKind::ImportDefault { name, source } => {
                if source.starts_with("tbx::") {
                    imports.insert(name);
                }
            }
            StatementKind::ImportModule { alias, source } => {
                if source.starts_with("tbx::") {
                    module_aliases.insert(alias);
                }
            }
            _ => {}
        }
    }

    (imports, module_aliases)
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: SemanticTokensLegend {
                                token_types: vec![
                                    "keyword".into(),     // 0
                                    "number".into(),      // 1
                                    "string".into(),      // 2
                                    "variable".into(),    // 3
                                    "function".into(),    // 4
                                    "operator".into(),    // 5
                                    "punctuation".into(), // 6
                                    "comment".into(),     // 7
                                    "type".into(),        // 8
                                    "class".into(),       // 9
                                    "regexp".into(),      // 10
                                ],
                                token_modifiers: vec![
                                    "declaration".into(),    // 0
                                    "readonly".into(),       // 1
                                    "static".into(),         // 2
                                    "defaultLibrary".into(), // 3
                                    "toolbox".into(),        // 4
                                ],
                            },
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            ..Default::default()
                        },
                    ),
                ),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".into(), ":".into(), "?".into()]),
                    ..Default::default()
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".into(), ",".into()]),
                    retrigger_characters: Some(vec![",".into()]),
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                }),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "ngn-lsp server initialized!")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let text = params.text_document.text;
        let state = self.validate_document(uri.clone(), &text).await;
        self.documents.write().await.insert(uri.to_string(), state);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        if let Some(change) = params.content_changes.last() {
            let text = change.text.clone();
            let state = self.validate_document(uri.clone(), &text).await;
            self.documents.write().await.insert(uri.to_string(), state);
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_string();

        let documents = self.documents.read().await;
        let (text, tokens_with_spans) = match documents.get(&uri) {
            Some(doc) => (doc.text.clone(), doc.tokens.clone()),
            None => return Ok(None),
        };
        drop(documents);

        let (toolbox_imports, toolbox_module_aliases) = collect_toolbox_imports(&text);
        let core_globals: HashSet<&'static str> = GLOBAL_NAMES.iter().copied().collect();

        // Scope-based constant tracking
        let mut scopes: Vec<std::collections::HashSet<String>> =
            vec![std::collections::HashSet::new()]; // Global scope

        // Build line/column index from the source (UTF-16 based for LSP)
        let mut _line_starts = vec![0];
        let mut _char_count = 0u32;
        for (_i, c) in text.char_indices() {
            _char_count += c.len_utf16() as u32;
            if c == '\n' {
                _line_starts.push(_char_count);
            }
        }

        // Helper to convert byte offset to (line, character_offset) in UTF-16 units
        let byte_to_line_col = |byte_offset: usize| -> (u32, u32) {
            let byte_offset = byte_offset.min(text.len());
            let mut current_line_start_byte = 0;
            let mut line_count = 0;

            for (i, c) in text.char_indices() {
                if i >= byte_offset {
                    break;
                }
                if c == '\n' {
                    line_count += 1;
                    current_line_start_byte = i + 1;
                }
            }

            // Re-calculate character offset in the line (UTF-16)
            let line_text = &text[current_line_start_byte..byte_offset];
            let char_offset = line_text.encode_utf16().count();

            (line_count, char_offset as u32)
        };

        let mut semantic_tokens = Vec::new();
        let mut prev_line = 0u32;
        let mut prev_char = 0u32;

        for i in 0..tokens_with_spans.len() {
            let (ref token, span) = tokens_with_spans[i];
            let prev_token = if i > 0 {
                Some(&tokens_with_spans[i - 1].0)
            } else {
                None
            };
            let next_token = tokens_with_spans.get(i + 1).map(|(t, _)| t);
            let prev_prev_token = if i > 1 {
                Some(&tokens_with_spans[i - 2].0)
            } else {
                None
            };

            // Skip newlines in semantic tokens
            if matches!(token, Token::Newline | Token::Error(_)) {
                continue;
            }

            if span.start > span.end || span.end > text.len() {
                continue;
            }

            let (line, char) = byte_to_line_col(span.start);

            let delta_line = line - prev_line;
            let delta_start = if delta_line == 0 {
                char - prev_char
            } else {
                char
            };

            // Handle Scopes
            match token {
                Token::LBrace => {
                    scopes.push(std::collections::HashSet::new());
                }
                Token::RBrace => {
                    if scopes.len() > 1 {
                        scopes.pop();
                    }
                }
                Token::Identifier(name) => {
                    // Check if declaration of const/static
                    if let Some(Token::Const | Token::Global) = prev_token {
                        if let Some(current_scope) = scopes.last_mut() {
                            current_scope.insert(name.clone());
                        }
                    }
                    // Handle variable shadowing (remove from current scope if var)
                    else if let Some(Token::Var) = prev_token {
                        if let Some(current_scope) = scopes.last_mut() {
                            current_scope.remove(name);
                        }
                    }
                }
                _ => {}
            }

            // Determine if current identifier is a constant lookup
            let mut is_constant = false;
            if let Token::Identifier(name) = token {
                // Determine if declaration (in which case modifiers handle it) or usage
                let is_decl = matches!(prev_token, Some(Token::Const | Token::Global | Token::Var));
                if !is_decl {
                    // Look up in scopes from top to bottom
                    for scope in scopes.iter().rev() {
                        if scope.contains(name) {
                            is_constant = true;
                            break;
                        }
                    }
                }
            }

            let is_toolbox_member = match (token, prev_token, prev_prev_token) {
                (
                    Token::Identifier(_),
                    Some(Token::Period | Token::DoubleColon),
                    Some(Token::Identifier(module)),
                ) => toolbox_module_aliases.contains(module),
                _ => false,
            };
            let is_default_library = match token {
                Token::Identifier(name) => {
                    toolbox_imports.contains(name)
                        || core_globals.contains(name.as_str())
                        || is_toolbox_member
                }
                _ => false,
            };

            let (token_type, token_modifiers_bitset) = get_semantic_type(
                token,
                prev_token,
                next_token,
                is_constant,
                is_default_library,
            );

            let token_text = &text[span.start..span.end];

            // Special handling for Regex with flags (split into regex body + flags)
            if let Token::Regex(content) = token {
                if let Some(last_slash_idx) = content.rfind('/') {
                    if last_slash_idx < content.len() - 1 {
                        let body_end = span.start + last_slash_idx + 1;
                        if body_end > span.end {
                            continue;
                        }
                        // We have flags
                        let body_len = (text[span.start..body_end].encode_utf16().count()) as u32;
                        let flags_len = (text[(span.start + last_slash_idx + 1)..span.end]
                            .encode_utf16()
                            .count()) as u32;

                        // Emit body (regexp)
                        semantic_tokens.push(SemanticToken {
                            delta_line,
                            delta_start,
                            length: body_len,
                            token_type: 10, // regexp
                            token_modifiers_bitset: 0,
                        });

                        // Emit flags (keyword.control or similar)
                        semantic_tokens.push(SemanticToken {
                            delta_line: 0,
                            delta_start: body_len,
                            length: flags_len,
                            token_type: 0, // keyword
                            token_modifiers_bitset: 0,
                        });

                        // Update tracking for next token in loop
                        prev_line = line;
                        prev_char = char + body_len + flags_len; // Approx advance, though loop resets this
                        continue;
                    }
                }
            }

            let length = token_text.encode_utf16().count() as u32;

            semantic_tokens.push(SemanticToken {
                delta_line,
                delta_start,
                length,
                token_type,
                token_modifiers_bitset,
            });

            prev_line = line;
            prev_char = char;
        }

        eprintln!(
            "Generated {} semantic tokens for {}",
            semantic_tokens.len(),
            uri
        );

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: semantic_tokens,
        })))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;

        let documents = self.documents.read().await;
        let doc = match documents.get(&uri) {
            Some(doc) => doc.clone(),
            None => return Ok(None),
        };
        drop(documents);

        let analysis = match &doc.analysis {
            Some(a) => a,
            None => return Ok(None),
        };

        let offset = position_to_offset(&doc.text, position);
        let token_info = find_token_at_offset(&doc.tokens, offset).or_else(|| {
            offset
                .checked_sub(1)
                .and_then(|off| find_token_at_offset(&doc.tokens, off))
        });

        let (token_idx, token, token_span) = match token_info {
            Some((idx, token, span)) => (idx, token, span),
            None => return Ok(None),
        };

        let mut hover_text: Option<String> = None;

        if let Some(builtin_name) = token_builtin_name(token) {
            if let Some(def) = get_type(builtin_name) {
                hover_text = Some(format!("{}: {}", builtin_name, def.ty));
            } else {
                hover_text = Some(format!("{}: builtin", builtin_name));
            }
        } else if let Token::Identifier(name) = token {
            let mut is_member = false;
            if token_idx > 0 {
                is_member = matches!(
                    doc.tokens[token_idx - 1].0,
                    Token::Period | Token::QuestionDot
                );
            }

            if is_member {
                if let Some(member) = find_member_access(analysis, offset) {
                    if let Some(ty) = analysis.expr_types.get(&member.span) {
                        hover_text = Some(format!("{}: {}", member.member, ty));
                    }
                }
                if hover_text.is_none() && token_idx >= 2 {
                    if let Token::Identifier(obj_name) = &doc.tokens[token_idx - 2].0 {
                        if let Some(exports) = doc.module_aliases.get(obj_name) {
                            if let Some(ty) = exports.exports.get(name) {
                                hover_text = Some(format!("{}: {}", name, ty));
                            }
                        }
                        if let Some(fields) = analysis.object_bindings.get(obj_name) {
                            if let Some((_, field_ty)) =
                                fields.iter().find(|(field_name, _)| field_name == name)
                            {
                                hover_text = Some(format!("{}: {}", name, field_ty));
                            }
                        }
                    }
                }
            } else {
                if let Some(ty) = doc.imported_symbols.get(name) {
                    hover_text = Some(format!("{}: {}", name, ty));
                } else {
                    let scope_id = find_scope_for_offset(&analysis.scopes, offset);
                    if let Some(symbol) = lookup_symbol_in_scope(&analysis.scopes, scope_id, name) {
                        hover_text = Some(format!("{}: {}", symbol.name, symbol.ty));
                    } else if let Some(ty) = analysis.expr_types.get(&token_span) {
                        hover_text = Some(format!("{}: {}", name, ty));
                    } else if let Some(def) = get_type(name) {
                        hover_text = Some(format!("{}: {}", name, def.ty));
                    }
                }
            }
        }

        let Some(value) = hover_text else {
            return Ok(None);
        };

        Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::PlainText,
                value,
            }),
            range: None,
        }))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri.to_string();
        let position = params.text_document_position.position;

        let documents = self.documents.read().await;
        let doc = match documents.get(&uri) {
            Some(doc) => doc.clone(),
            None => return Ok(None),
        };
        drop(documents);

        let analysis = match &doc.analysis {
            Some(a) => a,
            None => return Ok(None),
        };

        let offset = position_to_offset(&doc.text, position);

        let token_at = find_token_at_offset(&doc.tokens, offset).or_else(|| {
            offset
                .checked_sub(1)
                .and_then(|off| find_token_at_offset(&doc.tokens, off))
        });
        let mut member_context = false;
        let mut enum_context = false;
        let mut enum_name: Option<String> = None;
        let mut member_object_name: Option<String> = None;
        let mut member_object_span: Option<Span> = None;
        let mut member_optional = false;

        if let Some((idx, token, _)) = token_at {
            if matches!(token, Token::Identifier(_)) && idx > 0 {
                if matches!(doc.tokens[idx - 1].0, Token::Period | Token::QuestionDot) {
                    member_context = true;
                    member_optional = matches!(doc.tokens[idx - 1].0, Token::QuestionDot);
                    if idx >= 2 {
                        if let Token::Identifier(name) = &doc.tokens[idx - 2].0 {
                            member_object_name = Some(name.clone());
                            member_object_span = Some(doc.tokens[idx - 2].1);
                        }
                    }
                }
                if matches!(doc.tokens[idx - 1].0, Token::DoubleColon) && idx > 1 {
                    if let Token::Identifier(name) = &doc.tokens[idx - 2].0 {
                        enum_context = true;
                        enum_name = Some(name.clone());
                    }
                }
            }
        }

        if !member_context && !enum_context {
            if let Some((idx, token, _)) = find_token_before_offset(&doc.tokens, offset) {
                if matches!(token, Token::Period | Token::QuestionDot) {
                    member_context = true;
                    member_optional = matches!(token, Token::QuestionDot);
                    if idx > 0 {
                        if let Token::Identifier(name) = &doc.tokens[idx - 1].0 {
                            member_object_name = Some(name.clone());
                            member_object_span = Some(doc.tokens[idx - 1].1);
                        }
                    }
                }
                if matches!(token, Token::DoubleColon) && idx > 0 {
                    if let Token::Identifier(name) = &doc.tokens[idx - 1].0 {
                        enum_context = true;
                        enum_name = Some(name.clone());
                    }
                }
            }
        }

        if !enum_context && offset >= 2 {
            let bytes = doc.text.as_bytes();
            if bytes[offset - 2] == b':' && bytes[offset - 1] == b':' {
                let mut start = offset.saturating_sub(2);
                while start > 0 {
                    let ch = bytes[start - 1];
                    if ch.is_ascii_alphanumeric() || ch == b'_' {
                        start -= 1;
                    } else {
                        break;
                    }
                }
                if start < offset.saturating_sub(2) {
                    let name = &doc.text[start..offset.saturating_sub(2)];
                    if !name.is_empty() {
                        enum_context = true;
                        enum_name = Some(name.to_string());
                    }
                }
            }
        }

        let mut items: Vec<CompletionItem> = Vec::new();

        if enum_context {
            if let Some(enum_name) = enum_name {
                if let Some(enum_def) = analysis.enums.get(&enum_name) {
                    for variant in &enum_def.variants {
                        items.push(CompletionItem {
                            label: variant.name.clone(),
                            kind: Some(CompletionItemKind::ENUM_MEMBER),
                            detail: Some(format!("{}::{}", enum_name, variant.name)),
                            ..Default::default()
                        });
                    }
                }
            }
            return Ok(Some(CompletionResponse::Array(items)));
        }

        if member_context {
            if let Some(name) = &member_object_name {
                if let Some(exports) = doc.module_aliases.get(name) {
                    let mut items = Vec::new();
                    for (export_name, export_ty) in &exports.exports {
                        items.push(CompletionItem {
                            label: export_name.clone(),
                            kind: Some(CompletionItemKind::FUNCTION),
                            detail: Some(export_ty.to_string()),
                            ..Default::default()
                        });
                    }
                    return Ok(Some(CompletionResponse::Array(items)));
                }
            }

            let mut obj_ty = if let Some(member_access) = find_member_access(analysis, offset) {
                analysis
                    .expr_types
                    .get(&member_access.object_span)
                    .cloned()
                    .unwrap_or(Type::Any)
            } else if let Some(span) = member_object_span {
                analysis.expr_types.get(&span).cloned().unwrap_or(Type::Any)
            } else if let Some(name) = &member_object_name {
                let scope_id = find_scope_for_offset(&analysis.scopes, offset);
                lookup_symbol_in_scope(&analysis.scopes, scope_id, name)
                    .map(|s| s.ty)
                    .unwrap_or(Type::Any)
            } else {
                Type::Any
            };

            if member_optional {
                obj_ty = unwrap_maybe(&obj_ty).0;
            }

            let mut seen = HashSet::new();

            if let Some(span) = member_object_span {
                if let Some(fields) = analysis.object_fields.get(&span) {
                    for (field_name, field_ty) in fields {
                        if seen.insert(field_name.clone()) {
                            items.push(CompletionItem {
                                label: field_name.clone(),
                                kind: Some(CompletionItemKind::FIELD),
                                detail: Some(field_ty.to_string()),
                                ..Default::default()
                            });
                        }
                    }
                }
            }

            if let Some(name) = &member_object_name {
                if let Some(fields) = analysis.object_bindings.get(name) {
                    for (field_name, field_ty) in fields {
                        if seen.insert(field_name.clone()) {
                            items.push(CompletionItem {
                                label: field_name.clone(),
                                kind: Some(CompletionItemKind::FIELD),
                                detail: Some(field_ty.to_string()),
                                ..Default::default()
                            });
                        }
                    }
                }
            }

            for (field_name, field_ty) in model_fields_for_type(analysis, &obj_ty) {
                if seen.insert(field_name.clone()) {
                    items.push(CompletionItem {
                        label: field_name,
                        kind: Some(CompletionItemKind::FIELD),
                        detail: Some(field_ty.to_string()),
                        ..Default::default()
                    });
                }
            }

            if let Type::Role(role_name) = &obj_ty {
                if let Some(role_def) = analysis.roles.get(role_name) {
                    for method in &role_def.methods {
                        if let StatementKind::Function { name, .. } = &method.kind {
                            if seen.insert(name.clone()) {
                                items.push(CompletionItem {
                                    label: name.clone(),
                                    kind: Some(CompletionItemKind::METHOD),
                                    detail: Some("role method".to_string()),
                                    ..Default::default()
                                });
                            }
                        }
                    }
                }
            }

            let generic = generic_type_for_completion(&obj_ty);
            if let Some(methods) = analysis.custom_methods.get(&obj_ty) {
                for (name, ty) in methods {
                    if seen.insert(name.clone()) {
                        items.push(CompletionItem {
                            label: name.clone(),
                            kind: Some(CompletionItemKind::METHOD),
                            detail: Some(ty.to_string()),
                            ..Default::default()
                        });
                    }
                }
            }
            if let Some(methods) = analysis.custom_methods.get(&generic) {
                for (name, ty) in methods {
                    if seen.insert(name.clone()) {
                        items.push(CompletionItem {
                            label: name.clone(),
                            kind: Some(CompletionItemKind::METHOD),
                            detail: Some(ty.to_string()),
                            ..Default::default()
                        });
                    }
                }
            }

            for method in builtin_methods_for_type(&obj_ty) {
                if seen.insert(method.to_string()) {
                    let method_ty = builtin_method_type(&obj_ty, method)
                        .map(|ty| ty.to_string())
                        .unwrap_or_else(|| "builtin".to_string());
                    items.push(CompletionItem {
                        label: method.to_string(),
                        kind: Some(CompletionItemKind::METHOD),
                        detail: Some(method_ty),
                        ..Default::default()
                    });
                }
            }

            return Ok(Some(CompletionResponse::Array(items)));
        }

        let scope_id = find_scope_for_offset(&analysis.scopes, offset);
        let mut current = Some(scope_id);
        let mut seen = HashSet::new();

        while let Some(scope) = current.and_then(|id| analysis.scopes.get(id)) {
            for symbol in scope.symbols.iter().rev() {
                if seen.insert(symbol.name.clone()) {
                    let (kind, detail_ty) =
                        if let Some(import_ty) = doc.imported_symbols.get(&symbol.name) {
                            let kind = if matches!(import_ty, Type::Function { .. }) {
                                CompletionItemKind::FUNCTION
                            } else {
                                CompletionItemKind::VARIABLE
                            };
                            (kind, import_ty.clone())
                        } else {
                            let kind = match symbol.kind {
                                ngn::analyzer::SymbolKind::Function => CompletionItemKind::FUNCTION,
                                ngn::analyzer::SymbolKind::Variable => CompletionItemKind::VARIABLE,
                                ngn::analyzer::SymbolKind::Model => CompletionItemKind::STRUCT,
                                ngn::analyzer::SymbolKind::Enum => CompletionItemKind::ENUM,
                                ngn::analyzer::SymbolKind::EnumVariant => {
                                    CompletionItemKind::ENUM_MEMBER
                                }
                                ngn::analyzer::SymbolKind::Role => CompletionItemKind::INTERFACE,
                                ngn::analyzer::SymbolKind::TypeAlias => {
                                    CompletionItemKind::TYPE_PARAMETER
                                }
                            };
                            (kind, symbol.ty.clone())
                        };
                    items.push(CompletionItem {
                        label: symbol.name.clone(),
                        kind: Some(kind),
                        detail: Some(detail_ty.to_string()),
                        ..Default::default()
                    });
                }
            }
            current = scope
                .parent
                .and_then(|parent| analysis.scopes.get(parent))
                .map(|s| s.id);
        }

        const KEYWORDS: &[&str] = &[
            "var", "const", "global", "fn", "return", "if", "while", "loop", "for", "in", "match",
            "next", "break", "once", "import", "from", "as", "enum", "export", "default", "model",
            "role", "extend", "with", "this", "check", "null",
        ];

        for kw in KEYWORDS {
            if seen.insert(kw.to_string()) {
                items.push(CompletionItem {
                    label: kw.to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    ..Default::default()
                });
            }
        }

        for name in GLOBAL_NAMES {
            if seen.insert((*name).to_string()) {
                items.push(CompletionItem {
                    label: (*name).to_string(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some("builtin".to_string()),
                    ..Default::default()
                });
            }
        }

        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;

        let documents = self.documents.read().await;
        let doc = match documents.get(&uri) {
            Some(doc) => doc.clone(),
            None => return Ok(None),
        };
        drop(documents);

        let analysis = match &doc.analysis {
            Some(a) => a,
            None => return Ok(None),
        };

        let offset = position_to_offset(&doc.text, position);
        let call_site = match find_call_site(analysis, offset) {
            Some(site) => site,
            None => return Ok(None),
        };

        let signature = match &call_site.signature {
            Some(sig) => sig,
            None => return Ok(None),
        };

        let required = signature
            .params
            .len()
            .saturating_sub(signature.optional_count);

        let mut labels = Vec::with_capacity(signature.params.len());
        for (i, param_ty) in signature.params.iter().enumerate() {
            let suffix = if i >= required { "?" } else { "" };
            labels.push(format!("param{}{}: {}", i + 1, suffix, param_ty));
        }

        let label = format!(
            "{}({}) -> {}",
            call_site.callee,
            labels.join(", "),
            signature.return_type
        );

        let parameters: Vec<ParameterInformation> = labels
            .iter()
            .map(|p| ParameterInformation {
                label: ParameterLabel::Simple(p.clone()),
                documentation: None,
            })
            .collect();

        let mut active_param = 0u32;
        for (i, span) in call_site.arg_spans.iter().enumerate() {
            if offset > span.end {
                active_param = (i + 1) as u32;
            } else {
                break;
            }
        }
        if active_param as usize >= signature.params.len() {
            active_param = signature.params.len().saturating_sub(1) as u32;
        }

        Ok(Some(SignatureHelp {
            signatures: vec![SignatureInformation {
                label,
                documentation: None,
                parameters: Some(parameters),
                active_parameter: Some(active_param),
            }],
            active_signature: Some(0),
            active_parameter: Some(active_param),
        }))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        documents: Arc::new(RwLock::new(HashMap::new())),
        module_cache: Arc::new(RwLock::new(HashMap::new())),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
