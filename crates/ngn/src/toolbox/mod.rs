pub mod core;
pub mod encoding;
pub mod http;
pub mod io;
pub mod llm;
pub mod math;
pub mod process;
pub mod test;

pub const NATIVE_ABS: u16 = 1;
pub const NATIVE_ASSERT: u16 = 2;
pub const NATIVE_ROUND: u16 = 3;
pub const NATIVE_SERVE: u16 = 4;
pub const NATIVE_HEX_ENCODE: u16 = 5;
pub const NATIVE_HEX_DECODE: u16 = 11;
pub const NATIVE_BASE64_ENCODE: u16 = 12;
pub const NATIVE_BASE64_DECODE: u16 = 13;
pub const NATIVE_FILE_READ: u16 = 6;
pub const NATIVE_FILE_WRITE: u16 = 7;
pub const NATIVE_FILE_APPEND: u16 = 8;
pub const NATIVE_FILE_EXISTS: u16 = 9;
pub const NATIVE_FILE_DELETE: u16 = 10;
pub const NATIVE_PROCESS_RUN: u16 = 14;
pub const NATIVE_PROCESS_STREAM: u16 = 15;
pub const NATIVE_PROCESS_STREAM_RAW: u16 = 16;
pub const NATIVE_LLM_LOAD: u16 = 17;
pub const NATIVE_LLM_GENERATE: u16 = 18;
pub const NATIVE_LLM_STREAM: u16 = 19;

// Math native functions (keep IDs stable; append new ones at the end)
pub const NATIVE_FLOOR: u16 = 20;
pub const NATIVE_CEIL: u16 = 21;
pub const NATIVE_TRUNC: u16 = 22;
pub const NATIVE_SIGN: u16 = 23;
pub const NATIVE_SIN: u16 = 24;
pub const NATIVE_COS: u16 = 25;
pub const NATIVE_TAN: u16 = 26;
pub const NATIVE_ASIN: u16 = 27;
pub const NATIVE_ACOS: u16 = 28;
pub const NATIVE_ATAN: u16 = 29;
pub const NATIVE_ATAN2: u16 = 30;
pub const NATIVE_SQRT: u16 = 31;
pub const NATIVE_POW: u16 = 32;
pub const NATIVE_EXP: u16 = 33;
pub const NATIVE_LOG: u16 = 34;
pub const NATIVE_LOG10: u16 = 35;
pub const NATIVE_LOG2: u16 = 36;
pub const NATIVE_MIN: u16 = 37;
pub const NATIVE_MAX: u16 = 38;
pub const NATIVE_CLAMP: u16 = 39;
pub const NATIVE_PI: u16 = 40;

use std::{collections::HashMap, path::Path};

use crate::{error::RuntimeError, value::Value};

pub type BuiltinFn = fn(Vec<Value>) -> Result<Value, RuntimeError>;

pub struct ToolboxModule {
    pub functions: HashMap<String, BuiltinFn>,
}

pub struct Toolbox {
    modules: HashMap<String, ToolboxModule>,
}

impl Toolbox {
    pub fn new() -> Self {
        let mut modules = HashMap::new();
        modules.insert("math".to_string(), math::create_module());
        modules.insert("test".to_string(), test::create_module());
        modules.insert("http".to_string(), http::create_module());
        modules.insert("io".to_string(), io::create_module());
        modules.insert("process".to_string(), process::create_module());
        modules.insert("llm".to_string(), llm::create_module());
        modules.insert("encoding".to_string(), encoding::create_module());

        Self { modules }
    }

    /// Get a specific module: "math" from "tbx::math"
    pub fn get_module(&self, name: &str) -> Option<&ToolboxModule> {
        self.modules.get(name)
    }

    /// Get all module names (for `import { math, io } from "tbx"`)
    pub fn module_names(&self) -> Vec<String> {
        self.modules.keys().cloned().collect()
    }
}

pub enum ImportSource {
    Toolbox { module: Option<String> },
    File { path: String },
}

pub fn parse_import_source(path: &Path) -> ImportSource {
    let source_str = path.to_string_lossy();

    if source_str == "tbx" {
        ImportSource::Toolbox { module: None }
    } else if let Some(module) = source_str.strip_prefix("tbx::") {
        ImportSource::Toolbox {
            module: Some(module.to_string()),
        }
    } else {
        ImportSource::File {
            path: source_str.to_string(),
        }
    }
}

pub fn get_native_id(module: &str, name: &str) -> Option<u16> {
    match (module, name) {
        ("math", "abs") => Some(NATIVE_ABS),
        ("math", "floor") => Some(NATIVE_FLOOR),
        ("math", "ceil") => Some(NATIVE_CEIL),
        ("test", "assert") => Some(NATIVE_ASSERT),
        ("math", "round") => Some(NATIVE_ROUND),
        ("math", "trunc") => Some(NATIVE_TRUNC),
        ("math", "sign") => Some(NATIVE_SIGN),
        ("math", "sin") => Some(NATIVE_SIN),
        ("math", "cos") => Some(NATIVE_COS),
        ("math", "tan") => Some(NATIVE_TAN),
        ("math", "asin") => Some(NATIVE_ASIN),
        ("math", "acos") => Some(NATIVE_ACOS),
        ("math", "atan") => Some(NATIVE_ATAN),
        ("math", "atan2") => Some(NATIVE_ATAN2),
        ("math", "sqrt") => Some(NATIVE_SQRT),
        ("math", "pow") => Some(NATIVE_POW),
        ("math", "exp") => Some(NATIVE_EXP),
        ("math", "log") => Some(NATIVE_LOG),
        ("math", "log10") => Some(NATIVE_LOG10),
        ("math", "log2") => Some(NATIVE_LOG2),
        ("math", "min") => Some(NATIVE_MIN),
        ("math", "max") => Some(NATIVE_MAX),
        ("math", "clamp") => Some(NATIVE_CLAMP),
        ("math", "PI") => Some(NATIVE_PI),
        ("http", "serve") => Some(NATIVE_SERVE),
        ("encoding", "hexEncode") => Some(NATIVE_HEX_ENCODE),
        ("encoding", "hexDecode") => Some(NATIVE_HEX_DECODE),
        ("encoding", "base64Encode") => Some(NATIVE_BASE64_ENCODE),
        ("encoding", "base64Decode") => Some(NATIVE_BASE64_DECODE),
        ("io", "read") => Some(NATIVE_FILE_READ),
        ("io", "write") => Some(NATIVE_FILE_WRITE),
        ("io", "append") => Some(NATIVE_FILE_APPEND),
        ("io", "exists") => Some(NATIVE_FILE_EXISTS),
        ("io", "delete") => Some(NATIVE_FILE_DELETE),
        ("process", "run") => Some(NATIVE_PROCESS_RUN),
        ("process", "stream") => Some(NATIVE_PROCESS_STREAM),
        ("process", "streamRaw") => Some(NATIVE_PROCESS_STREAM_RAW),
        ("llm", "load") => Some(NATIVE_LLM_LOAD),
        ("llm", "generate") => Some(NATIVE_LLM_GENERATE),
        ("llm", "stream") => Some(NATIVE_LLM_STREAM),
        _ => None,
    }
}
