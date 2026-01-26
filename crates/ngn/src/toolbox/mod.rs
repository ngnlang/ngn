pub mod core;
#[cfg(feature = "core")]
pub mod encoding;
#[cfg(feature = "http")]
pub mod http;
#[cfg(feature = "os")]
pub mod io;
#[cfg(feature = "llm")]
pub mod llm;
#[cfg(feature = "core")]
pub mod math;
#[cfg(feature = "os")]
pub mod process;
#[cfg(feature = "core")]
pub mod test;

#[cfg(feature = "core")]
pub const NATIVE_ABS: u16 = 1;
#[cfg(feature = "core")]
pub const NATIVE_ASSERT: u16 = 2;
#[cfg(feature = "core")]
pub const NATIVE_ROUND: u16 = 3;
#[cfg(feature = "http")]
pub const NATIVE_SERVE: u16 = 4;
#[cfg(feature = "core")]
pub const NATIVE_HEX_ENCODE: u16 = 5;
#[cfg(feature = "core")]
pub const NATIVE_HEX_DECODE: u16 = 11;
#[cfg(feature = "core")]
pub const NATIVE_BASE64_ENCODE: u16 = 12;
#[cfg(feature = "core")]
pub const NATIVE_BASE64_DECODE: u16 = 13;
#[cfg(feature = "os")]
pub const NATIVE_FILE_READ: u16 = 6;
#[cfg(feature = "os")]
pub const NATIVE_FILE_WRITE: u16 = 7;
#[cfg(feature = "os")]
pub const NATIVE_FILE_APPEND: u16 = 8;
#[cfg(feature = "os")]
pub const NATIVE_FILE_EXISTS: u16 = 9;
#[cfg(feature = "os")]
pub const NATIVE_FILE_DELETE: u16 = 10;
#[cfg(feature = "os")]
pub const NATIVE_PROCESS_RUN: u16 = 14;
#[cfg(feature = "os")]
pub const NATIVE_PROCESS_STREAM: u16 = 15;
#[cfg(feature = "os")]
pub const NATIVE_PROCESS_STREAM_RAW: u16 = 16;
#[cfg(feature = "llm")]
pub const NATIVE_LLM_LOAD: u16 = 17;
#[cfg(feature = "llm")]
pub const NATIVE_LLM_GENERATE: u16 = 18;
#[cfg(feature = "llm")]
pub const NATIVE_LLM_STREAM: u16 = 19;

// Math native functions (keep IDs stable; append new ones at the end)
#[cfg(feature = "core")]
pub const NATIVE_FLOOR: u16 = 20;
#[cfg(feature = "core")]
pub const NATIVE_CEIL: u16 = 21;
#[cfg(feature = "core")]
pub const NATIVE_TRUNC: u16 = 22;
#[cfg(feature = "core")]
pub const NATIVE_SIGN: u16 = 23;
#[cfg(feature = "core")]
pub const NATIVE_SIN: u16 = 24;
#[cfg(feature = "core")]
pub const NATIVE_COS: u16 = 25;
#[cfg(feature = "core")]
pub const NATIVE_TAN: u16 = 26;
#[cfg(feature = "core")]
pub const NATIVE_ASIN: u16 = 27;
#[cfg(feature = "core")]
pub const NATIVE_ACOS: u16 = 28;
#[cfg(feature = "core")]
pub const NATIVE_ATAN: u16 = 29;
#[cfg(feature = "core")]
pub const NATIVE_ATAN2: u16 = 30;
#[cfg(feature = "core")]
pub const NATIVE_SQRT: u16 = 31;
#[cfg(feature = "core")]
pub const NATIVE_POW: u16 = 32;
#[cfg(feature = "core")]
pub const NATIVE_EXP: u16 = 33;
#[cfg(feature = "core")]
pub const NATIVE_LOG: u16 = 34;
#[cfg(feature = "core")]
pub const NATIVE_LOG10: u16 = 35;
#[cfg(feature = "core")]
pub const NATIVE_LOG2: u16 = 36;
#[cfg(feature = "core")]
pub const NATIVE_MIN: u16 = 37;
#[cfg(feature = "core")]
pub const NATIVE_MAX: u16 = 38;
#[cfg(feature = "core")]
pub const NATIVE_CLAMP: u16 = 39;
#[cfg(feature = "core")]
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
        #[allow(unused_mut)]
        let mut modules = HashMap::new();
        #[cfg(feature = "core")]
        modules.insert("math".to_string(), math::create_module());
        #[cfg(feature = "core")]
        modules.insert("test".to_string(), test::create_module());
        #[cfg(feature = "http")]
        modules.insert("http".to_string(), http::create_module());
        #[cfg(feature = "os")]
        modules.insert("io".to_string(), io::create_module());
        #[cfg(feature = "os")]
        modules.insert("process".to_string(), process::create_module());
        #[cfg(feature = "llm")]
        modules.insert("llm".to_string(), llm::create_module());
        #[cfg(feature = "core")]
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
        #[cfg(feature = "core")]
        ("math", "abs") => Some(NATIVE_ABS),
        #[cfg(feature = "core")]
        ("math", "floor") => Some(NATIVE_FLOOR),
        #[cfg(feature = "core")]
        ("math", "ceil") => Some(NATIVE_CEIL),
        #[cfg(feature = "core")]
        ("test", "assert") => Some(NATIVE_ASSERT),
        #[cfg(feature = "core")]
        ("math", "round") => Some(NATIVE_ROUND),
        #[cfg(feature = "core")]
        ("math", "trunc") => Some(NATIVE_TRUNC),
        #[cfg(feature = "core")]
        ("math", "sign") => Some(NATIVE_SIGN),
        #[cfg(feature = "core")]
        ("math", "sin") => Some(NATIVE_SIN),
        #[cfg(feature = "core")]
        ("math", "cos") => Some(NATIVE_COS),
        #[cfg(feature = "core")]
        ("math", "tan") => Some(NATIVE_TAN),
        #[cfg(feature = "core")]
        ("math", "asin") => Some(NATIVE_ASIN),
        #[cfg(feature = "core")]
        ("math", "acos") => Some(NATIVE_ACOS),
        #[cfg(feature = "core")]
        ("math", "atan") => Some(NATIVE_ATAN),
        #[cfg(feature = "core")]
        ("math", "atan2") => Some(NATIVE_ATAN2),
        #[cfg(feature = "core")]
        ("math", "sqrt") => Some(NATIVE_SQRT),
        #[cfg(feature = "core")]
        ("math", "pow") => Some(NATIVE_POW),
        #[cfg(feature = "core")]
        ("math", "exp") => Some(NATIVE_EXP),
        #[cfg(feature = "core")]
        ("math", "log") => Some(NATIVE_LOG),
        #[cfg(feature = "core")]
        ("math", "log10") => Some(NATIVE_LOG10),
        #[cfg(feature = "core")]
        ("math", "log2") => Some(NATIVE_LOG2),
        #[cfg(feature = "core")]
        ("math", "min") => Some(NATIVE_MIN),
        #[cfg(feature = "core")]
        ("math", "max") => Some(NATIVE_MAX),
        #[cfg(feature = "core")]
        ("math", "clamp") => Some(NATIVE_CLAMP),
        #[cfg(feature = "core")]
        ("math", "PI") => Some(NATIVE_PI),
        #[cfg(feature = "http")]
        ("http", "serve") => Some(NATIVE_SERVE),
        #[cfg(feature = "core")]
        ("encoding", "hexEncode") => Some(NATIVE_HEX_ENCODE),
        #[cfg(feature = "core")]
        ("encoding", "hexDecode") => Some(NATIVE_HEX_DECODE),
        #[cfg(feature = "core")]
        ("encoding", "base64Encode") => Some(NATIVE_BASE64_ENCODE),
        #[cfg(feature = "core")]
        ("encoding", "base64Decode") => Some(NATIVE_BASE64_DECODE),
        #[cfg(feature = "os")]
        ("io", "read") => Some(NATIVE_FILE_READ),
        #[cfg(feature = "os")]
        ("io", "write") => Some(NATIVE_FILE_WRITE),
        #[cfg(feature = "os")]
        ("io", "append") => Some(NATIVE_FILE_APPEND),
        #[cfg(feature = "os")]
        ("io", "exists") => Some(NATIVE_FILE_EXISTS),
        #[cfg(feature = "os")]
        ("io", "delete") => Some(NATIVE_FILE_DELETE),
        #[cfg(feature = "os")]
        ("process", "run") => Some(NATIVE_PROCESS_RUN),
        #[cfg(feature = "os")]
        ("process", "stream") => Some(NATIVE_PROCESS_STREAM),
        #[cfg(feature = "os")]
        ("process", "streamRaw") => Some(NATIVE_PROCESS_STREAM_RAW),
        #[cfg(feature = "llm")]
        ("llm", "load") => Some(NATIVE_LLM_LOAD),
        #[cfg(feature = "llm")]
        ("llm", "generate") => Some(NATIVE_LLM_GENERATE),
        #[cfg(feature = "llm")]
        ("llm", "stream") => Some(NATIVE_LLM_STREAM),
        _ => None,
    }
}
