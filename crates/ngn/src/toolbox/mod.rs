pub mod core;
pub mod encoding;
pub mod http;
pub mod io;
pub mod math;
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
        ("test", "assert") => Some(NATIVE_ASSERT),
        ("math", "round") => Some(NATIVE_ROUND),
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
        _ => None,
    }
}
