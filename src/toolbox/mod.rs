pub mod math;
pub mod test;

pub const NATIVE_ABS: u16 = 1;
pub const NATIVE_ASSERT: u16 = 2;
pub const NATIVE_ROUND: u16 = 3;

use std::{collections::HashMap, path::Path};

use crate::{value::Value, error::RuntimeError};

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
        ImportSource::Toolbox { module: Some(module.to_string()) }
    } else {
        ImportSource::File { path: source_str.to_string() }
    }
}

pub fn get_native_id(module: &str, name: &str) -> Option<u16> {
    match (module, name) {
        ("math", "abs") => Some(NATIVE_ABS),
        ("test", "assert") => Some(NATIVE_ASSERT),
        ("math", "round") => Some(NATIVE_ROUND),
        _ => None,
    }
}
