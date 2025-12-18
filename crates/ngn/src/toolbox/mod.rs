pub mod math;
pub mod signatures;

use std::collections::HashMap;

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

pub fn parse_import_source(source: &str) -> ImportSource {
    if source == "tbx" {
        ImportSource::Toolbox { module: None }
    } else if let Some(module) = source.strip_prefix("tbx::") {
        ImportSource::Toolbox { module: Some(module.to_string()) }
    } else {
        ImportSource::File { path: source.to_string() }
    }
}
