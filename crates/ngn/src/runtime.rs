use std::collections::HashMap;

use crate::{ast::{AssignKind, EnumDef, FnDef, ModelDef, Moved, Ownership, RoleDef, Stmt, Value}, error::RuntimeError};

#[derive(Debug, Clone, PartialEq)]
pub enum ImportKind {
    /// import { a, b } or import { a as x, b as y } from 'module'
    Named(Vec<(String, String)>),
    /// import * as name from 'module'
    Namespace(String),
    /// import name from 'module'
    Default(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportStmt {
    pub kind: ImportKind,
    pub source: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExportKind {
    /// export fn foo() {}
    Named(Box<Stmt>),
    /// export default foo
    Default(String),
}

#[derive(Debug, Clone, Default)]
pub struct ModuleExports {
    pub functions: HashMap<String, FnDef>,
    pub models: HashMap<String, ModelDef>,
    pub enums: HashMap<String, EnumDef>,
    pub default: Option<String>,
}

#[derive(Debug, Clone, Default)]
pub struct RuntimeContext {
    pub env: HashMap<String, (AssignKind, Value, Ownership, Moved)>,
    pub fns: HashMap<String, Callable>,
    pub models: HashMap<String, ModelDef>,
    pub roles: HashMap<String, RoleDef>,
    pub model_methods: HashMap<(String, String), FnDef>,
    pub model_roles: HashMap<(String, String), bool>,
    pub enums: HashMap<String, EnumDef>,
    pub exports: ModuleExports,
    pub module_cache: HashMap<String, ModuleExports>,
}

impl RuntimeContext {
    pub fn with_cache(module_cache: HashMap<String, ModuleExports>) -> Self {
        Self {
            module_cache,
            ..Default::default()
        }
    }

    /// Partial clone for independent environments
    pub fn fork_with_env(
        &self,
        env: HashMap<String, (AssignKind, Value, Ownership, Moved)>
    ) -> Self {
        Self {
            env,
            fns: self.fns.clone(),
            models: self.models.clone(),
            roles: self.roles.clone(),
            model_methods: self.model_methods.clone(),
            model_roles: self.model_roles.clone(),
            enums: self.enums.clone(),
            exports: ModuleExports::default(),
            module_cache: self.module_cache.clone(),
        }
    }

    /// Full clone for spawning independent contexts (threads, etc.)
    pub fn fork(&self) -> Self {
        Self {
            env: self.env.clone(),
            fns: self.fns.clone(),
            models: self.models.clone(),
            roles: self.roles.clone(),
            model_methods: self.model_methods.clone(),
            model_roles: self.model_roles.clone(),
            enums: self.enums.clone(),
            exports: ModuleExports::default(),
            module_cache: self.module_cache.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Callable {
    UserDefined(FnDef),
    Builtin(fn(Vec<Value>) -> Result<Value, RuntimeError>),
}
