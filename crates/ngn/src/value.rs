use tokio::sync::{mpsc, Mutex};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::ast::ClosureDef;
use crate::types::{AssignKind, Moved, Ownership};
use crate::{ast::FnDef, types::Type};

#[derive(Debug, Clone)]
pub enum Value {
    I64(i64),
    I32(i32),
    U64(u64),
    U32(u32),
    F64(f64),
    F32(f32),
    String(String),
    Bool(bool),
    Array(Vec<Value>),
    Function(FnDef),
    Closure(ClosureValue),
    Object(String, HashMap<String, Value>),
    Void,
    Regex(String),
    EnumValue(String, String, Option<Box<Value>>),
    Channel(mpsc::Sender<Value>, Arc<Mutex<mpsc::Receiver<Value>>>, Type),
    StateActor(
        mpsc::Sender<Value>,
        Arc<Mutex<mpsc::Receiver<Value>>>,
        Type,
    ),
    Namespace(String),
    Map(HashMap<MapKey, Value>, Type, Type),
    Set(HashSet<SetValue>, Type),
}

// Implement PartialEq manually to skip Channel comparison
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::I64(a), Value::I64(b)) => a == b,
            (Value::I32(a), Value::I32(b)) => a == b,
            (Value::U64(a), Value::U64(b)) => a == b,
            (Value::U32(a), Value::U32(b)) => a == b,
            (Value::F64(a), Value::F64(b)) => a == b,
            (Value::F32(a), Value::F32(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => a == b,
            (Value::Function(a), Value::Function(b)) => a == b,
            (Value::Closure(a), Value::Closure(b)) => a == b,
            (Value::Object(name_a, fields_a), Value::Object(name_b, fields_b)) => {
                name_a == name_b && fields_a == fields_b
            }
            (Value::Void, Value::Void) => true,
            (Value::Regex(a), Value::Regex(b)) => a == b,
            (Value::EnumValue(enum_a, var_a, data_a), Value::EnumValue(enum_b, var_b, data_b)) => {
                enum_a == enum_b && var_a == var_b && data_a == data_b
            }
            (Value::Channel(_, _, _), Value::Channel(_, _, _)) => false,
            (Value::StateActor(_, _, _), Value::StateActor(_, _, _)) => false,
            (Value::Namespace(a), Value::Namespace(b)) => a == b,
            (Value::Map(_, _, _), Value::Map(_, _, _)) => false,
            (Value::Set(_, _), Value::Set(_, _)) => false,
            _ => false, // Different types are not equal
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClosureValue {
    pub def: Box<ClosureDef>,
    pub captured_env: HashMap<String, (AssignKind, Value, Ownership, Moved, usize)>,
    pub live_vars: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ControlFlow {
    None,
    Break,
    Next,
    Return(Value),
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum MapKey {
    String(String),
    I64(i64),
    I32(i32),
    U64(u64),
    U32(u32),
    Bool(bool),
    Enum(String, String), // (enum_name, variant_name)
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum SetValue {
    I64(i64),
    I32(i32),
    U64(u64),
    U32(u32),
    String(String),
    Bool(bool),
    EnumValue(String, String),
}