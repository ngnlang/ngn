use crate::{ast::Value, error::RuntimeError};

use super::ToolboxModule;
use std::collections::HashMap;

pub fn abs(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("abs expects 1 argument".into()));
    }
    match &args[0] {
        Value::I64(n) => Ok(Value::I64(n.abs())),
        Value::I32(n) => Ok(Value::I32(n.abs())),
        Value::U64(n) => Ok(Value::U64(*n)),
        Value::U32(n) => Ok(Value::U32(*n)),
        Value::F64(n) => Ok(Value::F64(n.abs())),
        Value::F32(n) => Ok(Value::F32(n.abs())),
        _ => Err(RuntimeError::TypeError("abs expects numeric type".into())),
    }
}

pub fn round(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("round expects 1 argument".into()));
    }
    match &args[0] {
        Value::F64(n) => Ok(Value::F64(n.round())),
        Value::F32(n) => Ok(Value::F32(n.round())),
        Value::I64(n) => Ok(Value::I64(*n)),
        Value::I32(n) => Ok(Value::I32(*n)),
        Value::U64(n) => Ok(Value::U64(*n)),
        Value::U32(n) => Ok(Value::U32(*n)),
        _ => Err(RuntimeError::TypeError("round expects numeric type".into())),
    }
}

pub fn floor(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("floor expects 1 argument".into()));
    }
    match &args[0] {
        Value::F64(n) => Ok(Value::F64(n.floor())),
        Value::F32(n) => Ok(Value::F32(n.floor())),
        Value::I64(n) => Ok(Value::I64(*n)),
        Value::I32(n) => Ok(Value::I32(*n)),
        Value::U64(n) => Ok(Value::U64(*n)),
        Value::U32(n) => Ok(Value::U32(*n)),
        _ => Err(RuntimeError::TypeError("floor expects numeric type".into())),
    }
}

pub fn ceil(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("ceil expects 1 argument".into()));
    }
    match &args[0] {
        Value::F64(n) => Ok(Value::F64(n.ceil())),
        Value::F32(n) => Ok(Value::F32(n.ceil())),
        Value::I64(n) => Ok(Value::I64(*n)),
        Value::I32(n) => Ok(Value::I32(*n)),
        Value::U64(n) => Ok(Value::U64(*n)),
        Value::U32(n) => Ok(Value::U32(*n)),
        _ => Err(RuntimeError::TypeError("ceil expects numeric type".into())),
    }
}

pub fn sin(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("sin expects 1 argument".into()));
    }
    match &args[0] {
        Value::F64(n) => Ok(Value::F64(n.sin())),
        Value::F32(n) => Ok(Value::F32(n.sin())),
        _ => Err(RuntimeError::TypeError("sin expects float type".into())),
    }
}

pub fn create_module() -> ToolboxModule {
    let mut functions = HashMap::new();
    functions.insert("abs".to_string(), abs as fn(Vec<Value>) -> Result<Value, RuntimeError>);
    functions.insert("round".to_string(), round as fn(Vec<Value>) -> Result<Value, RuntimeError>);
    functions.insert("floor".to_string(), floor as fn(Vec<Value>) -> Result<Value, RuntimeError>);
    functions.insert("ceil".to_string(), ceil as fn(Vec<Value>) -> Result<Value, RuntimeError>);
    functions.insert("sin".to_string(), sin as fn(Vec<Value>) -> Result<Value, RuntimeError>);
    
    ToolboxModule { functions }
}
