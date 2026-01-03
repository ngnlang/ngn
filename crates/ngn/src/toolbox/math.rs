use crate::{error::RuntimeError, value::{Number, Value}};

use super::ToolboxModule;
use std::collections::HashMap;

pub fn abs(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("abs expects 1 argument".into()));
    }
    match &args[0] {
        Value::Numeric(num) => {
            let result_num = match num {
                Number::I64(n) => Number::I64(n.abs()),
                Number::I32(n) => Number::I32(n.abs()),
                Number::I16(n) => Number::I16(n.abs()),
                Number::I8(n) => Number::I8(n.abs()),
                Number::U64(n) => Number::U64(*n),
                Number::U32(n) => Number::U32(*n),
                Number::U16(n) => Number::U16(*n),
                Number::U8(n) => Number::U8(*n),
                Number::F64(n) => Number::F64(n.abs()),
                Number::F32(n) => Number::F32(n.abs()),
            };
            Ok(Value::Numeric(result_num))
        }
        _ => Err(RuntimeError::TypeError("abs expects numeric type".into())),
    }
}

pub fn round(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("round expects 1 argument".into()));
    }
    match &args[0] {
        Value::Numeric(num) => {
            let result_num = match num {
                Number::F64(n) => Number::F64(n.round()),
                Number::F32(n) => Number::F32(n.round()),
                Number::I64(n) => Number::I64(*n),
                Number::I32(n) => Number::I32(*n),
                Number::I16(n) => Number::I16(*n),
                Number::I8(n) => Number::I8(*n),
                Number::U64(n) => Number::U64(*n),
                Number::U32(n) => Number::U32(*n),
                Number::U16(n) => Number::U16(*n),
                Number::U8(n) => Number::U8(*n),
            };
            Ok(Value::Numeric(result_num))
        }
        _ => Err(RuntimeError::TypeError("round expects numeric type".into())),
    }
}

pub fn floor(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("floor expects 1 argument".into()));
    }
    match &args[0] {
        Value::Numeric(num) => {
            let result_num = match num {
                Number::F64(n) => Number::F64(n.floor()),
                Number::F32(n) => Number::F32(n.floor()),
                Number::I64(n) => Number::I64(*n),
                Number::I32(n) => Number::I32(*n),
                Number::I16(n) => Number::I16(*n),
                Number::I8(n) => Number::I8(*n),
                Number::U64(n) => Number::U64(*n),
                Number::U32(n) => Number::U32(*n),
                Number::U16(n) => Number::U16(*n),
                Number::U8(n) => Number::U8(*n),
            };
            Ok(Value::Numeric(result_num))
        }
        _ => Err(RuntimeError::TypeError("floor expects numeric type".into())),
    }
}

pub fn ceil(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("ceil expects 1 argument".into()));
    }
    match &args[0] {
        Value::Numeric(num) => {
            let result_num = match num {
                Number::F64(n) => Number::F64(n.ceil()),
                Number::F32(n) => Number::F32(n.ceil()),
                Number::I64(n) => Number::I64(*n),
                Number::I32(n) => Number::I32(*n),
                Number::I16(n) => Number::I16(*n),
                Number::I8(n) => Number::I8(*n),
                Number::U64(n) => Number::U64(*n),
                Number::U32(n) => Number::U32(*n),
                Number::U16(n) => Number::U16(*n),
                Number::U8(n) => Number::U8(*n),
            };
            Ok(Value::Numeric(result_num))
        }
        _ => Err(RuntimeError::TypeError("ceil expects numeric type".into())),
    }
}

pub fn sin(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("sin expects 1 argument".into()));
    }
    match &args[0] {
        Value::Numeric(num) => {
            let result_num = match num {
                Number::F64(n) => Number::F64(n.sin()),
                Number::F32(n) => Number::F32(n.sin()),
                _ => return Err(RuntimeError::TypeError("numeric type not supported".into()))
            };
            Ok(Value::Numeric(result_num))
        }
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
