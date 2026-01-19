use std::collections::HashMap;
use std::sync::Arc;

use crate::error::RuntimeError;
use crate::toolbox::ToolboxModule;
use crate::value::Value;

pub fn create_module() -> ToolboxModule {
    let mut functions: HashMap<String, crate::toolbox::BuiltinFn> = HashMap::new();

    functions.insert("hexEncode".to_string(), hex_encode);
    functions.insert("hexDecode".to_string(), hex_decode);
    functions.insert("base64Encode".to_string(), base64_encode);
    functions.insert("base64Decode".to_string(), base64_decode);

    ToolboxModule { functions }
}

pub fn hex_encode(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError(
            "hexEncode() expects 1 argument".to_string(),
        ));
    }

    match &args[0] {
        Value::Bytes(b) => Ok(Value::String(hex::encode(b.as_slice()))),
        _ => Err(RuntimeError::TypeError(
            "hexEncode() expects bytes".to_string(),
        )),
    }
}

pub fn hex_decode(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError(
            "hexDecode() expects 1 argument".to_string(),
        ));
    }

    match &args[0] {
        Value::String(s) => match hex::decode(s) {
            Ok(bytes) => Ok(Value::Bytes(Arc::new(bytes))),
            Err(_) => Err(RuntimeError::TypeError(
                "hexDecode() invalid hex string".to_string(),
            )),
        },
        _ => Err(RuntimeError::TypeError(
            "hexDecode() expects string".to_string(),
        )),
    }
}

pub fn base64_encode(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError(
            "base64Encode() expects 1 argument".to_string(),
        ));
    }

    match &args[0] {
        Value::Bytes(b) => {
            use base64::Engine;
            Ok(Value::String(
                base64::engine::general_purpose::STANDARD.encode(b.as_slice()),
            ))
        }
        _ => Err(RuntimeError::TypeError(
            "base64Encode() expects bytes".to_string(),
        )),
    }
}

pub fn base64_decode(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError(
            "base64Decode() expects 1 argument".to_string(),
        ));
    }

    match &args[0] {
        Value::String(s) => {
            use base64::Engine;
            match base64::engine::general_purpose::STANDARD.decode(s.as_bytes()) {
                Ok(bytes) => Ok(Value::Bytes(Arc::new(bytes))),
                Err(_) => Err(RuntimeError::TypeError(
                    "base64Decode() invalid base64 string".to_string(),
                )),
            }
        }
        _ => Err(RuntimeError::TypeError(
            "base64Decode() expects string".to_string(),
        )),
    }
}
