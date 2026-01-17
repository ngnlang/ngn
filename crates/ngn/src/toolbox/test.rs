use crate::{error::RuntimeError, value::Value};

use super::ToolboxModule;
use std::collections::HashMap;

pub fn assert(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.is_empty() {
        return Err(RuntimeError::ArityError(
            "assert expects at least 1 argument".into(),
        ));
    }

    let condition = match &args[0] {
        Value::Bool(b) => *b,
        _ => {
            return Err(RuntimeError::TypeError(
                "assert expects boolean condition".into(),
            ));
        }
    };

    // Extract custom message if provided
    let custom_message = if args.len() > 1 {
        Some(args[1].to_string())
    } else {
        None
    };

    if condition {
        let message = custom_message.unwrap_or_else(|| "Assertion passed".to_string());
        println!("✅ {}", message);
        Ok(Value::Void)
    } else {
        let message = custom_message.unwrap_or_else(|| "Assertion failed".to_string());
        println!("❌ {}", message);
        Err(RuntimeError::AssertionError(message))
    }
}

pub fn create_module() -> ToolboxModule {
    let mut functions = HashMap::new();
    functions.insert(
        "assert".to_string(),
        assert as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );

    ToolboxModule { functions }
}
