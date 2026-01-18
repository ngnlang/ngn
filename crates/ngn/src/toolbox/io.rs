//! I/O toolbox module: file operations
//! Import with: import { file } from "tbx::io"

use crate::{
    error::RuntimeError,
    value::{EnumData, Value},
};

use super::ToolboxModule;
use std::collections::HashMap;
use std::fs;
use std::io::Write;

/// Read entire file contents as a string
/// Returns Result<string, string>
pub fn file_read(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError(
            "file.read expects 1 argument (path)".into(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s.clone(),
        _ => {
            return Err(RuntimeError::TypeError(
                "file.read expects a string path".into(),
            ));
        }
    };

    match fs::read_to_string(&path) {
        Ok(content) => {
            // Return Result::Ok(content)
            Ok(EnumData::into_value(
                "Result".to_string(),
                "Ok".to_string(),
                Some(Box::new(Value::String(content))),
            ))
        }
        Err(e) => {
            // Return Result::Error(message)
            Ok(EnumData::into_value(
                "Result".to_string(),
                "Error".to_string(),
                Some(Box::new(Value::String(format!(
                    "Failed to read '{}': {}",
                    path, e
                )))),
            ))
        }
    }
}

/// Write content to a file (creates or overwrites)
/// Returns Result<void, string>
pub fn file_write(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::ArityError(
            "file.write expects 2 arguments (path, content)".into(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s.clone(),
        _ => {
            return Err(RuntimeError::TypeError(
                "file.write expects a string path".into(),
            ));
        }
    };

    let content = match &args[1] {
        Value::String(s) => s.clone(),
        _ => {
            return Err(RuntimeError::TypeError(
                "file.write expects a string content".into(),
            ));
        }
    };

    match fs::write(&path, &content) {
        Ok(_) => {
            // Return Result::Ok(void)
            Ok(EnumData::into_value(
                "Result".to_string(),
                "Ok".to_string(),
                Some(Box::new(Value::Void)),
            ))
        }
        Err(e) => {
            // Return Result::Error(message)
            Ok(EnumData::into_value(
                "Result".to_string(),
                "Error".to_string(),
                Some(Box::new(Value::String(format!(
                    "Failed to write '{}': {}",
                    path, e
                )))),
            ))
        }
    }
}

/// Append content to a file (creates if doesn't exist)
/// Returns Result<void, string>
pub fn file_append(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::ArityError(
            "file.append expects 2 arguments (path, content)".into(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s.clone(),
        _ => {
            return Err(RuntimeError::TypeError(
                "file.append expects a string path".into(),
            ));
        }
    };

    let content = match &args[1] {
        Value::String(s) => s.clone(),
        _ => {
            return Err(RuntimeError::TypeError(
                "file.append expects a string content".into(),
            ));
        }
    };

    let result = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&path)
        .and_then(|mut file| file.write_all(content.as_bytes()));

    match result {
        Ok(_) => {
            // Return Result::Ok(void)
            Ok(EnumData::into_value(
                "Result".to_string(),
                "Ok".to_string(),
                Some(Box::new(Value::Void)),
            ))
        }
        Err(e) => {
            // Return Result::Error(message)
            Ok(EnumData::into_value(
                "Result".to_string(),
                "Error".to_string(),
                Some(Box::new(Value::String(format!(
                    "Failed to append to '{}': {}",
                    path, e
                )))),
            ))
        }
    }
}

/// Check if a file exists
/// Returns bool
pub fn file_exists(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError(
            "file.exists expects 1 argument (path)".into(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s.clone(),
        _ => {
            return Err(RuntimeError::TypeError(
                "file.exists expects a string path".into(),
            ));
        }
    };

    let exists = std::path::Path::new(&path).exists();
    Ok(Value::Bool(exists))
}

/// Delete a file
/// Returns Result<void, string>
pub fn file_delete(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError(
            "file.delete expects 1 argument (path)".into(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s.clone(),
        _ => {
            return Err(RuntimeError::TypeError(
                "file.delete expects a string path".into(),
            ));
        }
    };

    match fs::remove_file(&path) {
        Ok(_) => Ok(EnumData::into_value(
            "Result".to_string(),
            "Ok".to_string(),
            Some(Box::new(Value::Void)),
        )),
        Err(e) => Ok(EnumData::into_value(
            "Result".to_string(),
            "Error".to_string(),
            Some(Box::new(Value::String(format!(
                "Failed to delete '{}': {}",
                path, e
            )))),
        )),
    }
}

pub fn create_module() -> ToolboxModule {
    let mut functions = HashMap::new();
    functions.insert(
        "read".to_string(),
        file_read as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "write".to_string(),
        file_write as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "append".to_string(),
        file_append as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "exists".to_string(),
        file_exists as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "delete".to_string(),
        file_delete as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );

    ToolboxModule { functions }
}
