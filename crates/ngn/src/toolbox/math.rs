use crate::{
    error::RuntimeError,
    value::{Number, Value},
};

use super::ToolboxModule;
use std::collections::HashMap;

/// Helper function to convert Number to f64
fn number_to_f64(num: &Number) -> f64 {
    match num {
        Number::I64(n) => *n as f64,
        Number::I32(n) => *n as f64,
        Number::I16(n) => *n as f64,
        Number::I8(n) => *n as f64,
        Number::U64(n) => *n as f64,
        Number::U32(n) => *n as f64,
        Number::U16(n) => *n as f64,
        Number::U8(n) => *n as f64,
        Number::F64(n) => *n,
        Number::F32(n) => *n as f64,
    }
}

// ============================================================================
// Rounding & Absolute Value
// ============================================================================

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

pub fn trunc(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("trunc expects 1 argument".into()));
    }
    match &args[0] {
        Value::Numeric(num) => {
            let result_num = match num {
                Number::F64(n) => Number::F64(n.trunc()),
                Number::F32(n) => Number::F32(n.trunc()),
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
        _ => Err(RuntimeError::TypeError("trunc expects numeric type".into())),
    }
}

// ============================================================================
// Trigonometric Functions
// ============================================================================

pub fn sin(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("sin expects 1 argument".into()));
    }
    match &args[0] {
        Value::Numeric(num) => {
            let result_num = match num {
                Number::F64(n) => Number::F64(n.sin()),
                Number::F32(n) => Number::F32(n.sin()),
                Number::I64(n) => Number::F64((*n as f64).sin()),
                Number::I32(n) => Number::F64((*n as f64).sin()),
                Number::I16(n) => Number::F64((*n as f64).sin()),
                Number::I8(n) => Number::F64((*n as f64).sin()),
                Number::U64(n) => Number::F64((*n as f64).sin()),
                Number::U32(n) => Number::F64((*n as f64).sin()),
                Number::U16(n) => Number::F64((*n as f64).sin()),
                Number::U8(n) => Number::F64((*n as f64).sin()),
            };
            Ok(Value::Numeric(result_num))
        }
        _ => Err(RuntimeError::TypeError("sin expects numeric type".into())),
    }
}

pub fn cos(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("cos expects 1 argument".into()));
    }
    match &args[0] {
        Value::Numeric(num) => {
            let result_num = match num {
                Number::F64(n) => Number::F64(n.cos()),
                Number::F32(n) => Number::F32(n.cos()),
                Number::I64(n) => Number::F64((*n as f64).cos()),
                Number::I32(n) => Number::F64((*n as f64).cos()),
                Number::I16(n) => Number::F64((*n as f64).cos()),
                Number::I8(n) => Number::F64((*n as f64).cos()),
                Number::U64(n) => Number::F64((*n as f64).cos()),
                Number::U32(n) => Number::F64((*n as f64).cos()),
                Number::U16(n) => Number::F64((*n as f64).cos()),
                Number::U8(n) => Number::F64((*n as f64).cos()),
            };
            Ok(Value::Numeric(result_num))
        }
        _ => Err(RuntimeError::TypeError("cos expects numeric type".into())),
    }
}

pub fn tan(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("tan expects 1 argument".into()));
    }
    match &args[0] {
        Value::Numeric(num) => {
            let result_num = match num {
                Number::F64(n) => Number::F64(n.tan()),
                Number::F32(n) => Number::F32(n.tan()),
                Number::I64(n) => Number::F64((*n as f64).tan()),
                Number::I32(n) => Number::F64((*n as f64).tan()),
                Number::I16(n) => Number::F64((*n as f64).tan()),
                Number::I8(n) => Number::F64((*n as f64).tan()),
                Number::U64(n) => Number::F64((*n as f64).tan()),
                Number::U32(n) => Number::F64((*n as f64).tan()),
                Number::U16(n) => Number::F64((*n as f64).tan()),
                Number::U8(n) => Number::F64((*n as f64).tan()),
            };
            Ok(Value::Numeric(result_num))
        }
        _ => Err(RuntimeError::TypeError("tan expects numeric type".into())),
    }
}

pub fn asin(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("asin expects 1 argument".into()));
    }
    match &args[0] {
        Value::Numeric(num) => {
            let result_num = match num {
                Number::F64(n) => Number::F64(n.asin()),
                Number::F32(n) => Number::F32(n.asin()),
                Number::I64(n) => Number::F64((*n as f64).asin()),
                Number::I32(n) => Number::F64((*n as f64).asin()),
                Number::I16(n) => Number::F64((*n as f64).asin()),
                Number::I8(n) => Number::F64((*n as f64).asin()),
                Number::U64(n) => Number::F64((*n as f64).asin()),
                Number::U32(n) => Number::F64((*n as f64).asin()),
                Number::U16(n) => Number::F64((*n as f64).asin()),
                Number::U8(n) => Number::F64((*n as f64).asin()),
            };
            Ok(Value::Numeric(result_num))
        }
        _ => Err(RuntimeError::TypeError("asin expects numeric type".into())),
    }
}

pub fn acos(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("acos expects 1 argument".into()));
    }
    match &args[0] {
        Value::Numeric(num) => {
            let result_num = match num {
                Number::F64(n) => Number::F64(n.acos()),
                Number::F32(n) => Number::F32(n.acos()),
                Number::I64(n) => Number::F64((*n as f64).acos()),
                Number::I32(n) => Number::F64((*n as f64).acos()),
                Number::I16(n) => Number::F64((*n as f64).acos()),
                Number::I8(n) => Number::F64((*n as f64).acos()),
                Number::U64(n) => Number::F64((*n as f64).acos()),
                Number::U32(n) => Number::F64((*n as f64).acos()),
                Number::U16(n) => Number::F64((*n as f64).acos()),
                Number::U8(n) => Number::F64((*n as f64).acos()),
            };
            Ok(Value::Numeric(result_num))
        }
        _ => Err(RuntimeError::TypeError("acos expects numeric type".into())),
    }
}

pub fn atan(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("atan expects 1 argument".into()));
    }
    match &args[0] {
        Value::Numeric(num) => {
            let result_num = match num {
                Number::F64(n) => Number::F64(n.atan()),
                Number::F32(n) => Number::F32(n.atan()),
                Number::I64(n) => Number::F64((*n as f64).atan()),
                Number::I32(n) => Number::F64((*n as f64).atan()),
                Number::I16(n) => Number::F64((*n as f64).atan()),
                Number::I8(n) => Number::F64((*n as f64).atan()),
                Number::U64(n) => Number::F64((*n as f64).atan()),
                Number::U32(n) => Number::F64((*n as f64).atan()),
                Number::U16(n) => Number::F64((*n as f64).atan()),
                Number::U8(n) => Number::F64((*n as f64).atan()),
            };
            Ok(Value::Numeric(result_num))
        }
        _ => Err(RuntimeError::TypeError("atan expects numeric type".into())),
    }
}

pub fn atan2(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::ArityError("atan2 expects 2 arguments".into()));
    }

    let y = match &args[0] {
        Value::Numeric(num) => number_to_f64(num),
        _ => {
            return Err(RuntimeError::TypeError(
                "atan2 expects numeric types".into(),
            ));
        }
    };

    let x = match &args[1] {
        Value::Numeric(num) => number_to_f64(num),
        _ => {
            return Err(RuntimeError::TypeError(
                "atan2 expects numeric types".into(),
            ));
        }
    };

    Ok(Value::Numeric(Number::F64(y.atan2(x))))
}

// ============================================================================
// Exponential & Logarithmic Functions
// ============================================================================

pub fn sqrt(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("sqrt expects 1 argument".into()));
    }
    match &args[0] {
        Value::Numeric(num) => {
            let result_num = match num {
                Number::F64(n) => Number::F64(n.sqrt()),
                Number::F32(n) => Number::F32(n.sqrt()),
                Number::I64(n) => Number::F64((*n as f64).sqrt()),
                Number::I32(n) => Number::F64((*n as f64).sqrt()),
                Number::I16(n) => Number::F64((*n as f64).sqrt()),
                Number::I8(n) => Number::F64((*n as f64).sqrt()),
                Number::U64(n) => Number::F64((*n as f64).sqrt()),
                Number::U32(n) => Number::F64((*n as f64).sqrt()),
                Number::U16(n) => Number::F64((*n as f64).sqrt()),
                Number::U8(n) => Number::F64((*n as f64).sqrt()),
            };
            Ok(Value::Numeric(result_num))
        }
        _ => Err(RuntimeError::TypeError("sqrt expects numeric type".into())),
    }
}

pub fn pow(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::ArityError("pow expects 2 arguments".into()));
    }

    let base = match &args[0] {
        Value::Numeric(num) => number_to_f64(num),
        _ => return Err(RuntimeError::TypeError("pow expects numeric types".into())),
    };

    let exponent = match &args[1] {
        Value::Numeric(num) => number_to_f64(num),
        _ => return Err(RuntimeError::TypeError("pow expects numeric types".into())),
    };

    Ok(Value::Numeric(Number::F64(base.powf(exponent))))
}

pub fn exp(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("exp expects 1 argument".into()));
    }
    match &args[0] {
        Value::Numeric(num) => {
            let result_num = match num {
                Number::F64(n) => Number::F64(n.exp()),
                Number::F32(n) => Number::F32(n.exp()),
                Number::I64(n) => Number::F64((*n as f64).exp()),
                Number::I32(n) => Number::F64((*n as f64).exp()),
                Number::I16(n) => Number::F64((*n as f64).exp()),
                Number::I8(n) => Number::F64((*n as f64).exp()),
                Number::U64(n) => Number::F64((*n as f64).exp()),
                Number::U32(n) => Number::F64((*n as f64).exp()),
                Number::U16(n) => Number::F64((*n as f64).exp()),
                Number::U8(n) => Number::F64((*n as f64).exp()),
            };
            Ok(Value::Numeric(result_num))
        }
        _ => Err(RuntimeError::TypeError("exp expects numeric type".into())),
    }
}

pub fn log(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("log expects 1 argument".into()));
    }
    match &args[0] {
        Value::Numeric(num) => {
            let result_num = match num {
                Number::F64(n) => Number::F64(n.ln()),
                Number::F32(n) => Number::F32(n.ln()),
                Number::I64(n) => Number::F64((*n as f64).ln()),
                Number::I32(n) => Number::F64((*n as f64).ln()),
                Number::I16(n) => Number::F64((*n as f64).ln()),
                Number::I8(n) => Number::F64((*n as f64).ln()),
                Number::U64(n) => Number::F64((*n as f64).ln()),
                Number::U32(n) => Number::F64((*n as f64).ln()),
                Number::U16(n) => Number::F64((*n as f64).ln()),
                Number::U8(n) => Number::F64((*n as f64).ln()),
            };
            Ok(Value::Numeric(result_num))
        }
        _ => Err(RuntimeError::TypeError("log expects numeric type".into())),
    }
}

pub fn log10(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("log10 expects 1 argument".into()));
    }
    match &args[0] {
        Value::Numeric(num) => {
            let result_num = match num {
                Number::F64(n) => Number::F64(n.log10()),
                Number::F32(n) => Number::F32(n.log10()),
                Number::I64(n) => Number::F64((*n as f64).log10()),
                Number::I32(n) => Number::F64((*n as f64).log10()),
                Number::I16(n) => Number::F64((*n as f64).log10()),
                Number::I8(n) => Number::F64((*n as f64).log10()),
                Number::U64(n) => Number::F64((*n as f64).log10()),
                Number::U32(n) => Number::F64((*n as f64).log10()),
                Number::U16(n) => Number::F64((*n as f64).log10()),
                Number::U8(n) => Number::F64((*n as f64).log10()),
            };
            Ok(Value::Numeric(result_num))
        }
        _ => Err(RuntimeError::TypeError("log10 expects numeric type".into())),
    }
}

pub fn log2(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("log2 expects 1 argument".into()));
    }
    match &args[0] {
        Value::Numeric(num) => {
            let result_num = match num {
                Number::F64(n) => Number::F64(n.log2()),
                Number::F32(n) => Number::F32(n.log2()),
                Number::I64(n) => Number::F64((*n as f64).log2()),
                Number::I32(n) => Number::F64((*n as f64).log2()),
                Number::I16(n) => Number::F64((*n as f64).log2()),
                Number::I8(n) => Number::F64((*n as f64).log2()),
                Number::U64(n) => Number::F64((*n as f64).log2()),
                Number::U32(n) => Number::F64((*n as f64).log2()),
                Number::U16(n) => Number::F64((*n as f64).log2()),
                Number::U8(n) => Number::F64((*n as f64).log2()),
            };
            Ok(Value::Numeric(result_num))
        }
        _ => Err(RuntimeError::TypeError("log2 expects numeric type".into())),
    }
}

// ============================================================================
// Utility Functions
// ============================================================================

pub fn min(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() < 2 {
        return Err(RuntimeError::ArityError(
            "min expects at least 2 arguments".into(),
        ));
    }

    let mut min_val = match &args[0] {
        Value::Numeric(num) => number_to_f64(num),
        _ => return Err(RuntimeError::TypeError("min expects numeric types".into())),
    };

    for arg in args.iter().skip(1) {
        let val = match arg {
            Value::Numeric(num) => number_to_f64(num),
            _ => return Err(RuntimeError::TypeError("min expects numeric types".into())),
        };
        if val < min_val {
            min_val = val;
        }
    }

    Ok(Value::Numeric(Number::F64(min_val)))
}

pub fn max(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() < 2 {
        return Err(RuntimeError::ArityError(
            "max expects at least 2 arguments".into(),
        ));
    }

    let mut max_val = match &args[0] {
        Value::Numeric(num) => number_to_f64(num),
        _ => return Err(RuntimeError::TypeError("max expects numeric types".into())),
    };

    for arg in args.iter().skip(1) {
        let val = match arg {
            Value::Numeric(num) => number_to_f64(num),
            _ => return Err(RuntimeError::TypeError("max expects numeric types".into())),
        };
        if val > max_val {
            max_val = val;
        }
    }

    Ok(Value::Numeric(Number::F64(max_val)))
}

pub fn sign(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError("sign expects 1 argument".into()));
    }
    match &args[0] {
        Value::Numeric(num) => {
            let val = number_to_f64(num);
            let result = if val > 0.0 {
                1.0
            } else if val < 0.0 {
                -1.0
            } else {
                0.0
            };
            Ok(Value::Numeric(Number::F64(result)))
        }
        _ => Err(RuntimeError::TypeError("sign expects numeric type".into())),
    }
}

pub fn clamp(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::ArityError(
            "clamp expects 3 arguments (value, min, max)".into(),
        ));
    }

    let value = match &args[0] {
        Value::Numeric(num) => number_to_f64(num),
        _ => {
            return Err(RuntimeError::TypeError(
                "clamp expects numeric types".into(),
            ));
        }
    };

    let min_val = match &args[1] {
        Value::Numeric(num) => number_to_f64(num),
        _ => {
            return Err(RuntimeError::TypeError(
                "clamp expects numeric types".into(),
            ));
        }
    };

    let max_val = match &args[2] {
        Value::Numeric(num) => number_to_f64(num),
        _ => {
            return Err(RuntimeError::TypeError(
                "clamp expects numeric types".into(),
            ));
        }
    };

    let result = if value < min_val {
        min_val
    } else if value > max_val {
        max_val
    } else {
        value
    };

    Ok(Value::Numeric(Number::F64(result)))
}

// ============================================================================
// Constants
// ============================================================================

pub fn pi(_args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Numeric(Number::F64(std::f64::consts::PI)))
}

// ============================================================================
// Module Registration
// ============================================================================

pub fn create_module() -> ToolboxModule {
    let mut functions = HashMap::new();

    // Rounding & Absolute Value
    functions.insert(
        "abs".to_string(),
        abs as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "round".to_string(),
        round as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "floor".to_string(),
        floor as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "ceil".to_string(),
        ceil as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "trunc".to_string(),
        trunc as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );

    // Trigonometric Functions
    functions.insert(
        "sin".to_string(),
        sin as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "cos".to_string(),
        cos as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "tan".to_string(),
        tan as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "asin".to_string(),
        asin as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "acos".to_string(),
        acos as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "atan".to_string(),
        atan as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "atan2".to_string(),
        atan2 as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );

    // Exponential & Logarithmic Functions
    functions.insert(
        "sqrt".to_string(),
        sqrt as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "pow".to_string(),
        pow as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "exp".to_string(),
        exp as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "log".to_string(),
        log as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "log10".to_string(),
        log10 as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "log2".to_string(),
        log2 as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );

    // Utility Functions
    functions.insert(
        "min".to_string(),
        min as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "max".to_string(),
        max as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "sign".to_string(),
        sign as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "clamp".to_string(),
        clamp as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );

    // Constants
    functions.insert(
        "PI".to_string(),
        pi as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );

    ToolboxModule { functions }
}
