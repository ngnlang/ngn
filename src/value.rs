use std::fmt;

use crate::bytecode::OpCode;

#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
#[derive(serde::Serialize, serde::Deserialize)]
pub enum Number {
    I64(i64),
    I32(i32),
    I16(i16),
    I8(i8),
    U64(u64),
    U32(u32),
    U16(u16),
    U8(u8),
    F64(f64),
    F32(f32),
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Number::I64(v) => write!(f, "{}", v),
            Number::I32(v) => write!(f, "{}", v),
            Number::I16(v) => write!(f, "{}", v),
            Number::I8(v) => write!(f, "{}", v),
            Number::U64(v) => write!(f, "{}", v),
            Number::U32(v) => write!(f, "{}", v),
            Number::U16(v) => write!(f, "{}", v),
            Number::U8(v) => write!(f, "{}", v),
            Number::F64(v) => write!(f, "{}", v),
            Number::F32(v) => write!(f, "{}", v),
        }
    }
}

impl Number {
    pub fn add(self, other: Number) -> Number {
        let r1 = self.rank();
        let r2 = other.rank();

        if r1 == r2 {
            // Same types
            return match (self, other) {
                (Number::I8(x), Number::I8(y)) => Number::I8(x + y),
                (Number::I16(x), Number::I16(y)) => Number::I16(x + y),
                (Number::I32(x), Number::I32(y)) => Number::I32(x + y),
                (Number::I64(x), Number::I64(y)) => Number::I64(x + y),
                (Number::U8(x), Number::U8(y))   => Number::U8(x + y),
                (Number::U16(x), Number::U16(y)) => Number::U16(x + y),
                (Number::U32(x), Number::U32(y)) => Number::U32(x + y),
                (Number::U64(x), Number::U64(y)) => Number::U64(x + y),
                (Number::F32(x), Number::F32(y)) => Number::F32(x + y),
                (Number::F64(x), Number::F64(y)) => Number::F64(x + y),
                _ => panic!("Addition not implemented for these types"),
            };
        }

        // Different types: Promote the smaller one
        if r1 < r2 {
            let promoted = self.promote_to(r2);
            // Safety Check: If rank didn't change, we missed a match arm in promote_to
            if promoted.rank() == r1 {
                panic!("Logic Error: Failed to promote rank {} to {} while adding {} and {}", r1, r2, promoted, other);
            }
            promoted.add(other)
        } else {
            let promoted = other.promote_to(r1);
            // Safety Check: If rank didn't change, we missed a match arm in promote_to
            if promoted.rank() == r2 {
                panic!("Logic Error: Failed to promote rank {} to {} while adding {} and {}", r2, r1, self, promoted);
            }
            self.add(promoted)
        }
    }

    pub fn subtract(self, other: Number) -> Number {
        let r1 = self.rank();
        let r2 = other.rank();

        if r1 == r2 {
            // Same types
            return match (self, other) {
                (Number::I8(x), Number::I8(y)) => Number::I8(x - y),
                (Number::I16(x), Number::I16(y)) => Number::I16(x - y),
                (Number::I32(x), Number::I32(y)) => Number::I32(x - y),
                (Number::I64(x), Number::I64(y)) => Number::I64(x - y),
                (Number::U8(x), Number::U8(y))   => Number::U8(x - y),
                (Number::U16(x), Number::U16(y)) => Number::U16(x - y),
                (Number::U32(x), Number::U32(y)) => Number::U32(x - y),
                (Number::U64(x), Number::U64(y)) => Number::U64(x - y),
                (Number::F32(x), Number::F32(y)) => Number::F32(x - y),
                (Number::F64(x), Number::F64(y)) => Number::F64(x - y),
                _ => panic!("Subtraction not implemented for these types"),
            };
        }

        // Different types: Promote the smaller one
        if r1 < r2 {
            let promoted = self.promote_to(r2);
            // Safety Check: If rank didn't change, we missed a match arm in promote_to
            if promoted.rank() == r1 {
                panic!("Logic Error: Failed to promote rank {} to {} while subtracting {} and {}", r1, r2, promoted, other);
            }
            promoted.subtract(other)
        } else {
            let promoted = other.promote_to(r1);
            // Safety Check: If rank didn't change, we missed a match arm in promote_to
            if promoted.rank() == r2 {
                panic!("Logic Error: Failed to promote rank {} to {} while subtracting {} and {}", r2, r1, self, promoted);
            }
            self.subtract(promoted)
        }
    }

     pub fn multiply(self, other: Number) -> Number {
        let r1 = self.rank();
        let r2 = other.rank();

        if r1 == r2 {
            // Same types
            return match (self, other) {
                (Number::I8(x), Number::I8(y)) => Number::I8(x * y),
                (Number::I16(x), Number::I16(y)) => Number::I16(x * y),
                (Number::I32(x), Number::I32(y)) => Number::I32(x * y),
                (Number::I64(x), Number::I64(y)) => Number::I64(x * y),
                (Number::U8(x), Number::U8(y))   => Number::U8(x * y),
                (Number::U16(x), Number::U16(y)) => Number::U16(x * y),
                (Number::U32(x), Number::U32(y)) => Number::U32(x * y),
                (Number::U64(x), Number::U64(y)) => Number::U64(x * y),
                (Number::F32(x), Number::F32(y)) => Number::F32(x * y),
                (Number::F64(x), Number::F64(y)) => Number::F64(x * y),
                _ => panic!("Multiplication not implemented for these types"),
            };
        }

        // Different types: Promote the smaller one
        if r1 < r2 {
            let promoted = self.promote_to(r2);
            // Safety Check: If rank didn't change, we missed a match arm in promote_to
            if promoted.rank() == r1 {
                panic!("Logic Error: Failed to promote rank {} to {} while multiplying {} and {}", r1, r2, promoted, other);
            }
            promoted.multiply(other)
        } else {
            let promoted = other.promote_to(r1);
            // Safety Check: If rank didn't change, we missed a match arm in promote_to
            if promoted.rank() == r2 {
                panic!("Logic Error: Failed to promote rank {} to {} while multiplying {} and {}", r2, r1, self, promoted);
            }
            self.multiply(promoted)
        }
    }

    pub fn divide(self, other: Number) -> Result<Number, String> {
        let r1 = self.rank();
        let r2 = other.rank();

        if r1 == r2 {
            // Same types
            return match (self, other) {
                (Number::I8(x), Number::I8(y)) => Ok(Number::I8(x / y)),
                (Number::I16(x), Number::I16(y)) => Ok(Number::I16(x / y)),
                (Number::I32(x), Number::I32(y)) => Ok(Number::I32(x / y)),
                (Number::I64(a), Number::I64(b)) => {
                    if b == 0 { return Err("Division by zero".to_string()); }
                    // NGN Choice: 5 / 2 = 2.5
                    Ok(Number::F64(a as f64 / b as f64))
                }
                (Number::U8(x), Number::U8(y))   => Ok(Number::U8(x / y)),
                (Number::U16(x), Number::U16(y)) => Ok(Number::U16(x / y)),
                (Number::U32(x), Number::U32(y)) => Ok(Number::U32(x / y)),
                (Number::U64(x), Number::U64(y)) => Ok(Number::U64(x / y)),
                (Number::F32(x), Number::F32(y)) => Ok(Number::F32(x / y)),
                (Number::F64(a), Number::F64(b)) => {
                    if b == 0.0 { return Err("Division by zero".to_string()); }
                    Ok(Number::F64(a / b))
                }
                _ => panic!("Division not implemented for these types"),
            };
        }

        // Different types: Promote the smaller one
        if r1 < r2 {
            let promoted = self.promote_to(r2);
            // Safety Check: If rank didn't change, we missed a match arm in promote_to
            if promoted.rank() == r1 {
                panic!("Logic Error: Failed to promote rank {} to {} while dividing {} and {}", r1, r2, promoted, other);
            }
            promoted.divide(other)
        } else {
            let promoted = other.promote_to(r1);
            // Safety Check: If rank didn't change, we missed a match arm in promote_to
            if promoted.rank() == r2 {
                panic!("Logic Error: Failed to promote rank {} to {} while dividing {} and {}", r2, r1, self, promoted);
            }
            self.divide(promoted)
        }
    }

    pub fn remainder(self, other: Number) -> Result<Number, String> {
        let r1 = self.rank();
        let r2 = other.rank();

        if r1 == r2 {
            return match (self, other) {
                (Number::I8(x), Number::I8(y)) => Ok(Number::I8(x % y)),
                (Number::I16(x), Number::I16(y)) => Ok(Number::I16(x % y)),
                (Number::I32(x), Number::I32(y)) => Ok(Number::I32(x % y)),
                (Number::I64(x), Number::I64(y)) => {
                    if y == 0 { return Err("Modulo by zero".to_string()); }
                    Ok(Number::I64(x % y))
                }
                (Number::U8(x), Number::U8(y)) => Ok(Number::U8(x % y)),
                (Number::U16(x), Number::U16(y)) => Ok(Number::U16(x % y)),
                (Number::U32(x), Number::U32(y)) => Ok(Number::U32(x % y)),
                (Number::U64(x), Number::U64(y)) => {
                    if y == 0 { return Err("Modulo by zero".to_string()); }
                    Ok(Number::U64(x % y))
                }
                (Number::F32(x), Number::F32(y)) => Ok(Number::F32(x % y)),
                (Number::F64(x), Number::F64(y)) => Ok(Number::F64(x % y)),
                _ => panic!("Modulo not implemented for these types"),
            };
        }

        if r1 < r2 {
            let promoted = self.promote_to(r2);
            promoted.remainder(other)
        } else {
            let promoted = other.promote_to(r1);
            self.remainder(promoted)
        }
    }

    pub fn power(self, other: Number) -> Number {
        let r1 = self.rank();
        let r2 = other.rank();

        if r1 == r2 {
            return match (self, other) {
                (Number::I8(x), Number::I8(y)) => Number::F64((x as f64).powf(y as f64)),
                (Number::I16(x), Number::I16(y)) => Number::F64((x as f64).powf(y as f64)),
                (Number::I32(x), Number::I32(y)) => Number::F64((x as f64).powf(y as f64)),
                (Number::I64(x), Number::I64(y)) => Number::F64((x as f64).powf(y as f64)),
                (Number::U8(x), Number::U8(y)) => Number::F64((x as f64).powf(y as f64)),
                (Number::U16(x), Number::U16(y)) => Number::F64((x as f64).powf(y as f64)),
                (Number::U32(x), Number::U32(y)) => Number::F64((x as f64).powf(y as f64)),
                (Number::U64(x), Number::U64(y)) => Number::F64((x as f64).powf(y as f64)),
                (Number::F32(x), Number::F32(y)) => Number::F32(x.powf(y)),
                (Number::F64(x), Number::F64(y)) => Number::F64(x.powf(y)),
                _ => panic!("Power not implemented for these types"),
            };
        }

        if r1 < r2 {
            let promoted = self.promote_to(r2);
            promoted.power(other)
        } else {
            let promoted = other.promote_to(r1);
            self.power(promoted)
        }
    }

    pub fn less_than(self, other: Number) -> bool {
        let r1 = self.rank();
        let r2 = other.rank();

        if r1 == r2 {
             match (self, other) {
                (Number::I8(x), Number::I8(y)) => x < y,
                (Number::I16(x), Number::I16(y)) => x < y,
                (Number::I32(x), Number::I32(y)) => x < y,
                (Number::I64(x), Number::I64(y)) => x < y,
                (Number::U8(x), Number::U8(y))   => x < y,
                (Number::U16(x), Number::U16(y)) => x < y,
                (Number::U32(x), Number::U32(y)) => x < y,
                (Number::U64(x), Number::U64(y)) => x < y,
                (Number::F32(x), Number::F32(y)) => x < y,
                (Number::F64(x), Number::F64(y)) => x < y,
                _ => false,
            }
        } else if r1 < r2 {
            let promoted = self.promote_to(r2);
            promoted.less_than(other)
        } else {
            let promoted = other.promote_to(r1);
            self.less_than(promoted)
        }
    }

    pub fn greater_than(self, other: Number) -> bool {
        let r1 = self.rank();
        let r2 = other.rank();

        if r1 == r2 {
             match (self, other) {
                (Number::I8(x), Number::I8(y)) => x > y,
                (Number::I16(x), Number::I16(y)) => x > y,
                (Number::I32(x), Number::I32(y)) => x > y,
                (Number::I64(x), Number::I64(y)) => x > y,
                (Number::U8(x), Number::U8(y))   => x > y,
                (Number::U16(x), Number::U16(y)) => x > y,
                (Number::U32(x), Number::U32(y)) => x > y,
                (Number::U64(x), Number::U64(y)) => x > y,
                (Number::F32(x), Number::F32(y)) => x > y,
                (Number::F64(x), Number::F64(y)) => x > y,
                _ => false,
            }
        } else if r1 < r2 {
            let promoted = self.promote_to(r2);
            promoted.greater_than(other)
        } else {
            let promoted = other.promote_to(r1);
            self.greater_than(promoted)
        }
    }

    pub fn negate(self) -> Number {
        match self {
            Number::I64(x) => Number::I64(-x),
            Number::I32(x) => Number::I32(-x),
            Number::I16(x) => Number::I16(-x),
            Number::I8(x) => Number::I8(-x),
            Number::U64(x) => Number::I64(-(x as i64)),
            Number::U32(x) => Number::I32(-(x as i32)),
            Number::U16(x) => Number::I16(-(x as i16)),
            Number::U8(x) => Number::I8(-(x as i8)),
            Number::F64(x) => Number::F64(-x),
            Number::F32(x) => Number::F32(-x),
        }
    }

    fn promote_to(self, target_rank: u8) -> Number {
        match (self, target_rank) {
            (Number::I8(v), 6) => Number::I16(v as i16),
            (Number::I8(v), 7) => Number::I32(v as i32),
            (Number::I8(v), 8) => Number::I64(v as i64),
            (Number::I8(v),  9) => Number::F32(v as f32),
            (Number::I8(v),  10) => Number::F64(v as f64),

            (Number::I16(v), 7) => Number::I32(v as i32),
            (Number::I16(v), 8) => Number::I64(v as i64),
            (Number::I16(v), 9) => Number::F32(v as f32),
            (Number::I16(v), 10) => Number::F64(v as f64),

            (Number::I32(v), 8) => Number::I64(v as i64),
            (Number::I32(v), 9) => Number::F32(v as f32), // Precision loss possible
            (Number::I32(v), 10) => Number::F64(v as f64),

            (Number::I64(v), 9) => Number::F32(v as f32), // Precision loss possible
            (Number::I64(v), 10) => Number::F64(v as f64), // Precision loss possible

            (Number::U8(v), 2) => Number::U16(v as u16),
            (Number::U8(v), 3) => Number::U32(v as u32),
            (Number::U8(v), 4) => Number::U64(v as u64),
            (Number::U8(v),  9) => Number::F32(v as f32),
            (Number::U8(v),  10) => Number::F64(v as f64),

            (Number::U16(v), 3) => Number::U32(v as u32),
            (Number::U16(v), 4) => Number::U64(v as u64),
            (Number::U16(v), 9) => Number::F32(v as f32),
            (Number::U16(v), 10) => Number::F64(v as f64),

            (Number::U32(v), 4) => Number::U64(v as u64),
            (Number::U32(v), 9) => Number::F32(v as f32), // Precision loss possible
            (Number::U32(v), 10) => Number::F64(v as f64),

            (Number::U64(v), 9) => Number::F32(v as f32), // Precision loss possible
            (Number::U64(v), 10) => Number::F64(v as f64), // Precision loss possible

            (Number::F32(v), 10) => Number::F64(v as f64),

            (val, _) => val, // Fallback if already at or above rank
        }
    }

    // Higher rank = more capacity
    pub fn rank(&self) -> u8 {
        match self {
            Number::U8(_)  => 1,
            Number::U16(_) => 2,
            Number::U32(_) => 3,
            Number::U64(_) => 4,
            Number::I8(_)  => 5,
            Number::I16(_) => 6,
            Number::I32(_) => 7,
            Number::I64(_) => 8,
            Number::F32(_) => 9,
            Number::F64(_) => 10,
        }
    }
}

use std::sync::{Arc, Mutex};
use std::collections::VecDeque;

#[derive(Debug, Clone)]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Channel {
    pub name: String,
    pub buffer: Arc<Mutex<VecDeque<Value>>>,
    pub capacity: usize,
    pub is_closed: Arc<Mutex<bool>>,
}

#[derive(Debug, Clone)]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Function {
    pub name: String,
    pub instructions: Arc<Vec<OpCode>>,
    pub constants: Arc<Vec<Value>>,
    pub param_count: usize,
    // Index of the param in this list matches the index in the function's local env
    // Index of the param in this list matches the index in the function's local env
    // bool = true if it's an owned parameter (<)
    pub param_ownership: Vec<bool>,
    pub reg_count: usize,
}

#[derive(Debug, Clone)]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Closure {
    pub function: Box<Function>,
    pub upvalues: Vec<Value>,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
#[derive(serde::Serialize, serde::Deserialize)]
pub enum Value {
    Bool(bool),
    Function(Box<Function>),
    Closure(Box<Closure>),
    NativeFunction(u16),
    Numeric(Number),
    String(String),
    Reference(usize, usize), // (EnvironmentIndex, VariableIndex)
    Array(Vec<Value>),
    Tuple(Vec<Value>),
    Channel(Channel),
    Enum {
        enum_name: String,
        variant_name: String,
        data: Option<Box<Value>>,
    },
    State(Arc<Mutex<Value>>),
    Object {
        model_name: String,
        fields: std::collections::HashMap<String, Value>,
    },
    Void,
}

impl Value {
    pub fn add(&self, other: &Value) -> Result<Value, String>  {
        match (self, other) {
            // If both are numbers, delegate to the Number enum
            (Value::Numeric(x), Value::Numeric(y)) => Ok(Value::Numeric(x.add(*y))),
            
            // If both are strings, handle concatenation
            (Value::String(x), Value::String(y)) => Ok(Value::String(format!("{}{}", x, y))),
            
            (x, y) => Err(format!("Runtime Error: Cannot add {:?} and {:?}", x, y)),
        }
    }
    pub fn subtract(&self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Numeric(x), Value::Numeric(y)) => Ok(Value::Numeric(x.subtract(*y))),
            
            (x, y) => Err(format!("Runtime Error: Cannot subtract {:?} and {:?}", x, y)),
        }
    }
    pub fn multiply(&self, other: &Value) -> Result<Value, String>  {
        match (self, other) {
            (Value::Numeric(x), Value::Numeric(y)) => Ok(Value::Numeric(x.multiply(*y))),
            
            (x, y) => Err(format!("Runtime Error: Cannot multiply {:?} by {:?}", x, y)),
        }
    }
    pub fn divide(&self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Numeric(x), Value::Numeric(y)) => x.divide(*y).map(Value::Numeric),
            
            (x, y) => Err(format!("Runtime Error: Cannot divide {:?} by {:?}", x, y)),
        }
    }

    pub fn power(self, other: Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Numeric(x), Value::Numeric(y)) => Ok(Value::Numeric(x.power(y))),
            (x, y) => Err(format!("Runtime Error: Cannot use power operator with {:?} and {:?}", x, y)),
        }
    }

    pub fn remainder(self, other: Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Numeric(x), Value::Numeric(y)) => x.remainder(y).map(Value::Numeric),
            (x, y) => Err(format!("Runtime Error: Cannot use modulo operator with {:?} and {:?}", x, y)),
        }
    }
    pub fn is_equal(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Numeric(a), Value::Numeric(b)) => {
                let r1 = a.rank();
                let r2 = b.rank();

                if r1 == r2 {
                    return match (a, b) {
                        (Number::I8(x), Number::I8(y)) => x == y,
                        (Number::I16(x), Number::I16(y)) => x == y,
                        (Number::I32(x), Number::I32(y)) => x == y,
                        (Number::I64(x), Number::I64(y)) => x == y,
                        (Number::U8(x),  Number::U8(y))  => x == y,
                        (Number::U16(x),  Number::U16(y))  => x == y,
                        (Number::U32(x),  Number::U32(y))  => x == y,
                        (Number::U64(x),  Number::U64(y))  => x == y,
                        (Number::F32(x), Number::F32(y)) => x == y,
                        (Number::F64(x), Number::F64(y)) => x == y,
                        _ => false,
                    };
                }

                // Different types: Promote the smaller one
                if r1 < r2 {
                    let promoted = a.promote_to(r2);
                    // Safety Check: If rank didn't change, we missed a match arm in promote_to
                    if promoted.rank() == r1 {
                        panic!("Logic Error: Failed to promote rank {} to {} while comparing {} and {}", r1, r2, promoted, b);
                    }
                    Value::Numeric(promoted).is_equal(&Value::Numeric(*b))
                } else {
                    let promoted = b.promote_to(r1);
                    // Safety Check: If rank didn't change, we missed a match arm in promote_to
                    if promoted.rank() == r2 {
                        panic!("Logic Error: Failed to promote rank {} to {} while comparing {} and {}", r2, r1, a, promoted);
                    }
                    Value::Numeric(*a).is_equal(&Value::Numeric(promoted))
                }
            }
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => {
                if a.len() != b.len() { return false; }
                for (v1, v2) in a.iter().zip(b.iter()) {
                    if !v1.is_equal(v2) {
                        return false;
                    }
                }
                true
            }
            (Value::Tuple(a), Value::Tuple(b)) => {
                if a.len() != b.len() { return false; }
                for (v1, v2) in a.iter().zip(b.iter()) {
                    if !v1.is_equal(v2) {
                        return false;
                    }
                }
                true
            }
            (Value::Enum { enum_name: e1, variant_name: v1, data: d1 }, Value::Enum { enum_name: e2, variant_name: v2, data: d2 }) => {
                // Two enums are equal if they belong to the same Enum type,
                // have the same variant name, and their associated data matches.
                if v1 != v2 || e1 != e2 { 
                    return false; 
                }

                match (d1, d2) {
                    (Some(a), Some(b)) => a.is_equal(b),
                    (None, None) => true,
                    _ => false,
                }
            }
            (Value::Object { model_name: m1, fields: f1 }, Value::Object { model_name: m2, fields: f2 }) => {
                if m1 != m2 || f1.len() != f2.len() { return false; }
                for (k, v1) in f1 {
                    if let Some(v2) = f2.get(k) {
                        if !v1.is_equal(v2) { return false; }
                    } else { return false; }
                }
                true
            }
            (Value::Void, Value::Void) => true,
            _ => false,
        }
    }

    pub fn type_name(&self) -> &str {
        match self {
            Value::Numeric(n) => match n {
                Number::I64(_) | Number::I32(_) | Number::I16(_) | Number::I8(_) |
                Number::U64(_) | Number::U32(_) | Number::U16(_) | Number::U8(_) => "i64",
                Number::F64(_) | Number::F32(_) => "f64",
            },
            Value::String(_) => "string",
            Value::Bool(_) => "bool",
            Value::Array(_) => "array",
            Value::Tuple(_) => "tuple",
            Value::Object { model_name, .. } => model_name,
            Value::Channel(_) => "channel",
            Value::State(_) => "state",
            Value::Enum { enum_name, .. } => enum_name,
            Value::Function(_) | Value::Closure(_) | Value::NativeFunction(_) => "function",
            Value::Void => "void",
            Value::Reference(_, _) => "reference",
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{}", b),
            Value::Function(func) => write!(f, "<fn {}>", func.name),
            Value::NativeFunction(id) => write!(f, "<fn {}>", id),
            Value::Numeric(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Reference(env_idx, var_idx) => write!(f, "&({}:{})", env_idx, var_idx),
            Value::Array(arr) => {
                write!(f, "[")?;
                for (i, v) in arr.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            Value::Tuple(tup) => {
                write!(f, "(")?;
                for (i, v) in tup.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", v)?;
                }
                write!(f, ")")
            }
            Value::Enum { enum_name, variant_name, data } => {
                if let Some(d) = data {
                    write!(f, "{}::{} ({})", enum_name, variant_name, d)
                } else {
                    write!(f, "{}::{}", enum_name, variant_name)
                }
            }
            Value::Closure(_) => write!(f, "<closure>"),
            Value::Channel(c) => write!(f, "<channel {}>", c.name),
            Value::State(s) => {
                let val = s.lock().unwrap();
                write!(f, "{}", *val)
            }
            Value::Object { model_name, fields } => {
                write!(f, "{} {{", model_name)?;
                let mut first = true;
                for (name, val) in fields {
                    if !first { write!(f, ", ")?; }
                    write!(f, "{}: {}", name, val)?;
                    first = false;
                }
                write!(f, "}}")
            }
            Value::Void => write!(f, "void"),
        }
    }
}