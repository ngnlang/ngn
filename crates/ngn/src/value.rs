use std::fmt;

use crate::bytecode::OpCode;
use crate::lexer::Span;

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
                (Number::U8(x), Number::U8(y)) => Number::U8(x + y),
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
                panic!(
                    "Logic Error: Failed to promote rank {} to {} while adding {} and {}",
                    r1, r2, promoted, other
                );
            }
            promoted.add(other)
        } else {
            let promoted = other.promote_to(r1);
            // Safety Check: If rank didn't change, we missed a match arm in promote_to
            if promoted.rank() == r2 {
                panic!(
                    "Logic Error: Failed to promote rank {} to {} while adding {} and {}",
                    r2, r1, self, promoted
                );
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
                (Number::U8(x), Number::U8(y)) => Number::U8(x - y),
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
                panic!(
                    "Logic Error: Failed to promote rank {} to {} while subtracting {} and {}",
                    r1, r2, promoted, other
                );
            }
            promoted.subtract(other)
        } else {
            let promoted = other.promote_to(r1);
            // Safety Check: If rank didn't change, we missed a match arm in promote_to
            if promoted.rank() == r2 {
                panic!(
                    "Logic Error: Failed to promote rank {} to {} while subtracting {} and {}",
                    r2, r1, self, promoted
                );
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
                (Number::U8(x), Number::U8(y)) => Number::U8(x * y),
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
                panic!(
                    "Logic Error: Failed to promote rank {} to {} while multiplying {} and {}",
                    r1, r2, promoted, other
                );
            }
            promoted.multiply(other)
        } else {
            let promoted = other.promote_to(r1);
            // Safety Check: If rank didn't change, we missed a match arm in promote_to
            if promoted.rank() == r2 {
                panic!(
                    "Logic Error: Failed to promote rank {} to {} while multiplying {} and {}",
                    r2, r1, self, promoted
                );
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
                    if b == 0 {
                        return Err("Division by zero".to_string());
                    }
                    // NGN Choice: 5 / 2 = 2.5
                    Ok(Number::F64(a as f64 / b as f64))
                }
                (Number::U8(x), Number::U8(y)) => Ok(Number::U8(x / y)),
                (Number::U16(x), Number::U16(y)) => Ok(Number::U16(x / y)),
                (Number::U32(x), Number::U32(y)) => Ok(Number::U32(x / y)),
                (Number::U64(x), Number::U64(y)) => Ok(Number::U64(x / y)),
                (Number::F32(x), Number::F32(y)) => Ok(Number::F32(x / y)),
                (Number::F64(a), Number::F64(b)) => {
                    if b == 0.0 {
                        return Err("Division by zero".to_string());
                    }
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
                panic!(
                    "Logic Error: Failed to promote rank {} to {} while dividing {} and {}",
                    r1, r2, promoted, other
                );
            }
            promoted.divide(other)
        } else {
            let promoted = other.promote_to(r1);
            // Safety Check: If rank didn't change, we missed a match arm in promote_to
            if promoted.rank() == r2 {
                panic!(
                    "Logic Error: Failed to promote rank {} to {} while dividing {} and {}",
                    r2, r1, self, promoted
                );
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
                    if y == 0 {
                        return Err("Modulo by zero".to_string());
                    }
                    Ok(Number::I64(x % y))
                }
                (Number::U8(x), Number::U8(y)) => Ok(Number::U8(x % y)),
                (Number::U16(x), Number::U16(y)) => Ok(Number::U16(x % y)),
                (Number::U32(x), Number::U32(y)) => Ok(Number::U32(x % y)),
                (Number::U64(x), Number::U64(y)) => {
                    if y == 0 {
                        return Err("Modulo by zero".to_string());
                    }
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
                (Number::U8(x), Number::U8(y)) => x < y,
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
                (Number::U8(x), Number::U8(y)) => x > y,
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
            (Number::I8(v), 9) => Number::F32(v as f32),
            (Number::I8(v), 10) => Number::F64(v as f64),

            (Number::I16(v), 7) => Number::I32(v as i32),
            (Number::I16(v), 8) => Number::I64(v as i64),
            (Number::I16(v), 9) => Number::F32(v as f32),
            (Number::I16(v), 10) => Number::F64(v as f64),

            (Number::I32(v), 8) => Number::I64(v as i64),
            (Number::I32(v), 9) => Number::F32(v as f32), // Precision loss possible
            (Number::I32(v), 10) => Number::F64(v as f64),

            (Number::I64(v), 9) => Number::F32(v as f32), // Precision loss possible
            (Number::I64(v), 10) => Number::F64(v as f64), // Precision loss possible

            // Unsigned integer promotion rules.
            // Note: we allow promotion to i64 to support mixed signed/unsigned arithmetic and
            // comparisons (e.g. u8 from bytes indexing compared with i64 literals).
            (Number::U8(v), 2) => Number::U16(v as u16),
            (Number::U8(v), 3) => Number::U32(v as u32),
            (Number::U8(v), 4) => Number::U64(v as u64),
            (Number::U8(v), 8) => Number::I64(v as i64), // Allow mixed
            (Number::U8(v), 9) => Number::F32(v as f32),
            (Number::U8(v), 10) => Number::F64(v as f64),

            (Number::U16(v), 3) => Number::U32(v as u32),
            (Number::U16(v), 4) => Number::U64(v as u64),
            (Number::U16(v), 8) => Number::I64(v as i64), // Allow mixed
            (Number::U16(v), 9) => Number::F32(v as f32),
            (Number::U16(v), 10) => Number::F64(v as f64),

            (Number::U32(v), 4) => Number::U64(v as u64),
            (Number::U32(v), 8) => Number::I64(v as i64), // Allow mixed
            (Number::U32(v), 9) => Number::F32(v as f32), // Precision loss possible
            (Number::U32(v), 10) => Number::F64(v as f64),

            (Number::U64(v), 8) => Number::I64(v as i64), // Allow mixed
            (Number::U64(v), 9) => Number::F32(v as f32), // Precision loss possible
            (Number::U64(v), 10) => Number::F64(v as f64), // Precision loss possible

            (Number::F32(v), 10) => Number::F64(v as f64),

            (val, _) => val, // Fallback if already at or above rank
        }
    }

    // Higher rank = more capacity
    pub fn rank(&self) -> u8 {
        match self {
            Number::U8(_) => 1,
            Number::U16(_) => 2,
            Number::U32(_) => 3,
            Number::U64(_) => 4,
            Number::I8(_) => 5,
            Number::I16(_) => 6,
            Number::I32(_) => 7,
            Number::I64(_) => 8,
            Number::F32(_) => 9,
            Number::F64(_) => 10,
        }
    }
}

use std::any::Any;
use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Channel {
    pub name: String,
    pub buffer: Arc<Mutex<VecDeque<Value>>>,
    pub capacity: usize,
    pub is_closed: Arc<Mutex<bool>>,
}

#[derive(Debug, Clone)]
pub struct ExternalValue {
    pub type_tag: String,
    pub inner: Arc<dyn Any + Send + Sync>,
}

impl serde::Serialize for ExternalValue {
    fn serialize<S>(&self, _serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        Err(serde::ser::Error::custom(
            "External values cannot be serialized",
        ))
    }
}

impl<'de> serde::Deserialize<'de> for ExternalValue {
    fn deserialize<D>(_deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Err(serde::de::Error::custom(
            "External values cannot be deserialized",
        ))
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Function {
    pub name: String,
    pub instructions: Arc<Vec<OpCode>>,
    #[serde(default = "default_instruction_spans")]
    pub instruction_spans: Arc<Vec<Span>>,
    #[serde(default = "default_source")]
    pub source: Arc<String>,
    #[serde(default = "default_filename")]
    pub filename: Arc<String>,
    pub constants: Arc<Vec<Value>>,
    pub home_globals: Option<Arc<Vec<Value>>>, // Reference to module's globals (None for main module functions)
    pub param_count: usize,
    // Index of the param in this list matches the index in the function's local env
    // bool = true if it's an owned parameter (<)
    pub param_ownership: Vec<bool>,
    pub param_types: Vec<crate::parser::Type>, // Type of each parameter
    pub default_values: Vec<Option<Value>>, // Default values for optional params (None = required)
    pub param_is_maybe_wrapped: Vec<bool>, // true if param was declared with ? (needs Maybe wrapping)
    pub return_type: crate::parser::Type,  // Return type of the function
    pub reg_count: usize,
    pub upvalues: Vec<crate::compiler::Upvalue>,
}

fn default_instruction_spans() -> Arc<Vec<Span>> {
    Arc::new(Vec::new())
}

fn default_source() -> Arc<String> {
    Arc::new(String::new())
}

fn default_filename() -> Arc<String> {
    Arc::new(String::new())
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Closure {
    pub function: Box<Function>,
    pub upvalues: Vec<Value>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct EnumData {
    pub enum_name: String,
    pub variant_name: String,
    pub data: Option<Box<Value>>,
}

impl EnumData {
    pub fn into_value(enum_name: String, variant_name: String, data: Option<Box<Value>>) -> Value {
        Value::Enum(Box::new(Self {
            enum_name,
            variant_name,
            data,
        }))
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ObjectData {
    pub model_name: String,
    pub fields: std::collections::HashMap<String, Value>,
}

impl ObjectData {
    pub fn into_value(
        model_name: String,
        fields: std::collections::HashMap<String, Value>,
    ) -> Value {
        Value::Object(Box::new(Self { model_name, fields }))
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ResponseData {
    pub status: u16,
    pub status_text: String,
    pub headers: std::collections::HashMap<String, String>,
    pub body: String,
    pub ok: bool,
}

impl ResponseData {
    pub fn new(
        status: u16,
        status_text: String,
        headers: std::collections::HashMap<String, String>,
        body: String,
    ) -> Self {
        let ok = status >= 200 && status < 300;
        Self {
            status,
            status_text,
            headers,
            body,
            ok,
        }
    }

    pub fn into_value(self) -> Value {
        Value::Response(Box::new(self))
    }
}

/// Streaming HTTP response backed by a channel
/// Each message from the channel becomes an HTTP chunk
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct StreamingResponseData {
    pub status: u16,
    pub headers: std::collections::HashMap<String, String>,
    pub body_channel: Channel, // Channel<string> that produces chunks
}

/// Server-Sent Events (SSE) response backed by a channel
/// Each message from the channel becomes one SSE record (framed and flushed)
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SseResponseData {
    pub status: u16,
    pub headers: std::collections::HashMap<String, String>,
    pub body_channel: Channel, // Channel<string | SseEvent | any>
    pub keep_alive_ms: u64,    // 0 disables keepalive
}

/// WebSocket response backed by channels
/// - recv_channel: messages received from the client
/// - send_channel: messages to send to the client
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct WebSocketResponseData {
    pub headers: std::collections::HashMap<String, String>,
    pub recv_channel: Channel, // Channel<string>
    pub send_channel: Channel, // Channel<string>
}

impl WebSocketResponseData {
    pub fn new(
        headers: std::collections::HashMap<String, String>,
        recv_channel: Channel,
        send_channel: Channel,
    ) -> Self {
        Self {
            headers,
            recv_channel,
            send_channel,
        }
    }

    pub fn into_value(self) -> Value {
        Value::WebSocketResponse(Box::new(self))
    }
}

impl SseResponseData {
    pub fn new(
        status: u16,
        headers: std::collections::HashMap<String, String>,
        body_channel: Channel,
        keep_alive_ms: u64,
    ) -> Self {
        Self {
            status,
            headers,
            body_channel,
            keep_alive_ms,
        }
    }

    pub fn into_value(self) -> Value {
        Value::SseResponse(Box::new(self))
    }
}

impl StreamingResponseData {
    pub fn new(
        status: u16,
        headers: std::collections::HashMap<String, String>,
        body_channel: Channel,
    ) -> Self {
        Self {
            status,
            headers,
            body_channel,
        }
    }

    pub fn into_value(self) -> Value {
        Value::StreamingResponse(Box::new(self))
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct RangeData {
    pub start: i64,
    pub end: i64,
    pub inclusive: bool,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[allow(dead_code)]
pub enum Value {
    Bool(bool),
    Function(Box<Function>),
    Closure(Box<Closure>),
    NativeFunction(u16),
    Numeric(Number),
    String(String),
    Bytes(Arc<Vec<u8>>),
    Reference(usize, usize), // (EnvironmentIndex, VariableIndex)
    Array(Vec<Value>),
    Tuple(Vec<Value>),
    Channel(Channel),
    Enum(Box<EnumData>),
    State(Arc<Mutex<Value>>),
    Object(Box<ObjectData>),
    Map(std::collections::HashMap<Value, Value>),
    Set(indexmap::IndexSet<Value>),
    Range(Box<RangeData>),
    Response(Box<ResponseData>),
    StreamingResponse(Box<StreamingResponseData>),
    SseResponse(Box<SseResponseData>),
    WebSocketResponse(Box<WebSocketResponseData>),
    External(ExternalValue),
    Void,
    Regex(String),
}

impl Default for Value {
    fn default() -> Self {
        Value::Void
    }
}

impl Value {
    pub fn add(&self, other: &Value) -> Result<Value, String> {
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

            (x, y) => Err(format!(
                "Runtime Error: Cannot subtract {:?} and {:?}",
                x, y
            )),
        }
    }
    pub fn multiply(&self, other: &Value) -> Result<Value, String> {
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
            (x, y) => Err(format!(
                "Runtime Error: Cannot use power operator with {:?} and {:?}",
                x, y
            )),
        }
    }

    pub fn remainder(self, other: Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Numeric(x), Value::Numeric(y)) => x.remainder(y).map(Value::Numeric),
            (x, y) => Err(format!(
                "Runtime Error: Cannot use modulo operator with {:?} and {:?}",
                x, y
            )),
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
                        (Number::U8(x), Number::U8(y)) => x == y,
                        (Number::U16(x), Number::U16(y)) => x == y,
                        (Number::U32(x), Number::U32(y)) => x == y,
                        (Number::U64(x), Number::U64(y)) => x == y,
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
                        panic!(
                            "Logic Error: Failed to promote rank {} to {} while comparing {} and {}",
                            r1, r2, promoted, b
                        );
                    }
                    Value::Numeric(promoted).is_equal(&Value::Numeric(*b))
                } else {
                    let promoted = b.promote_to(r1);
                    // Safety Check: If rank didn't change, we missed a match arm in promote_to
                    if promoted.rank() == r2 {
                        panic!(
                            "Logic Error: Failed to promote rank {} to {} while comparing {} and {}",
                            r2, r1, a, promoted
                        );
                    }
                    Value::Numeric(*a).is_equal(&Value::Numeric(promoted))
                }
            }
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Bytes(a), Value::Bytes(b)) => a.as_slice() == b.as_slice(),
            (Value::Array(a), Value::Array(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                for (v1, v2) in a.iter().zip(b.iter()) {
                    if !v1.is_equal(v2) {
                        return false;
                    }
                }
                true
            }
            (Value::Tuple(a), Value::Tuple(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                for (v1, v2) in a.iter().zip(b.iter()) {
                    if !v1.is_equal(v2) {
                        return false;
                    }
                }
                true
            }
            // Maybe auto-unwrap for comparisons: Maybe::Value(x) == x is true
            // Left is Maybe, right is not - check if Maybe::Value and compare inner
            (Value::Enum(e), other) if e.enum_name == "Maybe" => {
                match e.variant_name.as_str() {
                    "Value" => {
                        // Unwrap and compare with other
                        if let Some(inner) = &e.data {
                            inner.is_equal(other)
                        } else {
                            false
                        }
                    }
                    "Null" => {
                        // Maybe::Null only equals Maybe::Null
                        if let Value::Enum(e2) = other {
                            e2.enum_name == "Maybe" && e2.variant_name == "Null"
                        } else {
                            false
                        }
                    }
                    _ => false,
                }
            }
            // Right is Maybe, left is not - flip comparison
            (other, Value::Enum(e)) if e.enum_name == "Maybe" => {
                match e.variant_name.as_str() {
                    "Value" => {
                        if let Some(inner) = &e.data {
                            other.is_equal(inner)
                        } else {
                            false
                        }
                    }
                    "Null" => false, // T == Maybe::Null is always false (T is not Maybe)
                    _ => false,
                }
            }
            // Regular enum comparison (non-Maybe)
            (Value::Enum(e1), Value::Enum(e2)) => {
                if e1.variant_name != e2.variant_name || e1.enum_name != e2.enum_name {
                    return false;
                }

                match (&e1.data, &e2.data) {
                    (Some(a), Some(b)) => a.is_equal(b),
                    (None, None) => true,
                    _ => false,
                }
            }
            (Value::Object(o1), Value::Object(o2)) => {
                let m1 = &o1.model_name;
                let f1 = &o1.fields;
                let m2 = &o2.model_name;
                let f2 = &o2.fields;

                if m1 != m2 || f1.len() != f2.len() {
                    return false;
                }
                for (k, v1) in f1 {
                    if let Some(v2) = f2.get(k) {
                        if !v1.is_equal(v2) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
                true
            }
            (Value::Range(r1), Value::Range(r2)) => {
                r1.start == r2.start && r1.end == r2.end && r1.inclusive == r2.inclusive
            }
            (Value::Void, Value::Void) => true,
            (Value::Regex(a), Value::Regex(b)) => a == b,
            _ => false,
        }
    }

    pub fn type_name(&self) -> &str {
        match self {
            Value::Numeric(n) => match n {
                Number::I64(_)
                | Number::I32(_)
                | Number::I16(_)
                | Number::I8(_)
                | Number::U64(_)
                | Number::U32(_)
                | Number::U16(_)
                | Number::U8(_) => "i64",
                Number::F64(_) | Number::F32(_) => "f64",
            },
            Value::String(_) => "string",
            Value::Bytes(_) => "bytes",
            Value::Bool(_) => "bool",
            Value::Array(_) => "array",
            Value::Tuple(_) => "tuple",
            Value::Regex(_) => "regex",
            Value::Object(o) => &o.model_name,
            Value::Channel(_) => "channel",
            Value::State(_) => "state",
            Value::Enum(e) => &e.enum_name,
            Value::Map(_) => "map",
            Value::Set(_) => "set",
            Value::Range(_) => "range",
            Value::Function(_) | Value::Closure(_) | Value::NativeFunction(_) => "function",
            Value::Response(_) => "Response",
            Value::StreamingResponse(_) => "StreamingResponse",
            Value::SseResponse(_) => "SseResponse",
            Value::WebSocketResponse(_) => "WebSocketResponse",
            Value::External(_) => "External",
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
            Value::Bytes(b) => write!(f, "<bytes len={}>", b.len()),
            Value::Reference(env_idx, var_idx) => write!(f, "&({}:{})", env_idx, var_idx),
            Value::Array(arr) => {
                write!(f, "[")?;
                for (i, v) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            Value::Tuple(tup) => {
                write!(f, "(")?;
                for (i, v) in tup.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, ")")
            }
            Value::Enum(e) => {
                if let Some(d) = &e.data {
                    write!(f, "{}::{} ({})", e.enum_name, e.variant_name, d)
                } else {
                    write!(f, "{}::{}", e.enum_name, e.variant_name)
                }
            }
            Value::Closure(_) => write!(f, "<closure>"),
            Value::Channel(c) => write!(f, "<channel {}>", c.name),
            Value::State(s) => {
                let val = s.lock().unwrap();
                write!(f, "{}", *val)
            }
            Value::Object(o) => {
                // For anonymous objects, don't show the __anon__ prefix
                if o.model_name == "__anon__" {
                    write!(f, "{{")?;
                } else {
                    write!(f, "{} {{", o.model_name)?;
                }
                let mut first = true;
                for (name, val) in &o.fields {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, val)?;
                    first = false;
                }
                write!(f, "}}")
            }
            Value::Map(map) => {
                write!(f, "map{{")?;
                let mut first = true;
                for (k, v) in map {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", k, v)?;
                    first = false;
                }
                write!(f, "}}")
            }
            Value::Set(set) => {
                write!(f, "set{{")?;
                let mut first = true;
                for v in set {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                    first = false;
                }
                write!(f, "}}")
            }
            Value::Range(r) => {
                if r.inclusive {
                    write!(f, "{}..{}", r.start, r.end)
                } else {
                    write!(f, "{}..<{}", r.start, r.end)
                }
            }
            Value::Void => write!(f, "void"),
            Value::Regex(r) => write!(f, "/{}/", r),
            Value::Response(r) => {
                let headers_str: Vec<String> = r
                    .headers
                    .iter()
                    .map(|(k, v)| format!("\"{}\": \"{}\"", k, v))
                    .collect();
                write!(
                    f,
                    "Response {{ status: {}, statusText: \"{}\", ok: {}, headers: {{ {} }} }}",
                    r.status,
                    r.status_text,
                    r.ok,
                    headers_str.join(", ")
                )
            }
            Value::StreamingResponse(r) => {
                let headers_str: Vec<String> = r
                    .headers
                    .iter()
                    .map(|(k, v)| format!("\"{}\": \"{}\"", k, v))
                    .collect();
                write!(
                    f,
                    "StreamingResponse {{ status: {}, headers: {{ {} }}, body: <channel> }}",
                    r.status,
                    headers_str.join(", ")
                )
            }
            Value::SseResponse(r) => {
                let headers_str: Vec<String> = r
                    .headers
                    .iter()
                    .map(|(k, v)| format!("\"{}\": \"{}\"", k, v))
                    .collect();
                write!(
                    f,
                    "SseResponse {{ status: {}, headers: {{ {} }}, body: <channel>, keepAliveMs: {} }}",
                    r.status,
                    headers_str.join(", "),
                    r.keep_alive_ms
                )
            }
            Value::WebSocketResponse(r) => {
                let headers_str: Vec<String> = r
                    .headers
                    .iter()
                    .map(|(k, v)| format!("\"{}\": \"{}\"", k, v))
                    .collect();
                write!(
                    f,
                    "WebSocketResponse {{ headers: {{ {} }}, recv: <channel>, send: <channel> }}",
                    headers_str.join(", ")
                )
            }
            Value::External(e) => write!(f, "<external {}>", e.type_tag),
        }
    }
}

// Hash and Eq implementations for Value to support HashMap/HashSet keys
impl std::hash::Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Value::Numeric(n) => {
                // Hash based on the actual numeric value
                match n {
                    Number::I64(v) => v.hash(state),
                    Number::I32(v) => v.hash(state),
                    Number::I16(v) => v.hash(state),
                    Number::I8(v) => v.hash(state),
                    Number::U64(v) => v.hash(state),
                    Number::U32(v) => v.hash(state),
                    Number::U16(v) => v.hash(state),
                    Number::U8(v) => v.hash(state),
                    Number::F64(v) => v.to_bits().hash(state),
                    Number::F32(v) => v.to_bits().hash(state),
                }
            }
            Value::String(s) => s.hash(state),
            Value::Bytes(b) => b.as_slice().hash(state),
            Value::Bool(b) => b.hash(state),
            Value::Void => 0.hash(state),
            Value::Regex(r) => r.hash(state),
            // For unhashable types, panic with a clear error
            Value::Function(_) => {
                panic!("Runtime Error: Functions cannot be used as map keys or set values")
            }
            Value::Closure(_) => {
                panic!("Runtime Error: Closures cannot be used as map keys or set values")
            }
            Value::NativeFunction(_) => {
                panic!("Runtime Error: Native functions cannot be used as map keys or set values")
            }
            Value::Channel(_) => {
                panic!("Runtime Error: Channels cannot be used as map keys or set values")
            }
            Value::State(_) => {
                panic!("Runtime Error: State values cannot be used as map keys or set values")
            }
            Value::Array(_) => {
                panic!("Runtime Error: Arrays cannot be used as map keys or set values")
            }
            Value::Tuple(_) => {
                panic!("Runtime Error: Tuples cannot be used as map keys or set values")
            }
            Value::Enum(_) => {
                panic!("Runtime Error: Enums cannot be used as map keys or set values")
            }
            Value::Object(_) => {
                panic!("Runtime Error: Objects cannot be used as map keys or set values")
            }
            Value::Map(_) => panic!("Runtime Error: Maps cannot be used as map keys or set values"),
            Value::Set(_) => panic!("Runtime Error: Sets cannot be used as map keys or set values"),
            Value::Range(_) => {
                panic!("Runtime Error: Ranges cannot be used as map keys or set values")
            }
            Value::Reference(_, _) => {
                panic!("Runtime Error: References cannot be used as map keys or set values")
            }
            Value::Response(_) => {
                panic!("Runtime Error: Response values cannot be used as map keys or set values")
            }
            Value::StreamingResponse(_) => {
                panic!(
                    "Runtime Error: StreamingResponse values cannot be used as map keys or set values"
                )
            }
            Value::SseResponse(_) => {
                panic!("Runtime Error: SseResponse values cannot be used as map keys or set values")
            }
            Value::WebSocketResponse(_) => {
                panic!(
                    "Runtime Error: WebSocketResponse values cannot be used as map keys or set values"
                )
            }
            Value::External(_) => {
                panic!("Runtime Error: External values cannot be used as map keys or set values")
            }
        }
    }
}

impl std::cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.is_equal(other)
    }
}

impl std::cmp::Eq for Value {} // Value is Eq because is_equal handles all cases properly

// JSON Conversion Functions
impl Value {
    /// Convert Value to serde_json::Value for JSON.stringify
    pub fn to_json(&self) -> serde_json::Value {
        match self {
            Value::Void => serde_json::Value::Null,
            Value::Bool(b) => serde_json::Value::Bool(*b),
            Value::String(s) => serde_json::Value::String(s.clone()),
            Value::Bytes(_) => serde_json::Value::Null,
            Value::Numeric(n) => match n {
                Number::I64(v) => serde_json::json!(*v),
                Number::I32(v) => serde_json::json!(*v),
                Number::I16(v) => serde_json::json!(*v),
                Number::I8(v) => serde_json::json!(*v),
                Number::U64(v) => serde_json::json!(*v),
                Number::U32(v) => serde_json::json!(*v),
                Number::U16(v) => serde_json::json!(*v),
                Number::U8(v) => serde_json::json!(*v),
                Number::F64(v) => serde_json::json!(*v),
                Number::F32(v) => serde_json::json!(*v),
            },
            Value::Array(arr) => {
                serde_json::Value::Array(arr.iter().map(|v| v.to_json()).collect())
            }
            Value::Tuple(tup) => {
                serde_json::Value::Array(tup.iter().map(|v| v.to_json()).collect())
            }
            Value::Object(obj) => {
                let mut map = serde_json::Map::new();
                for (k, v) in &obj.fields {
                    map.insert(k.clone(), v.to_json());
                }
                serde_json::Value::Object(map)
            }
            Value::Map(m) => {
                let mut map = serde_json::Map::new();
                for (k, v) in m.iter() {
                    if let Value::String(key) = k {
                        map.insert(key.clone(), v.to_json());
                    }
                }
                serde_json::Value::Object(map)
            }
            Value::Set(s) => {
                // Set serializes to JSON array
                serde_json::Value::Array(s.iter().map(|v| v.to_json()).collect())
            }
            Value::External(_) => {
                panic!("Runtime Error: Cannot convert External value to JSON")
            }
            // Non-serializable types become null
            _ => serde_json::Value::Null,
        }
    }

    /// Convert serde_json::Value to Value for JSON.parse
    pub fn from_json(json: serde_json::Value) -> Value {
        match json {
            serde_json::Value::Null => Value::Void,
            serde_json::Value::Bool(b) => Value::Bool(b),
            serde_json::Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Value::Numeric(Number::I64(i))
                } else if let Some(f) = n.as_f64() {
                    Value::Numeric(Number::F64(f))
                } else {
                    Value::Void
                }
            }
            serde_json::Value::String(s) => Value::String(s),
            serde_json::Value::Array(arr) => {
                Value::Array(arr.into_iter().map(Value::from_json).collect())
            }
            serde_json::Value::Object(map) => {
                let fields: std::collections::HashMap<String, Value> = map
                    .into_iter()
                    .map(|(k, v)| (k, Value::from_json(v)))
                    .collect();
                Value::Object(Box::new(ObjectData {
                    model_name: "__anon__".to_string(),
                    fields,
                }))
            }
        }
    }
}
