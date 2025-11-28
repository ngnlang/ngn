use std::collections::HashMap;
use tokio::sync::mpsc;
use std::sync::Arc;
use tokio::sync::Mutex;

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum Type {
    I64,
    I32,
    U64,
    U32,
    F64,
    F32,
    String, // owned
    Str, // borrowed
    Bool,
    Array(Box<Type>),
    Object(HashMap<String, Type>), // dead code
    Function,
    Void,
    Model(String),
    Regex,
    Generic(String),
    Enum(String, Vec<Type>),
    Channel(Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    pub data_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDef {
    pub name: String,
    pub type_params: Vec<String>,
    pub variants: Vec<EnumVariant>,
}

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
            // ... implement for other simple types ...
            (Value::Channel(_, _, _), Value::Channel(_, _, _)) => false, // Channels are never "equal"
            _ => false, // Different types are not equal
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum Expr {
    I64(i64),
    I32(i32),
    U64(u64),
    U32(u32),
    F64(f64),
    F32(f32),
    String(String),
    Bool(bool),
    Array(Vec<Expr>),
    Not(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Subtract(Box<Expr>, Box<Expr>),
    Multiply(Box<Expr>, Box<Expr>),
    Divide(Box<Expr>, Box<Expr>),
    Negative(Box<Expr>),
    Power(Box<Expr>, Box<Expr>),
    Modulo(Box<Expr>, Box<Expr>),
    Equal(Box<Expr>, Box<Expr>),
    NotEqual(Box<Expr>, Box<Expr>),
    LessThan(Box<Expr>, Box<Expr>),
    LessThanOrEqual(Box<Expr>, Box<Expr>),
    GreaterThan(Box<Expr>, Box<Expr>),
    GreaterThanOrEqual(Box<Expr>, Box<Expr>),
    Assign { name: String, value: Box<Expr> },
    CompoundAssign { name: String, op: String, value: Box<Expr> },
    Var(String),
    Const(String), // dead code
    Lit(String), // dead code
    Static(String), // dead code
    Call { name: String, args: Vec<Expr> },
    InterpolatedString(Vec<InterpolationPart>),
    ModelInstance { name: String, fields: Vec<(String, Expr)> },
    FieldAccess { object: Box<Expr>, field: String, value: Option<Box<Expr>> },
    MethodCall { object: Box<Expr>, method: String, args: Vec<Expr> },
    Closure(Box<ClosureDef>),
    Regex(String),
    EnumVariant { enum_name: String, variant: String, data: Option<Box<Expr>> },
    Thread(Box<Expr>),
    MakeChannel(Option<Type>),
    Send(Box<Expr>, Box<Expr>),
    Receive(Box<Expr>),
    MaybeReceive(Box<Expr>),
    MakeState(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Echo(Expr),
    Print(Expr),
    Assign { kind: AssignKind, declared_type: Option<Type>, name: String, value: Expr, ownership: Ownership },
    Reassign { name: String, value: Expr },
    Rebind { name: String, value: Expr },
    RebindField { object: String, field: String, value: Expr },
    ExprStmt(Expr),
    If { 
        condition: Expr, 
        then_block: Vec<Stmt>, 
        else_ifs: Vec<(Expr, Vec<Stmt>)>, 
        else_block: Option<Vec<Stmt>> 
    },
    Match { 
        expr: Expr, 
        cases: Vec<(Vec<Pattern>, Vec<Stmt>)>, 
        default: Option<Vec<Stmt>>,
        match_type: MatchType,
    },
    While { condition: Expr, body: Vec<Stmt> },
    WhileOnce { condition: Expr, body: Vec<Stmt> },
    Until { condition: Expr, body: Vec<Stmt> },
    UntilOnce { condition: Expr, body: Vec<Stmt> },
    Break,
    Next,
    FnDef(FnDef),
    Return(Option<Expr>),
    ModelDef(ModelDef),
    RoleDef(RoleDef),
    ExtendModel {
        model_name: String,
        role_name: Option<String>,
        methods: Vec<FnDef>,
    },
    EnumDef(EnumDef),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignKind {
    Var,
    Const,
    Lit,
    Static,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ownership {
    Borrowed,
    Owned,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Moved {
    True,
    False
}

#[derive(Debug, Clone, PartialEq)]
pub enum ControlFlow {
    None,
    Break,
    Next,
    Return(Value),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MatchType {
    One,
    Any,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDef {
    pub name: String,
    pub params: Vec<(String, Option<Type>, Ownership)>,
    pub body: Option<Vec<Stmt>>,
    pub return_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InterpolationPart {
    Literal(String),
    Expression(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModelDef {
    pub name: String,
    pub fields: Vec<(String, Type)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RoleDef {
    pub name: String,
    pub methods: Vec<FnDef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClosureDef {
    pub params: Vec<(String, Option<Type>)>,
    pub body: Vec<Stmt>,
    pub return_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClosureValue {
    pub def: Box<ClosureDef>,
    pub captured_env: HashMap<String, (AssignKind, Value, Ownership, Moved)>,
    pub live_vars: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MethodMutationType {
    None,
    DirectAssignment,  // this.field = value
    Rebind,            // rebind this.field = value
    Mixed,             // both types
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Literal(Expr),
    EnumVariant { enum_name: String, variant: String, binding: Option<String> },
    Wildcard,
}
