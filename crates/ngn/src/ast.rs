use crate::{runtime::{ExportKind, ImportStmt}, types::{AssignKind, Ownership, Type}};

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
    CreateChannel(Option<Type>),
    Send(Box<Expr>, Box<Expr>),
    Receive(Box<Expr>),
    MaybeReceive(Box<Expr>),
    CountReceive(Box<Expr>, Box<Expr>),
    CreateState(Box<Expr>),
    CreateMap(Vec<(Box<Expr>, Box<Expr>)>, Type, Type),
    CreateSet(Vec<Box<Expr>>, Type),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Echo(Expr),
    Print(Expr),
    Assign { kind: AssignKind, declared_type: Option<Type>, name: String, value: Expr, ownership: Ownership },
    Reassign { name: String, value: Expr },
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
    For { 
        binding: String,
        index_binding: Option<String>,
        iterable: ForIterable,
        body: Vec<Stmt>,
    },
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
    Import(ImportStmt),
    Export(ExportKind),
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
    pub params: Vec<(String, Option<Type>, Ownership)>,
    pub body: Vec<Stmt>,
    pub return_type: Option<Type>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MethodMutationType {
    None,
    DirectAssignment,  // this.field = value
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Literal(Expr),
    EnumVariant { enum_name: String, variant: String, binding: Option<String> },
    Wildcard,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ForIterable {
    Collection(Expr),
    CountReceive(Expr, Expr),
    MaybeReceive(Expr),
}
