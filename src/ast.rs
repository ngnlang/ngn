use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Number,
    String, // owned
    Str, // borrowed
    Bool,
    Array(Box<Type>),
    Object(HashMap<String, Type>),
    Function,
    Void,
    Model(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Array(Vec<Value>),
    Function(FnDef),
    Object(String, HashMap<String, Value>),
    Void,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64),
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
    Const(String),
    Lit(String),
    Static(String),
    Call { name: String, args: Vec<Expr> },
    InterpolatedString(Vec<InterpolationPart>),
    ModelInstance { name: String, fields: Vec<(String, Expr)> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Echo(Expr),
    Print(Expr),
    Assign { kind: AssignKind, declared_type: Option<Type>, name: String, value: Expr, ownership: Ownership },
    Reassign { name: String, value: Expr },
    Rebind { name: String, value: Expr },
    ExprStmt(Expr),
    If { 
        condition: Expr, 
        then_block: Vec<Stmt>, 
        else_ifs: Vec<(Expr, Vec<Stmt>)>, 
        else_block: Option<Vec<Stmt>> 
    },
    Match { 
        expr: Expr, 
        cases: Vec<(Vec<Expr>, Vec<Stmt>)>, 
        default: Option<Vec<Stmt>>,
        match_type: MatchType,
    },
    While { condition: Expr, body: Vec<Stmt> },
    WhileOnce { condition: Expr, body: Vec<Stmt> },
    Until { condition: Expr, body: Vec<Stmt> },
    UntilOnce { condition: Expr, body: Vec<Stmt> },
    Break,
    Next,
    FnDef {
        name: String,
        params: Vec<(String, Option<Type>, Ownership)>,
        return_type: Option<Type>,
        body: Vec<Stmt>,
    },
    Return(Option<Expr>),
    ModelDef(ModelDef),
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
    pub params: Vec<(String, Option<Type>, Ownership)>,
    pub body: Vec<Stmt>,
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
