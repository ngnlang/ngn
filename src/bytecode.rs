#[repr(u8)]
#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
pub enum OpCode {
    Add,
    Subtract,
    Multiply,
    Divide,
    Call(usize),
    CallGlobal(usize),
    DefVar(usize, bool),
    DefGlobal(usize, bool),
    Equal,
    GetVar(usize),
    GetGlobal(usize),
    AssignVar(usize),
    AssignGlobal(usize),
    DeleteVar(usize),
    NativeCall(u16, u8), // (FunctionID, ArgCount)
    NotEqual,
    Print,
    Return,
    Halt,
    LoadConst(usize),
    BuildArray(usize),
    BuildTuple(usize),
    Jump(usize),
    JumpIfFalse(usize),
    LessThan,
    GreaterThan,
    Power,
    Modulo,
    Pop,
}
