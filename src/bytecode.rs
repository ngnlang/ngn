#[repr(u8)]
#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
pub enum OpCode {
    Add,
    Subtract,
    Multiply,
    Divide,
    Call(usize),
    DefVar(usize, bool),
    Equal,
    GetVar(usize),
    AssignVar(usize),
    DeleteVar(usize),
    NativeCall(u16, u8), // (FunctionID, ArgCount)
    NotEqual,
    Print,
    Return,
    Halt,
    LoadConst(usize),
}
