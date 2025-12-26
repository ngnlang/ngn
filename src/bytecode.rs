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
    GetVar(usize),
    AssignVar(usize),
    DeleteVar(usize),
    Print,
    Return,
    Halt,
    LoadConst(usize),
}
