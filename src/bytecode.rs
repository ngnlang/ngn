#[repr(u8)]
#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
pub enum OpCode {
    // Arithmetic: (DestReg, SrcReg1, SrcReg2)
    Add(u16, u16, u16),
    Subtract(u16, u16, u16),
    Multiply(u16, u16, u16),
    Divide(u16, u16, u16),
    Power(u16, u16, u16),
    Modulo(u16, u16, u16),

    // Comparison: (DestReg, SrcReg1, SrcReg2)
    Equal(u16, u16, u16),
    NotEqual(u16, u16, u16),
    LessThan(u16, u16, u16),
    GreaterThan(u16, u16, u16),

    // Data Movement
    LoadConst(u16, usize),   // (DestReg, ConstantIndex)
    Move(u16, u16),          // (DestReg, SrcReg)
    
    // Variables
    GetVar(u16, usize),      // (DestReg, EnvIndex) - for closure support/non-locals? 
                             // Wait, our current VM uses (EnvIdx, VarIdx) for references.
                             // For locals, it's just a Move from a slot.
    DefVar(usize, bool),     // Keep for now, but usually registers pre-exist
    AssignVar(u16, u16),     // (DestReg, SrcReg)
    
    // Globals
    DefGlobal(usize, bool),
    GetGlobal(u16, usize),   // (DestReg, GlobalIndex)
    AssignGlobal(usize, u16), // (GlobalIndex, SrcReg)

    // Calls
    Call(u16, u16, u16, u8),      // (DestReg, FuncReg, ArgStartReg, ArgCount)
    CallGlobal(u16, usize, u16, u8), // (DestReg, GlobalFuncIndex, ArgStartReg, ArgCount)
    NativeCall(u16, u16, u16, u8),   // (DestReg, NativeIDReg, ArgStartReg, ArgCount)

    // Control Flow
    Jump(usize),
    JumpIfFalse(u16, usize), // (CondReg, Offset)
    JumpIfTrue(u16, usize),  // (CondReg, Offset)
    
    // Iteration
    IterStart(u16, u16),      // (IterReg, SrcReg)
    IterNext(u16, u16, usize), // (ValueReg, IterReg, JumpOffset)

    // Data Structures
    BuildArray(u16, u16, usize), // (DestReg, StartReg, Count)
    BuildTuple(u16, u16, usize), // (DestReg, StartReg, Count)
    
    // Enums
    CreateEnum(u16, usize, u16, u8), // (DestReg, NamesConstIdx, StartReg, Count)
    IsVariant(u16, u16, usize, usize), // (DestReg, SrcReg, EnumNameIdx, VariantNameIdx)
    GetVariantData(u16, u16), // (DestReg, SrcReg)

    // Builtins/Misc
    Print(u16),
    Echo(u16),
    Sleep(u16),
    Return(u16),            // (SrcReg)
    ReturnVoid,
    Halt,
    
    // To be removed/refactored
    Pop,
    Dup,
    Concat(u16, u16, usize), // (DestReg, StartReg, Count)
}
