#[repr(u8)]
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
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
    LessThanEqual(u16, u16, u16),
    GreaterThanEqual(u16, u16, u16),

    // Unary: (DestReg, SrcReg)
    Negate(u16, u16),
    Not(u16, u16),

    // Data Movement
    LoadConst(u16, usize), // (DestReg, ConstantIndex)
    Move(u16, u16),        // (DestReg, SrcReg)
    MakeRegex(u16, usize), // (DestReg, PatternConstIdx)

    // Variables
    GetVar(u16, usize), // (DestReg, EnvIndex) - for closure support/non-locals?
    // Wait, our current VM uses (EnvIdx, VarIdx) for references.
    // For locals, it's just a Move from a slot.
    DefVar(usize, bool), // Keep for now, but usually registers pre-exist
    AssignVar(u16, u16), // (DestReg, SrcReg)

    // Globals
    DefGlobal(usize, bool),
    GetGlobal(u16, usize),    // (DestReg, GlobalIndex)
    AssignGlobal(usize, u16), // (GlobalIndex, SrcReg)

    // Calls
    Call(u16, u16, u16, u8), // (DestReg, FuncReg, ArgStartReg, ArgCount)
    CallGlobal(u16, usize, u16, u8), // (DestReg, GlobalFuncIndex, ArgStartReg, ArgCount)
    CallSelf(u16, u16, u8),  // (DestReg, ArgStartReg, ArgCount) - optimized self-recursion
    NativeCall(u16, u16, u16, u8), // (DestReg, NativeIDReg, ArgStartReg, ArgCount)

    // Control Flow
    Jump(usize),
    JumpIfFalse(u16, usize), // (CondReg, Offset)
    JumpIfTrue(u16, usize),  // (CondReg, Offset)

    // Iteration
    IterStart(u16, u16),       // (IterReg, SrcReg)
    IterNext(u16, u16, usize), // (ValueReg, IterReg, JumpOffset)

    // Data Structures
    BuildArray(u16, u16, usize),      // (DestReg, StartReg, Count)
    BuildTuple(u16, u16, usize),      // (DestReg, StartReg, Count)
    CreateMap(u16),                   // (DestReg) - creates empty HashMap
    CreateSet(u16),                   // (DestReg) - creates empty HashSet
    CreateRange(u16, u16, u16, bool), // (DestReg, StartReg, EndReg, Inclusive)
    RangeToArray(u16, u16),           // (DestReg, RangeReg)

    // Enums
    CreateEnum(u16, usize, u16, u8), // (DestReg, NamesConstIdx, StartReg, Count)
    IsVariant(u16, u16, usize, usize), // (DestReg, SrcReg, EnumNameIdx, VariantNameIdx)
    GetVariantData(u16, u16),        // (DestReg, SrcReg)

    // Closures
    MakeClosure(u16, usize, u16, u8), // (DestReg, FunctionConstIdx, UpvalueStartReg, UpvalueCount)
    GetUpvalue(u16, u16),             // (DestReg, UpvalueIndex)

    // Concurrency
    Spawn(u16, u16),      // (DestChannelReg, ClosureReg)
    SpawnCpu(u16, u16),   // (DestChannelReg, TaskReg)
    SpawnBlock(u16, u16), // (DestChannelReg, TaskReg)
    Fetch(u16, u16, u16), // (DestChannelReg, UrlReg, OptionsReg) - OptionsReg=u16::MAX means no options
    Yield,
    Send(u16, u16),              // (ChannelReg, ValueReg)
    Receive(u16, u16),           // (DestReg, ChannelReg)
    CreateChannel(u16, u16),     // (DestReg, Capacity)
    CreateState(u16, u16),       // (DestReg, InitialValueReg)
    CreateBytes(u16, u16),       // (DestReg, ArgReg) - ArgReg=u16::MAX means no args
    StateRead(u16, u16),         // (DestReg, StateReg)
    StateWrite(u16, u16),        // (StateReg, ValueReg)
    StateUpdate(u16, u16),       // (StateReg, ClosureReg)
    ReceiveCount(u16, u16, u16), // (DestReg, ChannelReg, CountReg)
    ReceiveMaybe(u16, u16),      // (DestReg, ChannelReg)

    // Spawn API for parallel task execution
    SpawnAll(u16, u16, u16), // (DestReg, TasksArrayReg, OptionsReg) - run all, return all results
    SpawnTry(u16, u16, u16), // (DestReg, TasksArrayReg, OptionsReg) - stop on first error
    SpawnRace(u16, u16),     // (DestReg, TasksArrayReg) - return first success

    // Builtins/Misc
    Print(u16),
    PrintNewline,
    Echo(u16),
    Sleep(u16),
    Panic(u16, usize), // (MessageReg, LocationConstIdx)
    Return(u16),       // (SrcReg)
    ReturnVoid,
    Halt,

    // To be removed/refactored
    Pop,
    Dup,
    Concat(u16, u16, usize), // (DestReg, StartReg, Count)
    GetIndex(u16, u16, u16), // (DestReg, ObjReg, IndexReg)
    CloseChannel(u16),       // (ChannelReg)

    // Method Calls on built-in types (arrays, strings, tuples, etc.)
    // (DestReg, ObjReg, MethodNameConstIdx, ArgStartReg, ArgCount)
    CallMethod(u16, u16, usize, u16, u8),

    // Mutating method calls - writes modified object back to ObjReg
    // (DestReg, ObjReg, MethodNameConstIdx, ArgStartReg, ArgCount)
    CallMethodMut(u16, u16, usize, u16, u8),

    // Objects/Models
    CreateObject(u16, usize, usize, u16, u8), // (DestReg, ModelNameConstIdx, FieldNamesConstIdx, ArgStartReg, FieldCount)
    GetField(u16, u16, usize),                // (DestReg, ObjReg, FieldNameConstIdx)
    GetFieldMaybe(u16, u16, usize), // (DestReg, ObjReg, FieldNameConstIdx) - returns Maybe for anon, direct for Models
    GetFieldSafe(u16, u16, usize),  // (DestReg, ObjReg, FieldNameConstIdx) - always returns Maybe
    WrapMaybe(u16, u16),            // (DestReg, SrcReg) - wrap value in Maybe if needed
    SetField(u16, usize, u16),      // (ObjReg, FieldNameConstIdx, SrcReg)
    DefMethod(usize, usize, u16),   // (TargetTypeIdx, MethodNameIdx, ClosureReg)

    // HTTP Server (for export default with fetch method)
    ServeHttp(usize), // (GlobalIndex) - starts HTTP server with handler from global

    // JSON
    JsonParse(u16, u16),     // (DestReg, StringReg) - parses JSON string to value
    JsonStringify(u16, u16), // (DestReg, ValueReg) - converts value to JSON string

    // Maybe/Result operations (for conditional binding)
    CheckMaybeValue(u16, u16), // (DestReg, MaybeReg) - sets DestReg to true if Maybe::Value or Result::Ok
    UnwrapMaybe(u16, u16), // (DestReg, MaybeReg) - extracts inner value from Maybe::Value or Result::Ok
    UnwrapResultError(u16, u16), // (DestReg, ResultReg) - extracts error value from Result::Error

    // Null Coalescing (??)
    // If MaybeReg is Maybe::Value, unwrap to DestReg; else set DestReg = FallbackReg
    NullCoalesce(u16, u16, u16), // (DestReg, MaybeReg, FallbackReg)

    // Environment Variables
    EnvGet(u16, u16), // (DestReg, KeyReg) - returns Maybe<string>
    EnvHas(u16, u16), // (DestReg, KeyReg) - returns bool

    // Destructuring
    ObjectRest(u16, u16, usize), // (DestReg, ObjReg, ExcludedFieldsConstIdx) - creates object with remaining fields

    // Time operations
    TimeNow(u16),    // (DestReg) - returns DateTime object for current local time
    TimeUtc(u16),    // (DestReg) - returns DateTime object for current UTC time
    TimeUnix(u16),   // (DestReg) - returns i64 Unix timestamp (seconds)
    TimeUnixMs(u16), // (DestReg) - returns i64 Unix timestamp (milliseconds)
    TimeParse(u16, u16, u16), // (DestReg, StringReg, FormatReg) - parses string to DateTime, returns Result
}
