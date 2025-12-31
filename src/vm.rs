use std::io::Write;
use std::sync::{Arc, Mutex};
use std::collections::VecDeque;
use crate::value::{Value, Number, Channel};
use crate::bytecode::OpCode;

#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Variable {
    pub value: Value,
    pub is_mutable: bool,
    pub reference_count: usize,
}

#[derive(Debug, Clone)]
pub struct CallFrame {
    instructions: Arc<Vec<OpCode>>,
    constants: Arc<Vec<Value>>,
    ip: usize,
    fp: usize,
    dest_reg: Option<u16>,
    closure: Option<Box<crate::value::Closure>>,
    update_state: Option<Arc<Mutex<Value>>>,
}

#[derive(Debug, Clone)]
pub struct Fiber {
    pub stack: Vec<Option<Variable>>,
    pub frames: Vec<CallFrame>,
    pub ip: usize,
    pub fp: usize,
    pub instructions: Arc<Vec<OpCode>>,
    pub constants: Arc<Vec<Value>>,
    pub current_closure: Option<Box<crate::value::Closure>>,
    pub status: FiberStatus,
    pub completion_channel: Option<Channel>,
}

#[derive(Debug, Clone)]
pub enum FiberStatus {
    Running,
    Suspended,
    Finished,
    Spawning(Box<Fiber>),
}

impl Fiber {
    pub fn new(closure: Box<crate::value::Closure>) -> Self {
        let func = closure.function.clone();
        let mut fiber = Self {
            stack: Vec::with_capacity(1024),
            frames: Vec::with_capacity(64),
            ip: 0,
            fp: 0,
            instructions: func.instructions.clone(),
            constants: func.constants.clone(),
            current_closure: Some(closure),
            status: FiberStatus::Running,
            completion_channel: None,
        };
        // Use invoke_function logic? Or simplified setup for new fiber.
        // A new fiber starts by calling the closure.
        // Stack resize handled by perform_call logic?
        // We simulate a call.
        fiber.stack.resize(func.reg_count, None);
        fiber
    }
    
    // Main script fiber (no closure)
    pub fn new_main(instructions: Vec<OpCode>, constants: Vec<Value>) -> Self {
        Self {
            stack: Vec::with_capacity(1024),
            frames: Vec::new(),
            ip: 0,
            fp: 0,
            instructions: Arc::new(instructions),
            constants: Arc::new(constants),
            current_closure: None,
            status: FiberStatus::Running,
            completion_channel: None,
        }
    }
}

pub struct VM {
    globals: Vec<Option<Variable>>,
    fibers: VecDeque<Fiber>,
    current_fiber: Option<Fiber>,
}
 
impl Fiber {
    // Updated for Sliding Window: Uses fp + idx
    fn get_reg_at(&self, idx: u16) -> Value {
        let abs = self.fp + idx as usize;
        // If reg is OOB or stack is small, uninitialized => Void
        if abs >= self.stack.len() {
             return Value::Void;
        }
        self.stack[abs]
            .as_ref()
            .map(|v| v.value.clone())
            .unwrap_or(Value::Void)
    }

    fn get_reg_ref(&self, idx: u16) -> &Value {
        self.stack[self.fp + idx as usize]
            .as_ref()
            .map(|v| &v.value)
            .expect("Runtime Error: Accessing uninitialized register")
    }

    fn set_reg_at(&mut self, idx: u16, val: Value) {
        let abs_idx = self.fp + idx as usize;
        if abs_idx >= self.stack.len() {
             // In sliding window, we usually ensure stack size on call, but resizing here is safe fallback
            self.stack.resize(abs_idx + 1, None);
        }
        self.stack[abs_idx] = Some(Variable {
            value: val,
            is_mutable: true,
            reference_count: 0,
        });
    }

    fn invoke_function(&mut self, func: &crate::value::Function, closure: Option<Box<crate::value::Closure>>, dest_reg: Option<u16>, arg_start: u16, arg_count: u8) -> bool {
                 let new_fp = self.stack.len();
                
                // Ensure room for registers
                self.stack.resize(new_fp + func.reg_count, None);

                for i in 0..arg_count as usize {
                    let arg = self.get_reg_at(arg_start + i as u16);
                    let _is_owned = if i < func.param_ownership.len() { func.param_ownership[i] } else { false };
                    
                    self.stack[new_fp + i] = Some(Variable {
                        value: arg,
                        is_mutable: true,
                        reference_count: 0
                    });
                }

                // Push CallFrame
                self.frames.push(CallFrame {
                    instructions: Arc::clone(&self.instructions),
                    constants: Arc::clone(&self.constants),
                    ip: self.ip,
                    fp: self.fp,
                    dest_reg,
                    closure: self.current_closure.clone(),
                    update_state: None,
                });

                // Switch to new function
                self.instructions = Arc::clone(&func.instructions);
                self.constants = Arc::clone(&func.constants);
                self.ip = 0;
                self.fp = new_fp;
                self.current_closure = closure;
                true
    }

    fn perform_call(&mut self, callable_value: Value, dest_reg: Option<u16>, arg_start: u16, arg_count: u8) -> bool {
        match callable_value {
            Value::Function(func) => {
                self.invoke_function(&func, None, dest_reg, arg_start, arg_count)
            }
            Value::Closure(closure) => {
                 let func = closure.function.clone();
                 self.invoke_function(&func, Some(closure), dest_reg, arg_start, arg_count)
            }
            Value::NativeFunction(id) => {
                let arg = if arg_count > 0 { self.get_reg_at(arg_start) } else { Value::Void };
                let resolved = arg;

                let result = match id {
                    crate::toolbox::NATIVE_ASSERT => {
                        match resolved {
                            Value::Bool(true) => {
                                println!("✅ Assertion passed");
                                Value::Void
                            }
                            Value::Bool(false) => panic!("❌ Assertion failed! Expected true, got false."),
                            other => panic!("❌ Assertion failed! Expected Boolean, got {}", other),
                        }
                    }
                    crate::toolbox::NATIVE_ABS => {
                        match resolved {
                             Value::Numeric(crate::value::Number::I64(n)) => Value::Numeric(crate::value::Number::I64(n.abs())),
                             Value::Numeric(crate::value::Number::F64(n)) => Value::Numeric(crate::value::Number::F64(n.abs())),
                             other => panic!("❌ ABS failed! Expected Number, got {}", other),
                        }
                    }
                     // Print, Echo, Sleep are OpCodes now, so NativeCall might not need them unless generalized?
                     // Currently checking ID.
                     // 0 = print? No, builtins have explicit opcodes.
                     // But NativeCall OpCode uses ID reg.
                    _ => panic!("Runtime Error: Unknown Native ID {}", id),
                };
                
                if let Some(dest) = dest_reg {
                    self.set_reg_at(dest, result);
                }
                false
            }
            _ => panic!("Runtime Error: Not a callable: {:?}", callable_value),
        }
    }

    pub fn run_step(&mut self, globals: &mut Vec<Option<Variable>>) -> FiberStatus {
        if self.ip >= self.instructions.len() {
            if let Some(frame) = self.frames.pop() {
                self.stack.truncate(self.fp);
                self.instructions = frame.instructions;
                self.constants = frame.constants;
                self.ip = frame.ip;
                self.fp = frame.fp;
                // No return value handling here (implicit return void)
                if let Some(dest) = frame.dest_reg {
                    self.set_reg_at(dest, Value::Void);
                }
                return FiberStatus::Running;
            } else {
                self.status = FiberStatus::Finished;
                return FiberStatus::Finished;
            }
        }

        let instruction = self.instructions[self.ip].clone();
        self.ip += 1;

        match instruction {
            OpCode::LoadConst(dest, idx) => {
                let val = self.constants[idx].clone();
                self.set_reg_at(dest, val);
            }
            OpCode::Move(dest, src) => {
                let val = self.get_reg_at(src);
                self.set_reg_at(dest, val);
            }
            OpCode::Add(dest, src1, src2) => {
                let res = {
                    let v1 = self.get_reg_ref(src1);
                    let v2 = self.get_reg_ref(src2);
                    v1.add(v2)
                };
                match res {
                    Ok(val) => self.set_reg_at(dest, val),
                    Err(e) => panic!("Runtime Error: {}", e),
                }
            }
            OpCode::Subtract(dest, src1, src2) => {
                let res = {
                    let v1 = self.get_reg_ref(src1);
                    let v2 = self.get_reg_ref(src2);
                    v1.subtract(v2)
                };
                match res {
                    Ok(val) => self.set_reg_at(dest, val),
                    Err(e) => panic!("Runtime Error: {}", e),
                }
            }
            OpCode::Multiply(dest, src1, src2) => {
                let res = {
                    let v1 = self.get_reg_ref(src1);
                    let v2 = self.get_reg_ref(src2);
                    v1.multiply(v2)
                };
                match res {
                    Ok(val) => self.set_reg_at(dest, val),
                    Err(e) => panic!("Runtime Error: {}", e),
                }
            }
            OpCode::Divide(dest, src1, src2) => {
                let res = {
                    let v1 = self.get_reg_ref(src1);
                    let v2 = self.get_reg_ref(src2);
                    v1.divide(v2)
                };
                match res {
                    Ok(val) => self.set_reg_at(dest, val),
                    Err(e) => panic!("Runtime Error: {}", e),
                }
            }
            OpCode::Power(dest, src1, src2) => {
                let res = {
                    let v1 = self.get_reg_ref(src1);
                    let v2 = self.get_reg_ref(src2);
                    v1.clone().power(v2.clone())
                };
                match res {
                    Ok(val) => self.set_reg_at(dest, val),
                    Err(e) => panic!("Runtime Error: {}", e),
                }
            }
            OpCode::Modulo(dest, src1, src2) => {
                let res = {
                    let v1 = self.get_reg_ref(src1);
                    let v2 = self.get_reg_ref(src2);
                    v1.clone().remainder(v2.clone())
                };
                match res {
                    Ok(val) => self.set_reg_at(dest, val),
                    Err(e) => panic!("Runtime Error: {}", e),
                }
            }
            OpCode::Equal(dest, src1, src2) => {
                let v1 = self.get_reg_ref(src1);
                let v2 = self.get_reg_ref(src2);
                self.set_reg_at(dest, Value::Bool(v1.is_equal(v2)));
            }
            OpCode::NotEqual(dest, src1, src2) => {
                let v1 = self.get_reg_ref(src1);
                let v2 = self.get_reg_ref(src2);
                self.set_reg_at(dest, Value::Bool(!v1.is_equal(v2)));
            }
            OpCode::LessThan(dest, src1, src2) => {
                let v1 = self.get_reg_ref(src1);
                let v2 = self.get_reg_ref(src2);
                match (v1, v2) {
                    (Value::Numeric(n1), Value::Numeric(n2)) => self.set_reg_at(dest, Value::Bool(n1.less_than(*n2))),
                    _ => panic!("Runtime Error: Invalid operands for <"),
                }
            }
            OpCode::GreaterThan(dest, src1, src2) => {
                let v1 = self.get_reg_ref(src1);
                let v2 = self.get_reg_ref(src2);
                match (v1, v2) {
                    (Value::Numeric(n1), Value::Numeric(n2)) => self.set_reg_at(dest, Value::Bool(n1.greater_than(*n2))),
                    _ => panic!("Runtime Error: Invalid operands for >"),
                }
            }
            OpCode::Call(dest_reg, callable_reg, arg_start_reg, arg_count) => {
                let callable = self.get_reg_at(callable_reg);
                self.perform_call(callable, Some(dest_reg), arg_start_reg, arg_count);
            }
            OpCode::CallGlobal(dest, idx, arg_start, arg_count) => {
                // Get global inline
                let callable = globals[idx as usize].as_ref().map(|v| v.value.clone()).unwrap_or(Value::Void);
                self.perform_call(callable, Some(dest), arg_start, arg_count);
            }
            OpCode::NativeCall(dest, id_reg, arg_start, arg_count) => {
                let val = self.get_reg_at(id_reg);
                self.perform_call(val, Some(dest), arg_start, arg_count);
            }
            OpCode::Print(src) => {
                let val = self.get_reg_at(src);
                println!("{}", val);
            }
            OpCode::Echo(src) => {
                let val = self.get_reg_at(src);
                print!("{}", val);
                let _ = std::io::stdout().flush();
            }
            OpCode::Sleep(src) => {
                let val = self.get_reg_at(src);
                if let Value::Numeric(crate::value::Number::I64(ms)) = val {
                    std::thread::sleep(std::time::Duration::from_millis(ms as u64));
                }
            }
            OpCode::Jump(target) => {
                self.ip = target;
            }
            OpCode::JumpIfFalse(cond, target) => {
                let val = self.get_reg_at(cond);
                if matches!(val, Value::Bool(false)) {
                    self.ip = target;
                }
            }
            OpCode::JumpIfTrue(cond, target) => {
                let val = self.get_reg_at(cond);
                if matches!(val, Value::Bool(true)) {
                    self.ip = target;
                }
            }
            OpCode::IterStart(iter_reg, src_reg) => {
                let src = self.get_reg_at(src_reg);
                self.set_reg_at(iter_reg, src);
                self.set_reg_at(iter_reg + 1, Value::Numeric(crate::value::Number::I64(0)));
            }
            OpCode::IterNext(dest, iter_reg, jump_offset) => {
                let collection = self.get_reg_at(iter_reg);
                let index_val = self.get_reg_at(iter_reg + 1);
                
                if let Value::Numeric(crate::value::Number::I64(idx)) = index_val {
                    match collection {
                        Value::Array(items) => {
                            if (idx as usize) < items.len() {
                                self.set_reg_at(dest, items[idx as usize].clone());
                                self.set_reg_at(iter_reg + 1, Value::Numeric(crate::value::Number::I64(idx + 1)));
                            } else {
                                self.ip = jump_offset;
                            }
                        }
                        Value::Tuple(items) => {
                            if (idx as usize) < items.len() {
                                self.set_reg_at(dest, items[idx as usize].clone());
                                self.set_reg_at(iter_reg + 1, Value::Numeric(crate::value::Number::I64(idx + 1)));
                            } else {
                                self.ip = jump_offset;
                            }
                        }
                        _ => panic!("Runtime Error: IterNext on non-collection: {:?}", collection),
                    }
                } else {
                    panic!("Runtime Error: IterNext on invalid index: {:?}", index_val);
                }
            }
            OpCode::Return(src) => {
                let result = self.get_reg_at(src);
                if let Some(frame) = self.frames.pop() {
                    self.stack.truncate(self.fp);
                    self.instructions = frame.instructions;
                    self.constants = frame.constants;
                    self.ip = frame.ip;
                    self.fp = frame.fp;
                    self.current_closure = frame.closure;
                    if let Some(dest) = frame.dest_reg {
                        self.set_reg_at(dest, result.clone());
                    }
                        
                    if let Some(state) = frame.update_state {
                        *state.lock().unwrap() = result;
                    }
                } else {
                    if let Some(chan) = &self.completion_channel {
                        chan.buffer.lock().unwrap().push_back(result);
                    }
                    self.status = FiberStatus::Finished;
                    return FiberStatus::Finished;
                }
            }
            OpCode::ReturnVoid => {
                if let Some(frame) = self.frames.pop() {
                    self.stack.truncate(self.fp);
                    self.instructions = frame.instructions;
                    self.constants = frame.constants;
                    self.ip = frame.ip;
                    self.fp = frame.fp;
                    self.current_closure = frame.closure;
                    if let Some(dest) = frame.dest_reg {
                        self.set_reg_at(dest, Value::Void);
                    }

                    if let Some(state) = frame.update_state {
                        *state.lock().unwrap() = Value::Void;
                    }
                } else {
                    if let Some(chan) = &self.completion_channel {
                        chan.buffer.lock().unwrap().push_back(Value::Void);
                    }
                    self.status = FiberStatus::Finished;
                    return FiberStatus::Finished;
                }
            }
            OpCode::Halt => {
                self.status = FiberStatus::Finished;
                return FiberStatus::Finished;
            }
            OpCode::BuildArray(dest, start, count) => {
                let mut elements = Vec::with_capacity(count as usize);
                for i in 0..count {
                    elements.push(self.get_reg_at(start + i as u16));
                }
                self.set_reg_at(dest, Value::Array(elements));
            }
            OpCode::BuildTuple(dest, start, count) => {
                let mut elements = Vec::with_capacity(count as usize);
                for i in 0..count {
                    elements.push(self.get_reg_at(start + i as u16));
                }
                self.set_reg_at(dest, Value::Tuple(elements));
            }
            OpCode::CreateEnum(dest, names_idx, start, count) => {
                let names_tuple = self.constants[names_idx].clone();
                let (enum_name, variant_name) = if let Value::Tuple(v) = names_tuple {
                    (v[0].to_string(), v[1].to_string())
                } else {
                    ("?".to_string(), "?".to_string())
                };

                let mut elements = Vec::new();
                for i in 0..count {
                    elements.push(self.get_reg_at(start + i as u16));
                }
                
                let val = Value::Enum { 
                    enum_name,
                    variant_name,
                    data: if elements.is_empty() { 
                        None 
                    } else if elements.len() == 1 {
                        Some(Box::new(elements[0].clone()))
                    } else {
                        Some(Box::new(Value::Tuple(elements)))
                    } 
                };
                self.set_reg_at(dest, val);
            }
            OpCode::IsVariant(dest, src, enum_idx, var_idx) => {
                let val = self.get_reg_at(src);
                let enum_name = self.constants[enum_idx].to_string(); 
                let variant_name = self.constants[var_idx].to_string();
                
                if let Value::Enum { enum_name: e, variant_name: v, .. } = val {
                    let eq = (enum_name.is_empty() || e == enum_name) && v == variant_name;
                    self.set_reg_at(dest, Value::Bool(eq));
                } else {
                    self.set_reg_at(dest, Value::Bool(false));
                }
            }
            OpCode::GetVariantData(dest, src) => {
                let val = self.get_reg_at(src);
                if let Value::Enum { data, .. } = val {
                    let data_val = match data {
                        Some(boxed) => *boxed,
                        None => Value::Tuple(Vec::new()),
                    };
                    self.set_reg_at(dest, data_val);
                } else {
                    panic!("Runtime Error: GetVariantData on non-enum");
                }
            }
            OpCode::MakeClosure(dest, func_idx, up_start, up_count) => {
                if let Value::Function(func) = &self.constants[func_idx] {
                    let mut upvalues = Vec::new();
                    for i in 0..up_count {
                        upvalues.push(self.get_reg_at(up_start + i as u16));
                    }
                    
                    let closure = crate::value::Closure {
                        function: func.clone(),
                        upvalues,
                    };
                    self.set_reg_at(dest, Value::Closure(Box::new(closure)));
                } else {
                    panic!("Runtime Error: MakeClosure on non-function constant");
                }
            }
            OpCode::GetUpvalue(dest, index) => {
                if let Some(closure) = &self.current_closure {
                    if (index as usize) < closure.upvalues.len() {
                        self.set_reg_at(dest, closure.upvalues[index as usize].clone());
                    } else {
                        panic!("Runtime Error: Upvalue index out of bounds");
                    }
                } else {
                    panic!("Runtime Error: GetUpvalue called outside of closure");
                }
            }
            OpCode::GetGlobal(dest, idx) => {
                let val = globals[idx as usize].as_ref().map(|v| v.value.clone()).unwrap_or(Value::Void);
                self.set_reg_at(dest, val);
            }
            OpCode::DefGlobal(idx, _mut) => {
                let val = self.get_reg_at(idx as u16);
                if idx as usize >= globals.len() {
                    globals.resize(idx as usize + 1, None);
                }
                globals[idx as usize] = Some(Variable { value: val, is_mutable: true, reference_count: 0 });
            }
            OpCode::AssignGlobal(idx, src) => {
                let val = self.get_reg_at(src);
                if idx as usize >= globals.len() {
                    globals.resize(idx as usize + 1, None);
                }
                globals[idx as usize] = Some(Variable { value: val, is_mutable: true, reference_count: 0 });
            }
            OpCode::AssignVar(idx, src) => {
                let val = self.get_reg_at(src);
                self.set_reg_at(idx, val);
            }
            OpCode::DefVar(_idx, _mut) => {}, // No-op
            OpCode::GetVar(dest, idx) => {
                let val = self.get_reg_at(idx as u16);
                self.set_reg_at(dest, val);
            }
            OpCode::Concat(dest, start, count) => {
                let mut res = String::new();
                for i in 0..count {
                    let val = self.get_reg_at(start + i as u16);
                    res.push_str(&val.to_string());
                }
                self.set_reg_at(dest, Value::String(res));
            }
            OpCode::GetIndex(dest, obj_reg, index_reg) => {
                let obj = self.get_reg_at(obj_reg);
                let index = self.get_reg_at(index_reg);
                
                match (obj.clone(), index.clone()) {
                    (Value::Array(arr), Value::Numeric(Number::I64(idx))) => {
                        if idx < 0 || idx as usize >= arr.len() {
                            panic!("Runtime Error: Array index out of bounds: {} (len: {})", idx, arr.len());
                        }
                        self.set_reg_at(dest, arr[idx as usize].clone());
                    }
                    (Value::String(s), Value::Numeric(Number::I64(idx))) => {
                        if idx < 0 || idx as usize >= s.len() {
                            panic!("Runtime Error: String index out of bounds: {} (len: {})", idx, s.len());
                        }
                        let char = s.chars().nth(idx as usize).unwrap();
                        self.set_reg_at(dest, Value::String(char.to_string()));
                    }
                    _ => panic!("Runtime Error: Invalid indexing operation on {:?} with index {:?}", obj, index),
                }
            }
            OpCode::Spawn(dest, closure_reg) => {
                let val = self.get_reg_at(closure_reg);
                if let Value::Closure(closure) = val {
                    let mut new_fiber = Fiber::new(closure);
                    
                    // Create completion channel
                    let chan = Channel {
                        name: "thread_result".to_string(),
                        buffer: Arc::new(Mutex::new(VecDeque::new())),
                        capacity: 1,
                    };
                    new_fiber.completion_channel = Some(chan.clone());
                    
                    self.set_reg_at(dest, Value::Channel(chan));
                    return FiberStatus::Spawning(Box::new(new_fiber));
                } else {
                    panic!("Runtime Error: Spawn expects a closure");
                }
            }
            OpCode::Yield => {
                return FiberStatus::Suspended;
            }
            OpCode::Send(chan_reg, val_reg) => {
                let chan_val = self.get_reg_at(chan_reg);
                let val = self.get_reg_at(val_reg);
                
                if let Value::Channel(chan) = chan_val {
                    let mut buffer = chan.buffer.lock().unwrap();
                    if buffer.len() < chan.capacity {
                        buffer.push_back(val);
                        // Success
                    } else {
                        // Channel full, yield and retry instruction
                        self.ip -= 1; 
                        return FiberStatus::Suspended;
                    }
                } else {
                    panic!("Runtime Error: Send on non-channel");
                }
            }
            OpCode::Receive(dest, chan_reg) => {
                let chan_val = self.get_reg_at(chan_reg);
                if let Value::Channel(chan) = chan_val {
                    let mut buffer = chan.buffer.lock().unwrap();
                    if let Some(val) = buffer.pop_front() {
                        self.set_reg_at(dest, val);
                    } else {
                        // Channel empty, yield and retry
                        self.ip -= 1;
                        return FiberStatus::Suspended;
                    }
                } else {
                    panic!("Runtime Error: Receive on non-channel");
                }
            }
            OpCode::CreateChannel(dest, capacity) => {
                let chan = crate::value::Channel {
                    name: "chan".to_string(),
                    buffer: Arc::new(Mutex::new(VecDeque::new())),
                    capacity: capacity as usize,
                };
                self.set_reg_at(dest, Value::Channel(chan));
            }
            OpCode::CreateState(dest, initial_reg) => {
                let initial = self.get_reg_at(initial_reg);
                let state = Value::State(Arc::new(Mutex::new(initial)));
                self.set_reg_at(dest, state);
            }
            OpCode::StateRead(dest, state_reg) => {
                let state_val = self.get_reg_at(state_reg);
                if let Value::State(state) = state_val {
                    let val = state.lock().unwrap().clone();
                    self.set_reg_at(dest, val);
                } else {
                    panic!("Runtime Error: .read() on non-state");
                }
            }
            OpCode::StateWrite(state_reg, val_reg) => {
                let state_val = self.get_reg_at(state_reg);
                let val = self.get_reg_at(val_reg);
                if let Value::State(state) = state_val {
                    *state.lock().unwrap() = val;
                } else {
                    panic!("Runtime Error: .write() on non-state");
                }
            }
            OpCode::StateUpdate(state_reg, closure_reg) => {
                let state_val = self.get_reg_at(state_reg);
                let closure_val = self.get_reg_at(closure_reg);
                if let (Value::State(state), Value::Closure(closure)) = (state_val, closure_val) {
                    let current_val = state.lock().unwrap().clone();
                    let arg_start = self.stack.len() as u16;
                    self.stack.push(Some(Variable { value: current_val, is_mutable: false, reference_count: 0 }));
                    
                    let func = closure.function.clone();
                    self.invoke_function(&func, Some(closure), None, arg_start, 1);
                    // Mark the new frame for state update
                    if let Some(frame) = self.frames.last_mut() {
                        frame.update_state = Some(state.clone());
                    }
                } else {
                    panic!("Runtime Error: .update() requires state and closure");
                }
            }
            OpCode::ReceiveCount(dest, chan_reg, count_reg) => {
                let chan_val = self.get_reg_at(chan_reg);
                let count_val = self.get_reg_at(count_reg);
                if let (Value::Channel(chan), Value::Numeric(crate::value::Number::I64(count))) = (chan_val, count_val) {
                    let mut buffer = chan.buffer.lock().unwrap();
                    if buffer.len() >= count as usize {
                        let mut res = Vec::new();
                        for _ in 0..count {
                            res.push(buffer.pop_front().unwrap());
                        }
                        self.set_reg_at(dest, Value::Array(res));
                    } else {
                        self.ip -= 1;
                        return FiberStatus::Suspended;
                    }
                } else {
                    panic!("Runtime Error: ReceiveCount expects channel and numeric count");
                }
            }
                OpCode::ReceiveMaybe(dest, chan_reg) => {
                    let chan_val = self.get_reg_at(chan_reg);
                    if let Value::Channel(chan) = chan_val {
                        let mut buffer = chan.buffer.lock().unwrap();
                        if let Some(val) = buffer.pop_front() {
                            self.set_reg_at(dest, Value::Enum {
                                enum_name: "Maybe".to_string(),
                                variant_name: "Value".to_string(),
                                data: Some(Box::new(val)),
                            });
                        } else {
                            self.set_reg_at(dest, Value::Enum {
                                enum_name: "Maybe".to_string(),
                                variant_name: "Null".to_string(),
                                data: None,
                            });
                        }
                    } else {
                        panic!("Runtime Error: ReceiveMaybe on non-channel");
                    }
                }
            _ => {} // Pop/Dup ignored
        }
        
        FiberStatus::Running
    }
}

impl VM {
    pub fn new(instructions: Vec<OpCode>, constants: Vec<Value>, global_slots: usize) -> Self {
        let mut globals = Vec::with_capacity(global_slots);
        globals.resize(global_slots, None);
        
        let main_fiber = Fiber::new_main(instructions, constants);

        Self {
            globals,
            fibers: VecDeque::new(),
            current_fiber: Some(main_fiber),
        }
    }
    pub fn init_globals(&mut self, size: usize) {
        if size > self.globals.len() {
            self.globals.resize(size, None);
        }
    }
    pub fn define_global(&mut self, idx: usize, val: Value, is_mutable: bool) {
        if idx >= self.globals.len() {
            self.globals.resize(idx + 1, None);
        }
        self.globals[idx] = Some(Variable { value: val, is_mutable, reference_count: 0 });
    }

    pub fn run(&mut self) {
        loop {
            if let Some(mut fiber) = self.current_fiber.take() {
                let status = fiber.run_step(&mut self.globals);
                match status {
                    FiberStatus::Running => {
                        self.current_fiber = Some(fiber);
                    }
                    FiberStatus::Finished => {
                        if let Some(next) = self.fibers.pop_front() {
                            self.current_fiber = Some(next);
                        } else {
                            break;
                        }
                    }
                    FiberStatus::Suspended => {
                         self.fibers.push_back(fiber);
                         if let Some(next) = self.fibers.pop_front() {
                             self.current_fiber = Some(next);
                         } else {
                             break;
                         }
                    }
                    FiberStatus::Spawning(new_fiber) => {
                        self.fibers.push_back(*new_fiber);
                        self.current_fiber = Some(fiber);
                    }
                }
            } else if let Some(next) = self.fibers.pop_front() {
                 self.current_fiber = Some(next);
            } else {
                 break;
            }
        }
    }
}
