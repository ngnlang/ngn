use std::collections::VecDeque;
use std::sync::{Arc, Mutex};
use std::io::Write;
use crate::value::{Value, Channel, Closure, Function};
use crate::bytecode::OpCode;

pub struct CallFrame {
    pub instructions: Arc<Vec<OpCode>>,
    pub constants: Arc<Vec<Value>>,
    pub ip: usize,
    pub fp: usize,
    pub closure: Option<Box<Closure>>,
    pub dest_reg: Option<u16>,
    pub update_state: Option<Arc<Mutex<Value>>>,
}

pub enum FiberStatus {
    Running,
    Suspended,
    Finished,
    Spawning(Box<Fiber>),
}

pub struct Fiber {
    pub stack: Vec<Value>,
    pub frames: Vec<CallFrame>,
    pub ip: usize,
    pub fp: usize,
    pub instructions: Arc<Vec<OpCode>>,
    pub constants: Arc<Vec<Value>>,
    pub current_closure: Option<Box<Closure>>,
    pub status: FiberStatus,
    pub completion_channel: Option<Channel>,
}

impl Fiber {
    pub fn new(closure: Box<Closure>) -> Self {
        let func = closure.function.clone();
        Self {
            stack: vec![Value::Void; func.reg_count],
            frames: Vec::new(),
            ip: 0,
            fp: 0,
            instructions: func.instructions.clone(),
            constants: func.constants.clone(),
            current_closure: Some(closure),
            status: FiberStatus::Running,
            completion_channel: None,
        }
    }

    pub fn new_main(func: Function) -> Self {
        Self {
            stack: vec![Value::Void; func.reg_count],
            frames: Vec::new(),
            ip: 0,
            fp: 0,
            instructions: func.instructions.clone(),
            constants: func.constants.clone(),
            current_closure: None,
            status: FiberStatus::Running,
            completion_channel: None,
        }
    }

    pub fn get_reg_at(&self, idx: u16) -> Value {
        self.stack[self.fp + idx as usize].clone()
    }

    pub fn set_reg_at(&mut self, idx: u16, val: Value) {
        let target_idx = self.fp + idx as usize;
        if target_idx >= self.stack.len() {
            self.stack.resize(target_idx + 1, Value::Void);
        }
        self.stack[target_idx] = val;
    }

    pub fn run_step(&mut self, globals: &mut Vec<Value>) -> FiberStatus {
        if self.ip >= self.instructions.len() {
            if let Some(chan) = &self.completion_channel {
                chan.buffer.lock().unwrap().push_back(Value::Void);
            }
            self.status = FiberStatus::Finished;
            return FiberStatus::Finished;
        }

        let opcode = self.instructions[self.ip].clone();
        self.ip += 1;

        match opcode {
            OpCode::LoadConst(dest, idx) => {
                let val = self.constants[idx].clone();
                self.set_reg_at(dest, val);
            }
            OpCode::Move(dest, src) => {
                let val = self.get_reg_at(src);
                self.set_reg_at(dest, val);
            }
            OpCode::AssignVar(dest, src) => {
                let val = self.get_reg_at(src);
                self.set_reg_at(dest, val);
            }
            OpCode::DefVar(_, _) => {}
            OpCode::GetVar(dest, src_idx) => {
                // GetVar reads from a local variable slot
                let val = self.get_reg_at(src_idx as u16);
                self.set_reg_at(dest, val);
            }
            OpCode::Add(dest, left, right) => {
                let l = self.get_reg_at(left);
                let r = self.get_reg_at(right);
                if let Ok(res) = l.add(&r) {
                    self.set_reg_at(dest, res);
                } else {
                    panic!("Runtime Error: Addition failed between {:?} and {:?}", l, r);
                }
            }
            OpCode::Subtract(dest, left, right) => {
                let l = self.get_reg_at(left);
                let r = self.get_reg_at(right);
                if let Ok(res) = l.subtract(&r) {
                    self.set_reg_at(dest, res);
                } else {
                    panic!("Runtime Error: Subtraction failed");
                }
            }
            OpCode::Multiply(dest, left, right) => {
                let l = self.get_reg_at(left);
                let r = self.get_reg_at(right);
                if let Ok(res) = l.multiply(&r) {
                    self.set_reg_at(dest, res);
                } else {
                    panic!("Runtime Error: Multiplication failed");
                }
            }
            OpCode::Divide(dest, left, right) => {
                let l = self.get_reg_at(left);
                let r = self.get_reg_at(right);
                if let Ok(res) = l.divide(&r) {
                    self.set_reg_at(dest, res);
                } else {
                    panic!("Runtime Error: Division failed");
                }
            }
            OpCode::Power(dest, left, right) => {
                let l = self.get_reg_at(left);
                let r = self.get_reg_at(right);
                if let Ok(res) = l.power(r) {
                    self.set_reg_at(dest, res);
                } else {
                    panic!("Runtime Error: Power failed");
                }
            }
            OpCode::Modulo(dest, left, right) => {
                let l = self.get_reg_at(left);
                let r = self.get_reg_at(right);
                if let Ok(res) = l.remainder(r) {
                    self.set_reg_at(dest, res);
                } else {
                    panic!("Runtime Error: Modulo failed");
                }
            }
            OpCode::Equal(dest, left, right) => {
                let l = self.get_reg_at(left);
                let r = self.get_reg_at(right);
                self.set_reg_at(dest, Value::Bool(l.is_equal(&r)));
            }
            OpCode::NotEqual(dest, left, right) => {
                let l = self.get_reg_at(left);
                let r = self.get_reg_at(right);
                self.set_reg_at(dest, Value::Bool(!l.is_equal(&r)));
            }
            OpCode::LessThan(dest, left, right) => {
                let l = self.get_reg_at(left);
                let r = self.get_reg_at(right);
                if let (Value::Numeric(n1), Value::Numeric(n2)) = (l, r) {
                    self.set_reg_at(dest, Value::Bool(n1.less_than(n2)));
                } else {
                    panic!("Runtime Error: LessThan expects numeric types");
                }
            }
            OpCode::GreaterThan(dest, left, right) => {
                let l = self.get_reg_at(left);
                let r = self.get_reg_at(right);
                if let (Value::Numeric(n1), Value::Numeric(n2)) = (l.clone(), r.clone()) {
                    self.set_reg_at(dest, Value::Bool(n1.greater_than(n2)));
                } else {
                    panic!("Runtime Error: GreaterThan expects numeric types, got {:?} and {:?}", l, r);
                }
            }
            OpCode::GetGlobal(dest, idx) => {
                let val = globals[idx].clone();
                self.set_reg_at(dest, val);
            }
            OpCode::DefGlobal(_idx, _is_mutable) => {
                // Not used in register VM usually
            }
            OpCode::AssignGlobal(idx, src) => {
                let val = self.get_reg_at(src);
                globals[idx] = val;
            }
            OpCode::Call(dest, func_reg, arg_start, arg_count) => {
                let val = self.get_reg_at(func_reg);
                match val {
                    Value::Function(func) => {
                        self.invoke_function(&func, None, Some(dest), arg_start, arg_count);
                    }
                    Value::Closure(closure) => {
                        let func = closure.function.clone();
                        self.invoke_function(&func, Some(closure), Some(dest), arg_start, arg_count);
                    }
                    _ => panic!("Runtime Error: Can only call functions and closures"),
                }
            }
            OpCode::CallGlobal(dest, idx, arg_start, arg_count) => {
                let val = globals[idx].clone();
                match val {
                    Value::NativeFunction(id) => {
                        // Collect arguments
                        let mut args = Vec::new();
                        for i in 0..arg_count {
                            args.push(self.get_reg_at(arg_start + i as u16));
                        }
                        
                        // Dispatch to the appropriate toolbox function
                        let result = match id {
                            1 => crate::toolbox::math::abs(args),      // NATIVE_ABS
                            2 => crate::toolbox::test::assert(args),   // NATIVE_ASSERT
                            3 => crate::toolbox::math::round(args),    // NATIVE_ROUND
                            _ => panic!("Runtime Error: Unknown native function ID: {}", id),
                        };
                        
                        match result {
                            Ok(val) => self.set_reg_at(dest, val),
                            Err(e) => panic!("Runtime Error: {}", e),
                        }
                    }
                    Value::Function(func) => {
                        self.invoke_function(&func, None, Some(dest), arg_start, arg_count);
                    }
                    Value::Closure(closure) => {
                        let func = closure.function.clone();
                        self.invoke_function(&func, Some(closure), Some(dest), arg_start, arg_count);
                    }
                    _ => {
                        panic!("Runtime Error: Global {} is not a function: {:?}", idx, val);
                    }
                }
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
                self.set_reg_at(iter_reg, src.clone());
                if let Value::Channel(_) = src {
                    self.set_reg_at(iter_reg + 1, Value::Numeric(crate::value::Number::I64(-1)));
                } else {
                    self.set_reg_at(iter_reg + 1, Value::Numeric(crate::value::Number::I64(0)));
                }
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
                        Value::Channel(chan) => {
                            let mut buffer = chan.buffer.lock().unwrap();
                            if let Some(val) = buffer.pop_front() {
                                self.set_reg_at(dest, val);
                            } else {
                                if *chan.is_closed.lock().unwrap() {
                                    self.ip = jump_offset;
                                } else {
                                    self.ip -= 1;
                                    return FiberStatus::Suspended;
                                }
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
            OpCode::BuildArray(dest, start, count) => {
                let mut arr = Vec::new();
                for i in 0..count {
                    arr.push(self.get_reg_at(start + i as u16));
                }
                self.set_reg_at(dest, Value::Array(arr));
            }
            OpCode::BuildTuple(dest, start, count) => {
                let mut tup = Vec::new();
                for i in 0..count {
                    tup.push(self.get_reg_at(start + i as u16));
                }
                self.set_reg_at(dest, Value::Tuple(tup));
            }
            OpCode::Concat(dest, start, count) => {
                let mut res = String::new();
                for i in 0..count {
                    let val = self.get_reg_at(start + i as u16);
                    res.push_str(&val.to_string());
                }
                self.set_reg_at(dest, Value::String(res));
            }
            OpCode::CreateEnum(dest, names_idx, start, count) => {
                let names = if let Value::Tuple(v) = &self.constants[names_idx] {
                    v
                } else { panic!("Internal Error: Invalid names tuple for CreateEnum"); };
                
                let enum_name = if let Value::String(s) = &names[0] { s.clone() } else { panic!("..."); };
                let variant_name = if let Value::String(s) = &names[1] { s.clone() } else { panic!("..."); };
                
                let data = if count > 0 {
                    Some(Box::new(self.get_reg_at(start)))
                } else {
                    None
                };
                
                self.set_reg_at(dest, Value::Enum { enum_name, variant_name, data });
            }
            OpCode::IsVariant(dest, src, enum_idx, variant_idx) => {
                let val = self.get_reg_at(src);
                let target_enum = if let Value::String(s) = &self.constants[enum_idx] { s } else { panic!("..."); };
                let target_variant = if let Value::String(s) = &self.constants[variant_idx] { s } else { panic!("..."); };
                
                if let Value::Enum { enum_name, variant_name, .. } = val {
                    // If target_enum is empty, only match on variant name (shorthand syntax)
                    let matches = if target_enum.is_empty() {
                        &variant_name == target_variant
                    } else {
                        &enum_name == target_enum && &variant_name == target_variant
                    };
                    self.set_reg_at(dest, Value::Bool(matches));
                } else {
                    self.set_reg_at(dest, Value::Bool(false));
                }
            }
            OpCode::GetVariantData(dest, src) => {
                let val = self.get_reg_at(src);
                if let Value::Enum { data, .. } = val {
                    if let Some(d) = data {
                        self.set_reg_at(dest, *d);
                    } else {
                        panic!("Runtime Error: Accessing data of variant without payload");
                    }
                } else {
                    panic!("Runtime Error: GetVariantData on non-enum");
                }
            }
            OpCode::MakeClosure(dest, func_idx, upvalue_start, upvalue_count) => {
                let func = if let Value::Function(f) = &self.constants[func_idx] { f.clone() } else { panic!("..."); };
                let mut upvalues = Vec::new();
                for i in 0..upvalue_count {
                    upvalues.push(self.get_reg_at(upvalue_start + i as u16));
                }
                self.set_reg_at(dest, Value::Closure(Box::new(Closure {
                    function: func,
                    upvalues,
                })));
            }
            OpCode::GetUpvalue(dest, idx) => {
                if let Some(closure) = &self.current_closure {
                    self.set_reg_at(dest, closure.upvalues[idx as usize].clone());
                } else {
                    panic!("Runtime Error: Accessing upvalue outside of closure");
                }
            }
            OpCode::GetIndex(dest, obj_reg, index_reg) => {
                let obj = self.get_reg_at(obj_reg);
                let index = self.get_reg_at(index_reg);
                
                match (obj, index) {
                    (Value::Array(items), Value::Numeric(crate::value::Number::I64(idx))) => {
                        if idx < 0 || idx >= items.len() as i64 {
                            panic!("Runtime Error: Array index out of bounds: {}", idx);
                        }
                        self.set_reg_at(dest, items[idx as usize].clone());
                    }
                    (Value::String(s), Value::Numeric(crate::value::Number::I64(idx))) => {
                        if idx < 0 || idx >= s.len() as i64 {
                            panic!("Runtime Error: String index out of bounds: {}", idx);
                        }
                        let char = s.chars().nth(idx as usize).unwrap();
                        self.set_reg_at(dest, Value::String(char.to_string()));
                    }
                    _ => panic!("Runtime Error: Invalid indexing operation"),
                }
            }
            OpCode::Spawn(dest, closure_reg) => {
                let val = self.get_reg_at(closure_reg);
                if let Value::Closure(closure) = val {
                    let mut new_fiber = Fiber::new(closure);
                    let chan = Channel {
                        name: "thread_result".to_string(),
                        buffer: Arc::new(Mutex::new(VecDeque::new())),
                        capacity: 1,
                        is_closed: Arc::new(Mutex::new(false)),
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
                    if *chan.is_closed.lock().unwrap() {
                        panic!("Runtime Error: Send to closed channel");
                    }
                    let mut buffer = chan.buffer.lock().unwrap();
                    if buffer.len() < chan.capacity {
                        buffer.push_back(val);
                    } else {
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
                        if *chan.is_closed.lock().unwrap() {
                            panic!("Runtime Error: Receive from closed and empty channel");
                        }
                        self.ip -= 1;
                        return FiberStatus::Suspended;
                    }
                } else {
                    panic!("Runtime Error: Receive on non-channel");
                }
            }
            OpCode::CreateChannel(dest, capacity) => {
                let chan = Channel {
                    name: "chan".to_string(),
                    buffer: Arc::new(Mutex::new(VecDeque::new())),
                    capacity: capacity as usize,
                    is_closed: Arc::new(Mutex::new(false)),
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
                    self.stack.push(current_val);
                    
                    let func = closure.function.clone();
                    self.invoke_function(&func, Some(closure), None, arg_start, 1);
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
                        if *chan.is_closed.lock().unwrap() {
                            panic!("Runtime Error: ReceiveCount from closed channel with insufficient data");
                        }
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
            OpCode::CloseChannel(chan_reg) => {
                let chan_val = self.get_reg_at(chan_reg);
                if let Value::Channel(chan) = chan_val {
                    *chan.is_closed.lock().unwrap() = true;
                } else {
                    panic!("Runtime Error: .close() on non-channel");
                }
            }
            OpCode::Halt => {
                self.status = FiberStatus::Finished;
                return FiberStatus::Finished;
            }
            _ => {}
        }

        FiberStatus::Running
    }

    fn invoke_function(&mut self, func: &Function, closure: Option<Box<Closure>>, dest_reg: Option<u16>, arg_start: u16, _arg_count: u8) {
        let frame = CallFrame {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
            ip: self.ip,
            fp: self.fp,
            closure: self.current_closure.take(),
            dest_reg,
            update_state: None,
        };
        self.frames.push(frame);
        
        self.fp = self.fp + arg_start as usize;
        let needed = self.fp + func.reg_count;
        if self.stack.len() < needed {
            self.stack.resize(needed, Value::Void);
        }
        
        self.instructions = func.instructions.clone();
        self.constants = func.constants.clone();
        self.ip = 0;
        self.current_closure = closure;
    }
}

pub struct VM {
    pub globals: Vec<Value>,
    pub fibers: VecDeque<Fiber>,
    pub current_fiber: Option<Fiber>,
}

impl VM {
    pub fn new(instructions: Vec<OpCode>, constants: Vec<Value>, reg_count: usize) -> Self {
        let main_func = Function {
            name: "main_wrapper".to_string(),
            instructions: Arc::new(instructions),
            constants: Arc::new(constants),
            param_count: 0,
            param_ownership: Vec::new(),
            reg_count,
        };
        let main_fiber = Fiber::new_main(main_func);
        let globals = vec![Value::Void; 1024];
        // Standard built-ins handled via OpCode are still mapped in compiler for name resolution
        // But we can also put NativeFunction wrappers in the global table if we want them to be call-by-reference
        
        Self {
            globals,
            fibers: VecDeque::new(),
            current_fiber: Some(main_fiber),
        }
    }

    pub fn run(&mut self) {
        while let Some(mut fiber) = self.current_fiber.take() {
            let status = fiber.run_step(&mut self.globals);
            match status {
                FiberStatus::Running => {
                    self.current_fiber = Some(fiber);
                }
                FiberStatus::Suspended => {
                    self.fibers.push_back(fiber);
                    self.current_fiber = self.fibers.pop_front();
                }
                FiberStatus::Finished => {
                    self.current_fiber = self.fibers.pop_front();
                }
                FiberStatus::Spawning(new_fiber) => {
                    self.fibers.push_back(*new_fiber);
                    self.current_fiber = Some(fiber);
                }
            }
            
            if self.current_fiber.is_none() && !self.fibers.is_empty() {
                self.current_fiber = self.fibers.pop_front();
            }
        }
    }
}
