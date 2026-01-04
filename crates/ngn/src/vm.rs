use crate::bytecode::OpCode;
use crate::value::{Channel, Closure, EnumData, Function, ObjectData, Value};
use regex::Regex as RegexLib;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::io::Write;
use std::sync::{Arc, Mutex};

/// Stores the globals for a single module
pub struct ModuleContext {
    pub globals: Vec<Value>,
}

/// Registry of all loaded module contexts
pub struct ModuleRegistry {
    modules: HashMap<usize, ModuleContext>,
}

impl ModuleRegistry {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }

    pub fn register(&mut self, id: usize, globals: Vec<Value>) {
        self.modules.insert(id, ModuleContext { globals });
    }

    pub fn get_globals(&self, id: usize) -> Option<&Vec<Value>> {
        self.modules.get(&id).map(|m| &m.globals)
    }
}

pub struct CallFrame {
    pub instructions: Arc<Vec<OpCode>>,
    pub constants: Arc<Vec<Value>>,
    pub ip: usize,
    pub fp: usize,
    pub closure: Option<Box<Closure>>,
    pub dest_reg: Option<u16>,
    pub update_state: Option<Arc<Mutex<Value>>>,
    pub home_globals: Option<Arc<Vec<Value>>>, // Saved home globals for restoration on return
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
    pub return_value: Option<Value>,
    pub home_globals: Option<Arc<Vec<Value>>>, // Current function's home module globals
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
            return_value: None,
            home_globals: func.home_globals.clone(),
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
            return_value: None,
            home_globals: func.home_globals.clone(),
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

    /// Extract a String constant from the constant pool, panicking if the type is wrong.
    pub fn expect_string_const(&self, idx: usize) -> String {
        match &self.constants[idx] {
            Value::String(s) => s.clone(),
            other => panic!(
                "Internal Error: Expected String constant at index {}, found {}",
                idx,
                other.type_name()
            ),
        }
    }

    /// Extract a Tuple constant from the constant pool, panicking if the type is wrong.
    pub fn expect_tuple_const(&self, idx: usize) -> &Vec<Value> {
        match &self.constants[idx] {
            Value::Tuple(t) => t,
            other => panic!(
                "Internal Error: Expected Tuple constant at index {}, found {}",
                idx,
                other.type_name()
            ),
        }
    }

    /// Run up to max_steps instructions, returning early if fiber completes or suspends.
    /// Returns (final_status, steps_executed) to enable cooperative async execution.
    pub fn run_steps(
        &mut self,
        globals: &mut Vec<Value>,
        custom_methods: &Arc<
            Mutex<std::collections::HashMap<String, std::collections::HashMap<String, Value>>>,
        >,
        max_steps: usize,
    ) -> (FiberStatus, usize) {
        for step in 0..max_steps {
            let status = self.run_step(globals, custom_methods);
            match status {
                FiberStatus::Running => continue,
                _ => return (status, step + 1),
            }
        }
        // Reached max_steps, still running
        (FiberStatus::Running, max_steps)
    }

    pub fn run_step(
        &mut self,
        globals: &mut Vec<Value>,
        custom_methods: &Arc<
            Mutex<std::collections::HashMap<String, std::collections::HashMap<String, Value>>>,
        >,
    ) -> FiberStatus {
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
            OpCode::MakeRegex(dest, idx) => {
                let val = self.constants[idx].clone();
                if let Value::String(pattern) = val {
                    self.set_reg_at(dest, Value::Regex(pattern));
                } else {
                    panic!("Runtime Error: MakeRegex expects string constant");
                }
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
                    panic!(
                        "Runtime Error: GreaterThan expects numeric types, got {:?} and {:?}",
                        l, r
                    );
                }
            }
            OpCode::Negate(dest, src) => {
                let val = self.get_reg_at(src);
                if let Value::Numeric(n) = val {
                    self.set_reg_at(dest, Value::Numeric(n.negate()));
                } else {
                    panic!("Runtime Error: Negate expects numeric type, got {:?}", val);
                }
            }
            OpCode::Not(dest, src) => {
                let val = self.get_reg_at(src);
                if let Value::Bool(b) = val {
                    self.set_reg_at(dest, Value::Bool(!b));
                } else {
                    panic!("Runtime Error: Not expects boolean type, got {:?}", val);
                }
            }
            OpCode::GetGlobal(dest, idx) => {
                // Access globals from the function's home module (or main globals if None)
                let val = if let Some(ref home_globals) = self.home_globals {
                    home_globals[idx].clone()
                } else {
                    globals[idx].clone()
                };
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
                        self.invoke_function(
                            &func,
                            Some(closure),
                            Some(dest),
                            arg_start,
                            arg_count,
                        );
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

                        // Handle serve specially - it blocks forever and needs thread-safe globals
                        if id == 4 || id == 5 {
                            // NATIVE_SERVE or NATIVE_SERVE_TLS
                            if args.is_empty() {
                                panic!(
                                    "Runtime Error: serve requires at least 2 arguments (port, handler)"
                                );
                            }

                            let port = match &args[0] {
                                Value::Numeric(crate::value::Number::I64(p)) => *p as u16,
                                Value::Numeric(crate::value::Number::I32(p)) => *p as u16,
                                _ => panic!("Runtime Error: serve port must be a number"),
                            };

                            let handler = args
                                .get(1)
                                .cloned()
                                .expect("Runtime Error: serve requires a handler function");

                            // Wrap globals and custom_methods in Arc<Mutex> for thread safety
                            let shared_globals = Arc::new(Mutex::new(globals.clone()));
                            let shared_methods = custom_methods.clone();

                            if id == 4 {
                                // HTTP (async tokio)
                                if let Err(e) = crate::toolbox::http::serve(
                                    port,
                                    handler,
                                    shared_globals,
                                    shared_methods,
                                ) {
                                    panic!("Runtime Error: {}", e);
                                }
                            } else {
                                // HTTPS (id == 5)
                                let cert = match args.get(2) {
                                    Some(Value::String(s)) => s.clone(),
                                    _ => panic!(
                                        "Runtime Error: serve_tls requires cert path as 3rd arg"
                                    ),
                                };
                                let key = match args.get(3) {
                                    Some(Value::String(s)) => s.clone(),
                                    _ => panic!(
                                        "Runtime Error: serve_tls requires key path as 4th arg"
                                    ),
                                };
                                if let Err(e) = crate::toolbox::http::serve_tls(
                                    port,
                                    handler,
                                    &cert,
                                    &key,
                                    shared_globals,
                                    shared_methods,
                                ) {
                                    panic!("Runtime Error: {}", e);
                                }
                            }
                            // serve never returns (blocks forever), but just in case:
                            self.set_reg_at(dest, Value::Void);
                            return FiberStatus::Finished;
                        }

                        // Dispatch to the appropriate toolbox function
                        let result = match id {
                            1 => crate::toolbox::math::abs(args),    // NATIVE_ABS
                            2 => crate::toolbox::test::assert(args), // NATIVE_ASSERT
                            3 => crate::toolbox::math::round(args),  // NATIVE_ROUND
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
                        self.invoke_function(
                            &func,
                            Some(closure),
                            Some(dest),
                            arg_start,
                            arg_count,
                        );
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
                                self.set_reg_at(
                                    iter_reg + 1,
                                    Value::Numeric(crate::value::Number::I64(idx + 1)),
                                );
                            } else {
                                self.ip = jump_offset;
                            }
                        }
                        Value::Tuple(items) => {
                            if (idx as usize) < items.len() {
                                self.set_reg_at(dest, items[idx as usize].clone());
                                self.set_reg_at(
                                    iter_reg + 1,
                                    Value::Numeric(crate::value::Number::I64(idx + 1)),
                                );
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
                        _ => panic!(
                            "Runtime Error: IterNext on non-collection: {:?}",
                            collection
                        ),
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
                    self.home_globals = frame.home_globals;
                    if let Some(dest) = frame.dest_reg {
                        self.set_reg_at(dest, result.clone());
                    }

                    if let Some(state) = frame.update_state {
                        *state.lock().unwrap() = result;
                    }
                } else {
                    if let Some(chan) = &self.completion_channel {
                        chan.buffer.lock().unwrap().push_back(result.clone());
                    }
                    self.status = FiberStatus::Finished;
                    self.return_value = Some(result);
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
                    self.home_globals = frame.home_globals; // Restore home globals context
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
                } else {
                    panic!("Internal Error: Invalid names tuple for CreateEnum");
                };

                let enum_name = if let Value::String(s) = &names[0] {
                    s.clone()
                } else {
                    panic!("...");
                };
                let variant_name = if let Value::String(s) = &names[1] {
                    s.clone()
                } else {
                    panic!("...");
                };

                let data = if count > 0 {
                    Some(Box::new(self.get_reg_at(start)))
                } else {
                    None
                };

                self.set_reg_at(dest, EnumData::into_value(enum_name, variant_name, data));
            }
            OpCode::IsVariant(dest, src, enum_idx, variant_idx) => {
                let val = self.get_reg_at(src);
                let target_enum = self.expect_string_const(enum_idx);
                let target_variant = self.expect_string_const(variant_idx);

                if let Value::Enum(e) = val {
                    // If target_enum is empty, only match on variant name (shorthand syntax)
                    let matches = if target_enum.is_empty() {
                        e.variant_name == target_variant
                    } else {
                        e.enum_name == target_enum && e.variant_name == target_variant
                    };
                    self.set_reg_at(dest, Value::Bool(matches));
                } else {
                    self.set_reg_at(dest, Value::Bool(false));
                }
            }
            OpCode::GetVariantData(dest, src) => {
                let val = self.get_reg_at(src);
                if let Value::Enum(e) = val {
                    if let Some(d) = e.data {
                        self.set_reg_at(dest, *d);
                    } else {
                        panic!("Runtime Error: Accessing data of variant without payload");
                    }
                } else {
                    panic!("Runtime Error: GetVariantData on non-enum");
                }
            }
            OpCode::MakeClosure(dest, func_idx, upvalue_start, upvalue_count) => {
                let func = if let Value::Function(f) = &self.constants[func_idx] {
                    f.clone()
                } else {
                    panic!("...");
                };
                let mut upvalues = Vec::new();
                for i in 0..upvalue_count {
                    upvalues.push(self.get_reg_at(upvalue_start + i as u16));
                }
                self.set_reg_at(
                    dest,
                    Value::Closure(Box::new(Closure {
                        function: func,
                        upvalues,
                    })),
                );
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
            OpCode::CreateMap(dest) => {
                self.set_reg_at(dest, Value::Map(std::collections::HashMap::new()));
            }
            OpCode::CreateSet(dest) => {
                self.set_reg_at(dest, Value::Set(std::collections::HashSet::new()));
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
                if let (Value::Channel(chan), Value::Numeric(crate::value::Number::I64(count))) =
                    (chan_val, count_val)
                {
                    let mut buffer = chan.buffer.lock().unwrap();
                    if buffer.len() >= count as usize {
                        let mut res = Vec::new();
                        for _ in 0..count {
                            res.push(buffer.pop_front().unwrap());
                        }
                        self.set_reg_at(dest, Value::Array(res));
                    } else {
                        if *chan.is_closed.lock().unwrap() {
                            panic!(
                                "Runtime Error: ReceiveCount from closed channel with insufficient data"
                            );
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
                        self.set_reg_at(
                            dest,
                            EnumData::into_value(
                                "Maybe".to_string(),
                                "Value".to_string(),
                                Some(Box::new(val)),
                            ),
                        );
                    } else {
                        self.set_reg_at(
                            dest,
                            EnumData::into_value("Maybe".to_string(), "Null".to_string(), None),
                        );
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
            OpCode::CallMethod(dest, obj_reg, method_idx, arg_start, arg_count) => {
                let obj = self.get_reg_at(obj_reg);
                let method_name = self.expect_string_const(method_idx);

                // Check custom methods from 'extend'
                let type_name = obj.type_name();
                let mut custom_method = None;
                {
                    let methods_map = custom_methods.lock().unwrap();
                    // First try exact type
                    if let Some(target_methods) = methods_map.get(type_name) {
                        if let Some(m) = target_methods.get(&method_name) {
                            custom_method = Some(m.clone());
                        }
                    }
                    // Fallback to "number" for numeric types (extend number { })
                    if custom_method.is_none() {
                        if let Value::Numeric(_) = &obj {
                            if let Some(target_methods) = methods_map.get("number") {
                                if let Some(m) = target_methods.get(&method_name) {
                                    custom_method = Some(m.clone());
                                }
                            }
                        }
                    }
                }

                if let Some(Value::Closure(closure)) = custom_method {
                    let new_arg_start = (self.stack.len() - self.fp) as u16;
                    self.stack.push(obj);
                    for i in 0..arg_count {
                        self.stack.push(self.get_reg_at(arg_start + i as u16));
                    }
                    let func = closure.function.clone();
                    self.invoke_function(
                        &func,
                        Some(closure),
                        Some(dest),
                        new_arg_start,
                        arg_count + 1,
                    );
                    return FiberStatus::Running;
                }

                // Collect arguments
                let mut args = Vec::new();
                for i in 0..arg_count {
                    args.push(self.get_reg_at(arg_start + i as u16));
                }

                // Dispatch to appropriate method implementation (non-mutating)
                let result = self.call_builtin_method(obj, &method_name, args);
                self.set_reg_at(dest, result);
            }
            OpCode::CallMethodMut(dest, obj_reg, method_idx, arg_start, arg_count) => {
                let obj = self.get_reg_at(obj_reg);
                let method_name = self.expect_string_const(method_idx);

                // Check custom methods (can also be used for mutating calls)
                let type_name = obj.type_name();
                let mut custom_method = None;
                {
                    let methods_map = custom_methods.lock().unwrap();
                    // First try exact type
                    if let Some(target_methods) = methods_map.get(type_name) {
                        if let Some(m) = target_methods.get(&method_name) {
                            custom_method = Some(m.clone());
                        }
                    }
                    // Fallback to "number" for numeric types (extend number { })
                    if custom_method.is_none() {
                        if let Value::Numeric(_) = &obj {
                            if let Some(target_methods) = methods_map.get("number") {
                                if let Some(m) = target_methods.get(&method_name) {
                                    custom_method = Some(m.clone());
                                }
                            }
                        }
                    }
                }

                if let Some(Value::Closure(closure)) = custom_method {
                    let new_arg_start = (self.stack.len() - self.fp) as u16;
                    self.stack.push(obj);
                    for i in 0..arg_count {
                        self.stack.push(self.get_reg_at(arg_start + i as u16));
                    }
                    let func = closure.function.clone();
                    // We might need to write back the object if it's 'this' that changed
                    // but ngn is generally immutable except for field assignments which use SetField.
                    self.invoke_function(
                        &func,
                        Some(closure),
                        Some(dest),
                        new_arg_start,
                        arg_count + 1,
                    );
                    return FiberStatus::Running;
                }

                // Collect arguments
                let mut args = Vec::new();
                for i in 0..arg_count {
                    args.push(self.get_reg_at(arg_start + i as u16));
                }

                // Dispatch to mutating method - returns (result, new_object)
                let (result, new_obj) = self.call_builtin_method_mut(obj, &method_name, args);
                self.set_reg_at(dest, result);
                self.set_reg_at(obj_reg, new_obj);
            }
            OpCode::CreateObject(dest, model_idx, fields_idx, start, count) => {
                let model_name = self.expect_string_const(model_idx);
                let field_names = self.expect_tuple_const(fields_idx);
                let mut fields = std::collections::HashMap::new();
                for i in 0..count {
                    let field_name = if let Value::String(s) = &field_names[i as usize] {
                        s.clone()
                    } else {
                        panic!("...");
                    };
                    fields.insert(field_name, self.get_reg_at(start + i as u16));
                }
                self.set_reg_at(dest, ObjectData::into_value(model_name, fields));
            }
            OpCode::GetField(dest, obj_reg, field_idx) => {
                let obj = self.get_reg_at(obj_reg);
                let field_name = self.expect_string_const(field_idx);
                if let Value::Object(o) = obj {
                    if let Some(val) = o.fields.get(&field_name) {
                        self.set_reg_at(dest, val.clone());
                    } else {
                        panic!("Runtime Error: Field '{}' not found in object", field_name);
                    }
                } else {
                    panic!("Runtime Error: GetField on non-object");
                }
            }
            OpCode::SetField(obj_reg, field_idx, src_reg) => {
                let mut obj = self.get_reg_at(obj_reg);
                let field_name = self.expect_string_const(field_idx);
                let val = self.get_reg_at(src_reg);
                if let Value::Object(ref mut o) = obj {
                    o.fields.insert(field_name, val);
                    self.set_reg_at(obj_reg, obj);
                } else {
                    panic!("Runtime Error: SetField on non-object");
                }
            }
            OpCode::DefMethod(target_idx, name_idx, closure_reg) => {
                let target_type = self.expect_string_const(target_idx);
                let method_name = self.expect_string_const(name_idx);
                let closure = self.get_reg_at(closure_reg);

                let mut methods = custom_methods.lock().unwrap();
                let target_methods = methods
                    .entry(target_type)
                    .or_insert_with(std::collections::HashMap::new);
                target_methods.insert(method_name, closure);
            }
            OpCode::Halt => {
                self.status = FiberStatus::Finished;
                return FiberStatus::Finished;
            }
            _ => {}
        }

        FiberStatus::Running
    }

    fn invoke_function(
        &mut self,
        func: &Function,
        closure: Option<Box<Closure>>,
        dest_reg: Option<u16>,
        arg_start: u16,
        _arg_count: u8,
    ) {
        let frame = CallFrame {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
            ip: self.ip,
            fp: self.fp,
            closure: self.current_closure.take(),
            dest_reg,
            update_state: None,
            home_globals: self.home_globals.clone(),
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

        // Switch to the function's home globals context
        self.home_globals = func.home_globals.clone();
    }

    /// Dispatch built-in method calls on arrays, strings, tuples, etc.
    fn call_builtin_method(&self, obj: Value, method: &str, args: Vec<Value>) -> Value {
        match obj {
            Value::Array(arr) => self.array_method(arr, method, args),
            Value::String(s) => self.string_method(&s, method, args),
            Value::Tuple(t) => self.tuple_method(&t, method, args),
            Value::Map(map) => self.map_method(map, method, args),
            Value::Set(set) => self.set_method(set, method, args),
            _ => panic!(
                "Runtime Error: Cannot call method '{}' on {:?}",
                method, obj
            ),
        }
    }

    /// Dispatch mutating built-in method calls. Returns (result, modified_object).
    fn call_builtin_method_mut(
        &self,
        obj: Value,
        method: &str,
        args: Vec<Value>,
    ) -> (Value, Value) {
        match obj {
            Value::Array(arr) => self.array_method_mut(arr, method, args),
            Value::String(s) => self.string_method_mut(s, method, args),
            Value::Map(map) => self.map_method_mut(map, method, args),
            Value::Set(set) => self.set_method_mut(set, method, args),
            _ => panic!(
                "Runtime Error: Cannot call mutating method '{}' on {:?}",
                method, obj
            ),
        }
    }

    /// Array non-mutating methods: size, copy, each (placeholder)
    fn array_method(&self, arr: Vec<Value>, method: &str, args: Vec<Value>) -> Value {
        match method {
            "size" => Value::Numeric(crate::value::Number::I64(arr.len() as i64)),
            "copy" => {
                let start = if !args.is_empty() {
                    self.to_usize(&args[0]).unwrap_or(0)
                } else {
                    0
                };
                let end = if args.len() > 1 {
                    self.to_usize(&args[1]).unwrap_or(arr.len())
                } else {
                    arr.len()
                };
                if start > end || end > arr.len() {
                    panic!("Range out of bounds");
                }
                Value::Array(arr[start..end].to_vec())
            }
            "each" => panic!(
                "Runtime Error: each() method not yet implemented in v2 (try 'for' loop instead)"
            ),
            _ => panic!(
                "Runtime Error: Unknown non-mutating array method '{}'",
                method
            ),
        }
    }

    /// Array mutating methods: push, pull, slice, splice
    fn array_method_mut(
        &self,
        mut arr: Vec<Value>,
        method: &str,
        args: Vec<Value>,
    ) -> (Value, Value) {
        match method {
            "push" => {
                if args.is_empty() {
                    panic!("push() requires item");
                }
                let item = args[0].clone();
                if args.len() > 1 {
                    let idx = self
                        .to_usize(&args[1])
                        .expect("push() index must be integer");
                    if idx > arr.len() {
                        panic!("push() index out of bounds");
                    }
                    arr.insert(idx, item);
                } else {
                    arr.push(item);
                }
                (
                    Value::Numeric(crate::value::Number::I64(arr.len() as i64)),
                    Value::Array(arr),
                )
            }
            "pull" => {
                let removed = if args.len() > 0 {
                    let idx = self
                        .to_usize(&args[0])
                        .expect("pull() index must be integer");
                    if idx >= arr.len() {
                        panic!("pull() index out of bounds");
                    }
                    arr.remove(idx)
                } else {
                    arr.pop().expect("pull() from empty array")
                };
                (removed, Value::Array(arr))
            }
            "slice" => {
                let start = self
                    .to_usize(&args[0])
                    .expect("slice() start must be an integer");
                let end = if args.len() > 1 {
                    self.to_usize(&args[1])
                        .expect("slice() stop must be an integer")
                } else {
                    arr.len()
                };
                if start > end || end > arr.len() {
                    panic!("Range out of bounds");
                }
                let removed: Vec<Value> = arr.drain(start..end).collect();
                (Value::Array(removed), Value::Array(arr))
            }
            "splice" => {
                let items = if let Value::Array(items) = &args[0] {
                    items.clone()
                } else {
                    panic!("splice() expects an array as first argument")
                };
                let start = if args.len() > 1 {
                    self.to_usize(&args[1]).unwrap_or(arr.len())
                } else {
                    arr.len()
                };
                if start > arr.len() {
                    panic!("Index out of bounds");
                }
                for (i, item) in items.into_iter().enumerate() {
                    arr.insert(start + i, item);
                }
                (
                    Value::Numeric(crate::value::Number::I64(arr.len() as i64)),
                    Value::Array(arr),
                )
            }
            _ => panic!("Runtime Error: Unknown mutating array method '{}'", method),
        }
    }

    /// String non-mutating methods: length, index, includes, starts, ends, split, replace, copy, upper, lower, trim, repeat
    fn string_method(&self, s: &str, method: &str, args: Vec<Value>) -> Value {
        match method {
            "length" => Value::Numeric(crate::value::Number::I64(s.len() as i64)),
            "index" => {
                if args.is_empty() {
                    panic!("index() requires search pattern");
                }
                let pattern = if let Value::String(p) = &args[0] {
                    p
                } else {
                    panic!("index() expects string pattern")
                };
                let start = if args.len() > 1 {
                    self.to_usize(&args[1]).unwrap_or(0)
                } else {
                    0
                };
                if start >= s.len() {
                    Value::Numeric(crate::value::Number::I64(-1))
                } else {
                    match s[start..].find(pattern.as_str()) {
                        Some(i) => Value::Numeric(crate::value::Number::I64((i + start) as i64)),
                        None => Value::Numeric(crate::value::Number::I64(-1)),
                    }
                }
            }
            "includes" => {
                if args.is_empty() {
                    panic!("includes() requires search pattern");
                }
                let search = if let Value::String(p) = &args[0] {
                    p
                } else {
                    panic!("includes() expects string")
                };
                Value::Bool(s.contains(search.as_str()))
            }
            "starts" => {
                if args.is_empty() {
                    panic!("starts() requires prefix");
                }
                let prefix = if let Value::String(p) = &args[0] {
                    p
                } else {
                    panic!("starts() expects string")
                };
                Value::Bool(s.starts_with(prefix.as_str()))
            }
            "ends" => {
                if args.is_empty() {
                    panic!("ends() requires suffix");
                }
                let suffix = if let Value::String(p) = &args[0] {
                    p
                } else {
                    panic!("ends() expects string")
                };
                Value::Bool(s.ends_with(suffix.as_str()))
            }
            "split" => {
                if args.is_empty() {
                    let parts: Vec<Value> =
                        s.chars().map(|c| Value::String(c.to_string())).collect();
                    Value::Array(parts)
                } else if let Value::String(delim) = &args[0] {
                    let parts: Vec<Value> = s
                        .split(delim.as_str())
                        .map(|part| Value::String(part.to_string()))
                        .collect();
                    Value::Array(parts)
                } else {
                    panic!("split() argument must be string");
                }
            }
            "replace" => {
                if args.len() < 2 {
                    panic!("replace() requires search and replacement patterns");
                }
                let replacement = if let Value::String(s) = &args[1] {
                    s
                } else {
                    panic!("replace() expects string replacement")
                };

                match &args[0] {
                    Value::String(search) => {
                        Value::String(s.replacen(search.as_str(), replacement.as_str(), 1))
                    }
                    Value::Regex(pattern_str) => {
                        let (pattern, flags) = if pattern_str.starts_with("(?") {
                            let end = pattern_str.find(')').expect("Invalid regex format");
                            let flags = &pattern_str[2..end];
                            let rest = &pattern_str[end + 1..];
                            (rest, flags)
                        } else {
                            (pattern_str.as_str(), "")
                        };

                        let is_global = flags.contains('g');

                        // Reconstruct pattern with standard flags (excluding 'g')
                        let standard_flags: String = flags.chars().filter(|&c| c != 'g').collect();
                        let final_pattern = if standard_flags.is_empty() {
                            pattern.to_string()
                        } else {
                            format!("(?{}){}", standard_flags, pattern)
                        };

                        match RegexLib::new(&final_pattern) {
                            Ok(re) => {
                                let result = if is_global {
                                    re.replace_all(s, replacement.as_str())
                                } else {
                                    re.replace(s, replacement.as_str())
                                };
                                Value::String(result.to_string())
                            }
                            Err(e) => panic!("Invalid regex '{}': {}", final_pattern, e),
                        }
                    }
                    _ => panic!("replace() expects string or regex search pattern"),
                }
            }
            "copy" => {
                let start = if !args.is_empty() {
                    self.to_usize(&args[0]).unwrap_or(0)
                } else {
                    0
                };
                let end = if args.len() > 1 {
                    self.to_usize(&args[1]).unwrap_or(s.len())
                } else {
                    s.len()
                };
                if start > end || end > s.len() {
                    panic!("Range out of bounds");
                }
                Value::String(s[start..end].to_string())
            }
            "upper" => Value::String(s.to_uppercase()),
            "lower" => Value::String(s.to_lowercase()),
            "trim" => Value::String(s.trim().to_string()),
            "repeat" => {
                if args.is_empty() {
                    panic!("repeat() requires count");
                }
                let count = self
                    .to_usize(&args[0])
                    .expect("repeat() expects integer count");
                Value::String(s.repeat(count))
            }
            _ => panic!(
                "Runtime Error: Unknown non-mutating string method '{}'",
                method
            ),
        }
    }

    /// String mutating methods: slice
    fn string_method_mut(&self, s: String, method: &str, args: Vec<Value>) -> (Value, Value) {
        match method {
            "slice" => {
                let start = self
                    .to_usize(&args[0])
                    .expect("slice() start must be integer");
                let end = if args.len() > 1 {
                    self.to_usize(&args[1])
                        .expect("slice() stop must be integer")
                } else {
                    s.len()
                };
                if start > end || end > s.len() {
                    panic!("Range out of bounds");
                }
                let removed = s[start..end].to_string();
                let mut new_s = s;
                new_s.replace_range(start..end, "");
                (Value::String(removed), Value::String(new_s))
            }
            _ => panic!("Runtime Error: Unknown mutating string method '{}'", method),
        }
    }

    /// Tuple methods: size, includes, index, toArray, copy, join
    fn tuple_method(&self, t: &[Value], method: &str, args: Vec<Value>) -> Value {
        match method {
            "size" => Value::Numeric(crate::value::Number::I64(t.len() as i64)),
            "includes" => {
                if args.is_empty() {
                    panic!("includes() requires a value to search for");
                }
                let target = &args[0];
                Value::Bool(t.iter().any(|v| v.is_equal(target)))
            }
            "index" => {
                if args.is_empty() {
                    panic!("index() requires a value to search for");
                }
                let target = &args[0];
                for (i, v) in t.iter().enumerate() {
                    if v.is_equal(target) {
                        return Value::Numeric(crate::value::Number::I64(i as i64));
                    }
                }
                Value::Numeric(crate::value::Number::I64(-1))
            }
            "toArray" => {
                if t.is_empty() {
                    return Value::Array(Vec::new());
                }

                // Type consistency check
                let first_val = &t[0];
                for (i, val) in t.iter().enumerate().skip(1) {
                    if !self.is_same_basic_type(first_val, val) {
                        panic!(
                            "Runtime Error: Cannot convert mixed-type tuple to array (element {} {:?} differs from first element {:?})",
                            i, val, first_val
                        );
                    }
                }
                Value::Array(t.to_vec())
            }
            "copy" => {
                let start = if !args.is_empty() {
                    self.to_usize(&args[0])
                        .expect("copy() start must be integer")
                } else {
                    0
                };
                let end = if args.len() > 1 {
                    self.to_usize(&args[1])
                        .expect("copy() stop must be integer")
                } else {
                    t.len()
                };
                if start > end || end > t.len() {
                    panic!("Range out of bounds");
                }
                Value::Tuple(t[start..end].to_vec())
            }
            "join" => {
                let delim = if args.is_empty() {
                    ""
                } else {
                    if let Value::String(s) = &args[0] {
                        s.as_str()
                    } else {
                        panic!("join() separator must be string")
                    }
                };
                let parts: Vec<String> = t.iter().map(|v| format!("{}", v)).collect();
                Value::String(parts.join(delim))
            }
            _ => panic!("Runtime Error: Unknown tuple method '{}'", method),
        }
    }

    fn is_same_basic_type(&self, v1: &Value, v2: &Value) -> bool {
        match (v1, v2) {
            (Value::Numeric(n1), Value::Numeric(n2)) => n1.rank() == n2.rank(),
            (Value::String(_), Value::String(_)) => true,
            (Value::Bool(_), Value::Bool(_)) => true,
            (Value::Array(_), Value::Array(_)) => true, // Could be stricter but for now variants match
            (Value::Tuple(_), Value::Tuple(_)) => true,
            (Value::Enum(e1), Value::Enum(e2)) => {
                e1.enum_name == e2.enum_name && e1.variant_name == e2.variant_name
            }
            (Value::Void, Value::Void) => true,
            _ => false,
        }
    }

    ///Map non-mutating methods: size, has, get
    fn map_method(
        &self,
        map: std::collections::HashMap<Value, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Value {
        match method {
            "size" => Value::Numeric(crate::value::Number::I64(map.len() as i64)),
            "has" => {
                if args.is_empty() {
                    panic!("has() requires a key");
                }
                Value::Bool(map.contains_key(&args[0]))
            }
            "get" => {
                if args.is_empty() {
                    panic!("get() requires a key");
                }
                map.get(&args[0]).cloned().unwrap_or(Value::Void)
            }
            _ => panic!(
                "Runtime Error: Unknown non-mutating map method '{}'",
                method
            ),
        }
    }

    /// Map mutating methods: set, remove
    fn map_method_mut(
        &self,
        mut map: std::collections::HashMap<Value, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> (Value, Value) {
        match method {
            "set" => {
                if args.len() < 2 {
                    panic!("set() requires key and value");
                }
                map.insert(args[0].clone(), args[1].clone());
                // Return the map itself for chaining
                (Value::Map(map.clone()), Value::Map(map))
            }
            "remove" => {
                if args.is_empty() {
                    panic!("remove() requires a key");
                }
                let removed = map.remove(&args[0]).unwrap_or(Value::Void);
                (removed, Value::Map(map))
            }
            _ => panic!("Runtime Error: Unknown mutating map method '{}'", method),
        }
    }

    /// Set non-mutating methods: size, has
    fn set_method(
        &self,
        set: std::collections::HashSet<Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Value {
        match method {
            "size" => Value::Numeric(crate::value::Number::I64(set.len() as i64)),
            "has" => {
                if args.is_empty() {
                    panic!("has() requires a value");
                }
                Value::Bool(set.contains(&args[0]))
            }
            _ => panic!(
                "Runtime Error: Unknown non-mutating set method '{}'",
                method
            ),
        }
    }

    /// Set mutating methods: add, remove
    fn set_method_mut(
        &self,
        mut set: std::collections::HashSet<Value>,
        method: &str,
        args: Vec<Value>,
    ) -> (Value, Value) {
        match method {
            "add" => {
                if args.is_empty() {
                    panic!("add() requires a value");
                }
                set.insert(args[0].clone());
                // Return the set itself for chaining
                (Value::Set(set.clone()), Value::Set(set))
            }
            "remove" => {
                if args.is_empty() {
                    panic!("remove() requires a value");
                }
                let was_removed = set.remove(&args[0]);
                (Value::Bool(was_removed), Value::Set(set))
            }
            _ => panic!("Runtime Error: Unknown mutating set method '{}'", method),
        }
    }

    /// Helper to convert Value to usize for indexing
    fn to_usize(&self, val: &Value) -> Option<usize> {
        match val {
            Value::Numeric(crate::value::Number::I64(n)) => {
                if *n >= 0 {
                    Some(*n as usize)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

pub struct VM {
    pub globals: Vec<Value>, // Main module globals
    pub fibers: VecDeque<Fiber>,
    pub current_fiber: Option<Fiber>,
    pub custom_methods:
        Arc<Mutex<std::collections::HashMap<String, std::collections::HashMap<String, Value>>>>,
    pub last_value: Option<Value>,
}

impl VM {
    pub fn new(instructions: Vec<OpCode>, constants: Vec<Value>, reg_count: usize) -> Self {
        let main_func = Function {
            name: "main_wrapper".to_string(),
            instructions: Arc::new(instructions),
            constants: Arc::new(constants),
            home_globals: None, // Main module uses VM.globals directly
            param_count: 0,
            param_ownership: Vec::new(),
            reg_count,
            upvalues: Vec::new(),
        };
        let main_fiber = Fiber::new_main(main_func);
        let globals = vec![Value::Void; 1024];

        Self {
            globals,
            fibers: VecDeque::new(),
            current_fiber: Some(main_fiber),
            custom_methods: Arc::new(Mutex::new(std::collections::HashMap::new())),
            last_value: None,
        }
    }

    pub fn new_from_fiber(fiber: Fiber) -> Self {
        Self {
            globals: vec![Value::Void; 1024],
            fibers: VecDeque::new(),
            current_fiber: Some(fiber),
            custom_methods: Arc::new(Mutex::new(std::collections::HashMap::new())),
            last_value: None,
        }
    }

    pub fn run(&mut self) {
        while let Some(mut fiber) = self.current_fiber.take() {
            let status = fiber.run_step(&mut self.globals, &self.custom_methods);
            match status {
                FiberStatus::Running => {
                    self.current_fiber = Some(fiber);
                }
                FiberStatus::Suspended => {
                    self.fibers.push_back(fiber);
                    self.current_fiber = self.fibers.pop_front();
                }
                FiberStatus::Finished => {
                    self.last_value = fiber.return_value.clone();
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
