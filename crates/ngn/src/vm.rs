use crate::bytecode::OpCode;
use crate::value::{Channel, Closure, EnumData, Function, ObjectData, Value};
use crate::blocking_pool::{global_blocking_pool, global_cpu_pool};
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

#[derive(Debug, Clone, Copy)]
pub struct GlobalSlotMeta {
    pub is_defined: bool,
    pub is_mutable: bool,
    pub is_initialized: bool,
}

impl Default for GlobalSlotMeta {
    fn default() -> Self {
        Self {
            is_defined: false,
            is_mutable: false,
            is_initialized: false,
        }
    }
}

impl GlobalSlotMeta {
    pub fn frozen_initialized() -> Self {
        Self {
            is_defined: true,
            is_mutable: false,
            is_initialized: true,
        }
    }
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
    Sleeping(u64),    // Fiber wants to sleep for N milliseconds
    Panicked(String), // User-level panic from ngn code
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
        global_meta: &mut Vec<GlobalSlotMeta>,
        custom_methods: &Arc<
            Mutex<std::collections::HashMap<String, std::collections::HashMap<String, Value>>>,
        >,
        max_steps: usize,
    ) -> (FiberStatus, usize) {
        for step in 0..max_steps {
            let status = self.run_step(globals, global_meta, custom_methods);
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
        global_meta: &mut Vec<GlobalSlotMeta>,
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
                match val {
                    Value::Bool(b) => {
                        self.set_reg_at(dest, Value::Bool(!b));
                    }
                    // !null returns true, !Value(x) returns false
                    Value::Enum(ref e) if e.enum_name == "Maybe" => {
                        let is_null = e.variant_name == "Null";
                        self.set_reg_at(dest, Value::Bool(is_null));
                    }
                    _ => {
                        panic!(
                            "Runtime Error: Not expects boolean or Maybe type, got {:?}",
                            val
                        );
                    }
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
                // Mark global slot metadata for runtime enforcement
                if global_meta.len() <= _idx {
                    global_meta.resize(_idx + 1, GlobalSlotMeta::default());
                }
                global_meta[_idx] = GlobalSlotMeta {
                    is_defined: true,
                    is_mutable: _is_mutable,
                    is_initialized: false,
                };
            }
            OpCode::AssignGlobal(idx, src) => {
                // Module globals are currently stored as Arc<Vec<Value>> (read-only snapshots).
                // Writing into them would either be impossible or would accidentally write into
                // the caller's globals. Since the language doesn't allow `var` at module scope
                // (only `global`, which is immutable), treat any AssignGlobal under home_globals
                // as a runtime error.
                if self.home_globals.is_some() {
                    panic!(
                        "Runtime Error: Cannot assign to module globals (slot {})",
                        idx
                    );
                }

                let val = self.get_reg_at(src);

                if global_meta.len() <= idx {
                    global_meta.resize(idx + 1, GlobalSlotMeta::default());
                }

                if !global_meta[idx].is_defined {
                    panic!(
                        "Runtime Error: Cannot assign to undefined global (slot {})",
                        idx
                    );
                }

                if global_meta[idx].is_initialized && !global_meta[idx].is_mutable {
                    panic!(
                        "Runtime Error: Cannot assign to immutable global (slot {})",
                        idx
                    );
                }

                globals[idx] = val;
                global_meta[idx].is_initialized = true;
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
                // Access globals from the function's home module (or main globals if None)
                let val = if let Some(ref home_globals) = self.home_globals {
                    home_globals[idx].clone()
                } else {
                    globals[idx].clone()
                };
                match val {
                    Value::NativeFunction(id) => {
                        // Collect arguments
                        let mut args = Vec::new();
                        for i in 0..arg_count {
                            args.push(self.get_reg_at(arg_start + i as u16));
                        }

                        // Handle serve specially - it blocks forever and needs thread-safe globals
                        if id == 4 {
                            if args.is_empty() {
                                panic!(
                                    "Runtime Error: serve requires at least 1 argument (handler)"
                                );
                            }

                            let handler = args
                                .get(0)
                                .cloned()
                                .expect("Runtime Error: serve requires a handler function");

                            // Optional config in the 2nd argument position:
                            // - serve(handler)
                            // - serve(handler, { port: 3000, tls: { cert: "./cert.pem", key: "./key.pem" } })
                            let mut port: u16 = 3000;
                            let mut keep_alive_timeout_ms: u64 = 30_000;
                            let mut max_requests_per_connection: usize = 1000;
                            let mut tls_cert: Option<String> = None;
                            let mut tls_key: Option<String> = None;

                            if let Some(Value::Object(cfg)) = args.get(1) {
                                if let Some(Value::Numeric(n)) = cfg.fields.get("port") {
                                    port = match n {
                                        crate::value::Number::I64(v) => *v as u16,
                                        crate::value::Number::I32(v) => *v as u16,
                                        _ => port,
                                    };
                                }

                                if let Some(Value::Numeric(n)) =
                                    cfg.fields.get("keepAliveTimeoutMs")
                                {
                                    keep_alive_timeout_ms = match n {
                                        crate::value::Number::I64(v) if *v > 0 => *v as u64,
                                        crate::value::Number::I32(v) if *v > 0 => *v as u64,
                                        _ => keep_alive_timeout_ms,
                                    };
                                }

                                if let Some(Value::Numeric(n)) =
                                    cfg.fields.get("maxRequestsPerConnection")
                                {
                                    max_requests_per_connection = match n {
                                        crate::value::Number::I64(v) if *v > 0 => *v as usize,
                                        crate::value::Number::I32(v) if *v > 0 => *v as usize,
                                        _ => max_requests_per_connection,
                                    };
                                }

                                if let Some(Value::Object(tls)) = cfg.fields.get("tls") {
                                    tls_cert = tls.fields.get("cert").and_then(|v| {
                                        if let Value::String(s) = v {
                                            Some(s.clone())
                                        } else {
                                            None
                                        }
                                    });
                                    tls_key = tls.fields.get("key").and_then(|v| {
                                        if let Value::String(s) = v {
                                            Some(s.clone())
                                        } else {
                                            None
                                        }
                                    });
                                }
                            }

                            // Wrap globals and custom_methods in Arc<Mutex> for thread safety
                            let shared_globals = Arc::new(Mutex::new(globals.clone()));
                            let shared_methods = custom_methods.clone();

                            let options = crate::toolbox::http::HttpServerOptions {
                                keep_alive_timeout: std::time::Duration::from_millis(
                                    keep_alive_timeout_ms,
                                ),
                                max_requests_per_connection,
                            };

                            let has_tls = tls_cert.is_some() || tls_key.is_some();

                            if let (Some(cert), Some(key)) = (&tls_cert, &tls_key) {
                                if let Err(e) = crate::toolbox::http::serve_tls_with_options(
                                    port,
                                    handler,
                                    cert,
                                    key,
                                    options,
                                    shared_globals,
                                    shared_methods,
                                ) {
                                    panic!("Runtime Error: {}", e);
                                }
                            } else if has_tls {
                                panic!("Runtime Error: tls config requires both cert and key");
                            } else {
                                if let Err(e) = crate::toolbox::http::serve_with_options(
                                    port,
                                    handler,
                                    options,
                                    shared_globals,
                                    shared_methods,
                                ) {
                                    panic!("Runtime Error: {}", e);
                                }
                            }

                            self.set_reg_at(dest, Value::Void);
                            return FiberStatus::Finished;
                        }

                        // Dispatch to the appropriate toolbox function
                        let result = match id {
                            1 => crate::toolbox::math::abs(args),             // NATIVE_ABS
                            2 => crate::toolbox::test::assert(args),          // NATIVE_ASSERT
                            3 => crate::toolbox::math::round(args),           // NATIVE_ROUND
                            6 => crate::toolbox::io::file_read(args),         // NATIVE_FILE_READ
                            7 => crate::toolbox::io::file_write(args),        // NATIVE_FILE_WRITE
                            8 => crate::toolbox::io::file_append(args),       // NATIVE_FILE_APPEND
                            9 => crate::toolbox::io::file_exists(args),       // NATIVE_FILE_EXISTS
                            5 => crate::toolbox::encoding::hex_encode(args),  // NATIVE_HEX_ENCODE
                            10 => crate::toolbox::io::file_delete(args),      // NATIVE_FILE_DELETE
                            11 => crate::toolbox::encoding::hex_decode(args), // NATIVE_HEX_DECODE
                            12 => crate::toolbox::encoding::base64_encode(args), // NATIVE_BASE64_ENCODE
                            13 => crate::toolbox::encoding::base64_decode(args), // NATIVE_BASE64_DECODE
                            14 => crate::toolbox::process::process_run(args), // NATIVE_PROCESS_RUN
                            15 => crate::toolbox::process::process_stream(args), // NATIVE_PROCESS_STREAM
                            16 => crate::toolbox::process::process_stream_raw(args), // NATIVE_PROCESS_STREAM_RAW
                            17 => crate::toolbox::llm::llm_load(args), // NATIVE_LLM_LOAD
                            18 => crate::toolbox::llm::llm_generate(args), // NATIVE_LLM_GENERATE
                            19 => crate::toolbox::llm::llm_stream(args), // NATIVE_LLM_STREAM
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
                    // Return Sleeping status - fiber runner handles async sleep
                    return FiberStatus::Sleeping(ms as u64);
                }
            }
            OpCode::Panic(src) => {
                let val = self.get_reg_at(src);
                // Return Panicked status - VM run loop handles it gracefully
                return FiberStatus::Panicked(format!("{}", val));
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
                        Value::Set(items) => {
                            if let Some(val) = items.get_index(idx as usize) {
                                self.set_reg_at(dest, val.clone());
                                self.set_reg_at(
                                    iter_reg + 1,
                                    Value::Numeric(crate::value::Number::I64(idx + 1)),
                                );
                            } else {
                                self.ip = jump_offset;
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
                    (Value::Bytes(b), Value::Numeric(crate::value::Number::I64(idx))) => {
                        if idx < 0 || idx >= b.len() as i64 {
                            panic!("Runtime Error: Bytes index out of bounds: {}", idx);
                        }
                        let byte = b[idx as usize];
                        self.set_reg_at(dest, Value::Numeric(crate::value::Number::U8(byte)));
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
            OpCode::SpawnCpu(dest, task_reg) => {
                let task_val = self.get_reg_at(task_reg);
                let chan = Channel {
                    name: "spawn_cpu_result".to_string(),
                    buffer: Arc::new(Mutex::new(VecDeque::new())),
                    capacity: 1,
                    is_closed: Arc::new(Mutex::new(false)),
                };

                let closure = match task_val {
                    Value::Closure(c) => c,
                    Value::Function(f) => Box::new(Closure {
                        function: f,
                        upvalues: Vec::new(),
                    }),
                    _ => {
                    let err = EnumData::into_value(
                        "Result".to_string(),
                        "Error".to_string(),
                        Some(Box::new(Value::String(
                            "spawn.cpu() expects a function or closure".to_string(),
                        ))),
                    );
                    chan.buffer.lock().unwrap().push_back(err);
                    *chan.is_closed.lock().unwrap() = true;
                    self.set_reg_at(dest, Value::Channel(chan));
                    return FiberStatus::Running;
                    }
                };
                let chan_clone = chan.clone();
                let globals_clone = globals.clone();
                let custom_methods_clone = custom_methods.clone();

                let submit = global_cpu_pool().try_submit(Box::new(move || {
                    let out = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        let mut fiber = Fiber::new(closure);
                        let mut thread_globals = globals_clone;
                        let mut thread_meta =
                            vec![GlobalSlotMeta::frozen_initialized(); thread_globals.len()];

                        loop {
                            let status = fiber.run_step(
                                &mut thread_globals,
                                &mut thread_meta,
                                &custom_methods_clone,
                            );
                            match status {
                                FiberStatus::Finished => {
                                    return fiber.return_value.unwrap_or(Value::Void);
                                }
                                FiberStatus::Running => continue,
                                _ => break,
                            }
                        }
                        Value::Void
                    }));

                    let val = match out {
                        Ok(v) => match &v {
                            Value::Enum(e) if e.enum_name == "Result" => v,
                            _ => EnumData::into_value(
                                "Result".to_string(),
                                "Ok".to_string(),
                                Some(Box::new(v)),
                            ),
                        },
                        Err(panic_info) => {
                            let msg = if let Some(s) = panic_info.downcast_ref::<&str>() {
                                s.to_string()
                            } else if let Some(s) = panic_info.downcast_ref::<String>() {
                                s.clone()
                            } else {
                                "Unknown panic".to_string()
                            };
                            EnumData::into_value(
                                "Result".to_string(),
                                "Error".to_string(),
                                Some(Box::new(Value::String(format!(
                                    "Thread panicked: {}",
                                    msg
                                )))),
                            )
                        }
                    };

                    chan_clone.buffer.lock().unwrap().push_back(val);
                    *chan_clone.is_closed.lock().unwrap() = true;
                }));

                if submit.is_err() {
                    let err = EnumData::into_value(
                        "Result".to_string(),
                        "Error".to_string(),
                        Some(Box::new(Value::String(
                            "spawn.cpu() queue is full".to_string(),
                        ))),
                    );
                    chan.buffer.lock().unwrap().push_back(err);
                    *chan.is_closed.lock().unwrap() = true;
                }

                self.set_reg_at(dest, Value::Channel(chan));
            }
            OpCode::SpawnBlock(dest, task_reg) => {
                let task_val = self.get_reg_at(task_reg);
                let chan = Channel {
                    name: "spawn_block_result".to_string(),
                    buffer: Arc::new(Mutex::new(VecDeque::new())),
                    capacity: 1,
                    is_closed: Arc::new(Mutex::new(false)),
                };

                let closure = match task_val {
                    Value::Closure(c) => c,
                    Value::Function(f) => Box::new(Closure {
                        function: f,
                        upvalues: Vec::new(),
                    }),
                    _ => {
                    let err = EnumData::into_value(
                        "Result".to_string(),
                        "Error".to_string(),
                        Some(Box::new(Value::String(
                            "spawn.block() expects a function or closure".to_string(),
                        ))),
                    );
                    chan.buffer.lock().unwrap().push_back(err);
                    *chan.is_closed.lock().unwrap() = true;
                    self.set_reg_at(dest, Value::Channel(chan));
                    return FiberStatus::Running;
                    }
                };
                let chan_clone = chan.clone();
                let globals_clone = globals.clone();
                let custom_methods_clone = custom_methods.clone();

                let submit = global_blocking_pool().try_submit(Box::new(move || {
                    let out = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        let mut fiber = Fiber::new(closure);
                        let mut thread_globals = globals_clone;
                        let mut thread_meta =
                            vec![GlobalSlotMeta::frozen_initialized(); thread_globals.len()];

                        loop {
                            let status = fiber.run_step(
                                &mut thread_globals,
                                &mut thread_meta,
                                &custom_methods_clone,
                            );
                            match status {
                                FiberStatus::Finished => {
                                    return fiber.return_value.unwrap_or(Value::Void);
                                }
                                FiberStatus::Running => continue,
                                _ => break,
                            }
                        }
                        Value::Void
                    }));

                    let val = match out {
                        Ok(v) => match &v {
                            Value::Enum(e) if e.enum_name == "Result" => v,
                            _ => EnumData::into_value(
                                "Result".to_string(),
                                "Ok".to_string(),
                                Some(Box::new(v)),
                            ),
                        },
                        Err(panic_info) => {
                            let msg = if let Some(s) = panic_info.downcast_ref::<&str>() {
                                s.to_string()
                            } else if let Some(s) = panic_info.downcast_ref::<String>() {
                                s.clone()
                            } else {
                                "Unknown panic".to_string()
                            };
                            EnumData::into_value(
                                "Result".to_string(),
                                "Error".to_string(),
                                Some(Box::new(Value::String(format!(
                                    "Thread panicked: {}",
                                    msg
                                )))),
                            )
                        }
                    };

                    chan_clone.buffer.lock().unwrap().push_back(val);
                    *chan_clone.is_closed.lock().unwrap() = true;
                }));

                if submit.is_err() {
                    let err = EnumData::into_value(
                        "Result".to_string(),
                        "Error".to_string(),
                        Some(Box::new(Value::String(
                            "spawn.block() queue is full".to_string(),
                        ))),
                    );
                    chan.buffer.lock().unwrap().push_back(err);
                    *chan.is_closed.lock().unwrap() = true;
                }

                self.set_reg_at(dest, Value::Channel(chan));
            }
            OpCode::Fetch(dest, url_reg, options_reg) => {
                let url_val = self.get_reg_at(url_reg);
                if let Value::String(url) = url_val {
                    // Parse options if provided
                    let mut method = "GET".to_string();
                    let mut body: Option<String> = None;
                    let mut headers: Vec<(String, String)> = Vec::new();
                    let mut timeout_ms: u64 = 10000; // Default 10s

                    if options_reg != u16::MAX {
                        let options_val = self.get_reg_at(options_reg);
                        if let Value::Object(opts) = options_val {
                            // method
                            if let Some(Value::String(m)) = opts.fields.get("method") {
                                method = m.to_uppercase();
                            }
                            // body
                            if let Some(Value::String(b)) = opts.fields.get("body") {
                                body = Some(b.clone());
                            }
                            // timeout
                            if let Some(Value::Numeric(n)) = opts.fields.get("timeout") {
                                timeout_ms = match n {
                                    crate::value::Number::I64(v) => *v as u64,
                                    crate::value::Number::I32(v) => *v as u64,
                                    crate::value::Number::U64(v) => *v,
                                    _ => 30000,
                                };
                            }
                            // headers (accepts Map<String, String> or object literal)
                            match opts.fields.get("headers") {
                                Some(Value::Map(h)) => {
                                    for (k, v) in h {
                                        if let (Value::String(key), Value::String(val)) = (k, v) {
                                            headers.push((key.clone(), val.clone()));
                                        }
                                    }
                                }
                                Some(Value::Object(obj)) => {
                                    for (key, val) in &obj.fields {
                                        if let Value::String(v) = val {
                                            headers.push((key.clone(), v.clone()));
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }

                    // Create a channel for the result
                    let chan = Channel {
                        name: "fetch_result".to_string(),
                        buffer: Arc::new(Mutex::new(VecDeque::new())),
                        capacity: 1,
                        is_closed: Arc::new(Mutex::new(false)),
                    };

                    let chan_clone = chan.clone();

                    // Run fetch on bounded blocking pool.
                    let submit = global_blocking_pool().try_submit(Box::new(move || {
                        let client = reqwest::blocking::Client::builder()
                            .timeout(std::time::Duration::from_millis(timeout_ms))
                            .build()
                            .unwrap_or_else(|_| reqwest::blocking::Client::new());

                        let mut request = match method.as_str() {
                            "GET" => client.get(&url),
                            "POST" => client.post(&url),
                            "PUT" => client.put(&url),
                            "DELETE" => client.delete(&url),
                            "PATCH" => client.patch(&url),
                            "HEAD" => client.head(&url),
                            "OPTIONS" => client.request(reqwest::Method::OPTIONS, &url),
                            _ => client.get(&url),
                        };

                        // Add headers
                        for (key, val) in headers {
                            request = request.header(&key, &val);
                        }

                        // Add body if present
                        if let Some(b) = body {
                            request = request.body(b);
                        }

                        let result = match request.send() {
                            Ok(response) => {
                                // Capture response metadata before consuming body
                                let status = response.status().as_u16();
                                let status_text = response
                                    .status()
                                    .canonical_reason()
                                    .unwrap_or("Unknown")
                                    .to_string();

                                // Convert headers to HashMap (lowercase keys for case-insensitivity)
                                let mut resp_headers = std::collections::HashMap::new();
                                for (key, val) in response.headers() {
                                    if let Ok(val_str) = val.to_str() {
                                        resp_headers.insert(
                                            key.as_str().to_lowercase(),
                                            val_str.to_string(),
                                        );
                                    }
                                }

                                // Get body as text
                                let body = response.text().unwrap_or_default();

                                crate::value::ResponseData::new(
                                    status,
                                    status_text,
                                    resp_headers,
                                    body,
                                )
                                .into_value()
                            }
                            Err(e) => {
                                // Return error as a Response with status 0
                                crate::value::ResponseData::new(
                                    0,
                                    format!("Error: {}", e),
                                    std::collections::HashMap::new(),
                                    String::new(),
                                )
                                .into_value()
                            }
                        };
                        chan_clone.buffer.lock().unwrap().push_back(result);

                        *chan_clone.is_closed.lock().unwrap() = true;
                    }));

                    if submit.is_err() {
                        let result = crate::value::ResponseData::new(
                            0,
                            "Error: fetch() queue is full".to_string(),
                            std::collections::HashMap::new(),
                            String::new(),
                        )
                        .into_value();
                        chan.buffer.lock().unwrap().push_back(result);
                        *chan.is_closed.lock().unwrap() = true;
                    }

                    self.set_reg_at(dest, Value::Channel(chan));
                } else {
                    panic!("Runtime Error: fetch() expects a string URL");
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
                self.set_reg_at(dest, Value::Set(indexmap::IndexSet::new()));
            }
            OpCode::CreateState(dest, initial_reg) => {
                let initial = self.get_reg_at(initial_reg);
                let state = Value::State(Arc::new(Mutex::new(initial)));
                self.set_reg_at(dest, state);
            }
            OpCode::CreateBytes(dest, arg_reg) => {
                if arg_reg == u16::MAX {
                    self.set_reg_at(dest, Value::Bytes(Arc::new(Vec::new())));
                } else {
                    let arg = self.get_reg_at(arg_reg);
                    match arg {
                        Value::String(s) => {
                            self.set_reg_at(dest, Value::Bytes(Arc::new(s.into_bytes())));
                        }
                        Value::Array(items) => {
                            let mut out = Vec::with_capacity(items.len());
                            for v in items {
                                match v {
                                    Value::Numeric(n) => {
                                        let x: i64 = match n {
                                            crate::value::Number::I64(v) => v,
                                            crate::value::Number::I32(v) => v as i64,
                                            crate::value::Number::I16(v) => v as i64,
                                            crate::value::Number::I8(v) => v as i64,
                                            crate::value::Number::U64(v) => v as i64,
                                            crate::value::Number::U32(v) => v as i64,
                                            crate::value::Number::U16(v) => v as i64,
                                            crate::value::Number::U8(v) => v as i64,
                                            crate::value::Number::F64(_)
                                            | crate::value::Number::F32(_) => {
                                                panic!(
                                                    "Runtime Error: bytes(array) expects array<u8>"
                                                )
                                            }
                                        };
                                        if x < 0 || x > 255 {
                                            panic!("Runtime Error: bytes(array) expects array<u8>");
                                        }
                                        out.push(x as u8);
                                    }
                                    _ => panic!("Runtime Error: bytes(array) expects array<u8>"),
                                }
                            }
                            self.set_reg_at(dest, Value::Bytes(Arc::new(out)));
                        }
                        _ => {
                            panic!("Runtime Error: bytes() expects a string or array<u8> argument")
                        }
                    }
                }
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
                    // arg_start must be relative to current fp, not absolute stack index
                    let arg_start = (self.stack.len() - self.fp) as u16;
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

                // Add defaults for Response models
                if model_name == "Response" {
                    // status defaults to 200
                    if !fields.contains_key("status") {
                        fields.insert(
                            "status".to_string(),
                            Value::Numeric(crate::value::Number::I64(200)),
                        );
                    }
                    // statusText defaults to ""
                    if !fields.contains_key("statusText") {
                        fields.insert("statusText".to_string(), Value::String(String::new()));
                    }
                    // headers defaults to empty map
                    if !fields.contains_key("headers") {
                        fields.insert(
                            "headers".to_string(),
                            Value::Map(std::collections::HashMap::new()),
                        );
                    }
                    // body defaults to ""
                    if !fields.contains_key("body") {
                        fields.insert("body".to_string(), Value::String(String::new()));
                    }
                    // ok is calculated from status
                    if !fields.contains_key("ok") {
                        let status = match fields.get("status") {
                            Some(Value::Numeric(crate::value::Number::I64(s))) => *s as u16,
                            _ => 200,
                        };
                        let ok = status >= 200 && status < 300;
                        fields.insert("ok".to_string(), Value::Bool(ok));
                    }
                }

                // Add defaults for SseResponse models
                if model_name == "SseResponse" {
                    if !fields.contains_key("status") {
                        fields.insert(
                            "status".to_string(),
                            Value::Numeric(crate::value::Number::I64(200)),
                        );
                    }
                    if !fields.contains_key("headers") {
                        fields.insert(
                            "headers".to_string(),
                            Value::Map(std::collections::HashMap::new()),
                        );
                    }
                    if !fields.contains_key("keepAliveMs") {
                        fields.insert(
                            "keepAliveMs".to_string(),
                            Value::Numeric(crate::value::Number::I64(0)),
                        );
                    }
                }

                // Add defaults for WebSocketResponse models
                if model_name == "WebSocketResponse" {
                    if !fields.contains_key("headers") {
                        fields.insert(
                            "headers".to_string(),
                            Value::Map(std::collections::HashMap::new()),
                        );
                    }
                }

                // Handle StreamingResponse specially - create a StreamingResponse Value
                if model_name == "StreamingResponse" {
                    let status = match fields.get("status") {
                        Some(Value::Numeric(crate::value::Number::I64(s))) => *s as u16,
                        Some(Value::Numeric(crate::value::Number::I32(s))) => *s as u16,
                        _ => 200, // Default status
                    };

                    let headers = match fields.get("headers") {
                        Some(Value::Map(m)) => {
                            let mut h = std::collections::HashMap::new();
                            for (k, v) in m {
                                if let (Value::String(key), Value::String(val)) = (k, v) {
                                    h.insert(key.clone(), val.clone());
                                }
                            }
                            h
                        }
                        Some(Value::Object(o)) => {
                            let mut h = std::collections::HashMap::new();
                            for (key, val) in &o.fields {
                                if let Value::String(v) = val {
                                    h.insert(key.clone(), v.clone());
                                }
                            }
                            h
                        }
                        _ => std::collections::HashMap::new(),
                    };

                    let body_channel = match fields.get("body") {
                        Some(Value::Channel(c)) => c.clone(),
                        _ => panic!("Runtime Error: StreamingResponse body must be a channel"),
                    };

                    let streaming =
                        crate::value::StreamingResponseData::new(status, headers, body_channel);
                    self.set_reg_at(dest, streaming.into_value());
                    return FiberStatus::Running;
                }

                // Handle SseResponse specially - create an SseResponse Value
                if model_name == "SseResponse" {
                    let status = match fields.get("status") {
                        Some(Value::Numeric(crate::value::Number::I64(s))) => *s as u16,
                        Some(Value::Numeric(crate::value::Number::I32(s))) => *s as u16,
                        _ => 200,
                    };

                    let headers = match fields.get("headers") {
                        Some(Value::Map(m)) => {
                            let mut h = std::collections::HashMap::new();
                            for (k, v) in m {
                                if let (Value::String(key), Value::String(val)) = (k, v) {
                                    h.insert(key.clone(), val.clone());
                                }
                            }
                            h
                        }
                        Some(Value::Object(o)) => {
                            let mut h = std::collections::HashMap::new();
                            for (key, val) in &o.fields {
                                if let Value::String(v) = val {
                                    h.insert(key.clone(), v.clone());
                                }
                            }
                            h
                        }
                        _ => std::collections::HashMap::new(),
                    };

                    let body_channel = match fields.get("body") {
                        Some(Value::Channel(c)) => c.clone(),
                        _ => panic!("Runtime Error: SseResponse body must be a channel"),
                    };

                    let keep_alive_ms = match fields.get("keepAliveMs") {
                        Some(Value::Numeric(crate::value::Number::I64(ms))) if *ms > 0 => {
                            *ms as u64
                        }
                        Some(Value::Numeric(crate::value::Number::I32(ms))) if *ms > 0 => {
                            *ms as u64
                        }
                        _ => 0,
                    };

                    let sse = crate::value::SseResponseData::new(
                        status,
                        headers,
                        body_channel,
                        keep_alive_ms,
                    );
                    self.set_reg_at(dest, sse.into_value());
                    return FiberStatus::Running;
                }

                // Handle WebSocketResponse specially - create a WebSocketResponse Value
                if model_name == "WebSocketResponse" {
                    let headers = match fields.get("headers") {
                        Some(Value::Map(m)) => {
                            let mut h = std::collections::HashMap::new();
                            for (k, v) in m {
                                if let (Value::String(key), Value::String(val)) = (k, v) {
                                    h.insert(key.clone(), val.clone());
                                }
                            }
                            h
                        }
                        Some(Value::Object(o)) => {
                            let mut h = std::collections::HashMap::new();
                            for (key, val) in &o.fields {
                                if let Value::String(v) = val {
                                    h.insert(key.clone(), v.clone());
                                }
                            }
                            h
                        }
                        _ => std::collections::HashMap::new(),
                    };

                    let recv_channel = match fields.get("recv") {
                        Some(Value::Channel(c)) => c.clone(),
                        _ => panic!("Runtime Error: WebSocketResponse recv must be a channel"),
                    };

                    let send_channel = match fields.get("send") {
                        Some(Value::Channel(c)) => c.clone(),
                        _ => panic!("Runtime Error: WebSocketResponse send must be a channel"),
                    };

                    let ws = crate::value::WebSocketResponseData::new(
                        headers,
                        recv_channel,
                        send_channel,
                    );
                    self.set_reg_at(dest, ws.into_value());
                    return FiberStatus::Running;
                }

                self.set_reg_at(dest, ObjectData::into_value(model_name, fields));
            }
            OpCode::GetField(dest, obj_reg, field_idx) => {
                let obj = self.get_reg_at(obj_reg);
                let field_name = self.expect_string_const(field_idx);
                match obj {
                    Value::Object(o) => {
                        if let Some(val) = o.fields.get(&field_name) {
                            self.set_reg_at(dest, val.clone());
                        } else {
                            panic!("Runtime Error: Field '{}' not found in object", field_name);
                        }
                    }
                    Value::Response(r) => {
                        // Handle Response properties
                        let val = match field_name.as_str() {
                            "status" => Value::Numeric(crate::value::Number::I64(r.status as i64)),
                            "statusText" => Value::String(r.status_text.clone()),
                            "ok" => Value::Bool(r.ok),
                            "body" => Value::String(r.body.clone()),
                            "headers" => {
                                // Return headers as a Map so .get() works
                                let mut map = std::collections::HashMap::new();
                                for (k, v) in r.headers.iter() {
                                    map.insert(Value::String(k.clone()), Value::String(v.clone()));
                                }
                                Value::Map(map)
                            }
                            _ => panic!("Runtime Error: Response has no field '{}'", field_name),
                        };
                        self.set_reg_at(dest, val);
                    }
                    _ => panic!(
                        "Runtime Error: GetField on non-object type: {:?}",
                        obj.type_name()
                    ),
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
            OpCode::ServeHttp(handler_idx) => {
                // Get the handler object from globals
                let handler = globals[handler_idx].clone();

                // Extract config from handler.config field
                // Supported config keys:
                // - port: i64
                // - keepAliveTimeoutMs: i64
                // - maxRequestsPerConnection: i64
                // - tls: { cert: string, key: string }
                let (port, tls_cert, tls_key, keep_alive_timeout_ms, max_requests_per_connection) =
                    if let Value::Object(obj) = &handler {
                        if let Some(Value::Object(config)) = obj.fields.get("config") {
                            let port = match config.fields.get("port") {
                                Some(Value::Numeric(n)) => match n {
                                    crate::value::Number::I64(v) => *v as u16,
                                    crate::value::Number::I32(v) => *v as u16,
                                    _ => 3000,
                                },
                                _ => 3000,
                            };

                            let keep_alive_timeout_ms = match config
                                .fields
                                .get("keepAliveTimeoutMs")
                            {
                                Some(Value::Numeric(crate::value::Number::I64(v))) if *v > 0 => {
                                    *v as u64
                                }
                                Some(Value::Numeric(crate::value::Number::I32(v))) if *v > 0 => {
                                    *v as u64
                                }
                                _ => 30_000,
                            };

                            let max_requests_per_connection = match config
                                .fields
                                .get("maxRequestsPerConnection")
                            {
                                Some(Value::Numeric(crate::value::Number::I64(v))) if *v > 0 => {
                                    *v as usize
                                }
                                Some(Value::Numeric(crate::value::Number::I32(v))) if *v > 0 => {
                                    *v as usize
                                }
                                _ => 1000,
                            };

                            let (cert, key) = match config.fields.get("tls") {
                                Some(Value::Object(tls)) => {
                                    let cert = tls.fields.get("cert").and_then(|v| {
                                        if let Value::String(s) = v {
                                            Some(s.clone())
                                        } else {
                                            None
                                        }
                                    });
                                    let key = tls.fields.get("key").and_then(|v| {
                                        if let Value::String(s) = v {
                                            Some(s.clone())
                                        } else {
                                            None
                                        }
                                    });
                                    (cert, key)
                                }
                                _ => (None, None),
                            };
                            (
                                port,
                                cert,
                                key,
                                keep_alive_timeout_ms,
                                max_requests_per_connection,
                            )
                        } else {
                            (3000, None, None, 30_000, 1000)
                        }
                    } else {
                        (3000, None, None, 30_000, 1000)
                    };

                let fetch_handler = if let Value::Object(obj) = &handler {
                    if !obj.model_name.is_empty() && obj.model_name != "__anon__" {
                        // Model instance: look up in custom_methods
                        let methods = custom_methods.lock().unwrap();
                        methods
                            .get(&obj.model_name)
                            .and_then(|m| m.get("fetch"))
                            .cloned()
                    } else {
                        // Anonymous object literal: fetch is directly in fields
                        obj.fields.get("fetch").cloned()
                    }
                } else {
                    None
                };

                if let Some(fetch_handler) = fetch_handler {
                    use crate::toolbox::http;

                    let globals_arc = std::sync::Arc::new(std::sync::Mutex::new(globals.clone()));
                    let methods_arc = custom_methods.clone();

                    let options = http::HttpServerOptions {
                        keep_alive_timeout: std::time::Duration::from_millis(keep_alive_timeout_ms),
                        max_requests_per_connection,
                    };

                    if let (Some(cert), Some(key)) = (tls_cert, tls_key) {
                        if let Err(e) = http::serve_tls_with_options(
                            port,
                            fetch_handler,
                            &cert,
                            &key,
                            options,
                            globals_arc,
                            methods_arc,
                        ) {
                            eprintln!("HTTPS Server Error: {}", e);
                        }
                    } else {
                        if let Err(e) = http::serve_with_options(
                            port,
                            fetch_handler,
                            options,
                            globals_arc,
                            methods_arc,
                        ) {
                            eprintln!("HTTP Server Error: {}", e);
                        }
                    }
                } else {
                    eprintln!("ngn Error: No fetch method found on exported handler");
                }

                self.status = FiberStatus::Finished;
                return FiberStatus::Finished;
            }
            OpCode::JsonParse(dest_reg, src_reg) => {
                let src_val = self.get_reg_at(src_reg);
                if let Value::String(json_str) = src_val {
                    match serde_json::from_str::<serde_json::Value>(&json_str) {
                        Ok(json) => {
                            self.set_reg_at(dest_reg, Value::from_json(json));
                        }
                        Err(e) => {
                            eprintln!("JSON parse error: {}", e);
                            self.set_reg_at(dest_reg, Value::Void);
                        }
                    }
                } else {
                    eprintln!("json.parse expects a string argument");
                    self.set_reg_at(dest_reg, Value::Void);
                }
            }
            OpCode::JsonStringify(dest_reg, src_reg) => {
                let src_val = self.get_reg_at(src_reg);
                let json = src_val.to_json();
                let json_str = serde_json::to_string(&json).unwrap_or_else(|_| "null".to_string());
                self.set_reg_at(dest_reg, Value::String(json_str));
            }
            OpCode::CheckMaybeValue(dest_reg, maybe_reg) => {
                // Check if the value is a "success" variant: Maybe::Value or Result::Ok
                let val = self.get_reg_at(maybe_reg);
                let is_success = match val {
                    Value::Enum(ref e) => {
                        (e.enum_name == "Maybe" && e.variant_name == "Value")
                            || (e.enum_name == "Result" && e.variant_name == "Ok")
                    }
                    _ => false,
                };
                self.set_reg_at(dest_reg, Value::Bool(is_success));
            }
            OpCode::UnwrapMaybe(dest_reg, maybe_reg) => {
                // Extract the inner value from Maybe::Value or Result::Ok
                let val = self.get_reg_at(maybe_reg);
                let inner = match val {
                    Value::Enum(ref e)
                        if (e.enum_name == "Maybe" && e.variant_name == "Value")
                            || (e.enum_name == "Result" && e.variant_name == "Ok") =>
                    {
                        e.data.clone().map(|b| *b).unwrap_or(Value::Void)
                    }
                    _ => Value::Void,
                };
                self.set_reg_at(dest_reg, inner);
            }
            OpCode::NullCoalesce(dest_reg, maybe_reg, fallback_reg) => {
                // If maybe_reg is Maybe::Value, unwrap to dest; else use fallback_reg
                let maybe_val = self.get_reg_at(maybe_reg);
                let result = match maybe_val {
                    Value::Enum(ref e) if e.enum_name == "Maybe" && e.variant_name == "Value" => {
                        // Unwrap the Value value
                        e.data.clone().map(|b| *b).unwrap_or(Value::Void)
                    }
                    Value::Enum(ref e) if e.enum_name == "Maybe" && e.variant_name == "Null" => {
                        // Use the fallback value
                        self.get_reg_at(fallback_reg)
                    }
                    _ => {
                        // Not a Maybe - just use the value as-is (or panic for strictness)
                        maybe_val
                    }
                };
                self.set_reg_at(dest_reg, result);
            }
            OpCode::SpawnAll(dest_reg, tasks_reg, options_reg) => {
                // spawn.all() - run all tasks in parallel, return all results
                let tasks = self.get_reg_at(tasks_reg);
                let concurrency = self.get_spawn_concurrency(options_reg);

                if let Value::Array(task_closures) = tasks {
                    let results = self.run_spawn_tasks(
                        &task_closures,
                        concurrency,
                        false,
                        globals,
                        custom_methods,
                    );
                    self.set_reg_at(dest_reg, Value::Array(results));
                } else {
                    panic!("spawn.all() expects an array of tasks");
                }
            }
            OpCode::SpawnTry(dest_reg, tasks_reg, options_reg) => {
                // spawn.try() - run tasks, stop on first error
                let tasks = self.get_reg_at(tasks_reg);
                let concurrency = self.get_spawn_concurrency(options_reg);

                if let Value::Array(task_closures) = tasks {
                    let results = self.run_spawn_tasks(
                        &task_closures,
                        concurrency,
                        true,
                        globals,
                        custom_methods,
                    );
                    self.set_reg_at(dest_reg, Value::Array(results));
                } else {
                    panic!("spawn.try() expects an array of tasks");
                }
            }
            OpCode::SpawnRace(dest_reg, tasks_reg) => {
                // spawn.race() - return first success (or first error if all fail)
                let tasks = self.get_reg_at(tasks_reg);

                if let Value::Array(task_closures) = tasks {
                    let result = self.run_spawn_race(&task_closures, globals, custom_methods);
                    self.set_reg_at(dest_reg, result);
                } else {
                    panic!("spawn.race() expects an array of tasks");
                }
            }
            OpCode::EnvGet(dest_reg, key_reg) => {
                // Get environment variable, returns Maybe<string>
                let key_val = self.get_reg_at(key_reg);
                if let Value::String(key) = key_val {
                    match crate::env::get(&key) {
                        Some(value) => {
                            // Return Maybe::Value(value)
                            self.set_reg_at(
                                dest_reg,
                                EnumData::into_value(
                                    "Maybe".to_string(),
                                    "Value".to_string(),
                                    Some(Box::new(Value::String(value))),
                                ),
                            );
                        }
                        None => {
                            // Return Maybe::Null
                            self.set_reg_at(
                                dest_reg,
                                EnumData::into_value("Maybe".to_string(), "Null".to_string(), None),
                            );
                        }
                    }
                } else {
                    panic!("Runtime Error: env.get() expects a string key");
                }
            }
            OpCode::EnvHas(dest_reg, key_reg) => {
                // Check if environment variable exists, returns bool
                let key_val = self.get_reg_at(key_reg);
                if let Value::String(key) = key_val {
                    let exists = crate::env::has(&key);
                    self.set_reg_at(dest_reg, Value::Bool(exists));
                } else {
                    panic!("Runtime Error: env.has() expects a string key");
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

    fn invoke_function(
        &mut self,
        func: &Function,
        closure: Option<Box<Closure>>,
        dest_reg: Option<u16>,
        arg_start: u16,
        arg_count: u8,
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

        // Wrap provided args in Maybe::Value for params declared with ?
        for i in 0..(arg_count as usize) {
            if i < func.param_is_maybe_wrapped.len() && func.param_is_maybe_wrapped[i] {
                let val = self.get_reg_at(i as u16);
                let wrapped = crate::value::EnumData::into_value(
                    "Maybe".to_string(),
                    "Value".to_string(),
                    Some(Box::new(val)),
                );
                self.set_reg_at(i as u16, wrapped);
            }
        }

        // Fill missing arguments with default values
        for i in (arg_count as usize)..func.param_count {
            if i < func.default_values.len() {
                if let Some(default_val) = &func.default_values[i] {
                    self.set_reg_at(i as u16, default_val.clone());
                }
            }
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
            Value::Bytes(b) => self.bytes_method(b, method, args),
            Value::Tuple(t) => self.tuple_method(&t, method, args),
            Value::Map(map) => self.map_method(map, method, args),
            Value::Set(set) => self.set_method(set, method, args),
            Value::Response(r) => self.response_method(&r, method, args),
            Value::Channel(c) => self.channel_method(&c, method, args),
            Value::Object(ref o) if o.model_name == "Request" => {
                self.request_method(o, method, args)
            }
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
            Value::Bytes(b) => self.bytes_method_mut(b, method, args),
            Value::Map(map) => self.map_method_mut(map, method, args),
            Value::Set(set) => self.set_method_mut(set, method, args),
            _ => panic!(
                "Runtime Error: Cannot call mutating method '{}' on {:?}",
                method, obj
            ),
        }
    }

    /// Extract concurrency limit from spawn options
    fn get_spawn_concurrency(&self, options_reg: u16) -> Option<usize> {
        if options_reg == u16::MAX {
            return None;
        }
        let options = self.get_reg_at(options_reg);
        if let Value::Object(obj) = options {
            if let Some(Value::Numeric(n)) = obj.fields.get("concurrency") {
                let concurrency = match n {
                    crate::value::Number::I64(v) => *v as usize,
                    crate::value::Number::I32(v) => *v as usize,
                    crate::value::Number::U64(v) => *v as usize,
                    crate::value::Number::U32(v) => *v as usize,
                    _ => 0,
                };
                return Some(concurrency);
            }
        }
        None
    }

    /// Run spawn tasks (used by spawn.all and spawn.try)
    fn run_spawn_tasks(
        &self,
        tasks: &[Value],
        _concurrency: Option<usize>,
        fail_fast: bool,
        globals: &mut Vec<Value>,
        custom_methods: &Arc<
            Mutex<std::collections::HashMap<String, std::collections::HashMap<String, Value>>>,
        >,
    ) -> Vec<Value> {
        use std::sync::mpsc;
        use std::thread;

        let mut results: Vec<Value> = Vec::new();
        let _task_count = tasks.len();

        // Create a channel for results
        let (tx, rx) = mpsc::channel::<(usize, Value)>();

        // Collect closures for spawning
        let mut handles = Vec::new();

        for (idx, task) in tasks.iter().enumerate() {
            if let Value::Closure(closure) = task.clone() {
                let tx = tx.clone();
                let closure_clone = closure.clone();
                let globals_clone = globals.clone();
                let custom_methods_clone = custom_methods.clone();

                let handle = thread::spawn(move || {
                    // Run the closure in a new fiber
                    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        let mut fiber = Fiber::new(closure_clone);
                        let mut thread_globals = globals_clone;
                        let mut thread_meta =
                            vec![GlobalSlotMeta::frozen_initialized(); thread_globals.len()];

                        // Run the fiber to completion
                        loop {
                            let status = fiber.run_step(
                                &mut thread_globals,
                                &mut thread_meta,
                                &custom_methods_clone,
                            );
                            match status {
                                FiberStatus::Finished => {
                                    return fiber.return_value.unwrap_or(Value::Void);
                                }
                                FiberStatus::Running => continue,
                                _ => break,
                            }
                        }
                        Value::Void
                    }));

                    match result {
                        Ok(value) => {
                            let _ = tx.send((idx, value));
                        }
                        Err(panic_info) => {
                            let msg = if let Some(s) = panic_info.downcast_ref::<&str>() {
                                s.to_string()
                            } else if let Some(s) = panic_info.downcast_ref::<String>() {
                                s.clone()
                            } else {
                                "Unknown panic".to_string()
                            };
                            let error = crate::value::EnumData::into_value(
                                "Result".to_string(),
                                "Error".to_string(),
                                Some(Box::new(Value::String(format!("Thread panicked: {}", msg)))),
                            );
                            let _ = tx.send((idx, error));
                        }
                    }
                });
                handles.push(handle);
            } else if let Value::Function(func) = task.clone() {
                // Handle raw functions (create closure from them)
                let tx = tx.clone();
                let func_clone = func.clone();
                let globals_clone = globals.clone();
                let custom_methods_clone = custom_methods.clone();

                let handle = thread::spawn(move || {
                    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        let closure = Box::new(Closure {
                            function: func_clone,
                            upvalues: Vec::new(),
                        });
                        let mut fiber = Fiber::new(closure);
                        let mut thread_globals = globals_clone;
                        let mut thread_meta =
                            vec![GlobalSlotMeta::frozen_initialized(); thread_globals.len()];

                        loop {
                            let status = fiber.run_step(
                                &mut thread_globals,
                                &mut thread_meta,
                                &custom_methods_clone,
                            );
                            match status {
                                FiberStatus::Finished => {
                                    return fiber.return_value.unwrap_or(Value::Void);
                                }
                                FiberStatus::Running => continue,
                                _ => break,
                            }
                        }
                        Value::Void
                    }));

                    match result {
                        Ok(value) => {
                            let _ = tx.send((idx, value));
                        }
                        Err(panic_info) => {
                            let msg = if let Some(s) = panic_info.downcast_ref::<&str>() {
                                s.to_string()
                            } else if let Some(s) = panic_info.downcast_ref::<String>() {
                                s.clone()
                            } else {
                                "Unknown panic".to_string()
                            };
                            let error = crate::value::EnumData::into_value(
                                "Result".to_string(),
                                "Error".to_string(),
                                Some(Box::new(Value::String(format!("Thread panicked: {}", msg)))),
                            );
                            let _ = tx.send((idx, error));
                        }
                    }
                });
                handles.push(handle);
            }
        }

        drop(tx); // Close the sender so rx iterator will terminate

        // Collect results in order
        let mut indexed_results: Vec<(usize, Value)> = Vec::new();
        let mut error_found = false;

        for (idx, result) in rx {
            indexed_results.push((idx, result.clone()));

            // Check for error in fail_fast mode
            if fail_fast && !error_found {
                if let Value::Enum(ref e) = result {
                    if e.enum_name == "Result" && e.variant_name == "Error" {
                        error_found = true;
                    }
                }
            }
        }

        // Wait for all threads (they've already finished since rx closed)
        for handle in handles {
            let _ = handle.join();
        }

        // Sort by original index and extract values
        indexed_results.sort_by_key(|(idx, _)| *idx);

        if fail_fast && error_found {
            // Return results up to and including first error
            for (_, val) in indexed_results {
                results.push(val.clone());
                if let Value::Enum(ref e) = val {
                    if e.enum_name == "Result" && e.variant_name == "Error" {
                        break;
                    }
                }
            }
        } else {
            results = indexed_results.into_iter().map(|(_, v)| v).collect();
        }

        results
    }

    /// Run spawn.race - return first success or first error if all fail
    fn run_spawn_race(
        &self,
        tasks: &[Value],
        globals: &mut Vec<Value>,
        custom_methods: &Arc<
            Mutex<std::collections::HashMap<String, std::collections::HashMap<String, Value>>>,
        >,
    ) -> Value {
        use std::sync::mpsc;
        use std::thread;

        let (tx, rx) = mpsc::channel::<Value>();

        for task in tasks.iter() {
            if let Value::Closure(closure) = task.clone() {
                let tx = tx.clone();
                let closure_clone = closure.clone();
                let globals_clone = globals.clone();
                let custom_methods_clone = custom_methods.clone();

                thread::spawn(move || {
                    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        let mut fiber = Fiber::new(closure_clone);
                        let mut thread_globals = globals_clone;
                        let mut thread_meta =
                            vec![GlobalSlotMeta::frozen_initialized(); thread_globals.len()];

                        loop {
                            let status = fiber.run_step(
                                &mut thread_globals,
                                &mut thread_meta,
                                &custom_methods_clone,
                            );
                            match status {
                                FiberStatus::Finished => {
                                    return fiber.return_value.unwrap_or(Value::Void);
                                }
                                FiberStatus::Running => continue,
                                _ => break,
                            }
                        }
                        Value::Void
                    }));

                    match result {
                        Ok(value) => {
                            let _ = tx.send(value);
                        }
                        Err(panic_info) => {
                            let msg = if let Some(s) = panic_info.downcast_ref::<&str>() {
                                s.to_string()
                            } else if let Some(s) = panic_info.downcast_ref::<String>() {
                                s.clone()
                            } else {
                                "Unknown panic".to_string()
                            };
                            let error = crate::value::EnumData::into_value(
                                "Result".to_string(),
                                "Error".to_string(),
                                Some(Box::new(Value::String(format!("Thread panicked: {}", msg)))),
                            );
                            let _ = tx.send(error);
                        }
                    }
                });
            } else if let Value::Function(func) = task.clone() {
                let tx = tx.clone();
                let func_clone = func.clone();
                let globals_clone = globals.clone();
                let custom_methods_clone = custom_methods.clone();

                thread::spawn(move || {
                    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        let closure = Box::new(Closure {
                            function: func_clone,
                            upvalues: Vec::new(),
                        });
                        let mut fiber = Fiber::new(closure);
                        let mut thread_globals = globals_clone;
                        let mut thread_meta =
                            vec![GlobalSlotMeta::frozen_initialized(); thread_globals.len()];

                        loop {
                            let status = fiber.run_step(
                                &mut thread_globals,
                                &mut thread_meta,
                                &custom_methods_clone,
                            );
                            match status {
                                FiberStatus::Finished => {
                                    return fiber.return_value.unwrap_or(Value::Void);
                                }
                                FiberStatus::Running => continue,
                                _ => break,
                            }
                        }
                        Value::Void
                    }));

                    match result {
                        Ok(value) => {
                            let _ = tx.send(value);
                        }
                        Err(panic_info) => {
                            let msg = if let Some(s) = panic_info.downcast_ref::<&str>() {
                                s.to_string()
                            } else if let Some(s) = panic_info.downcast_ref::<String>() {
                                s.clone()
                            } else {
                                "Unknown panic".to_string()
                            };
                            let error = crate::value::EnumData::into_value(
                                "Result".to_string(),
                                "Error".to_string(),
                                Some(Box::new(Value::String(format!("Thread panicked: {}", msg)))),
                            );
                            let _ = tx.send(error);
                        }
                    }
                });
            }
        }

        drop(tx);

        // Look for first success; if all fail return first error
        let mut first_error: Option<Value> = None;

        for result in rx {
            if let Value::Enum(ref e) = result {
                if e.enum_name == "Result" && e.variant_name == "Ok" {
                    return result; // First success wins
                }
                if first_error.is_none() {
                    first_error = Some(result);
                }
            } else {
                // Non-enum result is treated as success
                return crate::value::EnumData::into_value(
                    "Result".to_string(),
                    "Ok".to_string(),
                    Some(Box::new(result)),
                );
            }
        }

        // All failed, return first error
        first_error.unwrap_or_else(|| {
            crate::value::EnumData::into_value(
                "Result".to_string(),
                "Error".to_string(),
                Some(Box::new(Value::String("All tasks failed".to_string()))),
            )
        })
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

    /// Array mutating methods: push, pop, slice, splice
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
            "pop" => {
                let removed = if args.len() > 0 {
                    let idx = self
                        .to_usize(&args[0])
                        .expect("pop() index must be integer");
                    if idx >= arr.len() {
                        panic!("pop() index out of bounds");
                    }
                    arr.remove(idx)
                } else {
                    arr.pop().expect("pop() from empty array")
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

    /// Bytes non-mutating methods: length, copy, toString, toStringStrict
    fn bytes_method(&self, b: Arc<Vec<u8>>, method: &str, args: Vec<Value>) -> Value {
        match method {
            "length" => {
                if !args.is_empty() {
                    panic!("Runtime Error: .length() takes no arguments");
                }
                Value::Numeric(crate::value::Number::I64(b.len() as i64))
            }
            "copy" => {
                let start = if !args.is_empty() {
                    self.to_usize(&args[0]).unwrap_or(0)
                } else {
                    0
                };
                let end = if args.len() > 1 {
                    self.to_usize(&args[1]).unwrap_or(b.len())
                } else {
                    b.len()
                };
                if start > end || end > b.len() {
                    panic!("Range out of bounds");
                }
                Value::Bytes(Arc::new(b[start..end].to_vec()))
            }
            "toString" => {
                if !args.is_empty() {
                    panic!("Runtime Error: .toString() takes no arguments");
                }
                Value::String(String::from_utf8_lossy(&b).to_string())
            }
            "toStringStrict" => {
                if !args.is_empty() {
                    panic!("Runtime Error: .toStringStrict() takes no arguments");
                }
                let s = String::from_utf8(b.as_slice().to_vec())
                    .expect("Runtime Error: Invalid UTF-8 in bytes");
                Value::String(s)
            }
            _ => panic!(
                "Runtime Error: Unknown non-mutating bytes method '{}'",
                method
            ),
        }
    }

    /// Bytes mutating methods: slice
    fn bytes_method_mut(&self, b: Arc<Vec<u8>>, method: &str, args: Vec<Value>) -> (Value, Value) {
        match method {
            "slice" => {
                let start = self
                    .to_usize(&args[0])
                    .expect("slice() start must be integer");
                let end = if args.len() > 1 {
                    self.to_usize(&args[1])
                        .expect("slice() stop must be integer")
                } else {
                    b.len()
                };
                if start > end || end > b.len() {
                    panic!("Range out of bounds");
                }

                let mut owned = b.as_slice().to_vec();
                let removed: Vec<u8> = owned.drain(start..end).collect();
                (
                    Value::Bytes(Arc::new(removed)),
                    Value::Bytes(Arc::new(owned)),
                )
            }
            _ => panic!("Runtime Error: Unknown mutating bytes method '{}'", method),
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
                match map.get(&args[0]) {
                    Some(value) => crate::value::EnumData::into_value(
                        "Maybe".to_string(),
                        "Value".to_string(),
                        Some(Box::new(value.clone())),
                    ),
                    None => crate::value::EnumData::into_value(
                        "Maybe".to_string(),
                        "Null".to_string(),
                        None,
                    ),
                }
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
    fn set_method(&self, set: indexmap::IndexSet<Value>, method: &str, args: Vec<Value>) -> Value {
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

    /// Set mutating methods: add, remove, clear
    fn set_method_mut(
        &self,
        mut set: indexmap::IndexSet<Value>,
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
                let was_removed = set.shift_remove(&args[0]);
                (Value::Bool(was_removed), Value::Set(set))
            }
            "clear" => {
                if !args.is_empty() {
                    panic!("clear() takes no arguments");
                }
                set.clear();
                (Value::Void, Value::Set(set))
            }
            _ => panic!("Runtime Error: Unknown mutating set method '{}'", method),
        }
    }

    /// Request methods: clone(), formData(), text(), json()
    fn request_method(
        &self,
        request: &crate::value::ObjectData,
        method: &str,
        _args: Vec<Value>,
    ) -> Value {
        match method {
            "clone" => {
                // Deep clone the Request object
                Value::Object(Box::new(request.clone()))
            }
            "text" => {
                // Return body as string
                request
                    .fields
                    .get("body")
                    .cloned()
                    .unwrap_or(Value::String(String::new()))
            }
            "json" => {
                // Parse body as JSON and return Result enum
                let body = match request.fields.get("body") {
                    Some(Value::String(s)) => s.clone(),
                    _ => String::new(),
                };
                match serde_json::from_str::<serde_json::Value>(&body) {
                    Ok(json_value) => {
                        let ngn_value = self.json_to_value(json_value);
                        crate::value::EnumData::into_value(
                            "Result".to_string(),
                            "Ok".to_string(),
                            Some(Box::new(ngn_value)),
                        )
                    }
                    Err(e) => crate::value::EnumData::into_value(
                        "Result".to_string(),
                        "Error".to_string(),
                        Some(Box::new(Value::String(format!("JSON parse error: {}", e)))),
                    ),
                }
            }
            "formData" => {
                // Parse application/x-www-form-urlencoded body into Map<string, string>
                let body = match request.fields.get("body") {
                    Some(Value::String(s)) => s.clone(),
                    _ => String::new(),
                };
                let mut form_data: std::collections::HashMap<Value, Value> =
                    std::collections::HashMap::new();
                for pair in body.split('&') {
                    if pair.is_empty() {
                        continue;
                    }
                    if let Some(eq_idx) = pair.find('=') {
                        let key = pair[..eq_idx].to_string();
                        let value = pair[eq_idx + 1..].to_string();
                        // URL decode: replace + with space and decode %XX
                        let key = urlencoding::decode(&key.replace('+', " "))
                            .unwrap_or_else(|_| key.into())
                            .into_owned();
                        let value = urlencoding::decode(&value.replace('+', " "))
                            .unwrap_or_else(|_| value.into())
                            .into_owned();
                        form_data.insert(Value::String(key), Value::String(value));
                    } else {
                        // Key with no value
                        let key = urlencoding::decode(&pair.replace('+', " "))
                            .unwrap_or_else(|_| pair.into())
                            .into_owned();
                        form_data.insert(Value::String(key), Value::String(String::new()));
                    }
                }
                Value::Map(form_data)
            }
            _ => panic!("Runtime Error: Unknown Request method '{}'", method),
        }
    }

    /// Channel methods: close()
    fn channel_method(
        &self,
        channel: &crate::value::Channel,
        method: &str,
        args: Vec<Value>,
    ) -> Value {
        match method {
            "close" => {
                if !args.is_empty() {
                    panic!("Runtime Error: .close() takes no arguments");
                }
                *channel.is_closed.lock().unwrap() = true;
                Value::Void
            }
            _ => panic!("Runtime Error: Unknown Channel method '{}'", method),
        }
    }

    /// Response methods: text(), json()
    fn response_method(
        &self,
        response: &crate::value::ResponseData,
        method: &str,
        _args: Vec<Value>,
    ) -> Value {
        match method {
            "text" => Value::String(response.body.clone()),
            "json" => {
                // Parse body as JSON and return Result enum
                match serde_json::from_str::<serde_json::Value>(&response.body) {
                    Ok(json_value) => {
                        let ngn_value = self.json_to_value(json_value);
                        // Return Result::Ok(data)
                        crate::value::EnumData::into_value(
                            "Result".to_string(),
                            "Ok".to_string(),
                            Some(Box::new(ngn_value)),
                        )
                    }
                    Err(e) => {
                        // Return Result::Error(message)
                        crate::value::EnumData::into_value(
                            "Result".to_string(),
                            "Error".to_string(),
                            Some(Box::new(Value::String(format!("JSON parse error: {}", e)))),
                        )
                    }
                }
            }
            "headers" => {
                // Return a HeadersAccessor object that handles .get()
                // For now, return the headers as an object with a get method
                let mut fields = std::collections::HashMap::new();
                for (k, v) in &response.headers {
                    fields.insert(k.clone(), Value::String(v.clone()));
                }
                crate::value::ObjectData::into_value("__headers__".to_string(), fields)
            }
            _ => panic!("Runtime Error: Unknown Response method '{}'", method),
        }
    }

    /// Convert serde_json::Value to ngn Value
    fn json_to_value(&self, json: serde_json::Value) -> Value {
        match json {
            serde_json::Value::Null => {
                crate::value::EnumData::into_value("Maybe".to_string(), "Null".to_string(), None)
            }
            serde_json::Value::Bool(b) => Value::Bool(b),
            serde_json::Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Value::Numeric(crate::value::Number::I64(i))
                } else if let Some(f) = n.as_f64() {
                    Value::Numeric(crate::value::Number::F64(f))
                } else {
                    Value::Numeric(crate::value::Number::I64(0))
                }
            }
            serde_json::Value::String(s) => Value::String(s),
            serde_json::Value::Array(arr) => {
                Value::Array(arr.into_iter().map(|v| self.json_to_value(v)).collect())
            }
            serde_json::Value::Object(obj) => {
                let mut fields = std::collections::HashMap::new();
                for (k, v) in obj {
                    fields.insert(k, self.json_to_value(v));
                }
                crate::value::ObjectData::into_value("__anon__".to_string(), fields)
            }
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
    pub global_meta: Vec<GlobalSlotMeta>,
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
            param_types: Vec::new(),
            default_values: Vec::new(),
            param_is_maybe_wrapped: Vec::new(),
            return_type: crate::parser::Type::Void,
            reg_count,
            upvalues: Vec::new(),
        };
        let main_fiber = Fiber::new_main(main_func);
        let globals = vec![Value::Void; 1024];
        let global_meta = vec![GlobalSlotMeta::default(); 1024];

        Self {
            globals,
            global_meta,
            fibers: VecDeque::new(),
            current_fiber: Some(main_fiber),
            custom_methods: Arc::new(Mutex::new(std::collections::HashMap::new())),
            last_value: None,
        }
    }

    pub fn new_from_fiber(fiber: Fiber) -> Self {
        Self {
            globals: vec![Value::Void; 1024],
            global_meta: vec![GlobalSlotMeta::default(); 1024],
            fibers: VecDeque::new(),
            current_fiber: Some(fiber),
            custom_methods: Arc::new(Mutex::new(std::collections::HashMap::new())),
            last_value: None,
        }
    }

    pub fn run(&mut self) {
        while let Some(mut fiber) = self.current_fiber.take() {
            // Wrap fiber execution in catch_unwind to handle unexpected Rust panics
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                fiber.run_step(&mut self.globals, &mut self.global_meta, &self.custom_methods)
            }));

            match result {
                Ok(status) => match status {
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
                    FiberStatus::Sleeping(ms) => {
                        // Blocking sleep in main runtime (not async)
                        std::thread::sleep(std::time::Duration::from_millis(ms));
                        self.current_fiber = Some(fiber);
                    }
                    FiberStatus::Panicked(msg) => {
                        // User-level panic from ngn code
                        eprintln!("[ngn] Thread panicked: {}", msg);

                        // Send Error to completion channel if it exists
                        if let Some(chan) = &fiber.completion_channel {
                            let error = crate::value::EnumData::into_value(
                                "Result".to_string(),
                                "Error".to_string(),
                                Some(Box::new(Value::String(format!("Thread panicked: {}", msg)))),
                            );
                            chan.buffer.lock().unwrap().push_back(error);
                        }

                        // Move to next fiber
                        self.current_fiber = self.fibers.pop_front();
                    }
                },
                Err(panic_info) => {
                    // Internal Rust panic (VM bug) - extract message and continue
                    let msg = if let Some(s) = panic_info.downcast_ref::<&str>() {
                        s.to_string()
                    } else if let Some(s) = panic_info.downcast_ref::<String>() {
                        s.clone()
                    } else {
                        "Unknown internal error".to_string()
                    };

                    eprintln!("[ngn] Internal error: {}", msg);

                    // Send Error to completion channel if it exists
                    if let Some(chan) = &fiber.completion_channel {
                        let error = crate::value::EnumData::into_value(
                            "Result".to_string(),
                            "Error".to_string(),
                            Some(Box::new(Value::String(format!("Internal error: {}", msg)))),
                        );
                        chan.buffer.lock().unwrap().push_back(error);
                    }

                    // Move to next fiber
                    self.current_fiber = self.fibers.pop_front();
                }
            }

            if self.current_fiber.is_none() && !self.fibers.is_empty() {
                self.current_fiber = self.fibers.pop_front();
            }
        }
    }
}
