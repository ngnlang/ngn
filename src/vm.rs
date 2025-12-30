use crate::{bytecode::OpCode, value::Value};
use std::io::Write;
use std::sync::Arc;

#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Variable {
    pub value: Value,
    pub is_mutable: bool,
    pub reference_count: usize,
}

pub struct CallFrame {
    instructions: Arc<Vec<OpCode>>,
    constants: Arc<Vec<Value>>,
    ip: usize,
    fp: usize,
    dest_reg: Option<u16>,
}

pub struct VM {
    instructions: Arc<Vec<OpCode>>,
    constants: Arc<Vec<Value>>,
    stack: Vec<Option<Variable>>,
    frames: Vec<CallFrame>,
    ip: usize,
    fp: usize,
}
 
impl VM {
    pub fn new(instructions: Vec<OpCode>, constants: Vec<Value>, global_slots: usize) -> Self {
        let mut stack = Vec::with_capacity(1024);
        stack.resize(global_slots, None);
        Self {
            instructions: Arc::new(instructions),
            constants: Arc::new(constants),
            stack,
            frames: Vec::with_capacity(64),
            ip: 0,
            fp: 0,
        }
    }
    pub fn init_globals(&mut self, size: usize) {
        if size > self.stack.len() {
            self.stack.resize(size, None);
        }
    }
    pub fn define_global(&mut self, idx: usize, val: Value, is_mutable: bool) {
        if idx >= self.stack.len() {
            self.stack.resize(idx + 1, None);
        }
        self.stack[idx] = Some(Variable { value: val, is_mutable, reference_count: 0 });
    }

    /* Deleted duplicate current_env */

    fn perform_call(&mut self, callable_value: Value, dest_reg: Option<u16>, arg_start: u16, arg_count: u8) -> bool {
        match callable_value {
            Value::Function(func) => {
                let new_fp = self.stack.len();
                
                // Ensure room for registers
                self.stack.resize(new_fp + func.reg_count, None);

                for i in 0..arg_count as usize {
                    let arg = self.get_reg_at(arg_start + i as u16);
                    let is_owned = if i < func.param_ownership.len() { func.param_ownership[i] } else { false };
                    
                    if is_owned {
                         // ownership logic simplified: copy value. 
                         // To strictly enforce move, we'd nullify the source, but register index reuse makes that tricky.
                         // For performance, we copy.
                    }

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
                    ip: self.ip + 1, // Return to next instruction
                    fp: self.fp,
                    dest_reg,
                });

                // Switch to new function
                self.instructions = Arc::clone(&func.instructions);
                self.constants = Arc::clone(&func.constants);
                self.ip = 0;
                self.fp = new_fp;
                true
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

    // Updated for Sliding Window: Uses fp + idx
    fn get_reg_at(&self, idx: u16) -> Value {
        self.stack[self.fp + idx as usize]
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

    // Helper for globals (always at bottom of stack)
    fn get_global(&self, idx: u16) -> Value {
        self.stack[idx as usize]
            .as_ref()
            .map(|v| v.value.clone())
            .unwrap_or(Value::Void)
    }

    fn set_global(&mut self, idx: u16, val: Value) {
         if idx as usize >= self.stack.len() {
            self.stack.resize(idx as usize + 1, None);
        }
        self.stack[idx as usize] = Some(Variable {
            value: val,
            is_mutable: true, // Globals created via assignment? define_global handles mutability.
            reference_count: 0,
        });
    }

    pub fn run(&mut self) {
        loop {
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
                    continue; 
                } else {
                    break;
                }
            }

            let instruction = self.instructions[self.ip].clone();

            match instruction {
                OpCode::LoadConst(dest, idx) => {
                    let val = self.constants[idx].clone();
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
                OpCode::Call(dest, func_reg, arg_start, arg_count) => {
                    let callable = self.get_reg_at(func_reg);
                    if self.perform_call(callable, Some(dest), arg_start, arg_count) {
                        continue;
                    }
                }
                OpCode::CallGlobal(dest, idx, arg_start, arg_count) => {
                    // Global is at absolute index idx
                    let callable = self.get_global(idx as u16);
                    if self.perform_call(callable, Some(dest), arg_start, arg_count) {
                        continue;
                    }
                }
                OpCode::NativeCall(dest, id_reg, arg_start, arg_count) => {
                    let val = self.get_reg_at(id_reg);
                    if self.perform_call(val, Some(dest), arg_start, arg_count) {
                        continue;
                    }
                }
                OpCode::DefGlobal(_idx, _is_mutable) => {
                    // Logic for DefGlobal remains largely the same but doesn't pop
                }
                OpCode::DefVar(_idx, _is_mutable) => {
                    // Logic for DefVar remains largely the same
                }
                OpCode::GetVar(dest, idx) => {
                    let val = self.get_reg_at(idx as u16);
                    self.set_reg_at(dest, val);
                }
                OpCode::GetGlobal(dest, idx) => {
                    let val = self.get_global(idx as u16);
                    self.set_reg_at(dest, val);
                }
                OpCode::AssignVar(idx, src) => {
                    let _val = self.get_reg_at(src);
                    self.set_reg_at(idx, _val);
                }
                OpCode::AssignGlobal(idx, src) => {
                    let _val = self.get_reg_at(src);
                    self.set_global(idx as u16, _val);
                }
                OpCode::Move(dest, src) => {
                    let val = self.get_reg_at(src);
                    self.set_reg_at(dest, val);
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
                    continue;
                }
                OpCode::JumpIfFalse(cond, target) => {
                    let val = self.get_reg_at(cond);
                    if matches!(val, Value::Bool(false)) {
                        self.ip = target;
                        continue;
                    }
                }
                OpCode::JumpIfTrue(cond, target) => {
                    let val = self.get_reg_at(cond);
                    if matches!(val, Value::Bool(true)) {
                        self.ip = target;
                        continue;
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

                        if let Some(dest) = frame.dest_reg {
                            self.set_reg_at(dest, result);
                        }
                        continue;
                    } else {
                        break;
                    }
                }
                OpCode::ReturnVoid => {
                    if let Some(frame) = self.frames.pop() {
                        self.stack.truncate(self.fp);
                        
                        self.instructions = frame.instructions;
                        self.constants = frame.constants;
                        self.ip = frame.ip;
                        self.fp = frame.fp;

                        if let Some(dest) = frame.dest_reg {
                            self.set_reg_at(dest, Value::Void);
                        }
                        continue;
                    } else {
                        break;
                    }
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
                OpCode::IterStart(iter_dest, src) => {
                    let collection = self.get_reg_at(src);
                    self.set_reg_at(iter_dest, collection);
                    self.set_reg_at(iter_dest + 1, Value::Numeric(crate::value::Number::I64(0)));
                }
                OpCode::IterNext(val_dest, iter_reg, target) => {
                    let collection = self.get_reg_at(iter_reg);
                    let index_val = self.get_reg_at(iter_reg + 1);
                    if let Value::Numeric(crate::value::Number::I64(idx)) = index_val {
                        let items = match collection {
                            Value::Array(ref a) => a,
                            Value::Tuple(ref t) => t,
                            _ => panic!("IterNext on non-collection"),
                        };

                        if (idx as usize) < items.len() {
                            let val = items[idx as usize].clone();
                            self.set_reg_at(val_dest, val);
                            self.set_reg_at(iter_reg + 1, Value::Numeric(crate::value::Number::I64(idx + 1)));
                        } else {
                            self.ip = target;
                            continue;
                        }
                    }
                }
                OpCode::CreateEnum(dest, names_idx, start, count) => {
                    let names_val = self.constants[names_idx].clone();
                    let (enum_name, variant_name) = match names_val {
                        Value::Tuple(t) if t.len() == 2 => {
                            if let (Value::String(e), Value::String(v)) = (&t[0], &t[1]) {
                                (e.clone(), v.clone())
                            } else {
                                panic!("CreateEnum error");
                            }
                        }
                        _ => panic!("CreateEnum names error"),
                    };

                    let data = if count > 0 {
                        Some(Box::new(self.get_reg_at(start)))
                    } else {
                        None
                    };

                    self.set_reg_at(dest, Value::Enum { enum_name, variant_name, data });
                }
                OpCode::IsVariant(dest, src, enum_name_idx, var_name_idx) => {
                    let val = self.get_reg_at(src);
                    let e_name = match &self.constants[enum_name_idx] {
                        Value::String(s) => s.as_str(),
                        _ => "",
                    };
                    let v_name = match &self.constants[var_name_idx] {
                        Value::String(s) => s.as_str(),
                        _ => panic!("IsVariant error"),
                    };

                    let matches = if let Value::Enum { enum_name, variant_name, .. } = val {
                        (e_name.is_empty() || e_name == enum_name) && v_name == variant_name
                    } else {
                        false
                    };
                    self.set_reg_at(dest, Value::Bool(matches));
                }
                OpCode::GetVariantData(dest, src) => {
                    let val = self.get_reg_at(src);
                    if let Value::Enum { data, .. } = val {
                        let inner = data.map(|d| *d).unwrap_or(Value::Void);
                        self.set_reg_at(dest, inner);
                    }
                }
                OpCode::Halt => break,
                OpCode::Pop | OpCode::Dup => {} // Obsolete
                OpCode::Concat(dest, start, count) => {
                    let mut result = String::new();
                    for i in 0..count {
                        let part = self.get_reg_at(start as u16 + i as u16);
                        match part {
                             Value::String(s) => result.push_str(&s),
                             other => result.push_str(&other.to_string()),
                        }
                    }
                    self.set_reg_at(dest, Value::String(result));
                }
            }
            self.ip += 1;
        }
    }
}
