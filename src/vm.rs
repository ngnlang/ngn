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

pub struct VM {
    instructions: Arc<Vec<OpCode>>,
    constants: Arc<Vec<Value>>,
    env_stack: Vec<Vec<Option<Variable>>>,
    pub call_stack: Vec<(Arc<Vec<OpCode>>, Arc<Vec<Value>>, usize, usize, Option<u16>)>,
    ip: usize,
    current_env_idx: usize,
    env_pool: Vec<Vec<Option<Variable>>>,
}
 
impl VM {
    pub fn new(instructions: Vec<OpCode>, constants: Vec<Value>, global_slots: usize) -> Self {
        let mut globals = Vec::with_capacity(global_slots);
        globals.resize(global_slots, None);
        Self {
            instructions: Arc::new(instructions),
            constants: Arc::new(constants),
            env_stack: vec![globals],
            call_stack: Vec::new(),
            ip: 0,
            current_env_idx: 0,
            env_pool: Vec::with_capacity(32),
        }
    }
    pub fn init_globals(&mut self, size: usize) {
        self.env_stack[0].resize(size, None);
    }
    pub fn define_global(&mut self, idx: usize, val: Value, is_mutable: bool) {
        let env = &mut self.env_stack[0];
        if idx >= env.len() {
            env.resize(idx + 1, None);
        }
        env[idx] = Some(Variable { value: val, is_mutable, reference_count: 0 });
    }

    /* Deleted duplicate current_env */

    fn perform_call(&mut self, callable_value: Value, dest_reg: Option<u16>, arg_start: u16, arg_count: u8) -> bool {
        match callable_value {
            Value::Function(func) => {
                let mut new_env = self.env_pool.pop().unwrap_or_else(|| Vec::with_capacity(64));
                new_env.clear();
                // Ensure room for registers (params + locals + temps)
                new_env.resize(128, None);

                for i in 0..arg_count as usize {
                    let arg = self.get_reg_at(self.current_env_idx, arg_start + i as u16);
                    let resolved = arg; // For functions, we still resolve references if they are passed by value
                    let is_owned = if i < func.param_ownership.len() { func.param_ownership[i] } else { false };

                    if is_owned {
                        if let Value::Reference(env_idx, idx) = resolved {
                            let moved_var = self.env_stack[env_idx][idx].take()
                                .expect("Cannot move an already moved or undefined variable");
                            
                            new_env[i] = Some(Variable {
                                value: moved_var.value,
                                is_mutable: true,
                                reference_count: 0,
                            });
                        } else {
                            new_env[i] = Some(Variable { value: resolved, is_mutable: true, reference_count: 0 });
                        }
                    } else {
                        new_env[i] = Some(Variable { value: resolved, is_mutable: false, reference_count: 0 });
                    }
                }

                let next_env_idx = self.env_stack.len();
                self.call_stack.push((Arc::clone(&self.instructions), Arc::clone(&self.constants), self.ip + 1, self.current_env_idx, dest_reg));
                self.instructions = Arc::clone(&func.instructions);
                self.ip = 0;
                self.constants = Arc::clone(&func.constants);
                self.env_stack.push(new_env);
                self.current_env_idx = next_env_idx;
                true
            }
            Value::NativeFunction(id) => {
                let arg = if arg_count > 0 { self.get_reg_at(self.current_env_idx, arg_start) } else { Value::Void };
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
                    self.set_reg_at(self.current_env_idx, dest, result);
                }
                false
            }
            _ => panic!("Runtime Error: Not a callable: {:?}", callable_value),
        }
    }

    fn get_reg_at(&self, env_idx: usize, idx: u16) -> Value {
        let env = &self.env_stack[env_idx];
        env.get(idx as usize)
            .and_then(|v| v.as_ref())
            .map(|v| v.value.clone())
            .unwrap_or(Value::Void)
    }

    fn get_reg_ref(&self, env_idx: usize, idx: u16) -> &Value {
        let env = &self.env_stack[env_idx];
        env.get(idx as usize)
            .and_then(|v| v.as_ref())
            .map(|v| &v.value)
            .expect("Runtime Error: Accessing uninitialized register")
    }

    fn set_reg_at(&mut self, env_idx: usize, idx: u16, val: Value) {
        let env = &mut self.env_stack[env_idx];
        if idx as usize >= env.len() {
            env.resize(idx as usize + 1, None);
        }
        env[idx as usize] = Some(Variable {
            value: val,
            is_mutable: true,
            reference_count: 0,
        });
    }

    pub fn run(&mut self) {
        loop {
            if self.ip >= self.instructions.len() {
                if let Some((saved_instructions, saved_constants, saved_ip, saved_env_idx, _dest_reg)) = self.call_stack.pop() {
                    let mut old_env = self.env_stack.pop().expect("Stack underflow");
                    old_env.clear();
                    self.env_pool.push(old_env);

                    self.instructions = saved_instructions;
                    self.constants = saved_constants;
                    self.ip = saved_ip;
                    self.current_env_idx = saved_env_idx;
                    continue; 
                } else {
                    break;
                }
            }

            let instruction = self.instructions[self.ip].clone();

            match instruction {
                OpCode::LoadConst(dest, idx) => {
                    let val = self.constants[idx].clone();
                    self.set_reg_at(self.current_env_idx, dest, val);
                }
                OpCode::Add(dest, src1, src2) => {
                    let res = {
                        let v1 = self.get_reg_ref(self.current_env_idx, src1);
                        let v2 = self.get_reg_ref(self.current_env_idx, src2);
                        v1.add(v2)
                    };
                    match res {
                        Ok(val) => self.set_reg_at(self.current_env_idx, dest, val),
                        Err(e) => panic!("Runtime Error: {}", e),
                    }
                }
                OpCode::Subtract(dest, src1, src2) => {
                    let res = {
                        let v1 = self.get_reg_ref(self.current_env_idx, src1);
                        let v2 = self.get_reg_ref(self.current_env_idx, src2);
                        v1.subtract(v2)
                    };
                    match res {
                        Ok(val) => self.set_reg_at(self.current_env_idx, dest, val),
                        Err(e) => panic!("Runtime Error: {}", e),
                    }
                }
                OpCode::Multiply(dest, src1, src2) => {
                    let res = {
                        let v1 = self.get_reg_ref(self.current_env_idx, src1);
                        let v2 = self.get_reg_ref(self.current_env_idx, src2);
                        v1.multiply(v2)
                    };
                    match res {
                        Ok(val) => self.set_reg_at(self.current_env_idx, dest, val),
                        Err(e) => panic!("Runtime Error: {}", e),
                    }
                }
                OpCode::Divide(dest, src1, src2) => {
                    let res = {
                        let v1 = self.get_reg_ref(self.current_env_idx, src1);
                        let v2 = self.get_reg_ref(self.current_env_idx, src2);
                        v1.divide(v2)
                    };
                    match res {
                        Ok(val) => self.set_reg_at(self.current_env_idx, dest, val),
                        Err(e) => panic!("Runtime Error: {}", e),
                    }
                }
                OpCode::Power(dest, src1, src2) => {
                    let res = {
                        let v1 = self.get_reg_ref(self.current_env_idx, src1);
                        let v2 = self.get_reg_ref(self.current_env_idx, src2);
                        v1.clone().power(v2.clone())
                    };
                    match res {
                        Ok(val) => self.set_reg_at(self.current_env_idx, dest, val),
                        Err(e) => panic!("Runtime Error: {}", e),
                    }
                }
                OpCode::Modulo(dest, src1, src2) => {
                    let res = {
                        let v1 = self.get_reg_ref(self.current_env_idx, src1);
                        let v2 = self.get_reg_ref(self.current_env_idx, src2);
                        v1.clone().remainder(v2.clone())
                    };
                    match res {
                        Ok(val) => self.set_reg_at(self.current_env_idx, dest, val),
                        Err(e) => panic!("Runtime Error: {}", e),
                    }
                }
                OpCode::Equal(dest, src1, src2) => {
                    let v1 = self.get_reg_ref(self.current_env_idx, src1);
                    let v2 = self.get_reg_ref(self.current_env_idx, src2);
                    self.set_reg_at(self.current_env_idx, dest, Value::Bool(v1.is_equal(v2)));
                }
                OpCode::NotEqual(dest, src1, src2) => {
                    let v1 = self.get_reg_ref(self.current_env_idx, src1);
                    let v2 = self.get_reg_ref(self.current_env_idx, src2);
                    self.set_reg_at(self.current_env_idx, dest, Value::Bool(!v1.is_equal(v2)));
                }
                OpCode::LessThan(dest, src1, src2) => {
                    let v1 = self.get_reg_ref(self.current_env_idx, src1);
                    let v2 = self.get_reg_ref(self.current_env_idx, src2);
                    match (v1, v2) {
                        (Value::Numeric(n1), Value::Numeric(n2)) => self.set_reg_at(self.current_env_idx, dest, Value::Bool(n1.less_than(*n2))),
                        _ => panic!("Runtime Error: Invalid operands for <"),
                    }
                }
                OpCode::GreaterThan(dest, src1, src2) => {
                    let v1 = self.get_reg_ref(self.current_env_idx, src1);
                    let v2 = self.get_reg_ref(self.current_env_idx, src2);
                    match (v1, v2) {
                        (Value::Numeric(n1), Value::Numeric(n2)) => self.set_reg_at(self.current_env_idx, dest, Value::Bool(n1.greater_than(*n2))),
                        _ => panic!("Runtime Error: Invalid operands for >"),
                    }
                }
                OpCode::Call(dest, func_reg, arg_start, arg_count) => {
                    let callable = self.get_reg_at(self.current_env_idx, func_reg);
                    if self.perform_call(callable, Some(dest), arg_start, arg_count) {
                        continue;
                    }
                }
                OpCode::CallGlobal(dest, idx, arg_start, arg_count) => {
                    let callable = self.get_reg_at(0, idx as u16);
                    if self.perform_call(callable, Some(dest), arg_start, arg_count) {
                        continue;
                    }
                }
                OpCode::NativeCall(dest, id_reg, arg_start, arg_count) => {
                    let val = self.get_reg_at(self.current_env_idx, id_reg);
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
                    let val = self.get_reg_at(self.current_env_idx, idx as u16);
                    self.set_reg_at(self.current_env_idx, dest, val);
                }
                OpCode::GetGlobal(dest, idx) => {
                    let val = self.get_reg_at(0, idx as u16);
                    self.set_reg_at(self.current_env_idx, dest, val);
                }
                OpCode::AssignVar(idx, src) => {
                    let _val = self.get_reg_at(self.current_env_idx, src);
                    self.set_reg_at(self.current_env_idx, idx, _val);
                }
                OpCode::AssignGlobal(idx, src) => {
                    let _val = self.get_reg_at(self.current_env_idx, src);
                    self.set_reg_at(0, idx as u16, _val);
                }
                OpCode::Move(dest, src) => {
                    let val = self.get_reg_at(self.current_env_idx, src);
                    self.set_reg_at(self.current_env_idx, dest, val);
                }
                OpCode::Print(src) => {
                    let val = self.get_reg_at(self.current_env_idx, src);
                    println!("{}", val);
                }
                OpCode::Echo(src) => {
                    let val = self.get_reg_at(self.current_env_idx, src);
                    print!("{}", val);
                    let _ = std::io::stdout().flush();
                }
                OpCode::Sleep(src) => {
                    let val = self.get_reg_at(self.current_env_idx, src);
                    if let Value::Numeric(crate::value::Number::I64(ms)) = val {
                        std::thread::sleep(std::time::Duration::from_millis(ms as u64));
                    }
                }
                OpCode::Jump(target) => {
                    self.ip = target;
                    continue;
                }
                OpCode::JumpIfFalse(cond, target) => {
                    let val = self.get_reg_at(self.current_env_idx, cond);
                    if matches!(val, Value::Bool(false)) {
                        self.ip = target;
                        continue;
                    }
                }
                OpCode::JumpIfTrue(cond, target) => {
                    let val = self.get_reg_at(self.current_env_idx, cond);
                    if matches!(val, Value::Bool(true)) {
                        self.ip = target;
                        continue;
                    }
                }
                OpCode::Return(src) => {
                    let result = self.get_reg_at(self.current_env_idx, src);
                    if let Some((saved_instructions, saved_constants, saved_ip, saved_env_idx, dest_reg)) = self.call_stack.pop() {
                        let mut old_env = self.env_stack.pop().expect("Stack underflow");
                        old_env.clear();
                        self.env_pool.push(old_env);

                        self.instructions = saved_instructions;
                        self.constants = saved_constants;
                        self.ip = saved_ip;
                        self.current_env_idx = saved_env_idx;

                        if let Some(dest) = dest_reg {
                            self.set_reg_at(self.current_env_idx, dest, result);
                        }
                        continue;
                    } else {
                        break;
                    }
                }
                OpCode::ReturnVoid => {
                    if let Some((saved_instructions, saved_constants, saved_ip, saved_env_idx, dest_reg)) = self.call_stack.pop() {
                        let mut old_env = self.env_stack.pop().expect("Stack underflow");
                        old_env.clear();
                        self.env_pool.push(old_env);

                        self.instructions = saved_instructions;
                        self.constants = saved_constants;
                        self.ip = saved_ip;
                        self.current_env_idx = saved_env_idx;

                        if let Some(dest) = dest_reg {
                            self.set_reg_at(self.current_env_idx, dest, Value::Void);
                        }
                        continue;
                    } else {
                        break;
                    }
                }
                OpCode::BuildArray(dest, start, count) => {
                    let mut elements = Vec::with_capacity(count as usize);
                    for i in 0..count {
                        elements.push(self.get_reg_at(self.current_env_idx, start + i as u16));
                    }
                    self.set_reg_at(self.current_env_idx, dest, Value::Array(elements));
                }
                OpCode::BuildTuple(dest, start, count) => {
                    let mut elements = Vec::with_capacity(count as usize);
                    for i in 0..count {
                        elements.push(self.get_reg_at(self.current_env_idx, start + i as u16));
                    }
                    self.set_reg_at(self.current_env_idx, dest, Value::Tuple(elements));
                }
                OpCode::IterStart(iter_dest, src) => {
                    let collection = self.get_reg_at(self.current_env_idx, src);
                    self.set_reg_at(self.current_env_idx, iter_dest, collection);
                    self.set_reg_at(self.current_env_idx, iter_dest + 1, Value::Numeric(crate::value::Number::I64(0)));
                }
                OpCode::IterNext(val_dest, iter_reg, target) => {
                    let collection = self.get_reg_at(self.current_env_idx, iter_reg);
                    let index_val = self.get_reg_at(self.current_env_idx, iter_reg + 1);
                    if let Value::Numeric(crate::value::Number::I64(idx)) = index_val {
                        let items = match collection {
                            Value::Array(ref a) => a,
                            Value::Tuple(ref t) => t,
                            _ => panic!("IterNext on non-collection"),
                        };

                        if (idx as usize) < items.len() {
                            let val = items[idx as usize].clone();
                            self.set_reg_at(self.current_env_idx, val_dest, val);
                            self.set_reg_at(self.current_env_idx, iter_reg + 1, Value::Numeric(crate::value::Number::I64(idx + 1)));
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
                        Some(Box::new(self.get_reg_at(self.current_env_idx, start)))
                    } else {
                        None
                    };

                    self.set_reg_at(self.current_env_idx, dest, Value::Enum { enum_name, variant_name, data });
                }
                OpCode::IsVariant(dest, src, enum_name_idx, var_name_idx) => {
                    let val = self.get_reg_at(self.current_env_idx, src);
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
                    self.set_reg_at(self.current_env_idx, dest, Value::Bool(matches));
                }
                OpCode::GetVariantData(dest, src) => {
                    let val = self.get_reg_at(self.current_env_idx, src);
                    if let Value::Enum { data, .. } = val {
                        let inner = data.map(|d| *d).unwrap_or(Value::Void);
                        self.set_reg_at(self.current_env_idx, dest, inner);
                    }
                }
                OpCode::Halt => break,
                OpCode::Pop | OpCode::Dup => {} // Obsolete
                OpCode::Concat(dest, start, count) => {
                    let mut result = String::new();
                    for i in 0..count {
                        let part = self.get_reg_at(self.current_env_idx, start as u16 + i as u16);
                        result.push_str(&part.to_string());
                    }
                    self.set_reg_at(self.current_env_idx, dest, Value::String(result));
                }
            }
            self.ip += 1;
        }
    }
}
