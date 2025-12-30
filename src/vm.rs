use crate::{bytecode::OpCode, toolbox::{NATIVE_ABS, NATIVE_ASSERT}, value::{Value}};
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
    stack: Vec<Value>,
    env_stack: Vec<Vec<Option<Variable>>>,
    pub call_stack: Vec<(Arc<Vec<OpCode>>, Arc<Vec<Value>>, usize, usize)>,
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
            stack: Vec::new(),
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

    fn current_env(&mut self) -> &mut Vec<Option<Variable>> {
        &mut self.env_stack[self.current_env_idx]
    }

    fn perform_call(&mut self, callable_value: Value) -> bool {
        match callable_value {
            Value::Function(func) => {
                let mut new_env = self.env_pool.pop().unwrap_or_else(|| Vec::with_capacity(func.param_count));
                new_env.clear();

                for i in (0..func.param_count).rev() {
                    let arg = self.pop_stack();
                    let is_owned = func.param_ownership[i];

                    if is_owned {
                        if let Value::Reference(env_idx, idx) = arg {
                            // Perform the MOVE from the correct environment
                            let moved_var = self.env_stack[env_idx][idx].take()
                                .expect("Cannot move an already moved or undefined variable");
                            
                            new_env.push(Some(Variable {
                                value: moved_var.value,
                                is_mutable: true,
                                reference_count: 0,
                            }));
                        } else {
                            new_env.push(Some(Variable { value: arg, is_mutable: true, reference_count: 0 }));
                        }
                    } else {
                        new_env.push(Some(Variable { value: arg, is_mutable: false, reference_count: 0 }));
                    }
                }

                new_env.reverse();

                let next_env_idx = self.env_stack.len();
                self.call_stack.push((Arc::clone(&self.instructions), Arc::clone(&self.constants), self.ip + 1, self.current_env_idx));
                self.instructions = Arc::clone(&func.instructions);
                self.ip = 0;
                self.constants = Arc::clone(&func.constants); // Swap the constants pool
                self.env_stack.push(new_env);
                self.current_env_idx = next_env_idx;
                true // We switched IP
            }
            Value::NativeFunction(id) => {
                let arg = self.pop_stack();
                let resolved = self.resolve_value(arg);

                match id {
                    crate::toolbox::NATIVE_ASSERT => {
                        match resolved {
                            Value::Bool(true) => println!("✅ Assertion passed"),
                            Value::Bool(false) => panic!("❌ Assertion failed! Expected true, got false."),
                            other => panic!("❌ Assertion failed! Expected Boolean, got {}", other),
                        }
                    }
                    _ => panic!("Runtime Error: Unknown Native ID {}", id),
                }
                self.stack.push(Value::Void);
                false // We did NOT switch IP
            }
            _ => panic!("Runtime Error: Not a callable: {:?}", callable_value),
        }
    }
    fn get_callable_local(&self, idx: usize) -> Value {
        let env = self.env_stack.last().expect("No active environment!");
        if let Some(Some(var)) = env.get(idx) {
            if matches!(var.value, Value::Function(_) | Value::NativeFunction(_)) {
                return var.value.clone();
            }
        }
        panic!("Runtime Error: Local callable at index {} not found", idx);
    }
    fn get_callable_global(&self, idx: usize) -> Value {
        let env = &self.env_stack[0];
        if let Some(Some(var)) = env.get(idx) {
            if matches!(var.value, Value::Function(_) | Value::NativeFunction(_)) {
                return var.value.clone();
            }
        }
        panic!("Runtime Error: Global callable at index {} not found", idx);
    }
    fn pop_stack(&mut self) -> Value {
        let val = self.stack.pop().expect("Runtime error: Stack underflow");

        if let Value::Reference(env_idx, var_idx) = val {
            if let Some(env) = self.env_stack.get_mut(env_idx) {
                if let Some(Some(var)) = env.get_mut(var_idx) {
                    var.reference_count -= 1;
                }
            }
        }

        val
    }
    fn resolve_value(&mut self, val: Value) -> Value {
        match val {
            Value::Reference(env_idx, var_idx) => {
                let env = self.env_stack.get(env_idx)
                    .expect("Runtime error: Invalid environment index");
                let var = env.get(var_idx)
                    .and_then(|slot| slot.as_ref())
                    .expect(&format!("Runtime error: Dangling reference for environment {} index {}", env_idx, var_idx));
                self.resolve_value(var.value.clone())
            }
            _ => val
        }
    }
    pub fn run(&mut self) {
        loop {
            // If we've reached the end of the current code, stop.
            if self.ip >= self.instructions.len() {
                // Check if we have somewhere to return to
                if let Some((saved_instructions, saved_constants, saved_ip, saved_env_idx)) = self.call_stack.pop() {
                    // Resolve the return value if it's a reference to the env we're about to pop
                    if let Some(top) = self.stack.last() {
                        if let Value::Reference(e, _) = top {
                            if *e == self.current_env_idx {
                                let val = self.pop_stack();
                                let resolved = self.resolve_value(val);
                                self.stack.push(resolved);
                            }
                        }
                    }

                    let mut old_env = self.env_stack.pop().expect("Stack underflow"); // Pop the function's environment
                    old_env.clear();
                    self.env_pool.push(old_env);

                    self.instructions = saved_instructions;
                    self.constants = saved_constants; // Restore the constants pool
                    self.ip = saved_ip;
                    self.current_env_idx = saved_env_idx;
                    continue; 
                } else {
                    // We finished the entry-point instructions (bootstrap)
                    break;
                }
            }

            let instruction = self.instructions[self.ip].clone();

            match instruction {
                OpCode::LoadConst(idx) => {
                    self.stack.push(self.constants[idx].clone());
                }
                OpCode::Add => {
                    let y = self.pop_stack();
                    let x = self.pop_stack();

                    let rx = self.resolve_value(x);
                    let ry = self.resolve_value(y);

                    match rx.add(ry) {
                        Ok(result) => self.stack.push(result),
                        Err(e) => {
                            eprintln!("Runtime Error: {}", e);
                            break;
                        }
                    }
                }
                OpCode::Subtract => {
                    let y = self.pop_stack();
                    let x = self.pop_stack();

                    let rx = self.resolve_value(x);
                    let ry = self.resolve_value(y);

                    match rx.subtract(ry) {
                        Ok(result) => self.stack.push(result),
                        Err(e) => {
                            eprintln!("Runtime Error: {}", e);
                            break;
                        }
                    }
                }
                OpCode::Multiply => {
                    let y = self.pop_stack();
                    let x = self.pop_stack();
                    let rx = self.resolve_value(x);
                    let ry = self.resolve_value(y);

                    match rx.multiply(ry) {
                        Ok(result) => self.stack.push(result),
                        Err(e) => {
                            eprintln!("Runtime Error: {}", e);
                            break;
                        }
                    }
                }
                OpCode::Divide => {
                    let y = self.pop_stack();
                    let x = self.pop_stack();
                    let rx = self.resolve_value(x);
                    let ry = self.resolve_value(y);

                    match rx.divide(ry) {
                        Ok(result) => self.stack.push(result),
                        Err(e) => {
                            eprintln!("Runtime Error: {}", e);
                            break;
                        }
                    } 
                }
                OpCode::Power => {
                    let y = self.pop_stack();
                    let x = self.pop_stack();
                    let rx = self.resolve_value(x);
                    let ry = self.resolve_value(y);

                    match rx.power(ry) {
                        Ok(result) => self.stack.push(result),
                        Err(e) => {
                            eprintln!("Runtime Error: {}", e);
                            break;
                        }
                    }
                }
                OpCode::Modulo => {
                    let y = self.pop_stack();
                    let x = self.pop_stack();
                    let rx = self.resolve_value(x);
                    let ry = self.resolve_value(y);

                    match rx.remainder(ry) {
                        Ok(result) => self.stack.push(result),
                        Err(e) => {
                            eprintln!("Runtime Error: {}", e);
                            break;
                        }
                    }
                }
                OpCode::Equal => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let rx = self.resolve_value(a);
                    let ry = self.resolve_value(b);
                    
                    self.stack.push(Value::Bool(rx.is_equal(ry)));
                }
                OpCode::NotEqual => {
                    let y = self.pop_stack();
                    let x = self.pop_stack();
                    let rx = self.resolve_value(x);
                    let ry = self.resolve_value(y);

                    self.stack.push(Value::Bool(!rx.is_equal(ry)));
                }
                OpCode::Call(var_idx) => {
                    let callable_value = self.get_callable_local(var_idx);
                    if self.perform_call(callable_value) {
                        continue;
                    }
                }
                OpCode::CallGlobal(var_idx) => {
                    let callable_value = self.get_callable_global(var_idx);
                    if self.perform_call(callable_value) {
                        continue;
                    }
                }
                OpCode::DefGlobal(idx, is_mutable) => {
                    let val = self.pop_stack();
                    let new_var = Variable { value: val.clone(), is_mutable, reference_count: 0 };
                    let env = &mut self.env_stack[0];
                    if idx >= env.len() { env.resize(idx + 1, None); }
                    env[idx] = Some(new_var);
                    
                    // Restore reference count if we're pushing it back
                    if let Value::Reference(e, v) = &val {
                        if let Some(env) = self.env_stack.get_mut(*e) {
                            if let Some(Some(var)) = env.get_mut(*v) {
                                var.reference_count += 1;
                            }
                        }
                    }
                    self.stack.push(val);
                }
                OpCode::DefVar(idx, is_mutable) => {
                    let val = self.pop_stack();
                    let new_var = Variable { value: val.clone(), is_mutable, reference_count: 0 };
                    let env_idx = self.current_env_idx;
                    let env = &mut self.env_stack[env_idx];
                    if idx >= env.len() { env.resize(idx + 1, None); }
                    env[idx] = Some(new_var);
 
                    // Restore reference count if we're pushing it back
                    if let Value::Reference(e, v) = &val {
                        if let Some(env) = self.env_stack.get_mut(*e) {
                            if let Some(Some(var)) = env.get_mut(*v) {
                                var.reference_count += 1;
                            }
                        }
                    }
                    self.stack.push(val);
                }
                OpCode::GetVar(idx) => {
                    let env_idx = self.current_env_idx;
                    if let Some(Some(var)) = self.env_stack[env_idx].get_mut(idx) {
                        var.reference_count += 1;
                        self.stack.push(Value::Reference(env_idx, idx));
                    } else {
                        panic!("Runtime error: Undefined local variable '{}'", idx);
                    }
                }
                OpCode::GetGlobal(idx) => {
                    if let Some(Some(var)) = self.env_stack[0].get_mut(idx) {
                        var.reference_count += 1;
                        self.stack.push(Value::Reference(0, idx)); 
                    } else {
                        panic!("Runtime error: Undefined global variable '{}'", idx);
                    }
                }
                OpCode::AssignVar(idx) => {
                    let new_val = self.pop_stack();
                    let env_idx = self.current_env_idx;
                    if let Some(Some(var)) = self.env_stack[env_idx].get_mut(idx) {
                        if var.reference_count > 0 {
                            panic!("Borrow error: Cannot mutate '{}' while it is borrowed!", idx);
                        }
                        if !var.is_mutable {
                            panic!("Runtime error: Cannot assign to constant '{}'", idx);
                        }
                        var.value = new_val.clone();
                        // Increment count since we're pushing it back
                        if let Value::Reference(e, v) = &new_val {
                            if let Some(env) = self.env_stack.get_mut(*e) {
                                if let Some(Some(var)) = env.get_mut(*v) {
                                    var.reference_count += 1;
                                }
                            }
                        }
                        self.stack.push(new_val);
                    } else {
                        panic!("Runtime error: Cannot assign to undefined local variable '{}'", idx);
                    }
                }
                OpCode::AssignGlobal(idx) => {
                    let new_val = self.pop_stack();
                    if let Some(Some(var)) = self.env_stack[0].get_mut(idx) {
                        if var.reference_count > 0 {
                            panic!("Borrow error: Cannot mutate global '{}' while it is borrowed!", idx);
                        }
                        if !var.is_mutable {
                            panic!("Runtime error: Cannot assign to constant global '{}'", idx);
                        }
                        var.value = new_val.clone();
                        // Increment count since we're pushing it back
                        if let Value::Reference(e, v) = &new_val {
                            if let Some(env) = self.env_stack.get_mut(*e) {
                                if let Some(Some(var)) = env.get_mut(*v) {
                                    var.reference_count += 1;
                                }
                            }
                        }
                        self.stack.push(new_val);
                    } else {
                        panic!("Runtime error: Cannot assign to undefined global variable '{}'", idx);
                    }
                }
                OpCode::DeleteVar(idx) => {
                    if let Some(Some(var)) = self.current_env().get(idx) {
                        if var.reference_count > 0 {
                            panic!("Borrow error: Cannot delete '{}' while it is borrowed!", idx);
                        }

                        // Don't remove, just change the slot
                        self.current_env()[idx] = None;
                    } else {
                        panic!("Runtime error: Cannot delete undefinded variable '{}'", idx);
                    }
                }
                OpCode::NativeCall(id, arg_count) => {
                    let mut args = Vec::new();
                    for _ in 0..arg_count {
                        // Resolve references before passing to Rust
                        let raw = self.pop_stack();
                        args.push(self.resolve_value(raw));
                    }
                    // Args were popped in reverse order, so fix them
                    args.reverse();

                    let result = match id {
                        NATIVE_ABS => {
                            // Re-use your existing toolbox math logic here!
                            // Just wrap it in a result
                            crate::toolbox::math::abs(args).expect("Native Error")
                        }
                        NATIVE_ASSERT => {
                            let condition = &args[0];
                            match condition {
                                Value::Bool(true) => println!("✅ Assertion passed!"),
                                Value::Bool(false) => panic!("❌ Assertion failed!"),
                                _ => panic!("❌ Assertion error: Expected boolean"),
                            }
                            // Return a 'Void' or dummy value so the arm matches types
                            Value::Bool(true)
                        }
                        _ => panic!("Unknown Native ID"),
                    };
                    
                    self.stack.push(result);
                }
                OpCode::Print => {
                    let val = self.pop_stack();
                    let resolved = self.resolve_value(val);

                    println!("{}", resolved);
                }
                OpCode::Echo => {
                    let val = self.pop_stack();
                    let resolved = self.resolve_value(val);

                    print!("{}", resolved);
                    let _ = std::io::stdout().flush();
                }
                OpCode::Sleep => {
                    let val = self.pop_stack();
                    let resolved = self.resolve_value(val);
                    if let Value::Numeric(crate::value::Number::I64(ms)) = resolved {
                        std::thread::sleep(std::time::Duration::from_millis(ms as u64));
                    } else {
                        panic!("Runtime Error: sleep() requires an i64 duration in milliseconds");
                    }
                }
                OpCode::Concat(count) => {
                    let mut result = String::new();
                    let mut parts = Vec::new();
                    for _ in 0..count {
                        parts.push(self.pop_stack());
                    }
                    // Parts are in reverse order of how they were pushed
                    for part in parts.into_iter().rev() {
                        let resolved = self.resolve_value(part);
                        result.push_str(&resolved.to_string());
                    }
                    self.stack.push(Value::String(result));
                }
                OpCode::Return => {
                    // Restore the previous caller's state
                    if let Some((saved_instructions, saved_constants, saved_ip, saved_env_idx)) = self.call_stack.pop() {
                        // Resolve the return value if it's a reference to the env we're about to pop
                        if let Some(top) = self.stack.last() {
                            if let Value::Reference(e, _) = top {
                                if *e == self.current_env_idx {
                                    let val = self.pop_stack();
                                    let resolved = self.resolve_value(val);
                                    self.stack.push(resolved);
                                }
                            }
                        }

                        let mut old_env = self.env_stack.pop().expect("Stack underflow"); // Pop the isolated environment
                        old_env.clear();
                        self.env_pool.push(old_env);

                        self.instructions = saved_instructions;
                        self.constants = saved_constants; // Restore the constants pool
                        self.ip = saved_ip;
                        self.current_env_idx = saved_env_idx;

                        continue;
                    } else {
                        // If the call_stack is empty, we just finished main()
                        break; 
                    }
                }
                OpCode::Halt => break,
                OpCode::BuildArray(count) => {
                    let mut elements = Vec::new();
                    for _ in 0..count {
                        let val = self.pop_stack();
                        elements.push(self.resolve_value(val));
                    }
                    elements.reverse();
                    self.stack.push(Value::Array(elements));
                }
                OpCode::BuildTuple(count) => {
                    let mut elements = Vec::new();
                    for _ in 0..count {
                        let val = self.pop_stack();
                        elements.push(self.resolve_value(val));
                    }
                    elements.reverse();
                    self.stack.push(Value::Tuple(elements));
                }
                OpCode::Jump(target) => {
                    self.ip = target;
                    continue;
                }
                OpCode::JumpIfFalse(target) => {
                    let val = self.pop_stack();
                    let resolved = self.resolve_value(val);
                    match resolved {
                        Value::Bool(false) => {
                            self.ip = target;
                            continue;
                        }
                        _ => {} // Fall through (treat as true)
                    }
                }
                OpCode::JumpIfTrue(target) => {
                    let val = self.pop_stack();
                    let resolved = self.resolve_value(val);
                    match resolved {
                        Value::Bool(true) => {
                            self.ip = target;
                            continue;
                        }
                        _ => {} // Fall through (treat as false)
                    }
                }
                OpCode::Dup => {
                    let val = self.stack.last().expect("Runtime Error: Dup empty stack").clone();
                    // If it's a reference, we MUST increment the reference count!
                    if let Value::Reference(e, v) = &val {
                        if let Some(env) = self.env_stack.get_mut(*e) {
                            if let Some(Some(var)) = env.get_mut(*v) {
                                var.reference_count += 1;
                            }
                        }
                    }
                    self.stack.push(val);
                }
                OpCode::IterStart => {
                    let val = self.stack.last().expect("Runtime Error: IterStart empty stack");
                    let resolved = self.resolve_value(val.clone());
                    match resolved {
                        Value::Array(_) | Value::Tuple(_) => {
                            self.stack.push(Value::Numeric(crate::value::Number::I64(0)));
                        }
                        _ => panic!("Runtime Error: IterStart expected Array or Tuple, got {}", resolved),
                    }
                }
                OpCode::IterNext(target) => {
                    let index_val = self.pop_stack();
                    let array_val = self.pop_stack();
                    
                    let index = match self.resolve_value(index_val) {
                        Value::Numeric(crate::value::Number::I64(i)) => i,
                        _ => panic!("Runtime Error: IterNext expected i64 index"),
                    };
                    
                    let resolved_array = self.resolve_value(array_val.clone());
                    let collection = match &resolved_array {
                        Value::Array(a) => a,
                        Value::Tuple(t) => t,
                        _ => panic!("Runtime Error: IterNext expected Array or Tuple, got {}", resolved_array),
                    };
                    
                    if (index as usize) < collection.len() {
                        // Push collection and next index for next IterNext
                        
                        // Restore reference count for array_val since we're pushing it back
                        if let Value::Reference(e, v) = &array_val {
                            if let Some(env) = self.env_stack.get_mut(*e) {
                                if let Some(Some(var)) = env.get_mut(*v) {
                                    var.reference_count += 1;
                                }
                            }
                        }
                        self.stack.push(array_val);
                        self.stack.push(Value::Numeric(crate::value::Number::I64(index + 1)));
                        
                        // Push element and current index for the loop body bindings
                        self.stack.push(collection[index as usize].clone());
                        self.stack.push(Value::Numeric(crate::value::Number::I64(index)));
                    } else {
                        self.ip = target;
                        continue;
                    }
                }
                OpCode::LessThan => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let val_a = self.resolve_value(a);
                    let val_b = self.resolve_value(b);
                    
                    match (val_a, val_b) {
                        (Value::Numeric(n1), Value::Numeric(n2)) => {
                             self.stack.push(Value::Bool(n1.less_than(n2)));
                        }
                        _ => panic!("Runtime Error: Invalid operands for <"),
                    }
                }
                OpCode::GreaterThan => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let val_a = self.resolve_value(a);
                    let val_b = self.resolve_value(b);
                    
                    match (val_a, val_b) {
                        (Value::Numeric(n1), Value::Numeric(n2)) => {
                            self.stack.push(Value::Bool(n1.greater_than(n2)));
                        }
                        _ => panic!("Runtime Error: Invalid operands for >"),
                    }
                }
                OpCode::Pop => {
                    self.pop_stack();
                }
                OpCode::CreateEnum(const_idx, arg_count) => {
                    let names_val = self.constants[const_idx].clone();
                    let (enum_name, variant_name) = match names_val {
                        Value::Tuple(t) if t.len() == 2 => {
                            if let (Value::String(e), Value::String(v)) = (&t[0], &t[1]) {
                                (e.clone(), v.clone())
                            } else {
                                panic!("Runtime Error: CreateEnum expected strings in names tuple");
                            }
                        }
                        _ => panic!("Runtime Error: CreateEnum expected 2-element tuple for names"),
                    };

                    let data = if arg_count > 0 {
                        Some(Box::new(self.pop_stack()))
                    } else {
                        None
                    };

                    self.stack.push(Value::Enum { enum_name, variant_name, data });
                }
                OpCode::IsVariant(enum_const_idx, var_const_idx) => {
                    let val = self.pop_stack();
                    let resolved = self.resolve_value(val);
                    
                    let e_name = match &self.constants[enum_const_idx] {
                        Value::String(s) => s.as_str(),
                        _ => "",
                    };
                    let v_name = match &self.constants[var_const_idx] {
                        Value::String(s) => s.as_str(),
                        _ => panic!("Runtime Error: IsVariant expected variant name"),
                    };

                    if let Value::Enum { enum_name, variant_name, .. } = resolved {
                         let matches = (e_name.is_empty() || e_name == enum_name) && v_name == variant_name;
                         self.stack.push(Value::Bool(matches));
                    } else {
                        self.stack.push(Value::Bool(false));
                    }
                }
                OpCode::GetVariantData => {
                    let val = self.pop_stack();
                    let resolved = self.resolve_value(val);
                    if let Value::Enum { data, .. } = resolved {
                         if let Some(d) = data {
                             self.stack.push(*d);
                         } else {
                             self.stack.push(Value::Void);
                         }
                    } else {
                        panic!("Runtime Error: GetVariantData expected Enum");
                    }
                }
            }
            self.ip += 1;
        }
    }
}
