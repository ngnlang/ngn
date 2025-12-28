use crate::{bytecode::OpCode, toolbox::{NATIVE_ABS, NATIVE_ASSERT}, value::{Value}};

#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Variable {
    pub value: Value,
    pub is_mutable: bool,
    pub reference_count: usize,
}

pub struct VM {
    instructions: Vec<OpCode>,
    constants: Vec<Value>,
    stack: Vec<Value>,
    env_stack: Vec<Vec<Option<Variable>>>,
    pub call_stack: Vec<(Vec<OpCode>, Vec<Value>, usize)>,
    ip: usize,
}

impl VM {
    pub fn new(instructions: Vec<OpCode>, constants: Vec<Value>) -> Self {
        Self {
            instructions,
            constants,
            stack: Vec::new(),
            env_stack: vec![Vec::new()],
            call_stack: Vec::new(),
            ip: 0,
        }
    }
    fn current_env(&mut self) -> &mut Vec<Option<Variable>> {
        self.env_stack.last_mut().expect("No active environment!")
    }

    fn perform_call(&mut self, callable_value: Value) -> bool {
        match callable_value {
            Value::Function(func) => {
                let mut new_env = Vec::new();

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

                self.call_stack.push((self.instructions.clone(), self.constants.clone(), self.ip + 1));
                self.instructions = func.instructions.clone();
                self.ip = 0;
                self.constants = func.constants.clone(); // Swap the constants pool
                self.env_stack.push(new_env);
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
                if let Some((saved_instructions, saved_constants, saved_ip)) = self.call_stack.pop() {
                    self.instructions = saved_instructions;
                    self.constants = saved_constants; // Restore the constants pool
                    self.ip = saved_ip;
                    // Important: Don't continue the loop yet, let it increment 
                    // in the next step or just continue.
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
                    self.stack.push(val);
                }
                OpCode::DefVar(idx, is_mutable) => {
                    let val = self.pop_stack();
                    let new_var = Variable { value: val.clone(), is_mutable, reference_count: 0 };
                    let env = self.current_env();
                    if idx >= env.len() { env.resize(idx + 1, None); }
                    env[idx] = Some(new_var);
                    self.stack.push(val);
                }
                OpCode::GetVar(idx) => {
                    let current_env_idx = self.env_stack.len() - 1;
                    if let Some(Some(var)) = self.current_env().get_mut(idx) {
                        var.reference_count += 1;
                        self.stack.push(Value::Reference(current_env_idx, idx));
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
                    if let Some(Some(var)) = self.current_env().get_mut(idx) {
                        if var.reference_count > 0 {
                            panic!("Borrow error: Cannot mutate '{}' while it is borrowed!", idx);
                        }
                        if !var.is_mutable {
                            panic!("Runtime error: Cannot assign to constant '{}'", idx);
                        }
                        var.value = new_val.clone();
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
                OpCode::Return => {
                    // Pop the isolated environment
                    self.env_stack.pop();

                    // Restore the previous caller's state
                    if let Some((saved_instructions, saved_constants, saved_ip)) = self.call_stack.pop() {
                        self.instructions = saved_instructions;
                        self.constants = saved_constants; // Restore the constants pool
                        self.ip = saved_ip + 1;

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
            }
            self.ip += 1;
        }
    }
}
