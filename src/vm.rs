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
    fn get_callable(&self, idx: usize) -> Value {
        // 1. Try Local Scope
        let local_val = self.env_stack.last()
            .and_then(|env| env.get(idx))
            .and_then(|slot| slot.as_ref());

        if let Some(var) = local_val {
            // Return the value if it's any type of function
            if matches!(var.value, Value::Function(_) | Value::NativeFunction(_)) {
                return var.value.clone();
            }
        }

        // 2. Try Global Scope
        let global_val = self.env_stack[0].get(idx)
            .and_then(|slot| slot.as_ref());

        if let Some(var) = global_val {
            if matches!(var.value, Value::Function(_) | Value::NativeFunction(_)) {
                return var.value.clone();
            }
        }

        panic!("Runtime Error: Callable at index {} not found", idx);
    }
    fn pop_stack(&mut self) -> Value {
        let val = self.stack.pop().expect("Runtime error: Stack underflow");

        if let Value::Reference(idx) = val {
            if let Some(Some(var)) = self.current_env().get_mut(idx) {
                var.reference_count -= 1;
            }
        }

        val
    }
    fn resolve_value(&mut self, val: Value) -> Value {
        match val {
            Value::Reference(idx) => {
                let var = self.current_env().get(idx)
                    .and_then(|slot| slot.as_ref())
                    .expect(&format!("Runtime error: Dangling reference for {}", idx));
                var.value.clone()
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
                    let y_raw = self.pop_stack();
                    let x_raw = self.pop_stack();

                    let x = self.resolve_value(x_raw);
                    let y = self.resolve_value(y_raw);

                    match x.add(y) {
                        Ok(result) => self.stack.push(result),
                        Err(e) => {
                            eprintln!("Runtime Error: {}", e);
                            break;
                        }
                    }
                }
                OpCode::Subtract => {
                    let y_raw = self.pop_stack();
                    let x_raw = self.pop_stack();

                    let x = self.resolve_value(x_raw);
                    let y = self.resolve_value(y_raw);

                    match x.subtract(y) {
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

                    self.stack.push(rx.multiply(ry).expect("Math error")); 
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
                    let callable_value = self.get_callable(var_idx);

                    match callable_value {
                        Value::Function(func) => {
                            let caller_env_idx = self.env_stack.len() - 1;
                            let mut new_env = Vec::new();

                            for i in (0..func.param_count).rev() {
                                let arg = self.pop_stack();
                                let is_owned = func.param_ownership[i];

                                if is_owned {
                                    if let Value::Reference(idx) = arg {
                                        // Perform the MOVE
                                        let moved_var = self.env_stack[caller_env_idx][idx].take()
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

                            self.call_stack.push((self.instructions.clone(), self.constants.clone(), self.ip));
                            self.instructions = func.instructions.clone();
                            self.ip = 0;
                            self.constants = func.constants.clone(); // Swap the constants pool
                            self.env_stack.push(new_env);
                            continue;
                        }
                        Value::NativeFunction(id) => {
                            // Native functions don't need a new environment or instructions.
                            // We just pop the args and run the Rust code.
                            
                            // For now, let's assume all Native functions take 1 argument.
                            // (Later, we can store the arity in the Value::NativeFunction)
                            let arg = self.pop_stack();
                            let resolved = self.resolve_value(arg);

                            // Execute the Rust logic
                            match id {
                                crate::toolbox::NATIVE_ASSERT => {
                                    match resolved {
                                        Value::Bool(true) => println!("✅ Assertion passed"),
                                        Value::Bool(false) => panic!("❌ Assertion failed! Expected true, got false."),
                                        other => panic!("❌ Assertion failed! Expected Boolean, got {}", other),
                                    }
                                }
                                crate::toolbox::NATIVE_ABS => {
                                    // Call your math::abs logic here
                                }
                                _ => panic!("Unknown native function ID"),
                            }
                            // Native functions usually return something; for now, we just push Void
                            // self.stack.push(Value::Numeric(Number::I64(0)));
                        }
                        _ => panic!("Value at index {} is not callable", var_idx),
                    }
                }
                OpCode::DefVar(idx, is_mutable) => {
                    let val = self.pop_stack();
                    let new_var = Variable {
                        value: val,
                        is_mutable,
                        reference_count: 0
                    };

                    // Ensure the vector is large enough to hold this index
                    // This is a safety check for the manual main.rs tests
                    if idx >= self.current_env().len() {
                        self.current_env().resize(idx + 1, None);
                    }

                    self.current_env()[idx] = Some(new_var);
                }
                OpCode::GetVar(idx) => {
                    let mut found = false;

                    // Try local scope
                    if let Some(Some(var)) = self.current_env().get_mut(idx) {
                        var.reference_count += 1;
                        self.stack.push(Value::Reference(idx));
                        found = true;
                    }

                    // Try global scope 
                    if !found && self.env_stack.len() > 1 {
                        if let Some(Some(var)) = self.env_stack[0].get_mut(idx) {
                            var.reference_count += 1;
                            // We need a way to tell the difference between Global and Local refs
                            // For now, let's assume they are all local or add a 'is_global' flag
                            self.stack.push(Value::Reference(idx)); 
                            found = true;
                        }
                    }


                    if !found { panic!("Runtime error: Undefined variable '{}'", idx); }
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

                        var.value = new_val;
                    } else {
                        panic!("Runtime error: Cannot assign to undefined variable '{}'", idx);
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
            }
            self.ip += 1;
        }
    }
}
