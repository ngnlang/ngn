use crate::{bytecode::OpCode, value::{Function, Value}};

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
    fn get_function(&self, idx: usize) -> Function {
        // 1. Try Local Scope
        let local_val = self.env_stack.last()
            .and_then(|env| env.get(idx))
            .and_then(|slot| slot.as_ref());

        if let Some(var) = local_val {
            if let Value::Function(f) = &var.value {
                return (**f).clone();
            }
        }

        // 2. Try Global Scope
        let global_val = self.env_stack[0].get(idx)
            .and_then(|slot| slot.as_ref());

        if let Some(var) = global_val {
            if let Value::Function(f) = &var.value {
                return (**f).clone();
            }
        }

        panic!("Runtime Error: Function at index {} not found", idx);
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
                OpCode::Call(var_idx) => {
                    // 1. We extract the function data and CLONE it.
                    // This releases the borrow on self.env_stack immediately.
                    let func = self.get_function(var_idx);

                    let caller_env_idx = self.env_stack.len() - 1;
                    let mut new_env = Vec::new();

                    // 2. Now self.pop_stack() will work because self is no longer borrowed!
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

                    // 3. Complete the Call
                    self.call_stack.push((self.instructions.clone(), self.constants.clone(), self.ip));
                    self.instructions = func.instructions.clone();
                    self.ip = 0;
                    self.constants = func.constants.clone(); // Swap the constants pool
                    self.env_stack.push(new_env);
                    continue;
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
