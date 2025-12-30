use std::collections::HashMap;
use std::sync::Arc;
use crate::bytecode::OpCode;
use crate::lexer::Token;
use crate::parser::{Expr, Statement, Pattern, EnumDef};
use crate::value::{Function, Number, Value};

pub struct Compiler {
    pub symbol_table: HashMap<String, usize>,
    pub global_table: HashMap<String, usize>,
    pub next_index: usize,
    pub instructions: Vec<OpCode>,
    pub constants: Vec<Value>,
    pub break_patches: Vec<Vec<usize>>,
    pub match_state_vars: Vec<usize>,
    pub next_body_patches: Vec<Vec<usize>>,
    pub is_global: bool,
    pub enums: HashMap<String, EnumDef>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            symbol_table: HashMap::new(),
            global_table: HashMap::new(),
            next_index: 0,
            instructions: Vec::new(),
            constants: Vec::new(),
            break_patches: Vec::new(),
            match_state_vars: Vec::new(),
            next_body_patches: Vec::new(),
            is_global: true,
            enums: HashMap::new(),
        }
    }

    pub fn inject_builtins(&mut self) {
        // Built-in Result
        let result_enum = EnumDef {
            name: "Result".to_string(),
            variants: vec![
                crate::parser::EnumVariantDef { name: "Ok".to_string(), data_type: Some(crate::parser::Type::Any) },
                crate::parser::EnumVariantDef { name: "Error".to_string(), data_type: Some(crate::parser::Type::Any) },
            ],
        };
        
        // Built-in Maybe
        let maybe_enum = EnumDef {
            name: "Maybe".to_string(),
            variants: vec![
                crate::parser::EnumVariantDef { name: "Value".to_string(), data_type: Some(crate::parser::Type::Any) },
                crate::parser::EnumVariantDef { name: "Null".to_string(), data_type: None },
            ],
        };

        self.enums.insert("Result".to_string(), result_enum);
        self.enums.insert("Maybe".to_string(), maybe_enum);
    }

    // Helper to add a constant and return its index
    fn add_constant(&mut self, val: Value) -> usize {
        for (_idx, _existing) in self.constants.iter().enumerate() {
            // TODO - make this faster and less redundant for duplicate constants
            // We'll need to implement PartialEq for Value/Number to do this
            // if existing == &val { return idx; }
        }

        self.constants.push(val);
        self.constants.len() - 1
    }

    pub fn compile_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Assign { name, value } => {
                self.compile_expr(value);

                if let Some(&idx) = self.symbol_table.get(name) {
                    self.instructions.push(OpCode::AssignVar(idx));
                } else if let Some(&idx) = self.global_table.get(name) {
                    self.instructions.push(OpCode::AssignGlobal(idx));
                } else {
                    panic!("Compiler Error: Cannot assign to undefined variable '{}'", name);
                }
            }
            Expr::Bool(b) => {
                let idx = self.add_constant(Value::Bool(*b));
                self.instructions.push(OpCode::LoadConst(idx));
            }
            Expr::Call { name, args } => {
                for arg in args {
                    self.compile_expr(arg);
                }

                if let Some(&idx) = self.symbol_table.get(name) {
                    self.instructions.push(OpCode::Call(idx));
                } else if let Some(&idx) = self.global_table.get(name) {
                    self.instructions.push(OpCode::CallGlobal(idx));
                } else if let Some(e) = self.enums.values().find(|e| e.variants.iter().any(|v| &v.name == name)) {
                    let names_idx = self.add_constant(Value::Tuple(vec![Value::String(e.name.clone()), Value::String(name.clone())]));
                    self.instructions.push(OpCode::CreateEnum(names_idx, args.len() as u8));
                } else {
                    panic!("Compiler Error: Undefined function '{}'", name);
                }
            }
            Expr::Number(n) => {
                let idx = self.add_constant(Value::Numeric(Number::I64(*n)));
                self.instructions.push(OpCode::LoadConst(idx));
            }
            Expr::Float(n) => {
                let idx = self.add_constant(Value::Numeric(Number::F64(*n)));
                self.instructions.push(OpCode::LoadConst(idx));
            }
            Expr::String(s) => {
                let idx = self.add_constant(Value::String(s.clone()));
                self.instructions.push(OpCode::LoadConst(idx));
            }
            Expr::InterpolatedString(parts) => {
                for part in parts {
                    self.compile_expr(part);
                }
                self.instructions.push(OpCode::Concat(parts.len()));
            }
            Expr::Variable(name) => {
                if let Some(&idx) = self.symbol_table.get(name) {
                    self.instructions.push(OpCode::GetVar(idx));
                } else if let Some(e) = self.enums.values().find(|e| e.variants.iter().any(|v| &v.name == name)) {
                    let names_idx = self.add_constant(Value::Tuple(vec![Value::String(e.name.clone()), Value::String(name.clone())]));
                    self.instructions.push(OpCode::CreateEnum(names_idx, 0));
                } else if let Some(&idx) = self.global_table.get(name) {
                    self.instructions.push(OpCode::GetGlobal(idx));
                } else {
                    panic!("Compiler Error: Undefined variable '{}'", name);
                }
            }
            Expr::EnumVariant { enum_name, variant_name, args } => {
                for arg in args {
                    self.compile_expr(arg);
                }
                
                let actual_enum_name = if let Some(e) = enum_name {
                    e.clone()
                } else {
                    if let Some(e) = self.enums.values().find(|e| e.variants.iter().any(|v| &v.name == variant_name)) {
                        e.name.clone()
                    } else {
                        // Special case for built-ins if they were not in enums yet (should be)
                        panic!("Compiler Error: Unknown enum variant '{}'", variant_name);
                    }
                };

                let names_idx = self.add_constant(Value::Tuple(vec![Value::String(actual_enum_name), Value::String(variant_name.clone())]));
                self.instructions.push(OpCode::CreateEnum(names_idx, args.len() as u8));
            }
            Expr::Binary { left, op, right } => {
                self.compile_expr(left);
                self.compile_expr(right);
                match op {
                    Token::EqualEqual => self.instructions.push(OpCode::Equal),
                    Token::Plus => self.instructions.push(OpCode::Add),
                    Token::Minus => self.instructions.push(OpCode::Subtract),
                    Token::Star => self.instructions.push(OpCode::Multiply),
                    Token::Slash => self.instructions.push(OpCode::Divide),
                    Token::Power => self.instructions.push(OpCode::Power),
                    Token::Percent => self.instructions.push(OpCode::Modulo),
                    Token::LessThan => self.instructions.push(OpCode::LessThan),
                    Token::GreaterThan => self.instructions.push(OpCode::GreaterThan),
                    Token::NotEqual => self.instructions.push(OpCode::NotEqual),
                    _ => todo!("Other operators: {:?}", op),
                }
            }
            Expr::Array(elements) => {
                for element in elements {
                    self.compile_expr(element);
                }
                self.instructions.push(OpCode::BuildArray(elements.len()));
            }
            Expr::Tuple(elements) => {
                for element in elements {
                    self.compile_expr(element);
                }
                self.instructions.push(OpCode::BuildTuple(elements.len()));
            }
        }
    }

    pub fn compile_statement(&mut self, stmt: Statement) {
        match stmt {
            Statement::Enum(enum_def) => {
                self.enums.insert(enum_def.name.clone(), enum_def);
            }
            Statement::Declaration { name, is_mutable, is_static, value, declared_type: _ } => {
                self.compile_expr(&value);

                let var_idx = self.next_index;
                if is_static || self.is_global {
                    self.global_table.insert(name, var_idx);
                    self.instructions.push(OpCode::DefGlobal(var_idx, is_mutable));
                } else {
                    self.symbol_table.insert(name, var_idx);
                    self.instructions.push(OpCode::DefVar(var_idx, is_mutable));
                }
                self.next_index += 1;
                // Standardization: Pop the result of the declaration statement
                self.instructions.push(OpCode::Pop);
            }
            Statement::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.compile_expr(&expr);
                } else {
                    let null_idx = self.add_constant(Value::Void);
                    self.instructions.push(OpCode::LoadConst(null_idx));
                }
                self.instructions.push(OpCode::Return);
            }
            Statement::Expression(expr) => {
                self.compile_expr(&expr);
                self.instructions.push(OpCode::Pop);
            }
            Statement::Function { name, params, body, return_type: _ } => {
                let mut sub_compiler = Compiler::new();
                sub_compiler.is_global = false;

                // Inherit all current symbols as GLOBALS for the function
                for (n, &idx) in &self.symbol_table {
                    sub_compiler.global_table.insert(n.clone(), idx);
                }
                for (n, &idx) in &self.global_table {
                    sub_compiler.global_table.insert(n.clone(), idx);
                }
                sub_compiler.enums = self.enums.clone();

                sub_compiler.next_index = 0;

                let mut param_ownership = Vec::new();
                for param in &params {
                    let p_idx = sub_compiler.next_index;
                    sub_compiler.symbol_table.insert(param.name.clone(), p_idx);
                    sub_compiler.next_index += 1;
                    param_ownership.push(param.is_owned);
                }

                for stmt in body {
                    sub_compiler.compile_statement(stmt);
                }
                let null_idx = sub_compiler.add_constant(Value::Void);
                sub_compiler.instructions.push(OpCode::LoadConst(null_idx));
                sub_compiler.instructions.push(OpCode::Return);

                let func = Function {
                    name: name.clone(),
                    instructions: Arc::new(sub_compiler.instructions),
                    constants: Arc::new(sub_compiler.constants),
                    param_count: param_ownership.len(),
                    param_ownership,
                };

                let const_idx = self.add_constant(Value::Function(Box::new(func)));
                let var_idx = if let Some(&existing_idx) = self.global_table.get(&name) {
                    existing_idx
                } else if let Some(&existing_idx) = self.symbol_table.get(&name) {
                    existing_idx
                } else {
                    let idx = self.next_index;
                    if self.is_global {
                        self.global_table.insert(name.clone(), idx);
                    } else {
                        self.symbol_table.insert(name.clone(), idx);
                    }
                    self.next_index += 1;
                    idx
                };

                self.instructions.push(OpCode::LoadConst(const_idx));
                if self.is_global {
                    self.instructions.push(OpCode::DefGlobal(var_idx, false));
                } else {
                    self.instructions.push(OpCode::DefVar(var_idx, false));
                }
            }
            Statement::Import { names, source } => {
                // Identify if it's a toolbox import: "tbx::test"
                if source.starts_with("tbx::") {
                    let module_name = source.strip_prefix("tbx::").unwrap();
                    
                    for name in names {
                        // Map the name to our Native ID registry
                        if let Some(native_id) = crate::toolbox::get_native_id(module_name, &name) {
                            
                            // Store a "NativeFunction" in the Constants Pool
                            let const_idx = self.add_constant(Value::NativeFunction(native_id));
                            
                            // Reserve a slot in the Symbol Table (Global Index)
                            let var_idx = self.next_index;
                            self.next_index += 1;
                            
                            // Tell the VM to put the Native pointer into that slot
                            self.instructions.push(OpCode::LoadConst(const_idx));
                            if self.is_global {
                                self.global_table.insert(name.clone(), var_idx);
                                self.instructions.push(OpCode::DefGlobal(var_idx, false));
                            } else {
                                self.symbol_table.insert(name.clone(), var_idx);
                                self.instructions.push(OpCode::DefVar(var_idx, false));
                            }
                        } else {
                            panic!("Toolbox error: {} not found in tbx::{}", name, module_name);
                        }
                    }
                }
            }
            Statement::Print(expression) => {
                self.compile_expr(&expression);
                self.instructions.push(OpCode::Print);
            }
            Statement::Echo(expression) => {
                self.compile_expr(&expression);
                self.instructions.push(OpCode::Echo);
            }
            Statement::Sleep(expression) => {
                self.compile_expr(&expression);
                self.instructions.push(OpCode::Sleep);
            }
            Statement::Block(stmts) => {
                for stmt in stmts {
                    self.compile_statement(stmt);
                }
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => self.compile_if(condition, then_branch, else_branch),
            Statement::While {
                condition,
                body,
                is_once,
            } => self.compile_while(condition, body, is_once),
            Statement::Loop(body) => self.compile_loop(body),
            Statement::For { binding, index_binding, iterable, body } => self.compile_for(binding, index_binding, iterable, body),
            Statement::Match { condition, arms, is_any } => self.compile_match(condition, arms, is_any),
            Statement::Next => self.compile_next(),
            Statement::Break => self.compile_break(),
        }
    }

    fn compile_if(&mut self, condition: Expr, then_branch: Box<Statement>, else_branch: Option<Box<Statement>>) {
        // Compile the Condition
        // Output: [Instructions for Condition, Push Result]
        self.compile_expr(&condition);
        
        // The Decision Point (JumpIfFalse)
        // If condition is false, we want to SKIP the 'then' block.
        // We don't know how long the 'then' block is yet, so we emit
        // Emit JumpIfFalse(0) as a placeholder.
        let jump_false_idx = self.emit(OpCode::JumpIfFalse(0)); // 0 is placeholder

        // Compile 'Then' Block
        // These instructions follow immediately. If condition was true,
        // the VM just keeps stepping into this code.
        self.compile_statement(*then_branch);

        if let Some(else_branch) = else_branch {
            // --- CASE: IF / ELSE ---
            // Escape the 'Then' Block (Jump)
            // If we just executed the 'then' block, we MUST skip the 'else' block.
            // Again, we don't know how long the 'else' block is, so placeholder 0.
            let jump_end_idx = self.emit(OpCode::Jump(0));
            
            // Patch (Fix) the JumpIfFalse
            // Now we are at the start of the 'else' block.
            // We go back to 'jump_false_idx' and say: 
            // "Hey, if false, jump HERE (current instruction count)."
            self.patch_jump(jump_false_idx);
            
            // Compile 'Else' Block
            self.compile_statement(*else_branch);
            
            // Patch (Fix) the Escape Jump
            // Now we are at the very end.
            // We go back to 'jump_end_idx' and say:
            // "Hey, when 'then' finishes, jump HERE (the end)."
            self.patch_jump(jump_end_idx);
        } else {
            // --- CASE: IF ONLY ---
            // Patch (Fix) the JumpIfFalse
            // There is no else. So if false, jump straight to the end.
            self.patch_jump(jump_false_idx);
        }
    }

    fn compile_while(&mut self, condition: Expr, body: Box<Statement>, is_once: bool) {
        let loop_start = self.instructions.len();
        self.break_patches.push(Vec::new());

        if is_once {
            // "While Once" behavior: Body -> Condition -> JumpBack
            self.compile_statement(*body);
            
            self.compile_expr(&condition);
            let exit_jump = self.emit(OpCode::JumpIfFalse(0));
            
            self.emit(OpCode::Jump(loop_start));
            self.patch_jump(exit_jump);
        } else {
            // Standard While behavior: Condition -> JumpEnd -> Body -> JumpStart
            self.compile_expr(&condition);
            let jump_end_idx = self.emit(OpCode::JumpIfFalse(0));
            
            self.compile_statement(*body);
            
            // Jump back to start
            self.emit(OpCode::Jump(loop_start));
            
            // Patch exit jump
            self.patch_jump(jump_end_idx);
        }

        let patches = self.break_patches.pop().unwrap();
        for idx in patches {
            self.patch_jump(idx);
        }
    }

    fn compile_loop(&mut self, body: Box<Statement>) {
        let loop_start = self.instructions.len();
        self.break_patches.push(Vec::new());

        self.compile_statement(*body);

        // Jump back to start
        self.emit(OpCode::Jump(loop_start));

        let patches = self.break_patches.pop().unwrap();
        for idx in patches {
            self.patch_jump(idx);
        }
    }

    fn compile_for(&mut self, binding: String, index_binding: Option<String>, iterable: Expr, body: Box<Statement>) {
        // 1. Compile iterable
        self.compile_expr(&iterable);
        
        // 2. Emit IterStart (pushes array, then 0)
        self.emit(OpCode::IterStart);
        
        let loop_start = self.instructions.len();
        self.break_patches.push(Vec::new());
        
        // 3. Emit IterNext (pushes element, then current_index, or jumps end)
        let exit_jump = self.emit(OpCode::IterNext(0));
        
        // 4. Handle bindings
        if let Some(idx_name) = index_binding {
            let idx_var = self.next_index;
            self.next_index += 1;
            self.symbol_table.insert(idx_name, idx_var);
            self.emit(OpCode::DefVar(idx_var, false));
            self.emit(OpCode::Pop); // Pop result of DefVar
        } else {
            self.emit(OpCode::Pop); // Pop current_index
        }
        
        let binding_var = self.next_index;
        self.next_index += 1;
        self.symbol_table.insert(binding, binding_var);
        self.emit(OpCode::DefVar(binding_var, false));
        self.emit(OpCode::Pop); // Pop result of DefVar
        
        // 5. Compile body
        self.compile_statement(*body);
        
        // 6. Jump back to start
        self.emit(OpCode::Jump(loop_start));
        
        // 7. Patch exit jump
        self.patch_jump(exit_jump);
        
        // 8. Patch breaks
        let patches = self.break_patches.pop().unwrap();
        for idx in patches {
            self.patch_jump(idx);
        }
    }

    fn compile_match(&mut self, condition: Expr, arms: Vec<crate::parser::MatchArm>, is_any: bool) {
        // Condition value stays on stack, duplicated for each check.
        self.compile_expr(&condition);
        
        // Match State Variable:
        // 0: Done
        // 1: Searching (Normal)
        // 2: Searching (Any)
        // 3: Next (Checking exactly one more)
        let state_var = self.next_index;
        self.next_index += 1;
        self.match_state_vars.push(state_var);
        
        let initial_state = if is_any { 2 } else { 1 };
        let const_idx = self.add_constant(Value::Numeric(Number::I64(initial_state)));
        self.emit(OpCode::LoadConst(const_idx));
        self.emit(OpCode::DefVar(state_var, true));
        self.emit(OpCode::Pop); 

        self.break_patches.push(Vec::new());
        self.next_body_patches.push(Vec::new());

        for arm in arms {
            // Check State > 0
            self.emit(OpCode::GetVar(state_var));
            let zero_idx = self.add_constant(Value::Numeric(Number::I64(0)));
            self.emit(OpCode::LoadConst(zero_idx));
            self.emit(OpCode::GreaterThan);
            let skip_arm_if_inactive = self.emit(OpCode::JumpIfFalse(0));
            
            let mut body_entry_patches = Vec::new();
            let mut arm_failure_patches = Vec::new();

            if arm.patterns.is_empty() {
                // Default case: enters body unconditionally if arm is active
                let jump_to_body = self.emit(OpCode::Jump(0));
                body_entry_patches.push(jump_to_body);
            } else {
                for pattern in &arm.patterns {
                    let patches = self.compile_pattern(pattern);
                    body_entry_patches.extend(patches);
                }

                // If we reach here, no pattern in this arm matched.
                // If state was 3 (Next), set to 0.
                self.emit(OpCode::GetVar(state_var));
                let three_idx = self.add_constant(Value::Numeric(Number::I64(3)));
                self.emit(OpCode::LoadConst(three_idx));
                self.emit(OpCode::Equal);
                let not_next = self.emit(OpCode::JumpIfFalse(0));
                let zero_val_idx = self.add_constant(Value::Numeric(Number::I64(0)));
                self.emit(OpCode::LoadConst(zero_val_idx));
                self.emit(OpCode::AssignVar(state_var));
                self.emit(OpCode::Pop);
                self.patch_jump(not_next);

                // Jump OVER the body to the end of this arm
                arm_failure_patches.push(self.emit(OpCode::Jump(0)));
            }

            // Body Path
            for patch in body_entry_patches {
                self.patch_jump(patch);
            }

            // Transition state to 0 if not 2 (Any)
            self.emit(OpCode::GetVar(state_var));
            let two_idx_const = self.add_constant(Value::Numeric(Number::I64(2)));
            self.emit(OpCode::LoadConst(two_idx_const));
            self.emit(OpCode::Equal);
            let is_any_jump = self.emit(OpCode::JumpIfTrue(0));
            let zero_val_idx_2 = self.add_constant(Value::Numeric(Number::I64(0)));
            self.emit(OpCode::LoadConst(zero_val_idx_2));
            self.emit(OpCode::AssignVar(state_var));
            self.emit(OpCode::Pop);
            self.patch_jump(is_any_jump);

            self.compile_statement(*arm.body.clone());

            // Patch any 'next' jumps from within THIS arm's body
            let current_arm_next_patches = self.next_body_patches.last_mut().unwrap().drain(..).collect::<Vec<_>>();
            for patch in current_arm_next_patches {
                self.patch_jump(patch);
            }

            // End of Arm Label
            self.patch_jump(skip_arm_if_inactive);
            for patch in arm_failure_patches {
                self.patch_jump(patch);
            }
        }
        
        // Final Cleanup
        self.next_body_patches.pop();
        let breaks = self.break_patches.pop().unwrap();
        for patch in breaks {
            self.patch_jump(patch);
        }
        
        self.match_state_vars.pop();
        self.emit(OpCode::Pop); // Pop condition
    }

    fn compile_next(&mut self) {
        if let Some(&state_var) = self.match_state_vars.last() {
            let three_idx = self.add_constant(Value::Numeric(Number::I64(3)));
            self.emit(OpCode::LoadConst(three_idx));
            self.emit(OpCode::AssignVar(state_var));
            self.emit(OpCode::Pop);
            
            // Jump to end of current body
            let patch = self.emit(OpCode::Jump(0));
            self.next_body_patches.last_mut().unwrap().push(patch);
        } else {
            panic!("Compiler Error: 'next' used outside of match");
        }
    }

    fn compile_break(&mut self) {
        if self.break_patches.is_empty() {
            panic!("Compiler Error: 'break' used outside of loop or match");
        }
        
        // If we are inside a match, we should also set the state to 0
        if let Some(&state_var) = self.match_state_vars.last() {
             let zero_idx = self.add_constant(Value::Numeric(Number::I64(0)));
             self.emit(OpCode::LoadConst(zero_idx));
             self.emit(OpCode::AssignVar(state_var));
             self.emit(OpCode::Pop);
        }

        let idx = self.emit(OpCode::Jump(0));
        self.break_patches.last_mut().unwrap().push(idx);
    }

    fn emit(&mut self, opcode: OpCode) -> usize {
        self.instructions.push(opcode);
        self.instructions.len() - 1
    }

    fn patch_jump(&mut self, op_index: usize) {
        let target = self.instructions.len();
        match &mut self.instructions[op_index] {
             OpCode::JumpIfFalse(val) | OpCode::Jump(val) | OpCode::IterNext(val) | OpCode::JumpIfTrue(val) => {
                 *val = target;
            }
            _ => panic!("Attempted to patch non-jump opcode"),
        }
    }

    fn compile_pattern(&mut self, pattern: &Pattern) -> Vec<usize> {
        let mut patch_indices = Vec::new();
        match pattern {
            Pattern::Literal(expr) => {
                self.emit(OpCode::Dup); // Duplicate condition for comparison
                self.compile_expr(expr);
                self.emit(OpCode::Equal);
                patch_indices.push(self.emit(OpCode::JumpIfTrue(0)));
            }
            Pattern::EnumVariant { enum_name, variant_name, binding } => {
                self.emit(OpCode::Dup); // Duplicate condition for IsVariant
                let e_name = enum_name.clone().unwrap_or_else(|| "".to_string());
                let e_const = self.add_constant(Value::String(e_name));
                let v_const = self.add_constant(Value::String(variant_name.clone()));
                self.emit(OpCode::IsVariant(e_const, v_const));
                
                let is_match_jump = self.emit(OpCode::JumpIfTrue(0));
                let skip_arm = self.emit(OpCode::Jump(0));
                
                self.patch_jump(is_match_jump);
                if let Some(bind_name) = binding {
                    self.emit(OpCode::Dup); // Duplicate variant for GetVariantData
                    self.instructions.push(OpCode::GetVariantData);
                    let var_idx = self.next_index;
                    self.next_index += 1;
                    self.symbol_table.insert(bind_name.clone(), var_idx); // Note: this binding doesn't leave arm scope clean, but match handles scopes
                    self.instructions.push(OpCode::DefVar(var_idx, false));
                    self.instructions.push(OpCode::Pop);
                }
                patch_indices.push(self.emit(OpCode::Jump(0))); // Switch to success
                
                self.patch_jump(skip_arm);
            }
            Pattern::Wildcard => {
                patch_indices.push(self.emit(OpCode::Jump(0)));
            }
        }
        patch_indices
    }
}
