use std::collections::HashMap;
use crate::bytecode::OpCode;
use crate::lexer::Token;
use crate::parser::{Expr, Statement};
use crate::value::{Function, Number, Value};

pub struct Compiler {
    // Maps "name" -> Index in the VM's env
    pub symbol_table: HashMap<String, usize>,
    // Tracks the next available index
    pub next_index: usize,
    // The resulting bytecode we are building
    pub instructions: Vec<OpCode>,
    // The constant pool we are building
    pub constants: Vec<Value>,
    // Stack of loop contexts: each level contains a list of jump indices that need to be patched to the loop end
    pub break_patches: Vec<Vec<usize>>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            symbol_table: HashMap::new(),
            next_index: 0,
            instructions: Vec::new(),
            constants: Vec::new(),
            break_patches: Vec::new(),
        }
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

    // This simulates compiling "var name = value"
    pub fn compile_declaration(&mut self, name: String, expr: Expr, is_mutable: bool) {
        self.compile_expr(&expr);

        // 2. Assign this name a unique index
        let var_idx = self.next_index;
        self.symbol_table.insert(name, var_idx);
        self.next_index += 1;

        // 3. Emit the Define instruction
        self.instructions.push(OpCode::DefVar(var_idx, is_mutable));
    }

    pub fn compile_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Assign { name, value } => {
                self.compile_expr(value);

                if let Some(&idx) = self.symbol_table.get(name) {
                    self.instructions.push(OpCode::AssignVar(idx));
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

                // Find the index where the function lives in the env
                if let Some(&idx) = self.symbol_table.get(name) {
                    // 3. Just emit Call(idx). 
                    // We DON'T need GetVar here because the Call instruction 
                    // will do the lookup itself.
                    self.instructions.push(OpCode::Call(idx));
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
            Expr::Variable(name) => {
                if let Some(&idx) = self.symbol_table.get(name) {
                    self.instructions.push(OpCode::GetVar(idx));
                } else {
                    panic!("Compiler Error: Undefined variable '{}'", name);
                }
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
                    Token::LessThan => self.instructions.push(OpCode::LessThan),
                    Token::GreaterThan => self.instructions.push(OpCode::GreaterThan),
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

    // This simulates compiling "print name"
    pub fn compile_print_var(&mut self, name: &str) {
        if let Some(&idx) = self.symbol_table.get(name) {
            self.instructions.push(OpCode::GetVar(idx));
            self.instructions.push(OpCode::Print);
        } else {
            panic!("Compiler Error: Undefined variable '{}'", name);
        }
    }

    // This simulates compiling "name = value"
    pub fn compile_reassignment(&mut self, name: &str, value: Value) {
        if let Some(&idx) = self.symbol_table.get(name) {
            let const_idx = self.add_constant(value);
            self.instructions.push(OpCode::LoadConst(const_idx));
            self.instructions.push(OpCode::AssignVar(idx));
        } else {
            panic!("Compiler Error: Cannot reassign to undefined variable '{}'", name);
        }
    }

    pub fn compile_statement(&mut self, stmt: Statement) {
        match stmt {
            Statement::Declaration { name, is_mutable, value } => {
                // Compile the expression tree into bytecode
                self.compile_expr(&value);

                // Map name to index
                let var_idx = self.next_index;
                self.symbol_table.insert(name, var_idx);
                self.next_index += 1;

                // Emit the Define instruction
                self.instructions.push(OpCode::DefVar(var_idx, is_mutable));
            }
            Statement::Expression(expr) => {
                self.compile_expr(&expr);
            }
            Statement::Function { name, params, body } => {
                // Create a new compiler for the function's body
                // This ensures the function gets its own fresh symbol table!
                let mut sub_compiler = Compiler::new();

                // 1. ONLY inherit Global Functions, NOT Global Variables
                // We filter the parent's symbol table to only keep what was defined 
                // in the "Global Pass" (Pass 1). 
                for (name, &idx) in &self.symbol_table {
                    // For now, in Iteration 2, assume everything in the top-level table 
                    // at the start of a function compile is a sibling function.
                    sub_compiler.symbol_table.insert(name.clone(), idx);
                }

                // Inherit the global symbol table!
                // This allows the function to "see" other functions
                //sub_compiler.symbol_table = self.symbol_table.clone();

                // 2. Local variables start at 0
                sub_compiler.next_index = 0;

                // Inherit index pointer of parent
                //sub_compiler.next_index = self.next_index;

                // Register parameters in the sub-compiler's symbol table
                let mut param_ownership = Vec::new();
                for param in params {
                    sub_compiler.symbol_table.insert(param.name.clone(), sub_compiler.next_index);
                    sub_compiler.next_index += 1;
                    param_ownership.push(param.is_owned);
                }

                // Compile the function's body statements
                for stmt in body {
                    sub_compiler.compile_statement(stmt);
                }

                // Every function must end with a Return
                sub_compiler.instructions.push(OpCode::Return);

                // Create the Function object
                let func = Function {
                    name: name.clone(),
                    instructions: sub_compiler.instructions,
                    constants: sub_compiler.constants,
                    param_count: param_ownership.len(),
                    param_ownership,
                };

                // Store the function in the current compiler's constant pool
                let const_idx = self.add_constant(Value::Function(Box::new(func)));

                // Check if we already have an index for this name
                let var_idx = if let Some(&existing_idx) = self.symbol_table.get(&name) {
                    existing_idx
                } else {
                    // This part runs if you didn't do a pre-scan (like for nested functions)
                    let idx = self.next_index;
                    self.symbol_table.insert(name.clone(), idx);
                    self.next_index += 1;
                    idx
                };
                
                // Functions are basically "Constants" (immutables)
                self.instructions.push(OpCode::LoadConst(const_idx));
                self.instructions.push(OpCode::DefVar(var_idx, false));

                // Update parent index pointer to reflect our additions
                //self.next_index = sub_compiler.next_index;
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
                            self.symbol_table.insert(name.clone(), var_idx);
                            self.next_index += 1;
                            
                            // Tell the VM to put the Native pointer into that slot
                            self.instructions.push(OpCode::LoadConst(const_idx));
                            self.instructions.push(OpCode::DefVar(var_idx, false));
                        } else {
                            panic!("Toolbox error: {} not found in tbx::{}", name, module_name);
                        }
                    }
                }
            }
            Statement::Print(expression) => {
                // Compile the expression to put result on stack
                self.compile_expr(&expression);

                // Emit the Print instruction
                self.instructions.push(OpCode::Print);
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
            } => self.compile_while(condition, body),
            Statement::Loop(body) => self.compile_loop(body),
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

    fn compile_while(&mut self, condition: Expr, body: Box<Statement>) {
        let loop_start = self.instructions.len();
        self.break_patches.push(Vec::new());

        self.compile_expr(&condition);
        
        let jump_end_idx = self.emit(OpCode::JumpIfFalse(0));
        
        self.compile_statement(*body);
        
        // Jump back to start
        self.emit(OpCode::Jump(loop_start));
        
        // Patch exit jump
        self.patch_jump(jump_end_idx);

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

    fn compile_break(&mut self) {
        if self.break_patches.is_empty() {
            panic!("Compiler Error: 'break' used outside of loop");
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
             OpCode::JumpIfFalse(val) | OpCode::Jump(val) => {
                 *val = target;
            }
            _ => panic!("Attempted to patch non-jump opcode"),
        }
    }
}
