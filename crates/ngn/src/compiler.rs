use crate::bytecode::OpCode;
use crate::lexer::Token;
use crate::parser::{EnumDef, Expr, ExprKind, Pattern, Statement, StatementKind};
use crate::value::{Function, Number, Value};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Upvalue {
    pub index: u16,
    pub is_local: bool,
}

pub struct Compiler {
    pub enclosing: *mut Compiler,
    pub module_id: usize, // Which module this compiler is for
    pub symbol_table: HashMap<String, usize>,
    pub global_table: HashMap<String, usize>,
    pub static_values: HashMap<String, Value>, // For inlining small static constants
    pub next_index: usize,
    pub reg_top: u16,    // Current top of register "stack"
    pub max_reg: u16,    // Peak register usage
    pub temp_start: u16, // Where temporaries start (after locals)
    pub instructions: Vec<OpCode>,
    pub constants: Vec<Value>,
    pub break_patches: Vec<Vec<usize>>,
    pub match_state_vars: Vec<usize>,
    pub next_body_patches: Vec<Vec<usize>>,
    pub is_global: bool,
    pub enums: HashMap<String, EnumDef>,
    pub signatures: HashMap<String, Vec<bool>>,
    pub moved_locals: HashSet<String>,
    pub upvalues: Vec<Upvalue>,
}

impl Compiler {
    pub fn new(enclosing_opt: Option<&mut Compiler>) -> Self {
        let global_table = if let Some(parent) = &enclosing_opt {
            parent.global_table.clone()
        } else {
            HashMap::new()
        };

        let enclosing = match enclosing_opt {
            Some(c) => c as *mut Compiler,
            None => std::ptr::null_mut(),
        };

        Self {
            enclosing,
            module_id: 0, // Will be set by caller for non-main modules
            symbol_table: HashMap::new(),
            global_table,
            static_values: HashMap::new(),
            next_index: 0,
            reg_top: 0,
            max_reg: 0,
            temp_start: 0,
            instructions: Vec::new(),
            constants: Vec::new(),
            break_patches: Vec::new(),
            match_state_vars: Vec::new(),
            next_body_patches: Vec::new(),
            is_global: true,
            enums: HashMap::new(),
            signatures: HashMap::new(),
            moved_locals: HashSet::new(),
            upvalues: Vec::new(),
        }
    }

    pub fn alloc_reg(&mut self) -> u16 {
        let reg = self.reg_top;
        self.reg_top += 1;
        if self.reg_top > self.max_reg {
            self.max_reg = self.reg_top;
        }
        reg
    }

    // Registers are allocated using a simple bump allocator
    // and freed by resetting reg_top to a previous state (temp_start or saved value)

    pub fn resolve_upvalue(&mut self, name: &str) -> Option<u16> {
        if self.enclosing.is_null() {
            return None;
        }
        let parent = unsafe { &mut *self.enclosing };

        if let Some(&idx) = parent.symbol_table.get(name) {
            return Some(self.add_upvalue(idx as u16, true));
        }

        if let Some(up_idx) = parent.resolve_upvalue(name) {
            return Some(self.add_upvalue(up_idx, false));
        }
        None
    }

    fn add_upvalue(&mut self, index: u16, is_local: bool) -> u16 {
        for (i, up) in self.upvalues.iter().enumerate() {
            if up.index == index && up.is_local == is_local {
                return i as u16;
            }
        }
        self.upvalues.push(Upvalue { index, is_local });
        (self.upvalues.len() - 1) as u16
    }

    pub fn inject_builtins(&mut self) {
        // Built-in Result
        let result_enum = EnumDef {
            name: "Result".to_string(),
            variants: vec![
                crate::parser::EnumVariantDef {
                    name: "Ok".to_string(),
                    data_type: Some(crate::parser::Type::Any),
                },
                crate::parser::EnumVariantDef {
                    name: "Error".to_string(),
                    data_type: Some(crate::parser::Type::Any),
                },
            ],
        };

        // Built-in Maybe
        let maybe_enum = EnumDef {
            name: "Maybe".to_string(),
            variants: vec![
                crate::parser::EnumVariantDef {
                    name: "Value".to_string(),
                    data_type: Some(crate::parser::Type::Any),
                },
                crate::parser::EnumVariantDef {
                    name: "Null".to_string(),
                    data_type: None,
                },
            ],
        };

        self.enums.insert("Result".to_string(), result_enum);
        self.enums.insert("Maybe".to_string(), maybe_enum);

        // Standard Functions (handled as special opcodes, not CallGlobal)
        // These entries are just for name resolution in the compiler
        self.global_table.insert("print".to_string(), 0);
        self.global_table.insert("echo".to_string(), 1);
        self.global_table.insert("sleep".to_string(), 2);
        self.next_index = 3;
    }

    // Helper to add a constant and return its index
    pub fn add_constant(&mut self, val: Value) -> usize {
        // Deduplicate constants to reduce bytecode size
        for (idx, existing) in self.constants.iter().enumerate() {
            if existing == &val {
                return idx;
            }
        }

        self.constants.push(val);
        self.constants.len() - 1
    }

    // Compile a list of expressions into consecutive registers starting at current reg_top.
    // Returns the start_reg.
    fn compile_args(&mut self, args: &[Expr]) -> u16 {
        let start_reg = self.reg_top;
        for (i, arg) in args.iter().enumerate() {
            let expected_reg = start_reg + i as u16;
            let result_reg = self.compile_expr(arg);
            if result_reg != expected_reg {
                self.instructions
                    .push(OpCode::Move(expected_reg, result_reg));
            }
            if self.reg_top <= expected_reg {
                self.reg_top = expected_reg + 1;
            }
        }
        start_reg
    }

    pub fn compile_expr(&mut self, expr: &Expr) -> u16 {
        match &expr.kind {
            ExprKind::Assign { name, value } => {
                let src_reg = self.compile_expr(value);

                if let Some(&idx) = self.symbol_table.get(name) {
                    self.instructions
                        .push(OpCode::AssignVar(idx as u16, src_reg));
                    src_reg
                } else if let Some(&idx) = self.global_table.get(name) {
                    self.instructions.push(OpCode::AssignGlobal(idx, src_reg));
                    src_reg
                } else {
                    panic!(
                        "Compiler Error: Cannot assign to undefined variable '{}'",
                        name
                    );
                }
            }
            ExprKind::Bool(b) => {
                let dest = self.alloc_reg();
                let idx = self.add_constant(Value::Bool(*b));
                self.instructions.push(OpCode::LoadConst(dest, idx));
                dest
            }
            ExprKind::Call { name, args } => {
                match name.as_str() {
                    "print" => {
                        let reg = self.compile_expr(&args[0]);
                        self.instructions.push(OpCode::Print(reg));
                        return reg;
                    }
                    "echo" => {
                        let reg = self.compile_expr(&args[0]);
                        self.instructions.push(OpCode::Echo(reg));
                        return reg;
                    }
                    "sleep" => {
                        let reg = self.compile_expr(&args[0]);
                        self.instructions.push(OpCode::Sleep(reg));
                        return reg;
                    }
                    _ => {}
                }

                let start_reg = self.compile_args(args);
                let dest = self.alloc_reg();

                // Ownership Check
                if let Some(sig) = self.signatures.get(name) {
                    for (i, arg_expr) in args.iter().enumerate() {
                        if i < sig.len() && sig[i] {
                            // This param is OWNED. Check if we passed a variable.
                            if let ExprKind::Variable(var_name) = &arg_expr.kind {
                                self.moved_locals.insert(var_name.clone());
                            }
                        }
                    }
                }

                if let Some(&idx) = self.symbol_table.get(name) {
                    // Local function: load from local register into a temp, then call
                    let func_reg = self.alloc_reg();
                    self.instructions.push(OpCode::Move(func_reg, idx as u16));
                    self.instructions.push(OpCode::Call(
                        dest,
                        func_reg,
                        start_reg,
                        args.len() as u8,
                    ));
                    self.reg_top = dest + 1; // Free the temp func_reg
                } else if let Some(&idx) = self.global_table.get(name) {
                    // Global function: use CallGlobal which fetches from env_stack[0]
                    self.instructions.push(OpCode::CallGlobal(
                        dest,
                        idx,
                        start_reg,
                        args.len() as u8,
                    ));
                    self.reg_top = dest + 1;
                } else if let Some(e) = self
                    .enums
                    .values()
                    .find(|e| e.variants.iter().any(|v| &v.name == name))
                {
                    let names_idx = self.add_constant(Value::Tuple(vec![
                        Value::String(e.name.clone()),
                        Value::String(name.clone()),
                    ]));
                    self.instructions.push(OpCode::CreateEnum(
                        dest,
                        names_idx,
                        start_reg,
                        args.len() as u8,
                    ));
                    self.reg_top = dest + 1;
                } else {
                    panic!("Compiler Error: Undefined function '{}'", name);
                }

                dest
            }
            ExprKind::Number(n) => {
                let dest = self.alloc_reg();
                let idx = self.add_constant(Value::Numeric(Number::I64(*n)));
                self.instructions.push(OpCode::LoadConst(dest, idx));
                dest
            }
            ExprKind::Float(n) => {
                let dest = self.alloc_reg();
                let idx = self.add_constant(Value::Numeric(Number::F64(*n)));
                self.instructions.push(OpCode::LoadConst(dest, idx));
                dest
            }
            ExprKind::String(s) => {
                let dest = self.alloc_reg();
                let idx = self.add_constant(Value::String(s.clone()));
                self.instructions.push(OpCode::LoadConst(dest, idx));
                dest
            }
            ExprKind::Regex(pattern) => {
                let dest = self.alloc_reg();
                let idx = self.add_constant(Value::String(pattern.clone()));
                self.instructions.push(OpCode::MakeRegex(dest, idx));
                dest
            }
            ExprKind::InterpolatedString(parts) => {
                let start_reg = self.compile_args(parts);
                let dest = self.alloc_reg();
                self.instructions
                    .push(OpCode::Concat(dest, start_reg, parts.len()));
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::Variable(name) => {
                if self.moved_locals.contains(name) {
                    panic!("Compiler Error: Use of moved variable '{}'", name);
                }
                if self.static_values.contains_key(name) {
                    let val = self.static_values.get(name).unwrap().clone();
                    // Inline small static value
                    let dest = self.alloc_reg();
                    let idx = self.add_constant(val);
                    self.instructions.push(OpCode::LoadConst(dest, idx));
                    dest
                } else if let Some(&idx) = self.symbol_table.get(name) {
                    idx as u16
                } else if let Some(up_idx) = self.resolve_upvalue(name) {
                    let dest = self.alloc_reg();
                    self.instructions.push(OpCode::GetUpvalue(dest, up_idx));
                    dest
                } else if let Some(enum_name) = self
                    .enums
                    .values()
                    .find(|e| e.variants.iter().any(|v| &v.name == name))
                    .map(|e| e.name.clone())
                {
                    let dest = self.alloc_reg();
                    let names_idx = self.add_constant(Value::Tuple(vec![
                        Value::String(enum_name),
                        Value::String(name.clone()),
                    ]));
                    self.instructions
                        .push(OpCode::CreateEnum(dest, names_idx, 0, 0));
                    dest
                } else if let Some(&idx) = self.global_table.get(name) {
                    let dest = self.alloc_reg();
                    self.instructions.push(OpCode::GetGlobal(dest, idx));
                    dest
                } else {
                    panic!("Compiler Error: Undefined variable '{}'", name);
                }
            }
            ExprKind::Closure {
                params,
                body,
                return_type: _,
            } => {
                let (func, upvalues) = {
                    let global_table = self.global_table.clone();
                    let static_values = self.static_values.clone();
                    let enums = self.enums.clone();
                    let signatures = self.signatures.clone();

                    let mut sub_compiler = Compiler::new(Some(self));
                    sub_compiler.is_global = false;
                    sub_compiler.global_table = global_table;
                    sub_compiler.static_values = static_values;
                    sub_compiler.enums = enums;
                    sub_compiler.signatures = signatures;
                    sub_compiler.next_index = 0;

                    let mut param_ownership = Vec::new();
                    for param in params {
                        let p_idx = sub_compiler.next_index;
                        sub_compiler.symbol_table.insert(param.name.clone(), p_idx);
                        sub_compiler.next_index += 1;
                        param_ownership.push(param.is_owned);
                    }

                    sub_compiler.temp_start = sub_compiler.next_index as u16;
                    sub_compiler.reg_top = sub_compiler.temp_start;
                    sub_compiler.max_reg = sub_compiler.reg_top;

                    sub_compiler.compile_statement(*body.clone());
                    sub_compiler.instructions.push(OpCode::ReturnVoid);

                    let captured_upvalues = sub_compiler.upvalues.clone();

                    let func = Function {
                        name: "closure".to_string(),
                        instructions: Arc::new(sub_compiler.instructions),
                        constants: Arc::new(sub_compiler.constants),
                        home_globals: None, // Set by VM at load time
                        param_count: params.len(),
                        param_ownership,
                        reg_count: sub_compiler.max_reg as usize,
                        upvalues: captured_upvalues.clone(),
                    };

                    (func, captured_upvalues)
                };

                let upvalue_start = self.reg_top;

                for (i, up) in upvalues.iter().enumerate() {
                    let target_reg = upvalue_start + i as u16;
                    if self.reg_top <= target_reg {
                        self.reg_top = target_reg + 1;
                    }

                    if up.is_local {
                        self.instructions.push(OpCode::Move(target_reg, up.index));
                    } else {
                        self.instructions
                            .push(OpCode::GetUpvalue(target_reg, up.index));
                    }
                }

                let func_idx = self.add_constant(Value::Function(Box::new(func)));
                let dest = self.alloc_reg();
                self.instructions.push(OpCode::MakeClosure(
                    dest,
                    func_idx,
                    upvalue_start,
                    upvalues.len() as u8,
                ));
                self.reg_top = dest + 1;

                dest
            }
            ExprKind::EnumVariant {
                enum_name,
                variant_name,
                args,
            } => {
                let start_reg = self.compile_args(args);

                let actual_enum_name = if let Some(e) = enum_name {
                    e.clone()
                } else {
                    if let Some(e) = self
                        .enums
                        .values()
                        .find(|e| e.variants.iter().any(|v| &v.name == variant_name))
                    {
                        e.name.clone()
                    } else {
                        panic!("Compiler Error: Unknown enum variant '{}'", variant_name);
                    }
                };

                let dest = self.alloc_reg();
                let names_idx = self.add_constant(Value::Tuple(vec![
                    Value::String(actual_enum_name),
                    Value::String(variant_name.clone()),
                ]));
                self.instructions.push(OpCode::CreateEnum(
                    dest,
                    names_idx,
                    start_reg,
                    args.len() as u8,
                ));
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::Binary { left, op, right } => {
                let left_reg = self.compile_expr(left);
                let right_reg = self.compile_expr(right);
                let dest = self.alloc_reg();
                match op {
                    Token::EqualEqual => self
                        .instructions
                        .push(OpCode::Equal(dest, left_reg, right_reg)),
                    Token::Plus => self
                        .instructions
                        .push(OpCode::Add(dest, left_reg, right_reg)),
                    Token::Minus => self
                        .instructions
                        .push(OpCode::Subtract(dest, left_reg, right_reg)),
                    Token::Star => self
                        .instructions
                        .push(OpCode::Multiply(dest, left_reg, right_reg)),
                    Token::Slash => self
                        .instructions
                        .push(OpCode::Divide(dest, left_reg, right_reg)),
                    Token::Power => self
                        .instructions
                        .push(OpCode::Power(dest, left_reg, right_reg)),
                    Token::Percent => self
                        .instructions
                        .push(OpCode::Modulo(dest, left_reg, right_reg)),
                    Token::LessThan => self
                        .instructions
                        .push(OpCode::LessThan(dest, left_reg, right_reg)),
                    Token::GreaterThan => self
                        .instructions
                        .push(OpCode::GreaterThan(dest, left_reg, right_reg)),
                    Token::NotEqual => self
                        .instructions
                        .push(OpCode::NotEqual(dest, left_reg, right_reg)),
                    _ => todo!("Other operators: {:?}", op),
                }
                // Free temporaries but keep result
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::Array(elements) => {
                let start_reg = self.compile_args(elements);
                let dest = self.alloc_reg();
                self.instructions
                    .push(OpCode::BuildArray(dest, start_reg, elements.len()));
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::Tuple(elements) => {
                let start_reg = self.compile_args(elements);
                let dest = self.alloc_reg();
                self.instructions
                    .push(OpCode::BuildTuple(dest, start_reg, elements.len()));
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::Thread(expr) => {
                let closure_reg = self.compile_expr(expr);
                let dest = self.alloc_reg();
                self.instructions.push(OpCode::Spawn(dest, closure_reg));
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::Channel(_) => {
                let dest = self.alloc_reg();
                self.instructions.push(OpCode::CreateChannel(dest, 10)); // Default capacity
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::Receive(chan_expr) => {
                let chan_reg = self.compile_expr(chan_expr);
                let dest = self.alloc_reg();
                self.instructions.push(OpCode::Receive(dest, chan_reg));
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::Send(chan_expr, val_expr) => {
                let chan_reg = self.compile_expr(chan_expr);
                let val_reg = self.compile_expr(val_expr);
                let dest = self.alloc_reg();
                self.instructions.push(OpCode::Send(chan_reg, val_reg));
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::ReceiveCount(chan_expr, count_expr) => {
                let chan_reg = self.compile_expr(chan_expr);
                let count_reg = self.compile_expr(count_expr);
                let dest = self.alloc_reg();
                self.instructions
                    .push(OpCode::ReceiveCount(dest, chan_reg, count_reg));
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::ReceiveMaybe(chan_expr) => {
                let chan_reg = self.compile_expr(chan_expr);
                let dest = self.alloc_reg();
                self.instructions.push(OpCode::ReceiveMaybe(dest, chan_reg));
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::State(initial_expr) => {
                let initial_reg = self.compile_expr(initial_expr);
                let dest = self.alloc_reg();
                self.instructions
                    .push(OpCode::CreateState(dest, initial_reg));
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::ModelInstance { name, fields } => {
                let model_name_idx = self.add_constant(Value::String(name.clone()));
                // Collect field names and compile values into consecutive registers
                let mut field_names = Vec::new();
                for (f_name, _) in fields {
                    field_names.push(Value::String(f_name.clone()));
                }
                let fields_idx = self.add_constant(Value::Tuple(field_names));

                // Use compile_args-like pattern for correct consecutive register placement
                let start_reg = self.reg_top;
                for (i, (_, f_expr)) in fields.iter().enumerate() {
                    let expected_reg = start_reg + i as u16;
                    let result_reg = self.compile_expr(f_expr);
                    if result_reg != expected_reg {
                        self.instructions
                            .push(OpCode::Move(expected_reg, result_reg));
                    }
                    if self.reg_top <= expected_reg {
                        self.reg_top = expected_reg + 1;
                    }
                }

                let count = fields.len() as u8;
                let dest = self.alloc_reg();
                self.instructions.push(OpCode::CreateObject(
                    dest,
                    model_name_idx,
                    fields_idx,
                    start_reg,
                    count,
                ));
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::FieldAccess { object, field } => {
                let obj_reg = self.compile_expr(object);
                let field_idx = self.add_constant(Value::String(field.clone()));
                let dest = self.alloc_reg();
                self.instructions
                    .push(OpCode::GetField(dest, obj_reg, field_idx));
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::This => {
                if let Some(idx) = self.symbol_table.get("this").cloned() {
                    let dest = self.alloc_reg();
                    self.instructions.push(OpCode::Move(dest, idx as u16));
                    self.reg_top = dest + 1;
                    dest
                } else if let Some(up_idx) = self.resolve_upvalue("this") {
                    let dest = self.alloc_reg();
                    self.instructions.push(OpCode::GetUpvalue(dest, up_idx));
                    self.reg_top = dest + 1;
                    dest
                } else {
                    panic!("Compiler Error: 'this' used outside of method context");
                }
            }
            ExprKind::MethodCall(obj_expr, method, args) => {
                let obj_reg = self.compile_expr(obj_expr);
                let dest = self.alloc_reg();

                // State actor and channel methods use dedicated opcodes
                match method.as_str() {
                    "read" => {
                        self.instructions.push(OpCode::StateRead(dest, obj_reg));
                        self.reg_top = dest + 1;
                        return dest;
                    }
                    "write" => {
                        let arg_reg = self.compile_expr(&args[0]);
                        self.instructions.push(OpCode::StateWrite(obj_reg, arg_reg));
                        self.reg_top = dest + 1;
                        return dest;
                    }
                    "update" => {
                        let arg_reg = self.compile_expr(&args[0]);
                        self.instructions
                            .push(OpCode::StateUpdate(obj_reg, arg_reg));
                        self.reg_top = dest + 1;
                        return dest;
                    }
                    "close" => {
                        self.instructions.push(OpCode::CloseChannel(obj_reg));
                        self.reg_top = dest + 1;
                        return dest;
                    }
                    _ => {}
                }

                // For all other methods, use the generic CallMethod opcode
                let arg_start = self.compile_args(args);
                let method_idx = self.add_constant(Value::String(method.clone()));

                // Mutating methods need CallMethodMut to write back to the source register
                let is_mutating = matches!(
                    method.as_str(),
                    "push" | "pull" | "slice" | "splice" |  // array mutating methods
                    "set" | "remove" |  // map mutating methods (remove is shared with set)
                    "add" // set mutating methods
                );

                if is_mutating {
                    self.instructions.push(OpCode::CallMethodMut(
                        dest,
                        obj_reg,
                        method_idx,
                        arg_start,
                        args.len() as u8,
                    ));
                } else {
                    self.instructions.push(OpCode::CallMethod(
                        dest,
                        obj_reg,
                        method_idx,
                        arg_start,
                        args.len() as u8,
                    ));
                }
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::Index(obj_expr, index_expr) => {
                let obj_reg = self.compile_expr(obj_expr);
                let index_reg = self.compile_expr(index_expr);
                let dest = self.alloc_reg();
                self.instructions
                    .push(OpCode::GetIndex(dest, obj_reg, index_reg));
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::Map(_, _) => {
                let dest = self.alloc_reg();
                self.instructions.push(OpCode::CreateMap(dest));
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::Set(_) => {
                let dest = self.alloc_reg();
                self.instructions.push(OpCode::CreateSet(dest));
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::Unary { op, right } => {
                let reg = self.compile_expr(right);
                let dest = self.alloc_reg();
                match op {
                    Token::Minus => self.instructions.push(OpCode::Negate(dest, reg)),
                    Token::Bang => self.instructions.push(OpCode::Not(dest, reg)),
                    _ => panic!("Compiler Error: Unknown unary operator {:?}", op),
                }
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::Error(msg) => {
                panic!("Compiler received invalid AST: {}", msg);
            }
        }
    }

    fn extract_constant_literal(&self, expr: &Expr) -> Option<Value> {
        match &expr.kind {
            ExprKind::Number(n) => Some(Value::Numeric(Number::I64(*n))),
            ExprKind::Float(n) => Some(Value::Numeric(Number::F64(*n))),
            ExprKind::Bool(b) => Some(Value::Bool(*b)),
            ExprKind::String(s) if s.len() <= 64 => Some(Value::String(s.clone())),
            ExprKind::Array(elements)
                if elements.len() < 5 && elements.iter().all(|el| el.is_primitive()) =>
            {
                let mut values = Vec::new();
                for el in elements {
                    if let Some(v) = self.extract_constant_literal(el) {
                        values.push(v);
                    } else {
                        return None;
                    }
                }
                Some(Value::Array(values))
            }
            ExprKind::Tuple(elements)
                if elements.len() < 5 && elements.iter().all(|el| el.is_primitive()) =>
            {
                let mut values = Vec::new();
                for el in elements {
                    if let Some(v) = self.extract_constant_literal(el) {
                        values.push(v);
                    } else {
                        return None;
                    }
                }
                Some(Value::Tuple(values))
            }
            _ => None,
        }
    }

    pub fn compile_statement(&mut self, stmt: Statement) {
        match stmt.kind {
            StatementKind::Enum(enum_def) => {
                self.enums.insert(enum_def.name.clone(), enum_def);
            }
            StatementKind::Declaration {
                name,
                is_mutable,
                is_static,
                value,
                declared_type: _,
            } => {
                let var_idx = self.next_index;

                if is_static {
                    // Check if value is a "small" constant for inlining
                    // Primitives: Number, Float, Bool, Void, Null
                    // Strings: length <= 64
                    // Arrays: size < 10
                    // Tuples: size < 10
                    let inline_val = self.extract_constant_literal(&value);

                    if let Some(val) = inline_val {
                        self.static_values.insert(name.clone(), val);
                        // Optimization: If inlined, we don't need to generate a runtime global slot at all.
                        return;
                    }
                }

                if is_static || self.is_global {
                    // For global/static, compile expression and move result to global slot
                    let res_reg = self.compile_expr(&value);
                    self.global_table.insert(name, var_idx);
                    self.instructions
                        .push(OpCode::AssignGlobal(var_idx, res_reg));
                } else {
                    // For locals, compile into the variable slot directly if possible
                    let res_reg = self.compile_expr(&value);
                    self.symbol_table.insert(name, var_idx);
                    self.instructions
                        .push(OpCode::Move(var_idx as u16, res_reg));
                    self.instructions.push(OpCode::DefVar(var_idx, is_mutable));
                }
                self.next_index += 1;
                self.temp_start = self.next_index as u16;
                self.reg_top = self.temp_start;
            }
            StatementKind::Model(_) | StatementKind::Role(_) => {}
            StatementKind::Extend {
                target, methods, ..
            } => {
                let target_name = match target {
                    crate::parser::Type::I64
                    | crate::parser::Type::I32
                    | crate::parser::Type::I16
                    | crate::parser::Type::I8 => "i64".to_string(),
                    crate::parser::Type::U64
                    | crate::parser::Type::U32
                    | crate::parser::Type::U16
                    | crate::parser::Type::U8 => "i64".to_string(),
                    crate::parser::Type::F64 | crate::parser::Type::F32 => "f64".to_string(),
                    crate::parser::Type::String => "string".to_string(),
                    crate::parser::Type::Bool => "bool".to_string(),
                    crate::parser::Type::Array(_) => "array".to_string(),
                    crate::parser::Type::Tuple(_) => "tuple".to_string(),
                    crate::parser::Type::Model(ref name) => name.clone(),
                    crate::parser::Type::Channel(_) => "channel".to_string(),
                    crate::parser::Type::State(_) => "state".to_string(),
                    crate::parser::Type::Map(_, _) => "map".to_string(),
                    crate::parser::Type::Set(_) => "set".to_string(),
                    crate::parser::Type::Number => "number".to_string(),
                    _ => "".to_string(),
                };

                if target_name.is_empty() {
                    return;
                }
                let target_idx = self.add_constant(Value::String(target_name));

                for method in methods {
                    if let StatementKind::Function {
                        name, params, body, ..
                    } = method.kind
                    {
                        let mut method_params = vec![crate::parser::Parameter {
                            name: "this".to_string(),
                            ty: Some(target.clone()),
                            is_owned: false,
                            span: crate::lexer::Span::default(),
                        }];
                        method_params.extend(params);

                        let func = self.compile_function_helper(name.clone(), method_params, body);
                        let func_idx = self.add_constant(Value::Function(Box::new(func)));
                        let closure_reg = self.alloc_reg();
                        self.instructions
                            .push(OpCode::MakeClosure(closure_reg, func_idx, 0, 0));

                        let name_idx = self.add_constant(Value::String(name));
                        self.instructions.push(OpCode::DefMethod(
                            target_idx,
                            name_idx,
                            closure_reg,
                        ));
                        self.reg_top = self.temp_start;
                    }
                }
            }
            StatementKind::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    let reg = self.compile_expr(&expr);
                    self.instructions.push(OpCode::Return(reg));
                } else {
                    self.instructions.push(OpCode::ReturnVoid);
                }
            }
            StatementKind::Error(msg) => {
                panic!("Compiler received invalid AST: {}", msg);
            }
            StatementKind::Expression(expr) => {
                self.compile_expr(&expr);
                self.reg_top = self.temp_start;
            }
            StatementKind::Function {
                name, params, body, ..
            } => {
                let func = self.compile_function_helper(name.clone(), params, body);

                // Check if the function captures upvalues
                let upvalue_count = func.upvalues.len();
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

                if upvalue_count > 0 {
                    // Prepare upvalues in contiguous registers starting at reg_top
                    let base = self.reg_top;

                    // Retrieve upvalue info from the constant we just added
                    if let Value::Function(f) = &self.constants[const_idx] {
                        for (i, up) in f.upvalues.iter().enumerate() {
                            let target = base + i as u16;
                            self.max_reg = self.max_reg.max(target + 1);

                            if up.is_local {
                                // Captured local from current scope
                                self.instructions.push(OpCode::Move(target, up.index));
                            } else {
                                // Captured upvalue from current scope
                                self.instructions.push(OpCode::GetUpvalue(target, up.index));
                            }
                        }
                    }

                    // The destination for the closure
                    let dest = if self.is_global {
                        self.alloc_reg()
                    } else {
                        var_idx as u16
                    };

                    self.instructions.push(OpCode::MakeClosure(
                        dest,
                        const_idx,
                        base,
                        upvalue_count as u8,
                    ));

                    if self.is_global {
                        self.instructions.push(OpCode::DefGlobal(var_idx, false));
                        self.instructions.push(OpCode::AssignGlobal(var_idx, dest));
                    } else {
                        self.instructions.push(OpCode::DefVar(var_idx, false));
                        self.instructions.push(OpCode::Move(var_idx as u16, dest));
                    }

                    self.temp_start = self.next_index as u16;
                    self.reg_top = self.temp_start;
                } else {
                    // No upvalues, just load the function constant
                    let reg = self.alloc_reg();
                    self.instructions.push(OpCode::LoadConst(reg, const_idx));

                    if self.is_global {
                        self.instructions.push(OpCode::DefGlobal(var_idx, false));
                        self.instructions.push(OpCode::AssignGlobal(var_idx, reg));
                    } else {
                        self.instructions.push(OpCode::DefVar(var_idx, false));
                        self.instructions.push(OpCode::Move(var_idx as u16, reg));
                    }
                    self.temp_start = self.next_index as u16;
                    self.reg_top = self.temp_start;
                }
            }
            StatementKind::Import { names, source } => {
                // Identify if it's a toolbox import: "tbx::test"
                if source.starts_with("tbx::") {
                    let module_name = source.strip_prefix("tbx::").unwrap();

                    for (name, alias) in names {
                        // Map the name to our Native ID registry
                        if let Some(native_id) = crate::toolbox::get_native_id(module_name, &name) {
                            // Store a "NativeFunction" in the Constants Pool
                            let const_idx = self.add_constant(Value::NativeFunction(native_id));

                            // Reserve a slot in the Symbol Table (Global Index)
                            let var_idx = self.next_index;
                            self.next_index += 1;

                            // bind name
                            let bind_name = alias.as_ref().unwrap_or(&name);

                            // Tell the VM to put the Native pointer into that slot
                            let reg = self.alloc_reg();
                            self.instructions.push(OpCode::LoadConst(reg, const_idx));
                            if self.is_global {
                                self.global_table.insert(bind_name.clone(), var_idx);
                                self.instructions.push(OpCode::DefGlobal(var_idx, false));
                                self.instructions.push(OpCode::AssignGlobal(var_idx, reg));
                            } else {
                                self.symbol_table.insert(bind_name.clone(), var_idx);
                                self.instructions.push(OpCode::DefVar(var_idx, false));
                                self.instructions.push(OpCode::Move(var_idx as u16, reg));
                            }
                            self.reg_top = self.temp_start;
                        } else {
                            panic!("Toolbox error: {} not found in tbx::{}", name, module_name);
                        }
                    }
                }
            }
            StatementKind::Print(expression) => {
                let reg = self.compile_expr(&expression);
                self.instructions.push(OpCode::Print(reg));
                self.reg_top = self.temp_start;
            }
            StatementKind::Echo(expression) => {
                let reg = self.compile_expr(&expression);
                self.instructions.push(OpCode::Echo(reg));
                self.reg_top = self.temp_start;
            }
            StatementKind::Sleep(expression) => {
                let reg = self.compile_expr(&expression);
                self.instructions.push(OpCode::Sleep(reg));
                self.reg_top = self.temp_start;
            }
            StatementKind::Block(stmts) => {
                for stmt in stmts {
                    self.compile_statement(stmt);
                }
            }
            StatementKind::If {
                condition,
                then_branch,
                else_branch,
            } => self.compile_if(condition, then_branch, else_branch),
            StatementKind::While {
                condition,
                body,
                is_once,
            } => self.compile_while(condition, body, is_once),
            StatementKind::Loop(body) => self.compile_loop(body),
            StatementKind::For {
                binding,
                index_binding,
                iterable,
                body,
            } => self.compile_for(binding, index_binding, iterable, body),
            StatementKind::Match { condition, arms } => self.compile_match(condition, arms),
            StatementKind::Next => self.compile_next(),
            StatementKind::Break => self.compile_break(),
            StatementKind::ImportDefault { .. }
            | StatementKind::ImportModule { .. }
            | StatementKind::ExportDefault(_) => {}
        }
    }

    fn compile_if(
        &mut self,
        condition: Expr,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    ) {
        // Compile the Condition
        // Output: [Instructions for Condition, Push Result]
        let cond_reg = self.compile_expr(&condition);

        // The Decision Point (JumpIfFalse)
        // If condition is false, we want to SKIP the 'then' block.
        let jump_false_idx = self.emit(OpCode::JumpIfFalse(cond_reg, 0)); // 0 is placeholder
        self.reg_top = self.temp_start;

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

            let cond_reg = self.compile_expr(&condition);
            let exit_jump = self.emit(OpCode::JumpIfFalse(cond_reg, 0));
            self.reg_top = self.temp_start;

            self.emit(OpCode::Jump(loop_start));
            self.patch_jump(exit_jump);
        } else {
            // Standard While behavior: Condition -> JumpEnd -> Body -> JumpStart
            let cond_reg = self.compile_expr(&condition);
            let jump_end_idx = self.emit(OpCode::JumpIfFalse(cond_reg, 0));
            self.reg_top = self.temp_start;

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

    fn compile_for(
        &mut self,
        binding: String,
        index_binding: Option<String>,
        iterable: Expr,
        body: Box<Statement>,
    ) {
        // Special case: for (msg in <-? chan)
        let src_reg = if let ExprKind::ReceiveMaybe(chan_expr) = &iterable.kind {
            self.compile_expr(chan_expr)
        } else {
            // 1. Compile iterable normally
            self.compile_expr(&iterable)
        };

        // 2. Allocate two registers for iterator: [coll, index]
        let iter_reg = self.alloc_reg();
        let _idx_temp = self.alloc_reg(); // Reserved for internal index (or -1 for channel)

        // Keep next_index in sync to avoid collision with binding variables
        if (self.reg_top as usize) > self.next_index {
            self.next_index = self.reg_top as usize;
            self.temp_start = self.reg_top;
        }

        // 3. Emit IterStart
        self.instructions.push(OpCode::IterStart(iter_reg, src_reg));

        let loop_start = self.instructions.len();
        self.break_patches.push(Vec::new());

        // 4. Emit IterNext
        let val_reg = self.alloc_reg();
        let exit_jump = self.emit(OpCode::IterNext(val_reg, iter_reg, 0));

        // 5. Handle bindings
        if let Some(idx_name) = index_binding {
            // Register for user-facing index
            let idx_reg = self.alloc_reg();

            // Calculate 0-based index: internal_index - 1
            let one_reg = self.alloc_reg();
            let one_const = self.add_constant(Value::Numeric(Number::I64(1)));
            self.instructions
                .push(OpCode::LoadConst(one_reg, one_const));
            self.instructions
                .push(OpCode::Subtract(idx_reg, iter_reg + 1, one_reg));

            self.symbol_table.insert(idx_name, idx_reg as usize);
            self.instructions
                .push(OpCode::DefVar(idx_reg as usize, false));
        }

        // Reuse val_reg for the loop variable binding
        self.symbol_table.insert(binding, val_reg as usize);
        self.instructions
            .push(OpCode::DefVar(val_reg as usize, false));

        // Sync next_index to ensure subsequent allocations don't overwrite our loop variables
        if (self.reg_top as usize) > self.next_index {
            self.next_index = self.reg_top as usize;
        }
        self.temp_start = self.next_index as u16;

        // 6. Compile body
        self.compile_statement(*body);

        // 7. Jump back to start
        self.emit(OpCode::Jump(loop_start));

        // 8. Patch exit jump
        self.patch_jump(exit_jump);

        // 9. Patch breaks
        let patches = self.break_patches.pop().unwrap();
        for idx in patches {
            self.patch_jump(idx);
        }

        // Cleanup: free temporary registers used by iterator and val
        self.reg_top = iter_reg; // Frees iter_reg, idx_temp, and val_reg
    }

    fn compile_match(&mut self, condition: Expr, arms: Vec<crate::parser::MatchArm>) {
        let match_reg = self.compile_expr(&condition);

        // Sync next_index to preserve match_reg if it used temp regs
        if (self.reg_top as usize) > self.next_index {
            self.next_index = self.reg_top as usize;
        }

        let state_var = self.next_index;
        self.next_index += 1;
        self.match_state_vars.push(state_var);

        // Update temp_start to PROTECT state_var from being overwritten by temps
        self.temp_start = self.next_index as u16;

        // Define state var and initialize it to 1 (active)
        self.instructions.push(OpCode::DefVar(state_var, true));
        let const_state_idx = self.add_constant(Value::Numeric(Number::I64(1)));
        let temp_reg = self.alloc_reg();
        self.instructions
            .push(OpCode::LoadConst(temp_reg, const_state_idx));
        self.instructions
            .push(OpCode::AssignVar(state_var as u16, temp_reg));
        self.reg_top = self.temp_start;

        self.next_body_patches.push(Vec::new());

        for arm in arms {
            // Check State > 0
            let state_reg = self.alloc_reg();
            self.instructions.push(OpCode::GetVar(state_reg, state_var));

            let zero_idx = self.add_constant(Value::Numeric(Number::I64(0)));
            let zero_reg = self.alloc_reg();
            self.instructions
                .push(OpCode::LoadConst(zero_reg, zero_idx));

            let cond_reg = self.alloc_reg();
            self.instructions
                .push(OpCode::GreaterThan(cond_reg, state_reg, zero_reg));
            let skip_arm_if_inactive = self.emit(OpCode::JumpIfFalse(cond_reg, 0));
            self.reg_top = self.temp_start;

            let mut body_entry_patches = Vec::new();
            let mut arm_failure_patches = Vec::new();

            if arm.patterns.is_empty() {
                // Default case: enters body unconditionally if arm is active
                let jump_to_body = self.emit(OpCode::Jump(0));
                body_entry_patches.push(jump_to_body);
            } else {
                for pattern in &arm.patterns {
                    let patches = self.compile_pattern(pattern, match_reg);
                    body_entry_patches.extend(patches);
                }

                // If we reach here, no pattern in this arm matched.
                // If state was 3 (Next), set to 0.
                let state_reg = self.alloc_reg();
                self.instructions.push(OpCode::GetVar(state_reg, state_var));
                let three_idx = self.add_constant(Value::Numeric(Number::I64(3)));
                let three_reg = self.alloc_reg();
                self.instructions
                    .push(OpCode::LoadConst(three_reg, three_idx));

                let is_next_reg = self.alloc_reg();
                self.instructions
                    .push(OpCode::Equal(is_next_reg, state_reg, three_reg));
                let not_next = self.emit(OpCode::JumpIfFalse(is_next_reg, 0));

                let zero_idx = self.add_constant(Value::Numeric(Number::I64(0)));
                let zero_reg = self.alloc_reg();
                self.instructions
                    .push(OpCode::LoadConst(zero_reg, zero_idx));
                self.instructions
                    .push(OpCode::AssignVar(state_var as u16, zero_reg));
                self.reg_top = self.temp_start;
                self.patch_jump(not_next);

                // Jump OVER the body to the end of this arm
                arm_failure_patches.push(self.emit(OpCode::Jump(0)));
            }

            // Body Path
            for patch in body_entry_patches {
                self.patch_jump(patch);
            }

            // Set state to 0 after successful match (exit match)
            let zero_reg = self.alloc_reg();
            let zero_val_idx_2 = self.add_constant(Value::Numeric(Number::I64(0)));
            self.instructions
                .push(OpCode::LoadConst(zero_reg, zero_val_idx_2));
            self.instructions
                .push(OpCode::AssignVar(state_var as u16, zero_reg));
            self.reg_top = self.temp_start;

            self.compile_statement(*arm.body.clone());

            // Patch any 'next' jumps from within THIS arm's body
            let current_arm_next_patches = self
                .next_body_patches
                .last_mut()
                .unwrap()
                .drain(..)
                .collect::<Vec<_>>();
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
        self.match_state_vars.pop();
        // Condition was in match_reg, which will be freed when compile_match finishes
        self.reg_top = match_reg;
    }

    fn compile_next(&mut self) {
        if let Some(&state_var) = self.match_state_vars.last() {
            let three_idx = self.add_constant(Value::Numeric(Number::I64(3)));
            let reg = self.alloc_reg();
            self.instructions.push(OpCode::LoadConst(reg, three_idx));
            self.instructions
                .push(OpCode::AssignVar(state_var as u16, reg));
            self.reg_top = self.temp_start;

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
            let reg = self.alloc_reg();
            self.instructions.push(OpCode::LoadConst(reg, zero_idx));
            self.instructions
                .push(OpCode::AssignVar(state_var as u16, reg));
            self.reg_top = self.temp_start;
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
            OpCode::JumpIfFalse(_, val)
            | OpCode::JumpIfTrue(_, val)
            | OpCode::IterNext(_, _, val)
            | OpCode::Jump(val) => {
                *val = target;
            }
            _ => panic!(
                "Attempted to patch non-jump opcode at {}: {:?}",
                op_index, self.instructions[op_index]
            ),
        }
    }

    fn compile_pattern(&mut self, pattern: &Pattern, match_reg: u16) -> Vec<usize> {
        let mut patch_indices = Vec::new();
        match pattern {
            Pattern::Literal(expr) => {
                let lit_reg = self.compile_expr(expr);
                let cond_reg = self.alloc_reg();
                self.instructions
                    .push(OpCode::Equal(cond_reg, match_reg, lit_reg));
                patch_indices.push(self.emit(OpCode::JumpIfTrue(cond_reg, 0)));
                self.reg_top = self.temp_start;
            }
            Pattern::EnumVariant {
                enum_name,
                variant_name,
                binding,
            } => {
                let e_name = enum_name.clone().unwrap_or_else(|| "".to_string());
                let e_const = self.add_constant(Value::String(e_name));
                let v_const = self.add_constant(Value::String(variant_name.clone()));
                let cond_reg = self.alloc_reg();
                self.instructions
                    .push(OpCode::IsVariant(cond_reg, match_reg, e_const, v_const));

                let is_match_jump = self.emit(OpCode::JumpIfTrue(cond_reg, 0));
                let skip_arm = self.emit(OpCode::Jump(0));

                self.patch_jump(is_match_jump);
                if let Some(bind_name) = binding {
                    let bind_idx = self.next_index;
                    self.next_index += 1;
                    self.symbol_table.insert(bind_name.clone(), bind_idx);
                    self.instructions.push(OpCode::DefVar(bind_idx, false));

                    let data_reg = self.alloc_reg();
                    self.instructions
                        .push(OpCode::GetVariantData(data_reg, match_reg));
                    self.instructions
                        .push(OpCode::Move(bind_idx as u16, data_reg));

                    self.temp_start = self.next_index as u16;
                }
                self.reg_top = self.temp_start;

                patch_indices.push(self.emit(OpCode::Jump(0))); // Success path
                self.patch_jump(skip_arm);
            }
            Pattern::Wildcard => {
                patch_indices.push(self.emit(OpCode::Jump(0)));
            }
        }
        patch_indices
    }

    fn compile_function_helper(
        &mut self,
        name: String,
        params: Vec<crate::parser::Parameter>,
        body: Vec<Statement>,
    ) -> Function {
        let mut sub_compiler = Compiler::new(Some(self));
        sub_compiler.is_global = false;

        // Inherit all current symbols as GLOBALS for the function
        for (n, &idx) in &self.symbol_table {
            sub_compiler.global_table.insert(n.clone(), idx);
        }
        for (n, &idx) in &self.global_table {
            sub_compiler.global_table.insert(n.clone(), idx);
        }
        sub_compiler.enums = self.enums.clone();
        sub_compiler.static_values = self.static_values.clone();
        sub_compiler.signatures = self.signatures.clone();

        sub_compiler.next_index = 0;

        let mut param_ownership = Vec::new();
        for param in &params {
            let p_idx = sub_compiler.next_index;
            sub_compiler.symbol_table.insert(param.name.clone(), p_idx);
            sub_compiler.next_index += 1;
            param_ownership.push(param.is_owned);
        }

        sub_compiler.temp_start = sub_compiler.next_index as u16;
        sub_compiler.reg_top = sub_compiler.temp_start;
        sub_compiler.max_reg = sub_compiler.reg_top;

        for stmt in body {
            sub_compiler.compile_statement(stmt);
        }
        sub_compiler.instructions.push(OpCode::ReturnVoid);

        self.signatures
            .insert(name.clone(), param_ownership.clone());

        Function {
            name,
            instructions: Arc::new(sub_compiler.instructions),
            constants: Arc::new(sub_compiler.constants),
            home_globals: None, // Set by VM at load time
            param_count: param_ownership.len(),
            param_ownership,
            reg_count: sub_compiler.max_reg as usize,
            upvalues: sub_compiler.upvalues,
        }
    }
}
