use crate::bytecode::OpCode;
use crate::lexer::{is_emoji_identifier, Span, Token};
use crate::parser::{EnumDef, Expr, ExprKind, Pattern, Statement, StatementKind};
use crate::value::{Function, Number, Value};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Upvalue {
    pub index: u16,
    pub is_local: bool,
}

pub struct Compiler {
    pub enclosing: Option<Rc<EnclosingInfo>>,
    pub source: Arc<String>,
    pub filename: Arc<String>,
    pub module_id: usize, // Which module this compiler is for
    pub symbol_table: HashMap<String, usize>,
    pub global_table: HashMap<String, usize>,
    pub static_values: HashMap<String, Value>, // For inlining small static constants
    pub next_index: usize,
    pub reg_top: u16,    // Current top of register "stack"
    pub max_reg: u16,    // Peak register usage
    pub temp_start: u16, // Where temporaries start (after locals)
    pub instructions: InstructionList,
    pub constants: Vec<Value>,
    pub break_patches: Vec<Vec<usize>>,
    pub is_loop_break: Vec<bool>,
    pub match_state_vars: Vec<usize>,
    pub next_body_patches: Vec<Vec<usize>>,
    pub is_global: bool,
    pub enums: HashMap<String, EnumDef>,
    pub signatures: HashMap<String, Vec<bool>>,
    pub moved_locals: HashSet<String>,
    pub state_vars: HashSet<String>,
    pub upvalues: Rc<std::cell::RefCell<Vec<Upvalue>>>,
    pub current_function_name: Option<String>, // For detecting self-recursion
}

#[derive(Debug, Clone)]
pub struct InstructionList {
    ops: Vec<OpCode>,
    spans: Vec<Span>,
    current_span: Span,
}

impl InstructionList {
    pub fn new() -> Self {
        Self {
            ops: Vec::new(),
            spans: Vec::new(),
            current_span: Span::default(),
        }
    }

    pub fn set_span(&mut self, span: Span) {
        self.current_span = span;
    }

    pub fn current_span(&self) -> Span {
        self.current_span
    }

    pub fn push(&mut self, opcode: OpCode) {
        self.ops.push(opcode);
        self.spans.push(self.current_span);
    }

    pub fn len(&self) -> usize {
        self.ops.len()
    }

    pub fn is_empty(&self) -> bool {
        self.ops.is_empty()
    }

    pub fn into_parts(self) -> (Vec<OpCode>, Vec<Span>) {
        (self.ops, self.spans)
    }

    pub fn clone_parts(&self) -> (Vec<OpCode>, Vec<Span>) {
        (self.ops.clone(), self.spans.clone())
    }
}

impl Default for InstructionList {
    fn default() -> Self {
        Self::new()
    }
}

impl std::ops::Index<usize> for InstructionList {
    type Output = OpCode;

    fn index(&self, index: usize) -> &Self::Output {
        &self.ops[index]
    }
}

impl std::ops::IndexMut<usize> for InstructionList {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.ops[index]
    }
}

#[derive(Clone)]
pub struct EnclosingInfo {
    pub symbols: HashMap<String, usize>,
    pub state_vars: HashSet<String>,
    pub upvalues: Rc<std::cell::RefCell<Vec<Upvalue>>>,
    pub enclosing: Option<Rc<EnclosingInfo>>,
}

impl Compiler {
    pub fn new(
        enclosing: Option<Rc<EnclosingInfo>>,
        source: Arc<String>,
        filename: Arc<String>,
    ) -> Self {
        Self {
            enclosing,
            source,
            filename,
            module_id: 0, // Will be set by caller for non-main modules
            symbol_table: HashMap::new(),
            global_table: HashMap::new(),
            static_values: HashMap::new(),
            next_index: 0,
            reg_top: 0,
            max_reg: 0,
            temp_start: 0,
            instructions: InstructionList::new(),
            constants: Vec::new(),
            break_patches: Vec::new(),
            is_loop_break: Vec::new(),
            match_state_vars: Vec::new(),
            next_body_patches: Vec::new(),
            is_global: true,
            enums: HashMap::new(),
            signatures: HashMap::new(),
            moved_locals: HashSet::new(),
            state_vars: HashSet::new(),
            upvalues: Rc::new(std::cell::RefCell::new(Vec::new())),
            current_function_name: None,
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

    fn is_state_var(&self, name: &str) -> bool {
        if self.state_vars.contains(name) {
            return true;
        }

        let mut current = self.enclosing.clone();
        while let Some(parent) = current {
            if parent.state_vars.contains(name) {
                return true;
            }
            current = parent.enclosing.clone();
        }

        false
    }

    // Registers are allocated using a simple bump allocator
    // and freed by resetting reg_top to a previous state (temp_start or saved value)

    pub fn resolve_upvalue(&mut self, name: &str) -> Option<u16> {
        let enclosing = self.enclosing.clone()?;
        self.resolve_upvalue_from(&enclosing, name)
    }

    fn add_upvalue(&mut self, index: u16, is_local: bool) -> u16 {
        Self::add_upvalue_to_list(&self.upvalues, index, is_local)
    }

    fn add_upvalue_to_list(
        list: &Rc<std::cell::RefCell<Vec<Upvalue>>>,
        index: u16,
        is_local: bool,
    ) -> u16 {
        let mut upvalues = list.borrow_mut();
        for (i, up) in upvalues.iter().enumerate() {
            if up.index == index && up.is_local == is_local {
                return i as u16;
            }
        }
        upvalues.push(Upvalue { index, is_local });
        (upvalues.len() - 1) as u16
    }

    fn resolve_upvalue_from(&mut self, enclosing: &Rc<EnclosingInfo>, name: &str) -> Option<u16> {
        if let Some(&idx) = enclosing.symbols.get(name) {
            return Some(self.add_upvalue(idx as u16, true));
        }

        let parent = enclosing.enclosing.as_ref()?;
        let up_idx = self.resolve_upvalue_from(parent, name)?;
        let parent_up_idx = Self::add_upvalue_to_list(&enclosing.upvalues, up_idx, false);
        Some(self.add_upvalue(parent_up_idx, false))
    }

    pub fn inject_builtins(&mut self) {
        // Built-in Result
        // Result<T, E> { Ok(T), Error(E) }
        let result_enum = EnumDef {
            name: "Result".to_string(),
            type_params: vec!["T".to_string(), "E".to_string()],
            variants: vec![
                crate::parser::EnumVariantDef {
                    name: "Ok".to_string(),
                    data_type: Some(crate::parser::Type::TypeParam("T".to_string())),
                },
                crate::parser::EnumVariantDef {
                    name: "Error".to_string(),
                    data_type: Some(crate::parser::Type::TypeParam("E".to_string())),
                },
            ],
        };

        // Built-in Maybe
        // Maybe<T> { Value(T), Null }
        let maybe_enum = EnumDef {
            name: "Maybe".to_string(),
            type_params: vec!["T".to_string()],
            variants: vec![
                crate::parser::EnumVariantDef {
                    name: "Value".to_string(),
                    data_type: Some(crate::parser::Type::TypeParam("T".to_string())),
                },
                crate::parser::EnumVariantDef {
                    name: "Null".to_string(),
                    data_type: None,
                },
            ],
        };

        self.enums.insert("Result".to_string(), result_enum);
        self.enums.insert("Maybe".to_string(), maybe_enum);

        // Standard globals (sourced from toolbox::core for organizational clarity)
        // These are handled as special opcodes, not CallGlobal
        use crate::toolbox::core::GLOBAL_NAMES;
        for (i, name) in GLOBAL_NAMES.iter().enumerate() {
            self.global_table.insert(name.to_string(), i);
        }
        self.next_index = GLOBAL_NAMES.len();
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

    /// Compile a field assignment, handling nested paths by recursively writing back
    /// For simple: `obj.field = val` -> SetField(obj_reg, field, val_reg)
    /// For nested: `obj.a.b = val` -> Get obj.a, SetField(a_reg, b, val_reg), SetField(obj_reg, a, a_reg)
    fn compile_field_assign(&mut self, object: &Expr, field: &str, val_reg: u16) -> u16 {
        let field_idx = self.add_constant(Value::String(field.to_string()));

        match &object.kind {
            ExprKind::Variable(name) => {
                // Base case: direct variable access
                let obj_reg = self.compile_expr(object);
                self.instructions
                    .push(OpCode::SetField(obj_reg, field_idx, val_reg));

                // Write the modified object back to the variable
                if let Some(&idx) = self.symbol_table.get(name) {
                    self.instructions
                        .push(OpCode::AssignVar(idx as u16, obj_reg));
                } else if let Some(&idx) = self.global_table.get(name) {
                    self.instructions.push(OpCode::AssignGlobal(idx, obj_reg));
                }
                val_reg
            }
            ExprKind::FieldAccess {
                object: parent,
                field: parent_field,
            } => {
                // Nested case: get the intermediate object, modify it, then write back
                let intermediate_reg = self.compile_expr(object);
                self.instructions
                    .push(OpCode::SetField(intermediate_reg, field_idx, val_reg));

                // Now recursively write back this intermediate object to its parent
                self.compile_field_assign(parent, parent_field, intermediate_reg)
            }
            _ => {
                // Fallback for other cases (shouldn't happen in valid code)
                let obj_reg = self.compile_expr(object);
                self.instructions
                    .push(OpCode::SetField(obj_reg, field_idx, val_reg));
                val_reg
            }
        }
    }

    pub fn compile_expr(&mut self, expr: &Expr) -> u16 {
        let prev_span = self.instructions.current_span();
        self.instructions.set_span(expr.span);
        let result = match &expr.kind {
            ExprKind::Assign { name, value } => {
                if matches!(value.kind, ExprKind::State(_)) {
                    self.state_vars.insert(name.clone());
                } else {
                    self.state_vars.remove(name);
                }
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
            ExprKind::FieldAssign {
                object,
                field,
                value,
            } => {
                // Compile the value first
                let val_reg = self.compile_expr(value);

                // Handle nested field assignment by recursively writing back
                self.compile_field_assign(object, field, val_reg)
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
                    "panic" => {
                        let reg = self.compile_expr(&args[0]);
                        let location = expr
                            .span
                            .format_location(self.source.as_str(), self.filename.as_str());
                        let location_idx = self.add_constant(Value::String(location));
                        self.instructions.push(OpCode::Panic(reg, location_idx));
                        return reg;
                    }
                    "fetch" => {
                        let url_reg = self.compile_expr(&args[0]);
                        let options_reg = if args.len() > 1 {
                            self.compile_expr(&args[1])
                        } else {
                            u16::MAX // Sentinel value for "no options"
                        };
                        let dest = self.alloc_reg();
                        self.instructions
                            .push(OpCode::Fetch(dest, url_reg, options_reg));
                        self.reg_top = dest + 1;
                        return dest;
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
                } else if self.current_function_name.as_ref() == Some(name) {
                    // Self-recursion: use optimized CallSelf opcode
                    self.instructions
                        .push(OpCode::CallSelf(dest, start_reg, args.len() as u8));
                    self.reg_top = dest + 1;
                } else if let Some(&idx) = self.global_table.get(name) {
                    // Global function: use CallGlobal which fetches from env_stack[0]
                    self.instructions.push(OpCode::CallGlobal(
                        dest,
                        idx,
                        start_reg,
                        args.len() as u8,
                    ));
                    self.reg_top = dest + 1;
                } else if let Some(up_idx) = self.resolve_upvalue(name) {
                    // Captured function from enclosing scope: get upvalue then call
                    let func_reg = self.alloc_reg();
                    self.instructions.push(OpCode::GetUpvalue(func_reg, up_idx));
                    self.instructions.push(OpCode::Call(
                        dest,
                        func_reg,
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
                } else if is_emoji_identifier(name) {
                    let dest = self.alloc_reg();
                    let idx = self.add_constant(Value::String(name.clone()));
                    self.instructions.push(OpCode::LoadConst(dest, idx));
                    dest
                } else {
                    panic!("Compiler Error: Undefined variable '{}'", name);
                }
            }
            ExprKind::UnwrapGuard(name) => {
                // `name?` as an expression evaluates to bool (is Value/Ok).
                // In `if (name?)` conditions, the parser desugars it into an if-binding.
                let src_reg = self.compile_expr(&Expr {
                    kind: ExprKind::Variable(name.clone()),
                    span: expr.span,
                });

                let dest = self.alloc_reg();
                self.instructions
                    .push(OpCode::CheckMaybeValue(dest, src_reg));
                dest
            }
            ExprKind::Closure {
                params,
                body,
                return_type,
            } => {
                let enclosing = Rc::new(EnclosingInfo {
                    symbols: self.symbol_table.clone(),
                    state_vars: self.state_vars.clone(),
                    upvalues: self.upvalues.clone(),
                    enclosing: self.enclosing.clone(),
                });
                let mut sub_compiler =
                    Compiler::new(Some(enclosing), self.source.clone(), self.filename.clone());
                let (func, upvalues) = {
                    let global_table = self.global_table.clone();
                    let static_values = self.static_values.clone();
                    let enums = self.enums.clone();
                    let signatures = self.signatures.clone();
                    sub_compiler.is_global = false;
                    sub_compiler.global_table = global_table;
                    sub_compiler.static_values = static_values;
                    sub_compiler.enums = enums;
                    sub_compiler.signatures = signatures;
                    sub_compiler.next_index = 0;

                    let mut param_ownership = Vec::new();
                    let mut param_types = Vec::new();
                    let mut default_values = Vec::new();
                    let mut param_is_maybe_wrapped = Vec::new();
                    for param in params {
                        let p_idx = sub_compiler.next_index;
                        sub_compiler.symbol_table.insert(param.name.clone(), p_idx);
                        sub_compiler.next_index += 1;
                        param_ownership.push(param.is_owned);
                        param_types.push(param.ty.clone().unwrap_or(crate::parser::Type::Any));
                        param_is_maybe_wrapped.push(param.is_optional);

                        // Extract default value if provided
                        let default_val = if let Some(ref expr) = param.default_value {
                            self.extract_literal_for_default(expr) // Returns Some(Value) for literals
                        } else if param.is_optional {
                            // Return Maybe::Null for optional params with no default
                            Some(crate::value::EnumData::into_value(
                                "Maybe".to_string(),
                                "Null".to_string(),
                                None,
                            ))
                        } else {
                            None // Required param
                        };
                        default_values.push(default_val);
                    }

                    sub_compiler.temp_start = sub_compiler.next_index as u16;
                    sub_compiler.reg_top = sub_compiler.temp_start;
                    sub_compiler.max_reg = sub_compiler.reg_top;

                    sub_compiler.compile_statement(*body.clone());
                    sub_compiler.instructions.push(OpCode::ReturnVoid);

                    let captured_upvalues = sub_compiler.upvalues.borrow().clone();
                    let (instructions, instruction_spans) =
                        std::mem::take(&mut sub_compiler.instructions).into_parts();
                    let constants = std::mem::take(&mut sub_compiler.constants);

                    let func = Function {
                        name: "closure".to_string(),
                        instructions: Arc::new(instructions),
                        instruction_spans: Arc::new(instruction_spans),
                        source: sub_compiler.source.clone(),
                        filename: sub_compiler.filename.clone(),
                        constants: Arc::new(constants),
                        home_globals: None, // Set by VM at load time
                        param_count: params.len(),
                        param_ownership,
                        param_types,
                        default_values,
                        param_is_maybe_wrapped,
                        return_type: return_type.clone().unwrap_or(crate::parser::Type::Void),
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
                // Short-circuit logical operators must not eagerly compile RHS.
                if matches!(op, Token::AndAnd | Token::OrOr) {
                    let left_reg = self.compile_expr(left);
                    let dest = self.alloc_reg();
                    self.instructions.push(OpCode::Move(dest, left_reg));

                    let jump_idx = match op {
                        Token::AndAnd => self.emit(OpCode::JumpIfFalse(dest, 0)),
                        Token::OrOr => self.emit(OpCode::JumpIfTrue(dest, 0)),
                        _ => unreachable!(),
                    };

                    let right_reg = self.compile_expr(right);
                    self.instructions.push(OpCode::Move(dest, right_reg));
                    self.patch_jump(jump_idx);
                    self.reg_top = dest + 1;
                    return dest;
                }

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
                    Token::LessThanEqual => self
                        .instructions
                        .push(OpCode::LessThanEqual(dest, left_reg, right_reg)),
                    Token::GreaterThanEqual => self
                        .instructions
                        .push(OpCode::GreaterThanEqual(dest, left_reg, right_reg)),
                    Token::NotEqual => self
                        .instructions
                        .push(OpCode::NotEqual(dest, left_reg, right_reg)),
                    Token::QuestionQuestion => {
                        // Null-coalescing: if left is Maybe::Value, use unwrapped value, else use right
                        // left_reg contains the Maybe value
                        // right_reg should NOT be computed yet for short-circuit, but our current
                        // approach already computed it. For true short-circuit, we need to restructure.
                        // For now, use a simpler approach: compute both, then select at runtime.
                        self.instructions
                            .push(OpCode::NullCoalesce(dest, left_reg, right_reg));
                    }
                    _ => todo!("Other operators: {:?}", op),
                }
                // Free temporaries but keep result
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::Array(elements) => {
                if elements.len() == 1 && matches!(elements[0].kind, ExprKind::Range { .. }) {
                    let range_reg = self.compile_expr(&elements[0]);
                    let dest = self.alloc_reg();
                    self.instructions
                        .push(OpCode::RangeToArray(dest, range_reg));
                    self.reg_top = dest + 1;
                    dest
                } else {
                    let start_reg = self.compile_args(elements);
                    let dest = self.alloc_reg();
                    self.instructions
                        .push(OpCode::BuildArray(dest, start_reg, elements.len()));
                    self.reg_top = dest + 1;
                    dest
                }
            }
            ExprKind::Tuple(elements) => {
                let start_reg = self.compile_args(elements);
                let dest = self.alloc_reg();
                self.instructions
                    .push(OpCode::BuildTuple(dest, start_reg, elements.len()));
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::Range {
                start,
                end,
                inclusive,
            } => {
                let start_reg = self.compile_expr(start);
                let end_reg = self.compile_expr(end);
                let dest = self.alloc_reg();
                self.instructions
                    .push(OpCode::CreateRange(dest, start_reg, end_reg, *inclusive));
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
            ExprKind::Bytes(arg) => {
                let dest = self.alloc_reg();
                let arg_reg = if let Some(expr) = arg {
                    self.compile_expr(expr)
                } else {
                    u16::MAX
                };
                self.instructions.push(OpCode::CreateBytes(dest, arg_reg));
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
            ExprKind::Object(fields) => {
                // Anonymous object literal - use "__anon__" as type name
                let model_name_idx = self.add_constant(Value::String("__anon__".to_string()));

                let mut field_names = Vec::new();
                for (f_name, _) in fields {
                    field_names.push(Value::String(f_name.clone()));
                }
                let fields_idx = self.add_constant(Value::Tuple(field_names));

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
                // Special case: env.PROPERTY_NAME -> env.get("PROPERTY_NAME")
                if let ExprKind::Variable(var_name) = &object.kind {
                    if var_name == "env" {
                        // Treat env.FIELD as env.get("FIELD")
                        let dest = self.alloc_reg();
                        let key_idx = self.add_constant(Value::String(field.clone()));
                        let key_reg = self.alloc_reg();
                        self.instructions.push(OpCode::LoadConst(key_reg, key_idx));
                        self.instructions.push(OpCode::EnvGet(dest, key_reg));
                        self.reg_top = dest + 1;
                        return dest;
                    }
                }

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
                // Special case for json.parse() and json.stringify()
                if let ExprKind::Variable(var_name) = &obj_expr.kind {
                    if var_name == "json" {
                        let dest = self.alloc_reg();
                        match method.as_str() {
                            "parse" => {
                                let arg_reg = self.compile_expr(&args[0]);
                                self.instructions.push(OpCode::JsonParse(dest, arg_reg));
                                self.reg_top = dest + 1;
                                return dest;
                            }
                            "stringify" => {
                                let arg_reg = self.compile_expr(&args[0]);
                                self.instructions.push(OpCode::JsonStringify(dest, arg_reg));
                                self.reg_top = dest + 1;
                                return dest;
                            }
                            _ => {}
                        }
                    }

                    // Handle spawn.all(), spawn.try(), spawn.race()
                    if var_name == "spawn" {
                        let dest = self.alloc_reg();
                        match method.as_str() {
                            "cpu" => {
                                let task_reg = self.compile_expr(&args[0]);
                                self.instructions.push(OpCode::SpawnCpu(dest, task_reg));
                                self.reg_top = dest + 1;
                                return dest;
                            }
                            "block" => {
                                let task_reg = self.compile_expr(&args[0]);
                                self.instructions.push(OpCode::SpawnBlock(dest, task_reg));
                                self.reg_top = dest + 1;
                                return dest;
                            }
                            "all" => {
                                let tasks_reg = self.compile_expr(&args[0]);
                                let options_reg = if args.len() > 1 {
                                    self.compile_expr(&args[1])
                                } else {
                                    u16::MAX // No options
                                };
                                self.instructions.push(OpCode::SpawnAll(
                                    dest,
                                    tasks_reg,
                                    options_reg,
                                ));
                                self.reg_top = dest + 1;
                                return dest;
                            }
                            "try" => {
                                let tasks_reg = self.compile_expr(&args[0]);
                                let options_reg = if args.len() > 1 {
                                    self.compile_expr(&args[1])
                                } else {
                                    u16::MAX // No options
                                };
                                self.instructions.push(OpCode::SpawnTry(
                                    dest,
                                    tasks_reg,
                                    options_reg,
                                ));
                                self.reg_top = dest + 1;
                                return dest;
                            }
                            "race" => {
                                let tasks_reg = self.compile_expr(&args[0]);
                                self.instructions.push(OpCode::SpawnRace(dest, tasks_reg));
                                self.reg_top = dest + 1;
                                return dest;
                            }
                            _ => {}
                        }
                    }

                    // Handle env.get(), env.has()
                    if var_name == "env" {
                        let dest = self.alloc_reg();
                        match method.as_str() {
                            "get" => {
                                let arg_reg = self.compile_expr(&args[0]);
                                self.instructions.push(OpCode::EnvGet(dest, arg_reg));
                                self.reg_top = dest + 1;
                                return dest;
                            }
                            "has" => {
                                let arg_reg = self.compile_expr(&args[0]);
                                self.instructions.push(OpCode::EnvHas(dest, arg_reg));
                                self.reg_top = dest + 1;
                                return dest;
                            }
                            _ => {}
                        }
                    }

                    // Handle time.now(), time.utc(), time.unix(), time.unixMs(), time.parse()
                    if var_name == "time" {
                        let dest = self.alloc_reg();
                        match method.as_str() {
                            "now" => {
                                self.instructions.push(OpCode::TimeNow(dest));
                                self.reg_top = dest + 1;
                                return dest;
                            }
                            "utc" => {
                                self.instructions.push(OpCode::TimeUtc(dest));
                                self.reg_top = dest + 1;
                                return dest;
                            }
                            "unix" => {
                                self.instructions.push(OpCode::TimeUnix(dest));
                                self.reg_top = dest + 1;
                                return dest;
                            }
                            "unixMs" => {
                                self.instructions.push(OpCode::TimeUnixMs(dest));
                                self.reg_top = dest + 1;
                                return dest;
                            }
                            "parse" => {
                                let str_reg = self.compile_expr(&args[0]);
                                let fmt_reg = self.compile_expr(&args[1]);
                                self.instructions
                                    .push(OpCode::TimeParse(dest, str_reg, fmt_reg));
                                self.reg_top = dest + 1;
                                return dest;
                            }
                            _ => {}
                        }
                    }
                }

                let obj_reg = self.compile_expr(obj_expr);
                let dest = self.alloc_reg();

                let is_state_target = matches!(obj_expr.kind, ExprKind::State(_))
                    || matches!(&obj_expr.kind, ExprKind::Variable(name) if self.is_state_var(name));

                if is_state_target {
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
                        _ => {}
                    }
                }

                // For all other methods, use the generic CallMethod opcode
                let arg_start = self.compile_args(args);
                let method_idx = self.add_constant(Value::String(method.clone()));

                // Mutating methods need CallMethodMut to write back to the source register
                let is_mutating = matches!(
                    method.as_str(),
                    "push" | "pop" | "slice" | "splice" |  // array mutating methods
                    "set" | "remove" | "clear" |  // map/set mutating methods
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
            ExprKind::Null => {
                // null is syntactic sugar for Maybe::Null
                let dest = self.alloc_reg();
                let names_idx = self.add_constant(Value::Tuple(vec![
                    Value::String("Maybe".to_string()),
                    Value::String("Null".to_string()),
                ]));
                self.instructions
                    .push(OpCode::CreateEnum(dest, names_idx, 0, 0));
                self.reg_top = dest + 1;
                dest
            }
            ExprKind::OptionalFieldAccess { object, field } => {
                // obj?.field - short-circuit if null, otherwise unwrap and access field
                let obj_reg = self.compile_expr(object);
                let dest = self.alloc_reg();

                // Check if obj is Maybe::Value
                let check_reg = self.alloc_reg();
                self.instructions
                    .push(OpCode::CheckMaybeValue(check_reg, obj_reg));

                // Jump to null result if not a value
                let jump_to_null = self.instructions.len();
                self.instructions.push(OpCode::JumpIfFalse(check_reg, 0)); // patch later

                // Value path: unwrap, access field, wrap in Maybe::Value
                let unwrapped_reg = self.alloc_reg();
                self.instructions
                    .push(OpCode::UnwrapMaybe(unwrapped_reg, obj_reg));

                let field_idx = self.add_constant(Value::String(field.clone()));
                let field_result_reg = self.alloc_reg();
                self.instructions.push(OpCode::GetField(
                    field_result_reg,
                    unwrapped_reg,
                    field_idx,
                ));

                // Wrap result in Maybe::Value
                let value_names_idx = self.add_constant(Value::Tuple(vec![
                    Value::String("Maybe".to_string()),
                    Value::String("Value".to_string()),
                ]));
                self.instructions.push(OpCode::CreateEnum(
                    dest,
                    value_names_idx,
                    field_result_reg,
                    1,
                ));

                // Jump over null path
                let jump_to_end = self.instructions.len();
                self.instructions.push(OpCode::Jump(0)); // patch later

                // Null path: create Maybe::Null
                let null_label = self.instructions.len();
                let null_names_idx = self.add_constant(Value::Tuple(vec![
                    Value::String("Maybe".to_string()),
                    Value::String("Null".to_string()),
                ]));
                self.instructions
                    .push(OpCode::CreateEnum(dest, null_names_idx, 0, 0));

                // Patch jumps
                let end_label = self.instructions.len();
                self.instructions[jump_to_null] = OpCode::JumpIfFalse(check_reg, null_label);
                self.instructions[jump_to_end] = OpCode::Jump(end_label);

                self.reg_top = dest + 1;
                dest
            }
            ExprKind::OptionalMethodCall(object, method, args) => {
                // obj?.method(args) - short-circuit if null, otherwise unwrap and call method
                let obj_reg = self.compile_expr(object);
                let dest = self.alloc_reg();

                // Check if obj is Maybe::Value
                let check_reg = self.alloc_reg();
                self.instructions
                    .push(OpCode::CheckMaybeValue(check_reg, obj_reg));

                // Jump to null result if not a value
                let jump_to_null = self.instructions.len();
                self.instructions.push(OpCode::JumpIfFalse(check_reg, 0)); // patch later

                // Value path: unwrap
                let unwrapped_reg = self.alloc_reg();
                self.instructions
                    .push(OpCode::UnwrapMaybe(unwrapped_reg, obj_reg));

                // Compile arguments
                let args_start = self.reg_top;
                for (i, arg) in args.iter().enumerate() {
                    let expected_reg = args_start + i as u16;
                    let result_reg = self.compile_expr(arg);
                    if result_reg != expected_reg {
                        self.instructions
                            .push(OpCode::Move(expected_reg, result_reg));
                    }
                    if self.reg_top <= expected_reg {
                        self.reg_top = expected_reg + 1;
                    }
                }

                // Call method on unwrapped value
                let method_idx = self.add_constant(Value::String(method.clone()));
                let method_result_reg = self.alloc_reg();
                self.instructions.push(OpCode::CallMethod(
                    method_result_reg,
                    unwrapped_reg,
                    method_idx,
                    args_start,
                    args.len() as u8,
                ));

                // Wrap result in Maybe::Value
                let value_names_idx = self.add_constant(Value::Tuple(vec![
                    Value::String("Maybe".to_string()),
                    Value::String("Value".to_string()),
                ]));
                self.instructions.push(OpCode::CreateEnum(
                    dest,
                    value_names_idx,
                    method_result_reg,
                    1,
                ));

                // Jump over null path
                let jump_to_end = self.instructions.len();
                self.instructions.push(OpCode::Jump(0)); // patch later

                // Null path: create Maybe::Null
                let null_label = self.instructions.len();
                let null_names_idx = self.add_constant(Value::Tuple(vec![
                    Value::String("Maybe".to_string()),
                    Value::String("Null".to_string()),
                ]));
                self.instructions
                    .push(OpCode::CreateEnum(dest, null_names_idx, 0, 0));

                // Patch jumps
                let end_label = self.instructions.len();
                self.instructions[jump_to_null] = OpCode::JumpIfFalse(check_reg, null_label);
                self.instructions[jump_to_end] = OpCode::Jump(end_label);

                self.reg_top = dest + 1;
                dest
            }
            ExprKind::Error(msg) => {
                panic!("Compiler received invalid AST: {}", msg);
            }
        };
        self.instructions.set_span(prev_span);
        result
    }

    /// Extract a constant literal with size limits (for static declaration inlining)
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

    /// Extract a literal for default param values (no size limits)
    fn extract_literal_for_default(&self, expr: &Expr) -> Option<Value> {
        match &expr.kind {
            ExprKind::Number(n) => Some(Value::Numeric(Number::I64(*n))),
            ExprKind::Float(n) => Some(Value::Numeric(Number::F64(*n))),
            ExprKind::Bool(b) => Some(Value::Bool(*b)),
            ExprKind::String(s) => Some(Value::String(s.clone())),
            ExprKind::Array(elements) => {
                let mut values = Vec::new();
                for el in elements {
                    if let Some(v) = self.extract_literal_for_default(el) {
                        values.push(v);
                    } else {
                        return None;
                    }
                }
                Some(Value::Array(values))
            }
            ExprKind::Tuple(elements) => {
                let mut values = Vec::new();
                for el in elements {
                    if let Some(v) = self.extract_literal_for_default(el) {
                        values.push(v);
                    } else {
                        return None;
                    }
                }
                Some(Value::Tuple(values))
            }
            ExprKind::InterpolatedString(parts) => {
                let mut res = String::new();
                for part in parts {
                    if let Some(val) = self.extract_literal_for_default(part) {
                        res.push_str(&val.to_string());
                    } else {
                        return None;
                    }
                }
                Some(Value::String(res))
            }
            _ => None,
        }
    }

    pub fn compile_statement(&mut self, stmt: Statement) {
        let prev_span = self.instructions.current_span();
        self.instructions.set_span(stmt.span);
        match stmt.kind {
            StatementKind::TypeAlias { .. } => {}
            StatementKind::Enum(enum_def) => {
                self.enums.insert(enum_def.name.clone(), enum_def);
            }
            StatementKind::Declaration {
                name,
                is_mutable,
                is_global,
                value,
                declared_type: _,
            } => {
                if matches!(value.kind, ExprKind::State(_)) {
                    self.state_vars.insert(name.clone());
                } else {
                    self.state_vars.remove(&name);
                }
                let var_idx = self.next_index;

                if is_global {
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

                if is_global || self.is_global {
                    // For global/static, compile expression and move result to global slot
                    let res_reg = self.compile_expr(&value);
                    self.global_table.insert(name, var_idx);
                    self.instructions
                        .push(OpCode::DefGlobal(var_idx, is_mutable));
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
            StatementKind::DestructureObject {
                fields,
                rest,
                is_mutable,
                value,
            } => {
                // Compile the source expression
                let src_reg = self.compile_expr(&value);

                // For each field, emit GetField and store in a new local variable
                for (field_name, alias) in fields.iter() {
                    let var_idx = self.next_index;
                    let binding_name = alias.as_ref().unwrap_or(field_name);
                    self.symbol_table.insert(binding_name.clone(), var_idx);

                    let field_const_idx = self.add_constant(Value::String(field_name.clone()));
                    let dest_reg = self.alloc_reg();
                    self.instructions
                        .push(OpCode::GetField(dest_reg, src_reg, field_const_idx));
                    self.instructions
                        .push(OpCode::Move(var_idx as u16, dest_reg));
                    self.instructions.push(OpCode::DefVar(var_idx, is_mutable));

                    self.next_index += 1;
                }

                // Handle rest binding - collect remaining fields into a new object
                if let Some(rest_name) = rest {
                    let var_idx = self.next_index;
                    self.symbol_table.insert(rest_name.clone(), var_idx);

                    // Create constants for field names to exclude
                    let excluded_fields: Vec<String> =
                        fields.iter().map(|(name, _)| name.clone()).collect();
                    let excluded_const_idx = self.add_constant(Value::Array(
                        excluded_fields.into_iter().map(Value::String).collect(),
                    ));

                    let dest_reg = self.alloc_reg();
                    self.instructions.push(OpCode::ObjectRest(
                        dest_reg,
                        src_reg,
                        excluded_const_idx,
                    ));
                    self.instructions
                        .push(OpCode::Move(var_idx as u16, dest_reg));
                    self.instructions.push(OpCode::DefVar(var_idx, is_mutable));

                    self.next_index += 1;
                }

                self.temp_start = self.next_index as u16;
                self.reg_top = self.temp_start;
            }
            StatementKind::DestructureArray {
                bindings,
                rest,
                is_mutable,
                value,
            } => {
                // Compile the source expression
                let src_reg = self.compile_expr(&value);

                // For each binding, emit GetIndex and store in a new local variable
                for (i, binding_name) in bindings.iter().enumerate() {
                    let var_idx = self.next_index;
                    self.symbol_table.insert(binding_name.clone(), var_idx);

                    // Create index constant
                    let index_const_idx = self.add_constant(Value::Numeric(Number::I64(i as i64)));
                    let index_reg = self.alloc_reg();
                    self.instructions
                        .push(OpCode::LoadConst(index_reg, index_const_idx));

                    let dest_reg = self.alloc_reg();
                    self.instructions
                        .push(OpCode::GetIndex(dest_reg, src_reg, index_reg));
                    self.instructions
                        .push(OpCode::Move(var_idx as u16, dest_reg));
                    self.instructions.push(OpCode::DefVar(var_idx, is_mutable));

                    self.next_index += 1;
                }

                // Handle rest binding - collect remaining elements into a new array
                if let Some(rest_name) = rest {
                    let var_idx = self.next_index;
                    self.symbol_table.insert(rest_name.clone(), var_idx);

                    // Use copy method to get remaining elements
                    let skip_count = bindings.len();
                    let skip_const_idx =
                        self.add_constant(Value::Numeric(Number::I64(skip_count as i64)));
                    let skip_reg = self.alloc_reg();
                    self.instructions
                        .push(OpCode::LoadConst(skip_reg, skip_const_idx));

                    let method_idx = self.add_constant(Value::String("copy".to_string()));
                    let dest_reg = self.alloc_reg();
                    self.instructions.push(OpCode::CallMethod(
                        dest_reg, src_reg, method_idx, skip_reg, 1,
                    ));
                    self.instructions
                        .push(OpCode::Move(var_idx as u16, dest_reg));
                    self.instructions.push(OpCode::DefVar(var_idx, is_mutable));

                    self.next_index += 1;
                }

                self.temp_start = self.next_index as u16;
                self.reg_top = self.temp_start;
            }
            StatementKind::DestructureTuple {
                bindings,
                rest,
                is_mutable,
                value,
            } => {
                // Compile the source expression
                let src_reg = self.compile_expr(&value);

                // For each binding, emit GetIndex and store in a new local variable
                for (i, binding_name) in bindings.iter().enumerate() {
                    let var_idx = self.next_index;
                    self.symbol_table.insert(binding_name.clone(), var_idx);

                    // Create index constant
                    let index_const_idx = self.add_constant(Value::Numeric(Number::I64(i as i64)));
                    let index_reg = self.alloc_reg();
                    self.instructions
                        .push(OpCode::LoadConst(index_reg, index_const_idx));

                    let dest_reg = self.alloc_reg();
                    self.instructions
                        .push(OpCode::GetIndex(dest_reg, src_reg, index_reg));
                    self.instructions
                        .push(OpCode::Move(var_idx as u16, dest_reg));
                    self.instructions.push(OpCode::DefVar(var_idx, is_mutable));

                    self.next_index += 1;
                }

                // Handle rest binding - collect remaining elements into a new tuple
                if let Some(rest_name) = rest {
                    let var_idx = self.next_index;
                    self.symbol_table.insert(rest_name.clone(), var_idx);

                    // Use copy method to get remaining elements
                    let skip_count = bindings.len();
                    let skip_const_idx =
                        self.add_constant(Value::Numeric(Number::I64(skip_count as i64)));
                    let skip_reg = self.alloc_reg();
                    self.instructions
                        .push(OpCode::LoadConst(skip_reg, skip_const_idx));

                    let method_idx = self.add_constant(Value::String("copy".to_string()));
                    let dest_reg = self.alloc_reg();
                    self.instructions.push(OpCode::CallMethod(
                        dest_reg, src_reg, method_idx, skip_reg, 1,
                    ));
                    self.instructions
                        .push(OpCode::Move(var_idx as u16, dest_reg));
                    self.instructions.push(OpCode::DefVar(var_idx, is_mutable));

                    self.next_index += 1;
                }

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
                        name,
                        params,
                        body,
                        return_type,
                        ..
                    } = method.kind
                    {
                        let mut method_params = vec![crate::parser::Parameter {
                            name: "this".to_string(),
                            ty: Some(target.clone()),
                            is_owned: false,
                            is_optional: false,
                            default_value: None,
                            span: crate::lexer::Span::default(),
                        }];
                        method_params.extend(params);

                        let func = self.compile_function_helper(
                            name.clone(),
                            method_params,
                            body,
                            return_type,
                        );
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
                name,
                params,
                body,
                return_type,
                ..
            } => {
                let func = self.compile_function_helper(name.clone(), params, body, return_type);

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
            StatementKind::Print(expression_opt) => {
                if let Some(expression) = expression_opt {
                    let reg = self.compile_expr(&expression);
                    self.instructions.push(OpCode::Print(reg));
                } else {
                    self.instructions.push(OpCode::PrintNewline);
                }
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
                binding,
                then_branch,
                else_branch,
            } => self.compile_if(condition, binding, then_branch, else_branch),
            StatementKind::Check {
                binding,
                error_binding,
                source,
                failure_block,
            } => self.compile_check(binding, error_binding, source, failure_block),
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
        self.instructions.set_span(prev_span);
    }

    fn compile_if(
        &mut self,
        condition: Expr,
        binding: Option<String>,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    ) {
        // Compile the Condition
        // Output: [Instructions for Condition, Push Result]
        let cond_reg = self.compile_expr(&condition);

        if let Some(bind_name) = binding {
            // Unwrap guard binding for Maybe/Result.
            // User-facing form: `if (value?) { ... }`

            // Emit check for Maybe::Value variant
            let check_reg = self.alloc_reg();

            // CheckMaybeValue returns true if it's Value variant
            self.instructions
                .push(OpCode::CheckMaybeValue(check_reg, cond_reg));

            // Allocate binding as a proper local variable slot.
            // IMPORTANT: allow shadowing an existing variable name inside the if-branch.
            let prev_mapping = self.symbol_table.get(&bind_name).copied();

            let bind_idx = self.next_index;
            self.symbol_table.insert(bind_name.clone(), bind_idx);
            self.next_index += 1;

            let jump_false_idx = self.emit(OpCode::JumpIfFalse(check_reg, 0));

            // Reset temp area after binding allocation
            self.temp_start = self.next_index as u16;
            self.reg_top = self.temp_start;

            // UnwrapMaybe extracts the inner value
            self.instructions
                .push(OpCode::UnwrapMaybe(bind_idx as u16, cond_reg));

            // Compile then branch with binding available
            self.compile_statement(*then_branch);

            // Clean up binding from symbol table and restore previous mapping (if any)
            self.symbol_table.remove(&bind_name);
            if let Some(prev_idx) = prev_mapping {
                self.symbol_table.insert(bind_name.clone(), prev_idx);
            }
            self.next_index -= 1;

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
        } else {
            // Normal if: condition is already a bool
            let jump_false_idx = self.emit(OpCode::JumpIfFalse(cond_reg, 0));
            self.reg_top = self.temp_start;

            self.compile_statement(*then_branch);

            if let Some(else_branch) = else_branch {
                let jump_end_idx = self.emit(OpCode::Jump(0));
                self.patch_jump(jump_false_idx);
                self.compile_statement(*else_branch);
                self.patch_jump(jump_end_idx);
            } else {
                self.patch_jump(jump_false_idx);
            }
        }
    }

    fn compile_check(
        &mut self,
        binding: String,
        error_binding: Option<String>,
        source: Expr,
        failure_block: Box<Statement>,
    ) {
        // `check` guard for Maybe/Result.
        // User-facing preferred forms:
        // - `check value? { failure }`
        // - `check value?, err? { failure }`
        // If value is Null/Error, run failure block (which must return/break)
        // Otherwise, upgrade the binding to the unwrapped value for the rest of the scope

        let src_reg = self.compile_expr(&source);

        // Check if it's Maybe::Value or Result::Ok
        let check_reg = self.alloc_reg();
        self.instructions
            .push(OpCode::CheckMaybeValue(check_reg, src_reg));

        // Allocate binding slot up-front so temporaries don't reuse it.
        // IMPORTANT: do not put `binding` into `symbol_table` until after the failure
        // block is compiled. The failure block should not see the unwrapped binding.
        let bind_idx = self.next_index;
        self.next_index += 1;

        // If not Value (i.e., Null/Error), run failure block
        let jump_if_value_idx = self.emit(OpCode::JumpIfTrue(check_reg, 0));

        // Reset temp area AFTER binding slot allocation
        self.temp_start = self.next_index as u16;
        self.reg_top = self.temp_start;

        // If error binding is provided, extract error value and add to scope
        let prev_err_mapping = if let Some(err_name) = &error_binding {
            self.symbol_table.get(err_name).copied()
        } else {
            None
        };

        let err_bind_idx = if let Some(err_name) = error_binding {
            let err_idx = self.next_index;
            self.symbol_table.insert(err_name.clone(), err_idx);
            self.next_index += 1;
            self.temp_start = self.next_index as u16;
            self.reg_top = self.temp_start;

            // Extract error value from Result::Error
            self.instructions
                .push(OpCode::UnwrapResultError(err_idx as u16, src_reg));
            self.instructions.push(OpCode::DefVar(err_idx, false));
            Some((err_name, err_idx))
        } else {
            None
        };

        // Failure block runs here if Null/Error
        self.compile_statement(*failure_block);
        // Failure block must contain return/break/continue, so execution won't reach here

        // Clean up error binding from symbol table after failure block
        if let Some((err_name, _err_idx)) = err_bind_idx {
            self.symbol_table.remove(&err_name);
            if let Some(prev_idx) = prev_err_mapping {
                self.symbol_table.insert(err_name, prev_idx);
            }
        }

        // Patch jump to skip failure block if Value/Ok
        self.patch_jump(jump_if_value_idx);

        // Now that we're in the success path, expose the unwrapped binding.
        // IMPORTANT: allow rebinding/shadowing an existing name by overwriting the
        // mapping permanently (this is how `check msg? {}` upgrades `msg` for the
        // remainder of the scope).
        self.symbol_table.insert(binding, bind_idx);

        // Extract value into binding slot
        self.instructions
            .push(OpCode::UnwrapMaybe(bind_idx as u16, src_reg));

        self.instructions.push(OpCode::DefVar(bind_idx, false));
    }

    fn compile_while(&mut self, condition: Expr, body: Box<Statement>, is_once: bool) {
        let loop_start = self.instructions.len();
        self.break_patches.push(Vec::new());
        self.is_loop_break.push(true);

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

        self.is_loop_break.pop();
        let patches = self.break_patches.pop().unwrap();
        for idx in patches {
            self.patch_jump(idx);
        }

        // Reset reg_top and temp_start to ensure subsequent code doesn't overwrite variables
        self.reg_top = self.next_index as u16;
        self.temp_start = self.next_index as u16;
    }

    fn compile_loop(&mut self, body: Box<Statement>) {
        let loop_start = self.instructions.len();
        self.break_patches.push(Vec::new());
        self.is_loop_break.push(true);

        self.compile_statement(*body);

        // Jump back to start
        self.emit(OpCode::Jump(loop_start));

        self.is_loop_break.pop();
        let patches = self.break_patches.pop().unwrap();
        for idx in patches {
            self.patch_jump(idx);
        }

        // Reset reg_top and temp_start to ensure subsequent code doesn't overwrite variables
        self.reg_top = self.next_index as u16;
        self.temp_start = self.next_index as u16;
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
        self.is_loop_break.push(true);

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
        self.is_loop_break.pop();
        let patches = self.break_patches.pop().unwrap();
        for idx in patches {
            self.patch_jump(idx);
        }

        // Cleanup: reset registers to ensure subsequent code doesn't overwrite variables
        self.reg_top = self.next_index as u16;
        self.temp_start = self.next_index as u16;
    }

    fn compile_match(&mut self, condition: Expr, arms: Vec<crate::parser::MatchArm>) {
        let match_reg = self.compile_expr(&condition);
        let saved_temp_start = self.temp_start; // Save for restoration at end

        self.break_patches.push(Vec::new());
        self.is_loop_break.push(false);

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

        // Patch break jumps to exit the match
        self.is_loop_break.pop();
        let break_patches = self.break_patches.pop().unwrap();
        for idx in break_patches {
            self.patch_jump(idx);
        }

        // Condition was in match_reg, which will be freed when compile_match finishes
        self.reg_top = match_reg;
        self.temp_start = saved_temp_start.max(match_reg); // Restore but don't go below match_reg
                                                           // Note: DO NOT restore next_index - the state_var slot must stay reserved
                                                           // since DefVar was emitted for it and could cause collisions
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
        // Find the nearest loop context (not match)
        let loop_depth = self.is_loop_break.iter().rposition(|&is_loop| is_loop);

        match loop_depth {
            None => panic!("Compiler Error: 'break' used outside of loop"),
            Some(depth) => {
                // Reset all match state vars that are nested inside the loop
                // Count how many match contexts are between depth and end
                let match_contexts_after_loop = self
                    .is_loop_break
                    .iter()
                    .skip(depth + 1)
                    .filter(|&&is_loop| !is_loop)
                    .count();
                let match_state_start = self
                    .match_state_vars
                    .len()
                    .saturating_sub(match_contexts_after_loop);

                // Collect the state vars we need to reset (to avoid borrow issues)
                let state_vars_to_reset: Vec<usize> = self
                    .match_state_vars
                    .iter()
                    .skip(match_state_start)
                    .copied()
                    .collect();

                for state_var in state_vars_to_reset {
                    let zero_idx = self.add_constant(Value::Numeric(Number::I64(0)));
                    let reg = self.alloc_reg();
                    self.instructions.push(OpCode::LoadConst(reg, zero_idx));
                    self.instructions
                        .push(OpCode::AssignVar(state_var as u16, reg));
                    self.reg_top = self.temp_start;
                }

                let idx = self.emit(OpCode::Jump(0));
                self.break_patches[depth].push(idx);
            }
        }
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
            Pattern::Range {
                start,
                end,
                inclusive,
            } => {
                let start_reg = self.compile_expr(start);
                let end_reg = self.compile_expr(end);

                let lower_ok = self.alloc_reg();
                self.instructions
                    .push(OpCode::GreaterThanEqual(lower_ok, match_reg, start_reg));
                let fail_lower = self.emit(OpCode::JumpIfFalse(lower_ok, 0));

                let upper_ok = self.alloc_reg();
                if *inclusive {
                    self.instructions
                        .push(OpCode::LessThanEqual(upper_ok, match_reg, end_reg));
                } else {
                    self.instructions
                        .push(OpCode::LessThan(upper_ok, match_reg, end_reg));
                }
                let fail_upper = self.emit(OpCode::JumpIfFalse(upper_ok, 0));

                patch_indices.push(self.emit(OpCode::Jump(0)));
                self.patch_jump(fail_lower);
                self.patch_jump(fail_upper);
                self.reg_top = self.temp_start;
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
        return_type: Option<crate::parser::Type>,
    ) -> Function {
        let enclosing = Rc::new(EnclosingInfo {
            symbols: self.symbol_table.clone(),
            state_vars: self.state_vars.clone(),
            upvalues: self.upvalues.clone(),
            enclosing: self.enclosing.clone(),
        });
        let mut sub_compiler =
            Compiler::new(Some(enclosing), self.source.clone(), self.filename.clone());
        sub_compiler.is_global = false;

        // Only inherit actual globals, NOT local symbols from parent scope.
        // Local symbols (like sibling functions) should be resolved via upvalue mechanism.
        for (n, &idx) in &self.global_table {
            sub_compiler.global_table.insert(n.clone(), idx);
        }
        sub_compiler.enums = self.enums.clone();
        sub_compiler.static_values = self.static_values.clone();
        sub_compiler.signatures = self.signatures.clone();
        sub_compiler.current_function_name = Some(name.clone()); // For CallSelf optimization

        sub_compiler.next_index = 0;

        let mut param_ownership = Vec::new();
        let mut param_types = Vec::new();
        let mut default_values = Vec::new();
        let mut param_is_maybe_wrapped = Vec::new();
        for param in &params {
            let p_idx = sub_compiler.next_index;
            sub_compiler.symbol_table.insert(param.name.clone(), p_idx);
            sub_compiler.next_index += 1;
            param_ownership.push(param.is_owned);
            param_types.push(param.ty.clone().unwrap_or(crate::parser::Type::Any));
            param_is_maybe_wrapped.push(param.is_optional);

            // Extract default value if provided
            let default_val = if let Some(ref expr) = param.default_value {
                self.extract_literal_for_default(expr) // Returns Some(Value) for literals
            } else if param.is_optional {
                // Return Maybe::Null for optional params with no default
                Some(crate::value::EnumData::into_value(
                    "Maybe".to_string(),
                    "Null".to_string(),
                    None,
                ))
            } else {
                None // Required param
            };
            default_values.push(default_val);
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

        let (instructions, instruction_spans) =
            std::mem::take(&mut sub_compiler.instructions).into_parts();
        let constants = std::mem::take(&mut sub_compiler.constants);
        let upvalues = sub_compiler.upvalues.borrow().clone();

        Function {
            name,
            instructions: Arc::new(instructions),
            instruction_spans: Arc::new(instruction_spans),
            source: sub_compiler.source.clone(),
            filename: sub_compiler.filename.clone(),
            constants: Arc::new(constants),
            home_globals: None, // Set by VM at load time
            param_count: param_ownership.len(),
            param_ownership,
            param_types,
            default_values,
            param_is_maybe_wrapped,
            return_type: return_type.unwrap_or(crate::parser::Type::Void),
            reg_count: sub_compiler.max_reg as usize,
            upvalues,
        }
    }
}
