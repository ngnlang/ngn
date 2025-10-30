mod ast;
mod lexer;

use std::fs;
use crate::ast::{CallArg, FnDef, Ownership, Type};

lalrpop_mod!(pub ngn);

fn main() {
    let source = fs::read_to_string("main.ngn")
        .expect("Failed to read ngn file");

    let tokens = lexer::tokenize(&source);

    match ngn::ProgramParser::new().parse(tokens.into_iter()) {
        Ok(ast) => run(&ast),
        Err(e) => eprintln!("Parse error: {}", e),
    }
}

use std::collections::HashMap;
use ast::{Expr, Stmt};
use lalrpop_util::lalrpop_mod;

use crate::ast::{AssignKind, ControlFlow, MatchType, Value};

fn format_value(v: &Value) -> String {
    match v {
        Value::Number(n) => n.to_string(),
        Value::String(s) => s.clone(),
        Value::Bool(b) => b.to_string(),
        Value::Array(arr) => format!("[{}]", arr.iter().map(format_value).collect::<Vec<_>>().join(", ")),
        Value::Function(_) => "<function>".to_string(),
        Value::Void => "".to_string(),
    }
}

fn is_truthy(v: &Value) -> bool {
    match v {
        Value::Bool(b) => *b,
        Value::Number(n) => *n != 0.0,
        Value::String(s) => !s.is_empty(),
        Value::Array(arr) => !arr.is_empty(),
        Value::Function(_) => true,
        Value::Void => false,
    }
}

fn is_literal(e: &Expr) -> bool {
    matches!(e, 
        Expr::Number(_) | 
        Expr::String(_) | 
        Expr::Bool(_) |
        Expr::Array(_)
    )
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Number(x), Value::Number(y)) => x == y,
        (Value::String(x), Value::String(y)) => x == y,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        _ => false,
    }
}

fn infer_value_type(v: &Value) -> Type {
    match v {
        Value::Number(_) => Type::Number,
        Value::String(_) => Type::Str,
        Value::Bool(_) => Type::Bool,
        Value::Array(arr) => {
            if arr.is_empty() {
                Type::Array(Box::new(Type::Number))
            } else {
                let elem_type = infer_value_type(&arr[0]);
                
                for elem in arr.iter().skip(1) {
                    let elem_val_type = infer_value_type(elem);
                    if !types_compatible(&elem_type, &elem_val_type) {
                        panic!("Type error: mixed types in array {:?}", arr);
                    }
                }
                
                Type::Array(Box::new(elem_type))
            }
        }
        Value::Function(_) => Type::Function,
        Value::Void => Type::Void,
    }
}

fn infer_expr_type(e: &Expr, env: &HashMap<String, (AssignKind, Value, Ownership)>, fns: &HashMap<String, FnDef>) -> Type {
    match e {
        Expr::Number(_) => Type::Number,
        Expr::String(_) => Type::Str,
        Expr::Bool(_) => Type::Bool,
        Expr::Array(exprs) => {
            if exprs.is_empty() {
                Type::Array(Box::new(Type::Number))
            } else {
                let elem_type = infer_expr_type(&exprs[0], env, fns);
                for expr in exprs.iter().skip(1) {
                    let other_type = infer_expr_type(expr, env, fns);
                    if !types_compatible(&elem_type, &other_type) {
                        panic!("Array type error: mixed types");
                    }
                }
                Type::Array(Box::new(elem_type))
            }
        }
        Expr::Add(a, b) => {
            let left = infer_expr_type(a, env, fns);
            let right = infer_expr_type(b, env, fns);
            if types_compatible(&left, &Type::Number) && types_compatible(&right, &Type::Number) {
                Type::Number
            } else if types_compatible(&left, &Type::String) && types_compatible(&right, &Type::String) {
                Type::String
            } else {
                panic!("Type error: cannot add {:?} and {:?}", left, right);
            }
        }
        Expr::Subtract(a, b) | Expr::Multiply(a, b) | Expr::Divide(a, b) | Expr::Modulo(a, b) => {
            let left = infer_expr_type(a, env, fns);
            let right = infer_expr_type(b, env, fns);
            if !types_compatible(&left, &Type::Number) || !types_compatible(&right, &Type::Number) {
                panic!("Type error: arithmetic requires numbers");
            }
            Type::Number
        }
        Expr::Power(a, b) => {
            let left = infer_expr_type(a, env, fns);
            let right = infer_expr_type(b, env, fns);
            if !types_compatible(&left, &Type::Number) || !types_compatible(&right, &Type::Number) {
                panic!("Type error: power requires numbers");
            }
            Type::Number
        }
        Expr::Negative(e) => {
            let t = infer_expr_type(e, env, fns);
            if !types_compatible(&t, &Type::Number) {
                panic!("Type error: cannot negate non-number");
            }
            Type::Number
        }
        Expr::Equal(_, _) | Expr::NotEqual(_, _) | Expr::LessThan(_, _) | 
        Expr::LessThanOrEqual(_, _) | Expr::GreaterThan(_, _) | Expr::GreaterThanOrEqual(_, _) => {
            Type::Bool
        }
        Expr::Not(_) => Type::Bool,
        Expr::Var(name) | Expr::Const(name) | Expr::Lit(name) | Expr::Static(name) => {
            if let Some((_, v, _ownership)) = env.get(name) {
                infer_value_type(v)
            } else if fns.contains_key(name) {
                Type::Function
            } else {
                panic!("Unknown variable: {}", name);
            }
        }
        Expr::Call { name, .. } => {
            // Check if it's a function value in the environment
            if let Some((_, Value::Function(fn_def), _ownership)) = env.get(name) {
                fn_def.return_type.clone().unwrap_or(Type::Number)
            } else if let Some(fn_def) = fns.get(name) {
                // Check if it's a global function
                fn_def.return_type.clone().unwrap_or(Type::Number)
            } else {
                panic!("Unknown function: {}", name);
            }
        }
        Expr::Assign { value, .. } => infer_expr_type(value, env, fns),
        Expr::CompoundAssign { value, .. } => infer_expr_type(value, env, fns),
    }
}

fn types_compatible(expected: &Type, actual: &Type) -> bool {
    match (expected, actual) {
        (Type::Number, Type::Number) => true,
        (Type::Str, Type::Str) => true,
        (Type::String, Type::String) => true,
        (Type::Bool, Type::Bool) => true,
        (Type::Array(e1), Type::Array(e2)) => types_compatible(e1, e2),
        (Type::Void, Type::Void) => true,
        _ => false,
    }
}

fn convert_ownership(t: Type) -> Type {
    match t {
        Type::Str => Type::String,
        Type::Array(inner) => Type::Array(Box::new(convert_ownership(*inner))),
        Type::Object(map) => {
            Type::Object(
                map.into_iter()
                    .map(|(k, v)| (k, convert_ownership(v)))
                    .collect()
            )
        }
        other => other,
    }
}

fn convert_value_ownership(v: Value) -> Value {
    match v {
        Value::String(s) => Value::String(s),
        Value::Array(arr) => Value::Array(arr),
        other => other,
    }
}

fn is_copy_type(t: &Type) -> bool {
    matches!(t, Type::Number | Type::Bool)
}

fn eval_expr(e: &Expr, env: &mut HashMap<String, (AssignKind, Value, Ownership)>, fns: &mut HashMap<String, FnDef>) -> Value {
    match e {
        Expr::Number(n) => Value::Number(*n),
        Expr::String(s) => Value::String(s.clone()),
        Expr::Bool(b) => Value::Bool(*b),
        Expr::Array(exprs) => {
            let mut values: Vec<Value> = Vec::new();
            for e in exprs {
                values.push(eval_expr(e, env, fns));
            }
            Value::Array(values)
        },
        Expr::Not(e) => {
            let val = eval_expr(e, env, fns);
            Value::Bool(!is_truthy(&val))
        },
        Expr::Add(a, b) => {
            match (eval_expr(a, env, fns), eval_expr(b, env, fns)) {
                (Value::Number(x), Value::Number(y)) => Value::Number(x + y),
                (Value::String(x), Value::String(y)) => Value::String(format!("{}{}", x, y)),
                _ => panic!("Type error: cannot add these types"),
            }
        }
        Expr::Subtract(a, b) => {
            match (eval_expr(a, env, fns), eval_expr(b, env, fns)) {
                (Value::Number(x), Value::Number(y)) => Value::Number(x - y),
                _ => panic!("Type error: cannot subtract non-numbers"),
            }
        }
        Expr::Multiply(a, b) => {
            match (eval_expr(a, env, fns), eval_expr(b, env, fns)) {
                (Value::Number(x), Value::Number(y)) => Value::Number(x * y),
                _ => panic!("Type error: cannot multiply non-numbers"),
            }
        }
        Expr::Divide(a, b) => {
            match (eval_expr(a, env, fns), eval_expr(b, env, fns)) {
                (Value::Number(x), Value::Number(y)) => Value::Number(x / y),
                _ => panic!("Type error: cannot divide non-numbers"),
            }
        }
        Expr::Negative(e) => {
            match eval_expr(e, env, fns) {
                Value::Number(x) => Value::Number(-x),
                _ => panic!("Type error: cannot negate non-number"),
            }
        }
        Expr::Power(a, b) => {
            match (eval_expr(a, env, fns), eval_expr(b, env, fns)) {
                (Value::Number(x), Value::Number(y)) => Value::Number(x.powf(y)),
                _ => panic!("Type error: cannot exponentiate non-numbers"),
            }
        }
        Expr::Modulo(a, b) => {
            match (eval_expr(a, env, fns), eval_expr(b, env, fns)) {
                (Value::Number(x), Value::Number(y)) => Value::Number(x % y),
                _ => panic!("Type error: cannot modulo non-numbers"),
            }
        }
        Expr::Equal(a, b) => {
            let av = eval_expr(a, env, fns);
            let bv = eval_expr(b, env, fns);
            Value::Bool(values_equal(&av, &bv))
        }
        Expr::NotEqual(a, b) => {
            let av = eval_expr(a, env, fns);
            let bv = eval_expr(b, env, fns);
            Value::Bool(!values_equal(&av, &bv))
        }
        Expr::LessThan(a, b) => {
            match (eval_expr(a, env, fns), eval_expr(b, env, fns)) {
                (Value::Number(x), Value::Number(y)) => Value::Bool(x < y),
                _ => panic!("Type error: cannot compare non-numbers"),
            }
        }
        Expr::LessThanOrEqual(a, b) => {
            match (eval_expr(a, env, fns), eval_expr(b, env, fns)) {
                (Value::Number(x), Value::Number(y)) => Value::Bool(x <= y),
                _ => panic!("Type error: cannot compare non-numbers"),
            }
        }
        Expr::GreaterThan(a, b) => {
            match (eval_expr(a, env, fns), eval_expr(b, env, fns)) {
                (Value::Number(x), Value::Number(y)) => Value::Bool(x > y),
                _ => panic!("Type error: cannot compare non-numbers"),
            }
        }
        Expr::GreaterThanOrEqual(a, b) => {
            match (eval_expr(a, env, fns), eval_expr(b, env, fns)) {
                (Value::Number(x), Value::Number(y)) => Value::Bool(x >= y),
                _ => panic!("Type error: cannot compare non-numbers"),
            }
        }
        Expr::Assign { name, value } => {
            let v = eval_expr(value, env, fns);
            env.insert(name.clone(), (AssignKind::Var, v.clone(), Ownership::Borrowed));
            v
        }
        Expr::CompoundAssign { name, op: _, value } => {
            let v = eval_expr(value, env, fns);
            env.insert(name.clone(), (AssignKind::Var, v.clone(), Ownership::Borrowed));
            v
        }
        Expr::Var(name) => {
            if let Some((_, v, _ownership)) = env.get(name) {
                v.clone()
            } else if let Some(fn_def) = fns.get(name) {
                // If it's a function in fns, return it as a Value::Function
                Value::Function(fn_def.clone())
            } else {
                panic!("Undefined variable: {}", name);
            }
        }
        Expr::Const(name) => {
            if let Some((_, v, _ownership)) = env.get(name) {
                v.clone()
            } else if let Some(fn_def) = fns.get(name) {
                // If it's a function in fns, return it as a Value::Function
                Value::Function(fn_def.clone())
            } else {
                panic!("Undefined constant: {}", name);
            }
        },
        Expr::Lit(name) => {
            if let Some((_, v, _ownership)) = env.get(name) {
                v.clone()
            } else {
                panic!("Undefined literal: {}", name);
            }
        },
        Expr::Static(name) => {
            if let Some((_, v, _ownership)) = env.get(name) {
                v.clone()
            } else {
                panic!("Undefined static: {}", name);
            }
        },
        Expr::Call { name, args } => {
            let fn_def = if let Some((_, Value::Function(fn_def), _ownership)) = env.get(name) {
                fn_def.clone()
            } else if let Some(fn_def) = fns.get(name) {
                fn_def.clone()
            } else {
                panic!("Unknown function: {}", name);
            };

            // Create new scope for function
            let mut fn_env: HashMap<String, (AssignKind, Value, Ownership)> = HashMap::new();
            let fn_arg_count = fn_def.params.len();
            
            // Bind parameters
            for (i, (param_name, param_type, param_ownership)) in fn_def.params.iter().enumerate() {
                let call_arg = &args[i];
                let call_ownership = match call_arg {
                    CallArg::Normal(e) => {
                        // Check if the expression is a variable we know about
                        if let Expr::Var(var_name) = e.as_ref() {
                            if let Some((_, _, var_ownership)) = env.get(var_name) {
                                var_ownership.clone()
                            } else {
                                Ownership::Borrowed
                            }
                        } else {
                            Ownership::Borrowed
                        }
                    },
                    CallArg::Owned(_) => Ownership::Owned,
                };

                // Can not upgrade from Borrowed to Owned
                if *param_ownership == Ownership::Owned && call_ownership == Ownership::Borrowed {
                    panic!("Function {} param '{}' expects owned, but got borrowed", name, param_name);
                }
                
                let arg_val = if i < args.len() {
                    match &args[i] {
                        CallArg::Normal(e) => eval_expr(e, env, fns),
                        CallArg::Owned(e) => {
                            let v = eval_expr(e, env, fns);
                            convert_value_ownership(v)
                        }
                    }
                } else {
                    panic!("Function {} expects {} arguments, got {}", name, fn_arg_count, args.len())
                };

                // Validate parameter type
                if let Some(expected_type) = param_type {
                    let actual_type = infer_value_type(&arg_val);
                    if !types_compatible(expected_type, &actual_type) {
                        panic!("Type error: function param {}: expected {:?}, got {:?}", name, expected_type, actual_type);
                    }
                }

                let ownership = match &args[i] {
                    CallArg::Normal(_) => Ownership::Borrowed,
                    CallArg::Owned(_) => Ownership::Owned,
                };

                fn_env.insert(param_name.clone(), (AssignKind::Var, arg_val, ownership));
            }
            
            // Execute function body
            let flow = execute_block(&fn_def.body, &mut fn_env, fns, fn_def.return_type.as_ref());
            match flow {
                ControlFlow::Return(val) => val,
                _ => Value::Void,
            }
        }
    }
}

fn execute_stmt(
    stmt: &Stmt,
    env: &mut HashMap<String, (AssignKind, Value, Ownership)>,
    fns: &mut HashMap<String, FnDef>,
    expected_return_type: Option<&Type>,
) -> ControlFlow {
    match stmt {
        Stmt::Echo(e) => {
            let _type = infer_expr_type(e, env, fns);
            let v = eval_expr(e, env, fns);
            print!("{}", format_value(&v));
            ControlFlow::None
        }
        Stmt::Print(e) => {
            let _type = infer_expr_type(e, env, fns);
            let v = eval_expr(e, env, fns);
            println!("{}", format_value(&v));
            ControlFlow::None
        }
        Stmt::ExprStmt(e) => {
            let _type = infer_expr_type(e, env, fns);
            eval_expr(e, env, fns);
            ControlFlow::None
        }
        Stmt::Assign { kind, declared_type, name, value, ownership} => {
            // Disallow function assignments for all variable types
            if let AssignKind::Var | AssignKind::Const = kind {
                if let Expr::Var(fn_name) | Expr::Const(fn_name) = value {
                    if fns.contains_key(fn_name) {
                        panic!("Cannot assign function '{}' to variable. Use 'fn' to define functions.", fn_name);
                    }
                }
            }

            // For lit and static, only allow literals
            match kind {
                AssignKind::Lit | AssignKind::Static => {
                    if !is_literal(value) {
                        panic!("'{}' can only be used to assign literal values, not expressions or function calls", 
                            match kind {
                                AssignKind::Lit => "lit",
                                AssignKind::Static => "static",
                                _ => "<Illegal initializer>",
                            }
                        );
                    }
                }
                _ => {}
            }

            let _type = infer_expr_type(value, env, fns);
            let v = eval_expr(value, env, fns);
            let mut actual_type = infer_value_type(&v);
            let mut normalized_declared_type = declared_type.clone();

            normalized_declared_type = normalized_declared_type.map(|t| {
                match ownership {
                    Ownership::Owned => convert_ownership(t),
                    Ownership::Borrowed => t,
                }
            });

            match ownership {
                Ownership::Borrowed => {
                    // Value must be borrowable (&str, &[T], etc.)
                    if *kind == AssignKind::Var && actual_type == Type::String {
                        panic!("Cannot use '=' with String; use '<-' for owned assignment");
                    }
                }
                Ownership::Owned => {
                    actual_type = convert_ownership(actual_type);
                }
            }

            if let Some(expected_type) = normalized_declared_type {
                if !types_compatible(&expected_type, &actual_type) {
                    panic!("Type error: expected {:?}, got {:?}", expected_type, actual_type);
                }
            }

            env.insert(name.clone(), (kind.clone(), v, ownership.clone()));
            ControlFlow::None
        }
        Stmt::Reassign { name, value } => {
            let _type = infer_expr_type(value, env, fns);
            if !env.contains_key(name) {
                panic!("Variable '{}' not declared", name);
            }

            let (kind, existing_val, ownership) = env.get(name).unwrap().clone();
            let existing_type = infer_value_type(&existing_val);

            match kind {
                AssignKind::Var => {
                    let v = eval_expr(value, env, fns);
                    let actual_type = infer_value_type(&v);

                    // Only allow reassignment if value is mutable (owned)
                    if ownership == Ownership::Borrowed && !is_copy_type(&actual_type) {
                        panic!("Cannot reassign immutable borrowed value '{}'", name);
                    }

                    // Validate type matches original
                    if !types_compatible(&existing_type, &actual_type) {
                        panic!("Type error: variable {} is {:?}, cannot assign {:?}", name, existing_type, actual_type);
                    }

                    env.insert(name.clone(), (kind.clone(), v, ownership));
                    ControlFlow::None
                }
                kind => panic!("Cannot reassign for {}", match kind {
                    AssignKind::Const => "const",
                    AssignKind::Lit => "lit",
                    AssignKind::Static => "static",
                    _ => "an invalid initializer",
                })
            }
        }
        Stmt::Break => ControlFlow::Break,
        Stmt::Next => ControlFlow::Next,
        Stmt::If { condition, then_block, else_ifs, else_block } => {
            let _cond_type = infer_expr_type(condition, env, fns);
            let cond_value = eval_expr(condition, env, fns);
            
            if is_truthy(&cond_value) {
                execute_block(then_block, env, fns, None)
            } else {
                for (else_if_cond, else_if_stmts) in else_ifs {
                    let _type = infer_expr_type(else_if_cond, env, fns);
                    let else_if_value = eval_expr(else_if_cond, env, fns);
                    if is_truthy(&else_if_value) {
                        return execute_block(else_if_stmts, env, fns, None);
                    }
                }
                
                if let Some(else_stmts) = else_block {
                    execute_block(else_stmts, env, fns, None)
                } else {
                    ControlFlow::None
                }
            }
        }
        Stmt::While { condition, body } => {
            loop {
                let _cond_type = infer_expr_type(condition, env, fns);
                let cond_value = eval_expr(condition, env, fns);
                if !is_truthy(&cond_value) {
                    break;
                }
                
                match execute_block(body, env, fns, None) {
                    ControlFlow::Break => break,
                    ControlFlow::Next => continue,
                    ControlFlow::Return(_) => return ControlFlow::None,
                    ControlFlow::None => {}
                }
            }
            ControlFlow::None
        }
        Stmt::WhileOnce { condition, body } => {
            loop {
                match execute_block(body, env, fns, None) {
                    ControlFlow::Break => break,
                    ControlFlow::Next => continue,
                    ControlFlow::Return(_) => return ControlFlow::None,
                    ControlFlow::None => {}
                }

                let _cond_type = infer_expr_type(condition, env, fns);
                let cond_value = eval_expr(condition, env, fns);
                if !is_truthy(&cond_value) {
                    break;
                }
            }
            ControlFlow::None
        }
        Stmt::Until { condition, body } => {
            loop {
                let _cond_type = infer_expr_type(condition, env, fns);
                let cond_value = eval_expr(condition, env, fns);
                if is_truthy(&cond_value) {
                    break;
                }
                
                match execute_block(body, env, fns, None) {
                    ControlFlow::Break => break,
                    ControlFlow::Next => continue,
                    ControlFlow::Return(_) => return ControlFlow::None,
                    ControlFlow::None => {}
                }
            }
            ControlFlow::None
        }
        Stmt::UntilOnce { condition, body } => {
            loop {
                match execute_block(body, env, fns, None) {
                    ControlFlow::Break => break,
                    ControlFlow::Next => continue,
                    ControlFlow::Return(_) => return ControlFlow::None,
                    ControlFlow::None => {}
                }

                let _cond_type = infer_expr_type(condition, env, fns);
                let cond_value = eval_expr(condition, env, fns);
                if is_truthy(&cond_value) {
                    break;
                }
            }
            ControlFlow::None
        }
        Stmt::Match { expr, cases, default, match_type } => {
            let _expr_type = infer_expr_type(expr, env, fns);
            let val = eval_expr(expr, env, fns);
            let mut matched = false;
            
            for (tests, stmts) in cases {
                // For match any, continue if we already matched
                if *match_type == MatchType::Any && matched {
                    let flow = execute_block(stmts, env, fns, None);
                    if flow == ControlFlow::Break {
                        return ControlFlow::None;
                    }
                    continue;
                }
                
                for test in tests {
                    let _test_type = infer_expr_type(test, env, fns);
                    let test_val = eval_expr(test, env, fns);
                    if values_equal(&val, &test_val) {
                        matched = true;
                        let flow = execute_block(stmts, env, fns, None);
                        
                        match (match_type, flow) {
                            (MatchType::One, ControlFlow::Next) => continue,
                            (MatchType::One, _) => return ControlFlow::None,
                            
                            (MatchType::Any, ControlFlow::Break) => return ControlFlow::None,
                            (MatchType::Any, _) => continue,
                        }
                    }
                }
            }
            
            // No match found, execute default if it exists
            if let Some(default_stmts) = default {
                execute_block(default_stmts, env, fns, None)
            } else {
                ControlFlow::None
            }
        }
        Stmt::FnDef { name, params, return_type, body } => {
            fns.insert(name.clone(), FnDef {
                params: params.clone(),
                body: body.clone(),
                return_type: return_type.clone(),
            });
            ControlFlow::None
        }
        Stmt::Return(expr_opt) => {
            let val = match expr_opt {
                Some(e) => {
                    let _type = infer_expr_type(e, env, fns);
                    eval_expr(e, env, fns)
                }
                None => Value::Void,
            };

            if let Some(expected_type) = expected_return_type {
                let actual_type = infer_value_type(&val);
                if !types_compatible(expected_type, &actual_type) {
                    panic!("Return type error: expected {:?}, got {:?}", expected_type, actual_type);
                }
            }

            ControlFlow::Return(val)
        }
    }
}

fn execute_block(
    stmts: &[Stmt],
    env: &mut HashMap<String, (AssignKind, Value, Ownership)>,
    fns: &mut HashMap<String, FnDef>,
    expected_return_type: Option<&Type>,
) -> ControlFlow {
    for stmt in stmts {
        let flow = execute_stmt(stmt, env, fns, expected_return_type);
        match flow {
            ControlFlow::Break | ControlFlow::Next | ControlFlow::Return(_) => return flow,
            ControlFlow::None => {}
        }
    }
    ControlFlow::None
}

fn run(stmts: &[Stmt]) {
    let mut env: HashMap<String, (AssignKind, Value, Ownership)> = HashMap::new();
    let mut fns: HashMap<String, FnDef> = HashMap::new();

    for stmt in stmts {
        match stmt {
            Stmt::FnDef { name, params, return_type, body } => {
                fns.insert(name.clone(), FnDef {
                    params: params.clone(),
                    body: body.clone(),
                    return_type: return_type.clone(),
                });
            }
            Stmt::Assign { kind: AssignKind::Lit | AssignKind::Static, .. } => {
                // Execute top-level lit/static
                execute_stmt(stmt, &mut env, &mut fns, None);
            }
            _ => {
                panic!("Top-level statements must be function definitions or lit/static assignments");
            }
        }
    }

    if let Some(main_fn) = fns.get("main").cloned() {
        if !main_fn.params.is_empty() {
            panic!("main() must have no parameters");
        }
        
        let mut main_env = env.clone();
        execute_block(&main_fn.body, &mut main_env, &mut fns, main_fn.return_type.as_ref());
    } else {
        panic!("No main() function found. Entrypoint files must define main()");
    }
}
