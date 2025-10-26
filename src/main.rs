mod ast;
mod lexer;

use std::fs;
use crate::ast::{FnDef, Type};

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
    }
}

fn is_truthy(v: &Value) -> bool {
    match v {
        Value::Bool(b) => *b,
        Value::Number(n) => *n != 0.0,
        Value::String(s) => !s.is_empty(),
        Value::Array(arr) => !arr.is_empty(),
        Value::Function(_) => true
    }
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
        Value::String(_) => Type::String,
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
    }
}

fn types_compatible(expected: &Type, actual: &Type) -> bool {
    match (expected, actual) {
        (Type::Number, Type::Number) => true,
        (Type::String, Type::String) => true,
        (Type::Bool, Type::Bool) => true,
        (Type::Array(e1), Type::Array(e2)) => types_compatible(e1, e2),
        _ => false,
    }
}

fn eval_expr(e: &Expr, env: &mut HashMap<String, (AssignKind, Value)>, fns: &mut HashMap<String, FnDef>) -> Value {
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
            env.insert(name.clone(), (AssignKind::Var, v.clone()));
            v
        }
        Expr::CompoundAssign { name, op: _, value } => {
            let v = eval_expr(value, env, fns);
            env.insert(name.clone(), (AssignKind::Var, v.clone()));
            v
        }
        Expr::Var(name) => {
            if let Some((_, v)) = env.get(name) {
                v.clone()
            } else if let Some(fn_def) = fns.get(name) {
                // If it's a function in fns, return it as a Value::Function
                Value::Function(fn_def.clone())
            } else {
                panic!("Undefined variable: {}", name);
            }
        }
        Expr::Const(name) => {
            if let Some((_, v)) = env.get(name) {
                v.clone()
            } else if let Some(fn_def) = fns.get(name) {
                // If it's a function in fns, return it as a Value::Function
                Value::Function(fn_def.clone())
            } else {
                panic!("Undefined constant: {}", name);
            }
        },
        Expr::Lit(name) => {
            if let Some((_, v)) = env.get(name) {
                v.clone()
            } else {
                panic!("Undefined literal: {}", name);
            }
        },
        Expr::Static(name) => {
            if let Some((_, v)) = env.get(name) {
                v.clone()
            } else {
                panic!("Undefined static: {}", name);
            }
        },
        Expr::Call { name, args } => {
            if let Some((_, Value::Function(fn_def))) = env.get(name) {
                let fn_def = fn_def.clone();
                
                // Create new scope for function
                let mut fn_env = env.clone();
                
                // Bind parameters
                for (i, (param_name, param_type)) in fn_def.params.iter().enumerate() {
                    let arg_val = if i < args.len() {
                        eval_expr(&args[i], env, fns)
                    } else {
                        Value::Number(0.0)
                    };

                    // Validate parameter type
                    if let Some(expected_type) = param_type {
                        let actual_type = infer_value_type(&arg_val);
                        if !types_compatible(expected_type, &actual_type) {
                            panic!("Type error: function param {}: expected {:?}, got {:?}", name, expected_type, actual_type);
                        }
                    }

                    fn_env.insert(param_name.clone(), (AssignKind::Var, arg_val));
                }
                
                // Execute function body
                let flow = execute_block(&fn_def.body, &mut fn_env, fns, fn_def.return_type.as_ref());
                match flow {
                    ControlFlow::Return(val) => val,
                    _ => Value::Number(0.0),
                }
            } else if let Some(fn_def) = fns.get(name).cloned() {
                // Create new scope for function
                let mut fn_env = env.clone();
                
                // Bind parameters
                for (i, (param_name, _)) in fn_def.params.iter().enumerate() {
                    let arg_val = if i < args.len() {
                        eval_expr(&args[i], env, fns)
                    } else {
                        Value::Number(0.0)
                    };
                    fn_env.insert(param_name.clone(), (AssignKind::Var, arg_val));
                }
                
                // Execute function body
                let flow = execute_block(&fn_def.body, &mut fn_env, fns, fn_def.return_type.as_ref());
                match flow {
                    ControlFlow::Return(val) => val,
                    _ => Value::Number(0.0),
                }
            } else {
                panic!("Unknown function: {}", name);
            }
        }
    }
}

fn execute_stmt(
    stmt: &Stmt,
    env: &mut HashMap<String, (AssignKind, Value)>,
    fns: &mut HashMap<String, FnDef>,
    expected_return_type: Option<&Type>,
) -> ControlFlow {
    match stmt {
        Stmt::Echo(e) => {
            let v = eval_expr(e, env, fns);
            print!("{}", format_value(&v));
            ControlFlow::None
        }
        Stmt::Print(e) => {
            let v = eval_expr(e, env, fns);
            println!("{}", format_value(&v));
            ControlFlow::None
        }
        Stmt::ExprStmt(e) => {
            eval_expr(e, env, fns);
            ControlFlow::None
        }
        Stmt::Assign { kind, declared_type, name, value } => {
            let v = eval_expr(value, env, fns);
            let actual_type = infer_value_type(&v);

            if let Some(expected_type) = declared_type {
                if !types_compatible(expected_type, &actual_type) {
                    panic!("Type error: expected {:?}, got {:?}", expected_type, actual_type);
                }
            }

            env.insert(name.clone(), (kind.clone(), v));
            ControlFlow::None
        }
        Stmt::Reassign { name, value } => {
            if !env.contains_key(name) {
                panic!("Variable '{}' not declared", name);
            }

            let (kind, existing_val) = env.get(name).unwrap().clone();
            let existing_type = infer_value_type(&existing_val);

            match kind {
                AssignKind::Var => {
                    let v = eval_expr(value, env, fns);
                    let new_type = infer_value_type(&v);

                    // Validate type matches original
                    if !types_compatible(&existing_type, &new_type) {
                        panic!("Type error: variable {} is {:?}, cannot assign {:?}", name, existing_type, new_type);
                    }

                    env.insert(name.clone(), (kind.clone(), v));
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
            let cond_value = eval_expr(condition, env, fns);
            
            if is_truthy(&cond_value) {
                execute_block(then_block, env, fns, None)
            } else {
                for (else_if_cond, else_if_stmts) in else_ifs {
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

                let cond_value = eval_expr(condition, env, fns);
                if !is_truthy(&cond_value) {
                    break;
                }
            }
            ControlFlow::None
        }
        Stmt::Until { condition, body } => {
            loop {
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

                let cond_value = eval_expr(condition, env, fns);
                if is_truthy(&cond_value) {
                    break;
                }
            }
            ControlFlow::None
        }
        Stmt::Match { expr, cases, default, match_type } => {
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
                Some(e) => eval_expr(e, env, fns),
                None => Value::Number(0.0),
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
    env: &mut HashMap<String, (AssignKind, Value)>,
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
    let mut env: HashMap<String, (AssignKind, Value)> = HashMap::new();
    let mut fns: HashMap<String, FnDef> = HashMap::new();

    for stmt in stmts {
        if let Stmt::FnDef { name, params, return_type, body } = stmt {
            fns.insert(name.clone(), FnDef {
                params: params.clone(),
                body: body.clone(),
                return_type: return_type.clone(),
            });
        }
    }

    execute_block(stmts, &mut env, &mut fns, None);
}
