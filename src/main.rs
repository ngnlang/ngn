mod ast;
mod lexer;

use std::fs;

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
    }
}

fn is_truthy(v: &Value) -> bool {
    match v {
        Value::Bool(b) => *b,
        Value::Number(n) => *n != 0.0,
        Value::String(s) => !s.is_empty(),
        Value::Array(arr) => !arr.is_empty(),
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

fn eval_expr(e: &Expr, env: &mut HashMap<String, (AssignKind, Value)>) -> Value {
    match e {
        Expr::Number(n) => Value::Number(*n),
        Expr::String(s) => Value::String(s.clone()),
        Expr::Bool(b) => Value::Bool(*b),
        Expr::Array(exprs) => {
            let mut values: Vec<Value> = Vec::new();
            for e in exprs {
                values.push(eval_expr(e, env));
            }
            Value::Array(values)
        },
        Expr::Not(e) => {
            let val = eval_expr(e, env);
            Value::Bool(!is_truthy(&val))
        },
        Expr::Add(a, b) => {
            match (eval_expr(a, env), eval_expr(b, env)) {
                (Value::Number(x), Value::Number(y)) => Value::Number(x + y),
                (Value::String(x), Value::String(y)) => Value::String(format!("{}{}", x, y)),
                _ => panic!("Type error: cannot add these types"),
            }
        }
        Expr::Subtract(a, b) => {
            match (eval_expr(a, env), eval_expr(b, env)) {
                (Value::Number(x), Value::Number(y)) => Value::Number(x - y),
                _ => panic!("Type error: cannot subtract non-numbers"),
            }
        }
        Expr::Multiply(a, b) => {
            match (eval_expr(a, env), eval_expr(b, env)) {
                (Value::Number(x), Value::Number(y)) => Value::Number(x * y),
                _ => panic!("Type error: cannot multiply non-numbers"),
            }
        }
        Expr::Divide(a, b) => {
            match (eval_expr(a, env), eval_expr(b, env)) {
                (Value::Number(x), Value::Number(y)) => Value::Number(x / y),
                _ => panic!("Type error: cannot divide non-numbers"),
            }
        }
        Expr::Negative(e) => {
            match eval_expr(e, env) {
                Value::Number(x) => Value::Number(-x),
                _ => panic!("Type error: cannot negate non-number"),
            }
        }
        Expr::Power(a, b) => {
            match (eval_expr(a, env), eval_expr(b, env)) {
                (Value::Number(x), Value::Number(y)) => Value::Number(x.powf(y)),
                _ => panic!("Type error: cannot exponentiate non-numbers"),
            }
        }
        Expr::Modulo(a, b) => {
            match (eval_expr(a, env), eval_expr(b, env)) {
                (Value::Number(x), Value::Number(y)) => Value::Number(x % y),
                _ => panic!("Type error: cannot modulo non-numbers"),
            }
        }
        Expr::Equal(a, b) => {
            let av = eval_expr(a, env);
            let bv = eval_expr(b, env);
            Value::Bool(values_equal(&av, &bv))
        }
        Expr::NotEqual(a, b) => {
            let av = eval_expr(a, env);
            let bv = eval_expr(b, env);
            Value::Bool(!values_equal(&av, &bv))
        }
        Expr::LessThan(a, b) => {
            match (eval_expr(a, env), eval_expr(b, env)) {
                (Value::Number(x), Value::Number(y)) => Value::Bool(x < y),
                _ => panic!("Type error: cannot compare non-numbers"),
            }
        }
        Expr::LessThanOrEqual(a, b) => {
            match (eval_expr(a, env), eval_expr(b, env)) {
                (Value::Number(x), Value::Number(y)) => Value::Bool(x <= y),
                _ => panic!("Type error: cannot compare non-numbers"),
            }
        }
        Expr::GreaterThan(a, b) => {
            match (eval_expr(a, env), eval_expr(b, env)) {
                (Value::Number(x), Value::Number(y)) => Value::Bool(x > y),
                _ => panic!("Type error: cannot compare non-numbers"),
            }
        }
        Expr::GreaterThanOrEqual(a, b) => {
            match (eval_expr(a, env), eval_expr(b, env)) {
                (Value::Number(x), Value::Number(y)) => Value::Bool(x >= y),
                _ => panic!("Type error: cannot compare non-numbers"),
            }
        }
        Expr::Assign { name, value } => {
            let v = eval_expr(value, env);
            env.insert(name.clone(), (AssignKind::Var, v.clone()));
            v
        }
        Expr::CompoundAssign { name, op: _, value } => {
            let v = eval_expr(value, env);
            env.insert(name.clone(), (AssignKind::Var, v.clone()));
            v
        }
        Expr::Var(name) => env.get(name).map(|(_, v)| v.clone()).unwrap_or(Value::Number(0.0)),
        Expr::Const(name) => env.get(name).map(|(_, v)| v.clone()).unwrap_or(Value::Number(0.0)),
    }
}

fn execute_stmt(
    stmt: &Stmt,
    env: &mut HashMap<String, (AssignKind, Value)>,
) -> ControlFlow {
    match stmt {
        Stmt::Echo(e) => {
            let v = eval_expr(e, env);
            print!("{}", format_value(&v));
            ControlFlow::None
        }
        Stmt::Print(e) => {
            let v = eval_expr(e, env);
            println!("{}", format_value(&v));
            ControlFlow::None
        }
        Stmt::Assign { kind, declared_type: _, name, value } => {
            let v = eval_expr(value, env);
            env.insert(name.clone(), (kind.clone(), v));
            ControlFlow::None
        }
        Stmt::Reassign { name, value } => {
            if !env.contains_key(name) {
                panic!("Variable '{}' not declared", name);
            }
            let kind = env.get(name).map(|(k, _)| k.clone()).unwrap();
            match kind {
                AssignKind::Var => {
                    let v = eval_expr(value, env);
                    env.insert(name.clone(), (kind.clone(), v));
                    ControlFlow::None
                }
                AssignKind::Const => {
                    panic!("Cannot reassign {}", match kind {
                        AssignKind::Const => "const",
                        _ => "variable",
                    });
                }
            }
        }
        Stmt::Break => ControlFlow::Break,
        Stmt::Next => ControlFlow::Next,
        Stmt::If { condition, then_block, else_ifs, else_block } => {
            let cond_value = eval_expr(condition, env);
            
            if is_truthy(&cond_value) {
                execute_block(then_block, env)
            } else {
                for (else_if_cond, else_if_stmts) in else_ifs {
                    let else_if_value = eval_expr(else_if_cond, env);
                    if is_truthy(&else_if_value) {
                        return execute_block(else_if_stmts, env);
                    }
                }
                
                if let Some(else_stmts) = else_block {
                    execute_block(else_stmts, env)
                } else {
                    ControlFlow::None
                }
            }
        }
        Stmt::While { condition, body } => {
            loop {
                let cond_value = eval_expr(condition, env);
                if !is_truthy(&cond_value) {
                    break;
                }
                
                match execute_block(body, env) {
                    ControlFlow::Break => break,
                    ControlFlow::Next => continue,
                    ControlFlow::None => {}
                }
            }
            ControlFlow::None
        }
        Stmt::OnceWhile { condition, body } => {
            loop {
                match execute_block(body, env) {
                    ControlFlow::Break => break,
                    ControlFlow::Next => continue,
                    ControlFlow::None => {}
                }

                let cond_value = eval_expr(condition, env);
                if !is_truthy(&cond_value) {
                    break;
                }
            }
            ControlFlow::None
        }
        Stmt::Until { condition, body } => {
            loop {
                let cond_value = eval_expr(condition, env);
                if is_truthy(&cond_value) {
                    break;
                }
                
                match execute_block(body, env) {
                    ControlFlow::Break => break,
                    ControlFlow::Next => continue,
                    ControlFlow::None => {}
                }
            }
            ControlFlow::None
        }
        Stmt::OnceUntil { condition, body } => {
            loop {
                match execute_block(body, env) {
                    ControlFlow::Break => break,
                    ControlFlow::Next => continue,
                    ControlFlow::None => {}
                }

                let cond_value = eval_expr(condition, env);
                if is_truthy(&cond_value) {
                    break;
                }
            }
            ControlFlow::None
        }
        Stmt::Match { expr, cases, default, match_type } => {
            let val = eval_expr(expr, env);
            let mut matched = false;
            
            for (tests, stmts) in cases {
                // For match any, continue if we already matched
                if *match_type == MatchType::Any && matched {
                    let flow = execute_block(stmts, env);
                    if flow == ControlFlow::Break {
                        return ControlFlow::None;
                    }
                    continue;
                }
                
                for test in tests {
                    let test_val = eval_expr(test, env);
                    if values_equal(&val, &test_val) {
                        matched = true;
                        let flow = execute_block(stmts, env);
                        
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
                execute_block(default_stmts, env)
            } else {
                ControlFlow::None
            }
        }
    }
}

fn execute_block(
    stmts: &[Stmt],
    env: &mut HashMap<String, (AssignKind, Value)>,
) -> ControlFlow {
    for stmt in stmts {
        let flow = execute_stmt(stmt, env);
        if flow == ControlFlow::Break {
            return ControlFlow::Break;
        }
        if flow == ControlFlow::Next {
            return ControlFlow::Next;
        }
    }
    ControlFlow::None
}

fn run(stmts: &[Stmt]) {
    let mut env: HashMap<String, (AssignKind, Value)> = HashMap::new();
    execute_block(stmts, &mut env);
}
