use ngn::analyzer::Analyzer;
use ngn::bytecode::OpCode;
use ngn::compiler::Compiler;
use ngn::lexer::{Lexer, Token};
use ngn::parser::{Parser, StatementKind};
use ngn::vm::VM;
use std::fs;
use std::path::Path;
use std::time::Instant;

fn main() {
    let benchmarks_dir = "benchmarks";
    let paths = fs::read_dir(benchmarks_dir).expect("Could not read benchmarks directory");

    println!(
        "{: <20} | {: >10} | {: >10} | {: >10} | {: >10}",
        "Benchmark", "Parse", "Analyze", "Compile", "Execute"
    );
    println!(
        "{:-<21}|{:-<12}|{:-<12}|{:-<12}|{:-<11}",
        "", "", "", "", ""
    );

    for entry in paths {
        let entry = entry.expect("Could not read entry");
        let path = entry.path();

        if path.extension().and_then(|s| s.to_str()) == Some("ngn") {
            run_benchmark(&path);
        }
    }
}

fn run_benchmark(path: &Path) {
    let filename = path.file_name().unwrap().to_str().unwrap();
    let source = fs::read_to_string(path).expect("Could not read file");

    // 1. Parse
    let start_parse = Instant::now();
    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer);
    let mut statements = Vec::new();
    while parser.current_token != Token::EOF {
        if parser.current_token == Token::Newline {
            parser.advance();
            continue;
        }
        statements.push(parser.parse_statement());
    }
    let parse_time = start_parse.elapsed();

    // 2. Analyze
    let start_analyze = Instant::now();
    let mut analyzer = Analyzer::new();
    if let Err(errors) = analyzer.analyze(&statements) {
        println!("{: <20} | ANALYZE ERROR: {:?}", filename, errors);
        return;
    }
    let analyze_time = start_analyze.elapsed();

    // 3. Compile
    let start_compile = Instant::now();
    let mut compiler = Compiler::new(None);
    compiler.inject_builtins();

    // Register globals
    for stmt in &statements {
        if let StatementKind::Function { name, .. } = &stmt.kind {
            let var_idx = compiler.next_index;
            compiler.global_table.insert(name.clone(), var_idx);
            compiler.next_index += 1;
        }
    }

    for stmt in statements {
        compiler.compile_statement(stmt);
    }
    let compile_time = start_compile.elapsed();

    // 4. Execute
    let mut final_instructions = compiler.instructions.clone();
    if let Some(&main_idx) = compiler.global_table.get("main") {
        final_instructions.push(OpCode::CallGlobal(0, main_idx, 0, 0));
        final_instructions.push(OpCode::Halt);
    }

    let start_execute = Instant::now();
    let mut vm = VM::new(final_instructions, compiler.constants, compiler.next_index);
    vm.run();
    let execute_time = start_execute.elapsed();

    println!(
        "{: <20} | {: >10?} | {: >10?} | {: >10?} | {: >10?}",
        filename, parse_time, analyze_time, compile_time, execute_time
    );
}
