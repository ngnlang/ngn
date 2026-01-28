use ngn::bytecode::OpCode;
use ngn::value::{EnumData, Function, Number, Value};
use ngn::vm::{FiberStatus, VM};

fn result_ok(v: Value) -> Value {
    EnumData::into_value("Result".to_string(), "Ok".to_string(), Some(Box::new(v)))
}

fn result_err(msg: &str) -> Value {
    EnumData::into_value(
        "Result".to_string(),
        "Error".to_string(),
        Some(Box::new(Value::String(msg.to_string()))),
    )
}

fn run_main_fiber(mut vm: VM) -> ngn::vm::Fiber {
    let mut fiber = vm
        .current_fiber
        .take()
        .expect("VM should start with a main fiber");

    loop {
        let status = fiber.run_step(&mut vm.globals, &mut vm.global_meta, &vm.custom_methods);
        match status {
            FiberStatus::Finished => return fiber,
            FiberStatus::Suspended => {
                // Busy-wait is OK for tests, but be polite to the scheduler.
                std::thread::yield_now();
            }
            _ => {}
        }
    }
}

#[test]
fn spawn_cpu_accepts_function_and_returns_ok() {
    // task(): 7
    let task_fn = Function {
        name: "task".to_string(),
        instructions: std::sync::Arc::new(vec![OpCode::LoadConst(0, 0), OpCode::Return(0)]),
        instruction_spans: std::sync::Arc::new(vec![
            ngn::lexer::Span::default(),
            ngn::lexer::Span::default(),
        ]),
        source: std::sync::Arc::new(String::new()),
        filename: std::sync::Arc::new(String::new()),
        constants: std::sync::Arc::new(vec![Value::Numeric(Number::I64(7))]),
        home_globals: None,
        param_count: 0,
        param_ownership: vec![],
        param_types: vec![],
        default_values: vec![],
        param_is_maybe_wrapped: vec![],
        return_type: ngn::parser::Type::Any,
        reg_count: 1,
        upvalues: vec![],
    };

    // main:
    // r0 = task_fn
    // r1 = spawn.cpu(r0)
    // r2 = <- r1
    let instructions = vec![
        OpCode::LoadConst(0, 0),
        OpCode::SpawnCpu(1, 0),
        OpCode::Receive(2, 1),
        OpCode::Halt,
    ];
    let constants = vec![Value::Function(Box::new(task_fn))];

    let vm = VM::new(
        instructions,
        Vec::new(),
        constants,
        3,
        std::sync::Arc::new(String::new()),
        std::sync::Arc::new(String::new()),
    );
    let fiber = run_main_fiber(vm);

    assert_eq!(
        fiber.get_reg_at(2),
        result_ok(Value::Numeric(Number::I64(7)))
    );
}

#[test]
fn spawn_block_rejects_non_callable() {
    // main:
    // r0 = 123
    // r1 = spawn.block(r0)
    // r2 = <- r1
    let instructions = vec![
        OpCode::LoadConst(0, 0),
        OpCode::SpawnBlock(1, 0),
        OpCode::Receive(2, 1),
        OpCode::Halt,
    ];
    let constants = vec![Value::Numeric(Number::I64(123))];

    let vm = VM::new(
        instructions,
        Vec::new(),
        constants,
        3,
        std::sync::Arc::new(String::new()),
        std::sync::Arc::new(String::new()),
    );
    let fiber = run_main_fiber(vm);

    assert_eq!(
        fiber.get_reg_at(2),
        result_err("spawn.block() expects a function or closure")
    );
}
