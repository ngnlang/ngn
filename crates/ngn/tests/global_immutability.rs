use ngn::bytecode::OpCode;
use ngn::value::Value;
use ngn::vm::VM;

#[test]
#[should_panic(expected = "Cannot assign to immutable global")]
fn immutable_global_panics_on_reassign() {
    // Slot 0 is declared immutable, initialized once, then reassigned.
    let instructions = vec![
        OpCode::LoadConst(0, 0),
        OpCode::DefGlobal(0, false),
        OpCode::AssignGlobal(0, 0),
        OpCode::LoadConst(1, 1),
        OpCode::AssignGlobal(0, 1),
        OpCode::Halt,
    ];
    let constants = vec![
        Value::Numeric(ngn::value::Number::I64(1)),
        Value::Numeric(ngn::value::Number::I64(2)),
    ];

    // NOTE: we intentionally run the Fiber directly here.
    // VM::run() wraps execution in catch_unwind, which would swallow the panic
    // and convert it into a recoverable "internal error".
    let mut vm = VM::new(
        instructions,
        Vec::new(),
        constants,
        2,
        std::sync::Arc::new(String::new()),
        std::sync::Arc::new(String::new()),
    );
    let mut fiber = vm
        .current_fiber
        .take()
        .expect("VM should start with a main fiber");

    loop {
        let _ = fiber.run_step(&mut vm.globals, &mut vm.global_meta, &vm.custom_methods);
    }
}

#[test]
#[should_panic(expected = "Cannot assign to undefined global")]
fn assigning_undefined_global_panics() {
    // Assigning a global slot without a prior DefGlobal should always error.
    let instructions = vec![
        OpCode::LoadConst(0, 0),
        OpCode::AssignGlobal(42, 0),
        OpCode::Halt,
    ];
    let constants = vec![Value::Numeric(ngn::value::Number::I64(1))];

    let mut vm = VM::new(
        instructions,
        Vec::new(),
        constants,
        1,
        std::sync::Arc::new(String::new()),
        std::sync::Arc::new(String::new()),
    );
    let mut fiber = vm
        .current_fiber
        .take()
        .expect("VM should start with a main fiber");

    loop {
        let _ = fiber.run_step(&mut vm.globals, &mut vm.global_meta, &vm.custom_methods);
    }
}
