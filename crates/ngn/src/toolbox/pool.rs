//! Internal-only module kept for now.
//!
//! This file is intentionally not wired into the public toolbox.
//! We keep it around temporarily as a reference implementation for bridging
//! a bounded blocking pool and ngn channels.

use crate::blocking_pool::{BlockingJob, global_blocking_pool};
use crate::error::RuntimeError;
use crate::value::{Channel, Value};
use crate::vm::GlobalSlotMeta;

use super::ToolboxModule;
use std::collections::{HashMap, VecDeque};
use std::sync::{Arc, Mutex};

fn new_channel(name: &str, capacity: usize) -> Channel {
    Channel {
        name: name.to_string(),
        buffer: Arc::new(Mutex::new(VecDeque::new())),
        capacity,
        is_closed: Arc::new(Mutex::new(false)),
    }
}

fn close_channel(chan: &Channel) {
    *chan.is_closed.lock().unwrap() = true;
}

fn send_to_channel(chan: &Channel, val: Value) {
    loop {
        if *chan.is_closed.lock().unwrap() {
            return;
        }

        {
            let mut buffer = chan.buffer.lock().unwrap();
            if buffer.len() < chan.capacity {
                buffer.push_back(val);
                return;
            }
        }

        std::thread::sleep(std::time::Duration::from_millis(1));
    }
}

pub fn pool_submit(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::ArityError(
            "pool.submit expects 1 argument (closure)".into(),
        ));
    }

    let closure = match args[0].clone() {
        Value::Closure(c) => c,
        _ => {
            return Err(RuntimeError::TypeError(
                "pool.submit expects a closure".into(),
            ));
        }
    };

    let chan = new_channel("pool_submit", 1);
    let chan_clone = chan.clone();

    let job: BlockingJob = Box::new(move || {
        let out = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            // Run the closure in a new fiber (same pattern used elsewhere).
            let mut fiber = crate::vm::Fiber::new(closure);
            let mut globals: Vec<Value> = Vec::new();
            let mut global_meta: Vec<GlobalSlotMeta> = Vec::new();
            // TODO: this runs with empty globals/custom_methods. This module is currently intended
            // as a proof that the bounded pool works end-to-end.
            let custom_methods = Arc::new(Mutex::new(std::collections::HashMap::new()));
            loop {
                let status = fiber.run_step(&mut globals, &mut global_meta, &custom_methods);
                match status {
                    crate::vm::FiberStatus::Finished => {
                        return fiber.return_value.unwrap_or(Value::Void);
                    }
                    crate::vm::FiberStatus::Running => continue,
                    _ => break,
                }
            }
            Value::Void
        }));

        match out {
            Ok(v) => send_to_channel(&chan_clone, v),
            Err(_) => send_to_channel(&chan_clone, Value::String("Thread panicked".to_string())),
        }

        close_channel(&chan_clone);
    });

    if global_blocking_pool().try_submit(job).is_err() {
        let err = crate::value::EnumData::into_value(
            "Result".to_string(),
            "Error".to_string(),
            Some(Box::new(Value::String(
                "Blocking pool queue is full".to_string(),
            ))),
        );
        send_to_channel(&chan, err);
        close_channel(&chan);
        return Ok(Value::Channel(chan));
    }

    Ok(Value::Channel(chan))
}

pub fn create_module() -> ToolboxModule {
    let mut functions = HashMap::new();
    functions.insert(
        "submit".to_string(),
        pool_submit as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );

    ToolboxModule { functions }
}
