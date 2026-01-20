//! LLM toolbox module (v1 fake backend)
//!
//! This is a placeholder implementation used to validate ngn ergonomics:
//! - a model handle backed by Value::External
//! - generate() returns Result<string, string>
//! - stream() returns channel<string>
//!
//! The real llama.cpp binding will replace the fake backend.

use crate::{
    blocking_pool::{BlockingJob, global_blocking_pool},
    error::RuntimeError,
    value::{Channel, ExternalValue, ObjectData, Value},
};

use super::ToolboxModule;
use std::{
    collections::{HashMap, VecDeque},
    sync::{Arc, Mutex},
    time::Duration,
};

#[derive(Debug)]
struct FakeLlmModel {
    path: String,
}

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

        std::thread::sleep(Duration::from_millis(1));
    }
}

fn make_llm_model(handle: ExternalValue) -> Value {
    let mut fields = std::collections::HashMap::new();
    fields.insert("_handle".to_string(), Value::External(handle));
    ObjectData::into_value("LlmModel".to_string(), fields)
}

fn ok_result(v: Value) -> Value {
    crate::value::EnumData::into_value("Result".to_string(), "Ok".to_string(), Some(Box::new(v)))
}

fn get_model_handle(
    model_obj: &crate::value::ObjectData,
) -> Result<Arc<FakeLlmModel>, RuntimeError> {
    match model_obj.fields.get("_handle") {
        Some(Value::External(e)) => {
            let any = e.inner.clone();
            match any.downcast::<FakeLlmModel>() {
                Ok(m) => Ok(m),
                Err(_) => Err(RuntimeError::TypeError(
                    "LLM model handle has wrong type".into(),
                )),
            }
        }
        _ => Err(RuntimeError::TypeError(
            "LLM model is missing internal handle".into(),
        )),
    }
}

pub fn llm_load(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.is_empty() || args.len() > 2 {
        return Err(RuntimeError::ArityError(
            "LLM.load expects 1 or 2 arguments (path, opts?)".into(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s.clone(),
        _ => {
            return Err(RuntimeError::TypeError(
                "LLM.load expects a string path".into(),
            ));
        }
    };

    // opts is ignored in fake backend
    let _ = args.get(1);

    let handle = ExternalValue {
        type_tag: "LlmModel".to_string(),
        inner: Arc::new(FakeLlmModel { path }),
    };

    Ok(make_llm_model(handle))
}

pub fn llm_generate(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(RuntimeError::ArityError(
            "LLM.generate expects 2 or 3 arguments (model, prompt, opts?)".into(),
        ));
    }

    let model_obj = match &args[0] {
        Value::Object(o) => &**o,
        _ => {
            return Err(RuntimeError::TypeError(
                "LLM.generate expects a model object".into(),
            ));
        }
    };

    if model_obj.model_name != "LlmModel" {
        return Err(RuntimeError::TypeError(
            "LLM.generate expects an LlmModel".into(),
        ));
    }

    let prompt = match &args[1] {
        Value::String(s) => s.clone(),
        _ => {
            return Err(RuntimeError::TypeError(
                "LLM.generate expects a string prompt".into(),
            ));
        }
    };

    // opts ignored for fake backend
    let _ = args.get(2);

    let model = get_model_handle(model_obj)?;

    // Return quickly with a deterministic fake response.
    let text = format!("[fake-llm:{}] {}", model.path, prompt.replace('\n', " "));

    Ok(ok_result(Value::String(text)))
}

pub fn llm_stream(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(RuntimeError::ArityError(
            "LLM.stream expects 2 or 3 arguments (model, prompt, opts?)".into(),
        ));
    }

    let model_obj = match &args[0] {
        Value::Object(o) => &**o,
        _ => {
            return Err(RuntimeError::TypeError(
                "LLM.stream expects a model object".into(),
            ));
        }
    };

    if model_obj.model_name != "LlmModel" {
        return Err(RuntimeError::TypeError(
            "LLM.stream expects an LlmModel".into(),
        ));
    }

    let prompt = match &args[1] {
        Value::String(s) => s.clone(),
        _ => {
            return Err(RuntimeError::TypeError(
                "LLM.stream expects a string prompt".into(),
            ));
        }
    };

    // opts ignored for fake backend
    let _ = args.get(2);

    let model = get_model_handle(model_obj)?;

    let out = new_channel("llm_stream", 256);
    let out_clone = out.clone();

    let job: BlockingJob = Box::new(move || {
        // Fake tokenization: stream words.
        let base = format!("[fake-llm:{}] ", model.path);
        for part in base.split(' ') {
            if *out_clone.is_closed.lock().unwrap() {
                break;
            }
            if !part.is_empty() {
                send_to_channel(&out_clone, Value::String(format!("{} ", part)));
            }
        }

        for word in prompt.split_whitespace() {
            if *out_clone.is_closed.lock().unwrap() {
                break;
            }
            send_to_channel(&out_clone, Value::String(format!("{} ", word)));
            std::thread::sleep(Duration::from_millis(20));
        }

        close_channel(&out_clone);
    });

    if global_blocking_pool().try_submit(job).is_err() {
        // Fail fast: close channel and return a channel that yields an error string as first chunk.
        // (Keeps stream() type stable as channel<string> for now.)
        send_to_channel(&out, Value::String("[llm busy]".to_string()));
        close_channel(&out);
        return Ok(Value::Channel(out));
    }

    Ok(Value::Channel(out))
}

pub fn create_module() -> ToolboxModule {
    let mut functions = HashMap::new();

    // Expose as LLM.load/generate/stream
    functions.insert(
        "load".to_string(),
        llm_load as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "generate".to_string(),
        llm_generate as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "stream".to_string(),
        llm_stream as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );

    ToolboxModule { functions }
}
