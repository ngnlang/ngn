//! LLM toolbox module (llama.cpp CPU backend)
//!
//! API:
//! - `load(path, opts?) -> Result<LlmModel, string>`
//! - `generate(model, prompt, opts?) -> Result<string, string>`
//! - `stream(model, prompt, opts?) -> channel<string>`
//!
//! Notes:
//! - `stream()` runs generation on the bounded blocking pool.
//! - Closing the returned channel cancels generation (between tokens).

use crate::{
    blocking_pool::{BlockingJob, global_blocking_pool},
    error::RuntimeError,
    value::{Channel, ExternalValue, ObjectData, Value},
};

use super::ToolboxModule;
use std::{
    collections::{HashMap, VecDeque},
    ffi::{CStr, CString},
    os::raw::{c_char, c_void},
    sync::{Arc, Mutex},
    time::Duration,
};

#[repr(C)]
#[derive(Copy, Clone)]
struct NgnLlamaLoadOpts {
    n_ctx: i32,
    n_threads: i32,
    use_mmap: bool,
}

#[repr(C)]
#[derive(Copy, Clone)]
struct NgnLlamaGenOpts {
    max_tokens: i32,
    temperature: f32,
    top_p: f32,
    top_k: i32,
    seed: u32,
}

#[allow(non_camel_case_types)]
type ngn_llama_handle = c_void;

type ChunkCb = extern "C" fn(data: *const c_char, len: i32, user_data: *mut c_void) -> bool;

unsafe extern "C" {
    fn ngn_llama_backend_init();

    fn ngn_llama_load(
        path: *const c_char,
        opts: NgnLlamaLoadOpts,
        err_buf: *mut c_char,
        err_len: i32,
    ) -> *mut ngn_llama_handle;

    fn ngn_llama_free(h: *mut ngn_llama_handle);

    fn ngn_llama_generate(
        h: *mut ngn_llama_handle,
        prompt: *const c_char,
        opts: NgnLlamaGenOpts,
        cb: ChunkCb,
        user_data: *mut c_void,
        err_buf: *mut c_char,
        err_len: i32,
    ) -> i32;
}

#[derive(Debug)]
struct LlmHandle {
    raw: *mut ngn_llama_handle,
}

unsafe impl Send for LlmHandle {}
unsafe impl Sync for LlmHandle {}

impl Drop for LlmHandle {
    fn drop(&mut self) {
        unsafe {
            if !self.raw.is_null() {
                ngn_llama_free(self.raw);
                self.raw = std::ptr::null_mut();
            }
        }
    }
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

fn ok_result(v: Value) -> Value {
    crate::value::EnumData::into_value("Result".to_string(), "Ok".to_string(), Some(Box::new(v)))
}

fn err_result(msg: String) -> Value {
    crate::value::EnumData::into_value(
        "Result".to_string(),
        "Error".to_string(),
        Some(Box::new(Value::String(msg))),
    )
}

fn make_llm_model(handle: ExternalValue) -> Value {
    let mut fields = HashMap::new();
    fields.insert("_handle".to_string(), Value::External(handle));
    ObjectData::into_value("LlmModel".to_string(), fields)
}

fn get_model_handle(
    model_obj: &crate::value::ObjectData,
) -> Result<Arc<Mutex<LlmHandle>>, RuntimeError> {
    match model_obj.fields.get("_handle") {
        Some(Value::External(e)) => {
            let any = e.inner.clone();
            match any.downcast::<Mutex<LlmHandle>>() {
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

#[derive(Debug, Clone)]
struct GenOpts {
    max_tokens: i32,
    temperature: f32,
    top_p: f32,
    top_k: i32,
    seed: u32,
}

fn parse_gen_opts(opts: Option<&crate::value::ObjectData>) -> GenOpts {
    let mut out = GenOpts {
        max_tokens: 128,
        temperature: 0.8,
        top_p: 0.95,
        top_k: 40,
        seed: 0,
    };

    let Some(o) = opts else {
        return out;
    };

    if let Some(Value::Numeric(n)) = o.fields.get("max_tokens") {
        let v = match n {
            crate::value::Number::I64(v) => *v,
            crate::value::Number::I32(v) => *v as i64,
            crate::value::Number::U64(v) => *v as i64,
            _ => -1,
        };
        if v > 0 {
            out.max_tokens = v as i32;
        }
    }

    if let Some(Value::Numeric(n)) = o.fields.get("temperature") {
        out.temperature = match n {
            crate::value::Number::F64(v) => *v as f32,
            crate::value::Number::F32(v) => *v,
            crate::value::Number::I64(v) => *v as f32,
            crate::value::Number::I32(v) => *v as f32,
            crate::value::Number::U64(v) => *v as f32,
            _ => out.temperature,
        };
    }

    if let Some(Value::Numeric(n)) = o.fields.get("top_p") {
        out.top_p = match n {
            crate::value::Number::F64(v) => *v as f32,
            crate::value::Number::F32(v) => *v,
            crate::value::Number::I64(v) => *v as f32,
            crate::value::Number::I32(v) => *v as f32,
            crate::value::Number::U64(v) => *v as f32,
            _ => out.top_p,
        };
    }

    if let Some(Value::Numeric(n)) = o.fields.get("top_k") {
        out.top_k = match n {
            crate::value::Number::I64(v) => *v as i32,
            crate::value::Number::I32(v) => *v,
            crate::value::Number::U64(v) => *v as i32,
            _ => out.top_k,
        };
    }

    if let Some(Value::Numeric(n)) = o.fields.get("seed") {
        out.seed = match n {
            crate::value::Number::I64(v) => *v as u32,
            crate::value::Number::I32(v) => *v as u32,
            crate::value::Number::U64(v) => *v as u32,
            _ => out.seed,
        };
    }

    out
}

fn parse_load_opts(opts: Option<&crate::value::ObjectData>) -> NgnLlamaLoadOpts {
    let mut out = NgnLlamaLoadOpts {
        n_ctx: 0,
        n_threads: 0,
        use_mmap: true,
    };

    let Some(o) = opts else {
        return out;
    };

    if let Some(Value::Numeric(n)) = o.fields.get("context") {
        out.n_ctx = match n {
            crate::value::Number::I64(v) if *v > 0 => *v as i32,
            crate::value::Number::I32(v) if *v > 0 => *v,
            crate::value::Number::U64(v) if *v > 0 => *v as i32,
            _ => 0,
        };
    }

    if let Some(Value::Numeric(n)) = o.fields.get("threads") {
        out.n_threads = match n {
            crate::value::Number::I64(v) => *v as i32,
            crate::value::Number::I32(v) => *v,
            crate::value::Number::U64(v) => *v as i32,
            _ => 0,
        };
    }

    if let Some(Value::Bool(b)) = o.fields.get("mmap") {
        out.use_mmap = *b;
    }

    out
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

    let opts_obj = match args.get(1) {
        Some(Value::Object(o)) => Some(&**o),
        Some(_) => {
            return Err(RuntimeError::TypeError(
                "LLM.load opts must be an object".into(),
            ));
        }
        None => None,
    };

    let load_opts = parse_load_opts(opts_obj);

    // Avoid calling into llama.cpp when the file is missing.
    // This keeps error output clean and matches typical user expectations.
    if !std::path::Path::new(&path).exists() {
        return Ok(err_result(format!("Model file not found: {}", path)));
    }

    let path_c = match CString::new(path.clone()) {
        Ok(s) => s,
        Err(_) => {
            return Ok(err_result("LLM.load path contains null byte".to_string()));
        }
    };

    unsafe {
        ngn_llama_backend_init();

        let mut err_buf = vec![0u8; 512];
        let raw = ngn_llama_load(
            path_c.as_ptr(),
            load_opts,
            err_buf.as_mut_ptr() as *mut c_char,
            err_buf.len() as i32,
        );
        if raw.is_null() {
            let msg = CStr::from_ptr(err_buf.as_ptr() as *const c_char)
                .to_string_lossy()
                .to_string();
            let msg = if msg.is_empty() {
                format!("Failed to load model from '{}'", path)
            } else {
                msg
            };
            return Ok(err_result(msg));
        }

        let handle = ExternalValue {
            type_tag: "LlmModel".to_string(),
            inner: Arc::new(Mutex::new(LlmHandle { raw })),
        };

        Ok(ok_result(make_llm_model(handle)))
    }
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

    let opts_obj = match args.get(2) {
        Some(Value::Object(o)) => Some(&**o),
        Some(_) => {
            return Err(RuntimeError::TypeError(
                "LLM.generate opts must be an object".into(),
            ));
        }
        None => None,
    };
    let opts = parse_gen_opts(opts_obj);

    let model = get_model_handle(model_obj)?;
    let guard = model.lock().unwrap();

    let prompt_c = match CString::new(prompt) {
        Ok(s) => s,
        Err(_) => return Ok(err_result("Prompt contains null byte".to_string())),
    };

    struct CollectCtx {
        out: String,
    }

    extern "C" fn collect_cb(data: *const c_char, len: i32, user: *mut c_void) -> bool {
        if user.is_null() {
            return false;
        }
        if data.is_null() || len <= 0 {
            return true;
        }
        let ctx = unsafe { &mut *(user as *mut CollectCtx) };
        let bytes = unsafe { std::slice::from_raw_parts(data as *const u8, len as usize) };
        ctx.out.push_str(&String::from_utf8_lossy(bytes));
        true
    }

    let mut ctx = CollectCtx { out: String::new() };

    let gen_opts = NgnLlamaGenOpts {
        max_tokens: opts.max_tokens,
        temperature: opts.temperature,
        top_p: opts.top_p,
        top_k: opts.top_k,
        seed: opts.seed,
    };

    unsafe {
        let mut err_buf = vec![0u8; 512];
        let rc = ngn_llama_generate(
            guard.raw,
            prompt_c.as_ptr(),
            gen_opts,
            collect_cb,
            (&mut ctx as *mut CollectCtx) as *mut c_void,
            err_buf.as_mut_ptr() as *mut c_char,
            err_buf.len() as i32,
        );

        match rc {
            0 => Ok(ok_result(Value::String(ctx.out))),
            1 => Ok(err_result("Generation cancelled".to_string())),
            _ => {
                let msg = CStr::from_ptr(err_buf.as_ptr() as *const c_char)
                    .to_string_lossy()
                    .to_string();
                let msg = if msg.is_empty() {
                    "Generation failed".to_string()
                } else {
                    msg
                };
                Ok(err_result(msg))
            }
        }
    }
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

    let opts_obj = match args.get(2) {
        Some(Value::Object(o)) => Some(&**o),
        Some(_) => {
            return Err(RuntimeError::TypeError(
                "LLM.stream opts must be an object".into(),
            ));
        }
        None => None,
    };
    let opts = parse_gen_opts(opts_obj);

    let model = get_model_handle(model_obj)?;

    let out = new_channel("llm_stream", 256);
    let out_clone = out.clone();

    let job: BlockingJob = Box::new(move || {
        let prompt_c = match CString::new(prompt) {
            Ok(s) => s,
            Err(_) => {
                send_to_channel(
                    &out_clone,
                    Value::String("[llm error: prompt contains null byte]".to_string()),
                );
                close_channel(&out_clone);
                return;
            }
        };

        let guard = match model.lock() {
            Ok(g) => g,
            Err(_) => {
                send_to_channel(
                    &out_clone,
                    Value::String("[llm error: model lock poisoned]".to_string()),
                );
                close_channel(&out_clone);
                return;
            }
        };

        struct StreamCtx {
            ch: Channel,
        }

        extern "C" fn stream_cb(data: *const c_char, len: i32, user: *mut c_void) -> bool {
            if user.is_null() {
                return false;
            }
            let ctx = unsafe { &mut *(user as *mut StreamCtx) };

            if *ctx.ch.is_closed.lock().unwrap() {
                return false;
            }

            if data.is_null() || len <= 0 {
                return true;
            }
            let bytes = unsafe { std::slice::from_raw_parts(data as *const u8, len as usize) };
            let s = String::from_utf8_lossy(bytes).to_string();
            send_to_channel(&ctx.ch, Value::String(s));
            true
        }

        let gen_opts = NgnLlamaGenOpts {
            max_tokens: opts.max_tokens,
            temperature: opts.temperature,
            top_p: opts.top_p,
            top_k: opts.top_k,
            seed: opts.seed,
        };

        let mut stream_ctx = StreamCtx {
            ch: out_clone.clone(),
        };
        unsafe {
            let mut err_buf = vec![0u8; 512];
            let rc = ngn_llama_generate(
                guard.raw,
                prompt_c.as_ptr(),
                gen_opts,
                stream_cb,
                (&mut stream_ctx as *mut StreamCtx) as *mut c_void,
                err_buf.as_mut_ptr() as *mut c_char,
                err_buf.len() as i32,
            );
            if rc < 0 {
                let msg = CStr::from_ptr(err_buf.as_ptr() as *const c_char)
                    .to_string_lossy()
                    .to_string();
                let msg = if msg.is_empty() {
                    "[llm error]".to_string()
                } else {
                    format!("[llm error: {}]", msg)
                };
                send_to_channel(&out_clone, Value::String(msg));
            }
        }

        close_channel(&out_clone);
    });

    if global_blocking_pool().try_submit(job).is_err() {
        send_to_channel(&out, Value::String("[llm busy]".to_string()));
        close_channel(&out);
        return Ok(Value::Channel(out));
    }

    Ok(Value::Channel(out))
}

pub fn create_module() -> ToolboxModule {
    let mut functions = HashMap::new();

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn llama_bridge_symbols_link() {
        unsafe { ngn_llama_backend_init() };
    }
}
