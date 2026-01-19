//! Process toolbox module: spawn and manage OS processes
//! Import with: import { run, stream } from "tbx::process"

use crate::{
    error::RuntimeError,
    value::{Channel, ExternalValue, ObjectData, Value},
};

use super::ToolboxModule;
use std::{
    collections::{HashMap, VecDeque},
    io::{BufRead, BufReader, Read},
    process::{Child, Command, Stdio},
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
    },
    time::{Duration, Instant},
};

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

fn parse_options(opts: Option<&crate::value::ObjectData>) -> (Option<String>, Option<u64>, bool) {
    let mut cwd: Option<String> = None;
    let mut timeout_ms: Option<u64> = None;
    let mut raw: bool = false;

    if let Some(o) = opts {
        if let Some(Value::String(s)) = o.fields.get("cwd") {
            cwd = Some(s.clone());
        }
        if let Some(Value::Numeric(n)) = o.fields.get("timeoutMs") {
            timeout_ms = match n {
                crate::value::Number::I64(v) if *v > 0 => Some(*v as u64),
                crate::value::Number::I32(v) if *v > 0 => Some(*v as u64),
                crate::value::Number::U64(v) if *v > 0 => Some(*v),
                _ => None,
            };
        }
        if let Some(Value::Bool(b)) = o.fields.get("raw") {
            raw = *b;
        }
    }

    (cwd, timeout_ms, raw)
}

fn force_raw_options(opts: Option<&crate::value::ObjectData>) -> crate::value::ObjectData {
    let mut fields: std::collections::HashMap<String, Value> =
        opts.map(|o| o.fields.clone()).unwrap_or_default();
    fields.insert("raw".to_string(), Value::Bool(true));
    crate::value::ObjectData {
        model_name: "__anon__".to_string(),
        fields,
    }
}

fn make_process_output(code: i64, stdout: String, stderr: String) -> Value {
    let mut fields = std::collections::HashMap::new();
    fields.insert(
        "code".to_string(),
        Value::Numeric(crate::value::Number::I64(code)),
    );
    fields.insert("stdout".to_string(), Value::String(stdout));
    fields.insert("stderr".to_string(), Value::String(stderr));

    ObjectData::into_value("ProcessOutput".to_string(), fields)
}

fn make_process_stream(
    stdout: Channel,
    stderr: Channel,
    done: Channel,
    handle: ExternalValue,
) -> Value {
    let mut fields = std::collections::HashMap::new();
    fields.insert("stdout".to_string(), Value::Channel(stdout));
    fields.insert("stderr".to_string(), Value::Channel(stderr));
    fields.insert("done".to_string(), Value::Channel(done));

    // Keep handle alive even if user drops all channels.
    fields.insert("_handle".to_string(), Value::External(handle));

    ObjectData::into_value("ProcessStream".to_string(), fields)
}

fn spawn_shell(cmd: &str, cwd: Option<String>) -> Result<Child, std::io::Error> {
    let mut c = Command::new("/bin/sh");
    c.arg("-c").arg(cmd);
    c.stdin(Stdio::null());
    c.stdout(Stdio::piped());
    c.stderr(Stdio::piped());
    if let Some(dir) = cwd {
        c.current_dir(dir);
    }
    c.spawn()
}

/// Run a shell command and return a channel that yields Result<ProcessOutput, string>
pub fn process_run(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.is_empty() || args.len() > 2 {
        return Err(RuntimeError::ArityError(
            "process.run expects 1 or 2 arguments (cmd, opts?)".into(),
        ));
    }

    let cmd = match &args[0] {
        Value::String(s) => s.clone(),
        _ => {
            return Err(RuntimeError::TypeError(
                "process.run expects a string command".into(),
            ));
        }
    };

    let opts_obj = match args.get(1) {
        Some(Value::Object(o)) => Some(&**o),
        Some(_) => {
            return Err(RuntimeError::TypeError(
                "process.run opts must be an object".into(),
            ));
        }
        None => None,
    };

    let (cwd, timeout_ms, _raw) = parse_options(opts_obj);

    let result_chan = new_channel("process_run", 1);
    let result_chan_clone = result_chan.clone();

    std::thread::spawn(move || {
        let result_value = (|| {
            let mut child = match spawn_shell(&cmd, cwd) {
                Ok(c) => c,
                Err(e) => {
                    return crate::value::EnumData::into_value(
                        "Result".to_string(),
                        "Error".to_string(),
                        Some(Box::new(Value::String(format!(
                            "Failed to spawn process: {}",
                            e
                        )))),
                    );
                }
            };

            let mut stdout = Vec::new();
            let mut stderr = Vec::new();

            let mut out_reader = child.stdout.take().unwrap();
            let mut err_reader = child.stderr.take().unwrap();

            let stdout_done = Arc::new(AtomicBool::new(false));
            let stderr_done = Arc::new(AtomicBool::new(false));

            let stdout_done2 = stdout_done.clone();
            let stderr_done2 = stderr_done.clone();

            let out_handle = std::thread::spawn(move || {
                let _ = out_reader.read_to_end(&mut stdout);
                stdout_done2.store(true, Ordering::Relaxed);
                stdout
            });

            let err_handle = std::thread::spawn(move || {
                let _ = err_reader.read_to_end(&mut stderr);
                stderr_done2.store(true, Ordering::Relaxed);
                stderr
            });

            let start = Instant::now();
            let code: i64;

            loop {
                match child.try_wait() {
                    Ok(Some(status)) => {
                        code = status.code().unwrap_or(-1) as i64;
                        break;
                    }
                    Ok(None) => {
                        if let Some(ms) = timeout_ms {
                            if start.elapsed() >= Duration::from_millis(ms) {
                                let _ = child.kill();
                                let _ = child.wait();
                                return crate::value::EnumData::into_value(
                                    "Result".to_string(),
                                    "Error".to_string(),
                                    Some(Box::new(Value::String("Process timed out".to_string()))),
                                );
                            }
                        }
                        std::thread::sleep(Duration::from_millis(5));
                    }
                    Err(e) => {
                        return crate::value::EnumData::into_value(
                            "Result".to_string(),
                            "Error".to_string(),
                            Some(Box::new(Value::String(format!(
                                "Process wait failed: {}",
                                e
                            )))),
                        );
                    }
                }
            }

            // Ensure output threads are done.
            while !stdout_done.load(Ordering::Relaxed) || !stderr_done.load(Ordering::Relaxed) {
                std::thread::sleep(Duration::from_millis(1));
            }

            let stdout_bytes = out_handle.join().unwrap_or_default();
            let stderr_bytes = err_handle.join().unwrap_or_default();

            let stdout_str = String::from_utf8_lossy(&stdout_bytes).to_string();
            let stderr_str = String::from_utf8_lossy(&stderr_bytes).to_string();

            let out = make_process_output(code, stdout_str, stderr_str);

            crate::value::EnumData::into_value(
                "Result".to_string(),
                "Ok".to_string(),
                Some(Box::new(out)),
            )
        })();

        send_to_channel(&result_chan_clone, result_value);
        close_channel(&result_chan_clone);
    });

    Ok(Value::Channel(result_chan))
}

/// Stream a shell command and return Result<ProcessStream, string>
pub fn process_stream(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.is_empty() || args.len() > 2 {
        return Err(RuntimeError::ArityError(
            "process.stream expects 1 or 2 arguments (cmd, opts?)".into(),
        ));
    }

    let cmd = match &args[0] {
        Value::String(s) => s.clone(),
        _ => {
            return Err(RuntimeError::TypeError(
                "process.stream expects a string command".into(),
            ));
        }
    };

    let opts_obj = match args.get(1) {
        Some(Value::Object(o)) => Some(&**o),
        Some(_) => {
            return Err(RuntimeError::TypeError(
                "process.stream opts must be an object".into(),
            ));
        }
        None => None,
    };

    let (cwd, timeout_ms, raw) = parse_options(opts_obj);

    let child = match spawn_shell(&cmd, cwd) {
        Ok(c) => c,
        Err(e) => {
            return Ok(crate::value::EnumData::into_value(
                "Result".to_string(),
                "Error".to_string(),
                Some(Box::new(Value::String(format!(
                    "Failed to spawn process: {}",
                    e
                )))),
            ));
        }
    };

    let stdout_chan = new_channel("process_stdout", 100);
    let stderr_chan = new_channel("process_stderr", 100);
    let done_chan = new_channel("process_done", 1);

    let stdout_chan_clone = stdout_chan.clone();
    let stderr_chan_clone = stderr_chan.clone();
    let done_chan_clone = done_chan.clone();

    let cancelled = Arc::new(AtomicBool::new(false));
    let cancelled_out = cancelled.clone();
    let cancelled_err = cancelled.clone();
    let cancelled_wait = cancelled.clone();

    let child_arc = Arc::new(Mutex::new(child));

    // Store handle so the child stays alive as long as ProcessStream is referenced.
    let handle = ExternalValue {
        type_tag: "ProcessHandle".to_string(),
        inner: child_arc.clone(),
    };

    // Stdout reader
    {
        let child_arc = child_arc.clone();
        std::thread::spawn(move || {
            let stdout_opt = { child_arc.lock().unwrap().stdout.take() };
            let Some(stdout) = stdout_opt else {
                close_channel(&stdout_chan_clone);
                return;
            };

            if raw {
                let mut r = stdout;
                let mut buf = vec![0u8; 8192];
                loop {
                    if cancelled_out.load(Ordering::Relaxed) {
                        break;
                    }
                    if *stdout_chan_clone.is_closed.lock().unwrap() {
                        break;
                    }
                    match r.read(&mut buf) {
                        Ok(0) => break,
                        Ok(n) => {
                            send_to_channel(
                                &stdout_chan_clone,
                                Value::Bytes(Arc::new(buf[..n].to_vec())),
                            );
                        }
                        Err(_) => break,
                    }
                }
            } else {
                let reader = BufReader::new(stdout);
                for line in reader.lines() {
                    if cancelled_out.load(Ordering::Relaxed) {
                        break;
                    }
                    if *stdout_chan_clone.is_closed.lock().unwrap() {
                        break;
                    }
                    match line {
                        Ok(s) => send_to_channel(&stdout_chan_clone, Value::String(s)),
                        Err(_) => break,
                    }
                }
            }

            close_channel(&stdout_chan_clone);
        });
    }

    // Stderr reader
    {
        let child_arc = child_arc.clone();
        std::thread::spawn(move || {
            let stderr_opt = { child_arc.lock().unwrap().stderr.take() };
            let Some(stderr) = stderr_opt else {
                close_channel(&stderr_chan_clone);
                return;
            };

            if raw {
                let mut r = stderr;
                let mut buf = vec![0u8; 8192];
                loop {
                    if cancelled_err.load(Ordering::Relaxed) {
                        break;
                    }
                    if *stderr_chan_clone.is_closed.lock().unwrap() {
                        break;
                    }
                    match r.read(&mut buf) {
                        Ok(0) => break,
                        Ok(n) => {
                            send_to_channel(
                                &stderr_chan_clone,
                                Value::Bytes(Arc::new(buf[..n].to_vec())),
                            );
                        }
                        Err(_) => break,
                    }
                }
            } else {
                let reader = BufReader::new(stderr);
                for line in reader.lines() {
                    if cancelled_err.load(Ordering::Relaxed) {
                        break;
                    }
                    if *stderr_chan_clone.is_closed.lock().unwrap() {
                        break;
                    }
                    match line {
                        Ok(s) => send_to_channel(&stderr_chan_clone, Value::String(s)),
                        Err(_) => break,
                    }
                }
            }

            close_channel(&stderr_chan_clone);
        });
    }

    // Waiter + timeout
    {
        let child_arc = child_arc.clone();
        std::thread::spawn(move || {
            let start = Instant::now();
            let mut timed_out = false;

            loop {
                if cancelled_wait.load(Ordering::Relaxed) {
                    break;
                }

                // If both output channels are closed, continue waiting.
                // Closing the read channels is a common pattern (e.g. after reading enough output)
                // and should not automatically cancel the underlying process.

                let status = { child_arc.lock().unwrap().try_wait() };
                match status {
                    Ok(Some(status)) => {
                        let code = status.code().unwrap_or(-1) as i64;
                        let out = make_process_output(code, String::new(), String::new());
                        send_to_channel(
                            &done_chan_clone,
                            crate::value::EnumData::into_value(
                                "Result".to_string(),
                                "Ok".to_string(),
                                Some(Box::new(out)),
                            ),
                        );
                        close_channel(&done_chan_clone);
                        return;
                    }
                    Ok(None) => {
                        if let Some(ms) = timeout_ms {
                            if start.elapsed() >= Duration::from_millis(ms) {
                                timed_out = true;
                                break;
                            }
                        }
                        std::thread::sleep(Duration::from_millis(5));
                    }
                    Err(e) => {
                        send_to_channel(
                            &done_chan_clone,
                            crate::value::EnumData::into_value(
                                "Result".to_string(),
                                "Error".to_string(),
                                Some(Box::new(Value::String(format!(
                                    "Process wait failed: {}",
                                    e
                                )))),
                            ),
                        );
                        close_channel(&done_chan_clone);
                        return;
                    }
                }
            }

            // Cancel/timeout path
            cancelled_wait.store(true, Ordering::Relaxed);
            let _ = { child_arc.lock().unwrap().kill() };
            let _ = { child_arc.lock().unwrap().wait() };

            if timed_out {
                send_to_channel(
                    &done_chan_clone,
                    crate::value::EnumData::into_value(
                        "Result".to_string(),
                        "Error".to_string(),
                        Some(Box::new(Value::String("Process timed out".to_string()))),
                    ),
                );
            } else {
                send_to_channel(
                    &done_chan_clone,
                    crate::value::EnumData::into_value(
                        "Result".to_string(),
                        "Error".to_string(),
                        Some(Box::new(Value::String("Process cancelled".to_string()))),
                    ),
                );
            }

            close_channel(&done_chan_clone);
        });
    }

    Ok(crate::value::EnumData::into_value(
        "Result".to_string(),
        "Ok".to_string(),
        Some(Box::new(make_process_stream(
            stdout_chan,
            stderr_chan,
            done_chan,
            handle,
        ))),
    ))
}

pub fn process_stream_raw(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.is_empty() || args.len() > 2 {
        return Err(RuntimeError::ArityError(
            "process.streamRaw expects 1 or 2 arguments (cmd, opts?)".into(),
        ));
    }

    let cmd = args[0].clone();
    let opts_obj = match args.get(1) {
        Some(Value::Object(o)) => Some(&**o),
        Some(_) => {
            return Err(RuntimeError::TypeError(
                "process.streamRaw opts must be an object".into(),
            ));
        }
        None => None,
    };

    let forced = force_raw_options(opts_obj);
    process_stream(vec![cmd, Value::Object(Box::new(forced))])
}

pub fn create_module() -> ToolboxModule {
    let mut functions = HashMap::new();
    functions.insert(
        "run".to_string(),
        process_run as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "stream".to_string(),
        process_stream as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );
    functions.insert(
        "streamRaw".to_string(),
        process_stream_raw as fn(Vec<Value>) -> Result<Value, RuntimeError>,
    );

    ToolboxModule { functions }
}
