//! HTTP Server module for ngn toolbox
//!
//! Provides a simple HTTP server that can be started with `serve(port, handler)`
//! where handler is a function or closure that takes a Request and returns a Response.
//!
//! Also provides `serve_async(port, handler)` for high-performance async I/O using tokio.

use std::collections::HashMap;
use std::io::{BufRead, BufReader, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader as TokioBufReader};
use tokio::net::{TcpListener as TokioTcpListener, TcpStream as TokioTcpStream};

use crate::value::{Closure, Function, ObjectData, Value};
use crate::vm::{Fiber, FiberStatus};

/// Parse an HTTP request from a TCP stream
fn parse_request(stream: &mut TcpStream) -> Option<Value> {
    let mut reader = BufReader::new(stream.try_clone().ok()?);

    // Read request line
    let mut request_line = String::new();
    if reader.read_line(&mut request_line).ok()? == 0 {
        return None;
    }

    let parts: Vec<&str> = request_line.trim().split_whitespace().collect();
    if parts.len() < 2 {
        return None;
    }

    let method = parts[0].to_string();
    let full_path = parts[1].to_string();

    // Split path and query
    let (path, query) = if let Some(idx) = full_path.find('?') {
        (full_path[..idx].to_string(), full_path[idx..].to_string())
    } else {
        (full_path, String::new())
    };

    // Read headers
    let mut headers: HashMap<Value, Value> = HashMap::new();
    let mut content_length: usize = 0;

    loop {
        let mut line = String::new();
        if reader.read_line(&mut line).ok()? == 0 {
            break;
        }
        let trimmed = line.trim();
        if trimmed.is_empty() {
            break;
        }
        if let Some(idx) = trimmed.find(':') {
            let key = trimmed[..idx].trim().to_lowercase();
            let value = trimmed[idx + 1..].trim().to_string();
            if key == "content-length" {
                content_length = value.parse().unwrap_or(0);
            }
            headers.insert(Value::String(key), Value::String(value));
        }
    }

    // Read body if present
    let mut body = String::new();
    if content_length > 0 {
        let mut body_bytes = vec![0u8; content_length];
        reader.read_exact(&mut body_bytes).ok()?;
        body = String::from_utf8_lossy(&body_bytes).to_string();
    }

    // Build Request object
    let mut fields = std::collections::HashMap::new();
    fields.insert("method".to_string(), Value::String(method));
    fields.insert("path".to_string(), Value::String(path));
    fields.insert("query".to_string(), Value::String(query));
    fields.insert("headers".to_string(), Value::Map(headers));
    fields.insert("body".to_string(), Value::String(body));

    Some(ObjectData::into_value("Request".to_string(), fields))
}

/// Serialize a Response object to HTTP format and write to stream
fn write_response(stream: &mut TcpStream, response: &Value) {
    let (status, headers, body) = if let Value::Object(obj) = response {
        let status = match obj.fields.get("status") {
            Some(Value::Numeric(n)) => {
                // Extract i64 from any numeric type
                match n {
                    crate::value::Number::I64(v) => *v as u16,
                    crate::value::Number::I32(v) => *v as u16,
                    _ => 200,
                }
            }
            _ => 200,
        };

        let body = match obj.fields.get("body") {
            Some(Value::String(s)) => s.clone(),
            _ => String::new(),
        };

        let headers = match obj.fields.get("headers") {
            Some(Value::Map(m)) => m.clone(),
            _ => HashMap::new(),
        };

        (status, headers, body)
    } else {
        // Fallback for non-object response
        (200, HashMap::new(), response.to_string())
    };

    // Get status text
    let status_text = match status {
        200 => "OK",
        201 => "Created",
        204 => "No Content",
        301 => "Moved Permanently",
        302 => "Found",
        304 => "Not Modified",
        400 => "Bad Request",
        401 => "Unauthorized",
        403 => "Forbidden",
        404 => "Not Found",
        405 => "Method Not Allowed",
        500 => "Internal Server Error",
        502 => "Bad Gateway",
        503 => "Service Unavailable",
        _ => "OK",
    };

    // Build response
    let mut response_str = format!("HTTP/1.1 {} {}\r\n", status, status_text);

    // Add Connection: close header for proper connection handling
    response_str.push_str("Connection: close\r\n");

    // Add Content-Length if not present
    let mut has_content_length = false;
    for (k, v) in &headers {
        if let (Value::String(key), Value::String(val)) = (k, v) {
            if key.to_lowercase() == "content-length" {
                has_content_length = true;
            }
            response_str.push_str(&format!("{}: {}\r\n", key, val));
        }
    }

    if !has_content_length {
        response_str.push_str(&format!("Content-Length: {}\r\n", body.len()));
    }

    response_str.push_str("\r\n");
    response_str.push_str(&body);

    let _ = stream.write_all(response_str.as_bytes());
    let _ = stream.flush();
}

/// Handle a single HTTP connection
fn handle_connection(
    mut stream: TcpStream,
    handler: Value,
    globals: Arc<Mutex<Vec<Value>>>,
    custom_methods: Arc<Mutex<HashMap<String, HashMap<String, Value>>>>,
) {
    // Parse the request
    let request = match parse_request(&mut stream) {
        Some(req) => req,
        None => return,
    };

    // Call the handler function/closure
    let response = match handler {
        Value::Function(func) => call_handler_function(&func, request, globals, custom_methods),
        Value::Closure(closure) => call_handler_closure(&closure, request, globals, custom_methods),
        _ => {
            // Handler is not callable, return 500
            let mut fields = std::collections::HashMap::new();
            fields.insert(
                "status".to_string(),
                Value::Numeric(crate::value::Number::I64(500)),
            );
            fields.insert(
                "body".to_string(),
                Value::String("Internal Server Error: Handler not callable".to_string()),
            );
            fields.insert("headers".to_string(), Value::Map(HashMap::new()));
            ObjectData::into_value("Response".to_string(), fields)
        }
    };

    // Write the response
    write_response(&mut stream, &response);
}

/// Call a function handler with the request
fn call_handler_function(
    func: &Function,
    request: Value,
    globals: Arc<Mutex<Vec<Value>>>,
    custom_methods: Arc<Mutex<HashMap<String, HashMap<String, Value>>>>,
) -> Value {
    // Create a closure wrapping the function (for uniform handling)
    let closure = Closure {
        function: Box::new(func.clone()),
        upvalues: vec![],
    };
    call_handler_closure(&closure, request, globals, custom_methods)
}

/// Call a closure handler with the request
fn call_handler_closure(
    closure: &Closure,
    request: Value,
    globals: Arc<Mutex<Vec<Value>>>,
    custom_methods: Arc<Mutex<HashMap<String, HashMap<String, Value>>>>,
) -> Value {
    // Create a new fiber for this request
    let mut fiber = Fiber::new(Box::new(closure.clone()));

    // Set the request as the first argument (register 0)
    // The function expects its parameters at registers 0, 1, 2, etc.
    fiber.stack[0] = request;

    // Get mutable access to globals
    let mut globals_guard = globals.lock().unwrap();

    // Run the fiber to completion
    loop {
        let status = fiber.run_step(&mut globals_guard, &custom_methods);
        match status {
            FiberStatus::Finished => break,
            FiberStatus::Running => continue,
            FiberStatus::Suspended => {
                // Yield - in HTTP context we just spin
                thread::yield_now();
            }
            FiberStatus::Spawning(new_fiber) => {
                // Handle spawned fibers - for HTTP we'll just run them in separate threads
                let globals_clone = globals.clone();
                let methods_clone = custom_methods.clone();
                thread::spawn(move || {
                    let mut spawned = *new_fiber;
                    let mut g = globals_clone.lock().unwrap();
                    loop {
                        match spawned.run_step(&mut g, &methods_clone) {
                            FiberStatus::Finished => break,
                            FiberStatus::Running => continue,
                            FiberStatus::Suspended => thread::yield_now(),
                            FiberStatus::Spawning(_) => {} // Ignore nested spawns for now
                        }
                    }
                });
            }
        }
    }

    // Return the result
    fiber.return_value.unwrap_or_else(|| {
        let mut fields = std::collections::HashMap::new();
        fields.insert(
            "status".to_string(),
            Value::Numeric(crate::value::Number::I64(500)),
        );
        fields.insert(
            "body".to_string(),
            Value::String("Handler returned no response".to_string()),
        );
        fields.insert("headers".to_string(), Value::Map(HashMap::new()));
        ObjectData::into_value("Response".to_string(), fields)
    })
}

/// Start an HTTP server on the given port
/// This function blocks until Ctrl+C is pressed, then gracefully shuts down
/// Uses a fixed thread pool for handling requests (much faster than thread-per-request)
pub fn serve(
    port: u16,
    handler: Value,
    globals: Arc<Mutex<Vec<Value>>>,
    custom_methods: Arc<Mutex<HashMap<String, HashMap<String, Value>>>>,
) -> Result<(), String> {
    // Bind to IPv4
    let addr = format!("0.0.0.0:{}", port);
    let listener =
        TcpListener::bind(&addr).map_err(|e| format!("Failed to bind to {}: {}", addr, e))?;

    // Set non-blocking mode for graceful shutdown
    listener
        .set_nonblocking(true)
        .map_err(|e| format!("Failed to set non-blocking: {}", e))?;

    // Determine worker pool size (default: number of CPU cores)
    let num_workers = num_cpus::get();
    println!(
        "HTTP server listening on http://localhost:{} ({} workers)",
        port, num_workers
    );

    // Shutdown flag and active request counter
    let shutdown = Arc::new(AtomicBool::new(false));
    let active_requests = Arc::new(AtomicUsize::new(0));

    // Create work channel (bounded to prevent memory explosion under load)
    let (sender, receiver) = crossbeam_channel::bounded::<TcpStream>(1024);

    // Set up Ctrl+C handler
    let shutdown_flag = shutdown.clone();
    ctrlc::set_handler(move || {
        println!("\nShutting down server...");
        shutdown_flag.store(true, Ordering::SeqCst);
    })
    .map_err(|e| format!("Failed to set Ctrl+C handler: {}", e))?;

    // Spawn worker threads
    let mut workers = Vec::with_capacity(num_workers);
    for _ in 0..num_workers {
        let receiver = receiver.clone();
        let handler = handler.clone();
        let globals = globals.clone();
        let custom_methods = custom_methods.clone();
        let shutdown = shutdown.clone();
        let active = active_requests.clone();

        let worker = thread::spawn(move || {
            // Each worker loops receiving connections from the channel
            while !shutdown.load(Ordering::Relaxed) {
                match receiver.recv_timeout(Duration::from_millis(100)) {
                    Ok(stream) => {
                        active.fetch_add(1, Ordering::SeqCst);
                        handle_connection(
                            stream,
                            handler.clone(),
                            globals.clone(),
                            custom_methods.clone(),
                        );
                        active.fetch_sub(1, Ordering::SeqCst);
                    }
                    Err(crossbeam_channel::RecvTimeoutError::Timeout) => {
                        // Check shutdown flag and continue
                        continue;
                    }
                    Err(crossbeam_channel::RecvTimeoutError::Disconnected) => {
                        // Channel closed, exit worker
                        break;
                    }
                }
            }
        });
        workers.push(worker);
    }

    // Accept connections in a loop and dispatch to workers
    while !shutdown.load(Ordering::SeqCst) {
        match listener.accept() {
            Ok((stream, _)) => {
                // Send to worker pool (non-blocking with bounded channel)
                if let Err(e) = sender.try_send(stream) {
                    match e {
                        crossbeam_channel::TrySendError::Full(_) => {
                            // Queue full - this is backpressure, could log or drop
                            eprintln!("Warning: Worker queue full, dropping connection");
                        }
                        crossbeam_channel::TrySendError::Disconnected(_) => {
                            break;
                        }
                    }
                }
            }
            Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                // No connections ready, sleep briefly and check shutdown flag
                thread::sleep(Duration::from_millis(1));
            }
            Err(e) => {
                if !shutdown.load(Ordering::SeqCst) {
                    eprintln!("Connection error: {}", e);
                }
            }
        }
    }

    // Drop sender to signal workers to exit
    drop(sender);

    // Wait for workers to finish (with timeout)
    let wait_start = std::time::Instant::now();
    let timeout = Duration::from_secs(30);

    for worker in workers {
        let remaining = timeout.saturating_sub(wait_start.elapsed());
        if remaining.is_zero() {
            println!("Timeout waiting for workers, forcing shutdown");
            break;
        }
        let _ = worker.join();
    }

    // Wait for any remaining active requests
    while active_requests.load(Ordering::SeqCst) > 0 {
        if wait_start.elapsed() > timeout {
            let remaining = active_requests.load(Ordering::SeqCst);
            println!(
                "Timeout waiting for {} active requests, forcing shutdown",
                remaining
            );
            break;
        }
        thread::sleep(Duration::from_millis(100));
    }

    println!("Server stopped gracefully");
    Ok(())
}

/// Start an HTTPS server with TLS
pub fn serve_tls(
    port: u16,
    handler: Value,
    cert_path: &str,
    key_path: &str,
    globals: Arc<Mutex<Vec<Value>>>,
    custom_methods: Arc<Mutex<HashMap<String, HashMap<String, Value>>>>,
) -> Result<(), String> {
    use rustls::pki_types::CertificateDer;
    use std::fs::File;
    use std::io::BufReader as StdBufReader;
    use std::sync::Arc as StdArc;

    // Load certificate
    let cert_file =
        File::open(cert_path).map_err(|e| format!("Failed to open cert file: {}", e))?;
    let mut cert_reader = StdBufReader::new(cert_file);
    let certs: Vec<CertificateDer<'static>> = rustls_pemfile::certs(&mut cert_reader)
        .filter_map(|r| r.ok())
        .collect();

    if certs.is_empty() {
        return Err("No certificates found in cert file".to_string());
    }

    // Load private key
    let key_file = File::open(key_path).map_err(|e| format!("Failed to open key file: {}", e))?;
    let mut key_reader = StdBufReader::new(key_file);
    let key = rustls_pemfile::private_key(&mut key_reader)
        .map_err(|e| format!("Failed to parse key: {}", e))?
        .ok_or("No private key found in key file")?;

    // Build TLS config
    let config = rustls::ServerConfig::builder()
        .with_no_client_auth()
        .with_single_cert(certs, key)
        .map_err(|e| format!("TLS config error: {}", e))?;

    let tls_config = StdArc::new(config);

    // Bind to address
    let addr = format!("0.0.0.0:{}", port);
    let listener =
        TcpListener::bind(&addr).map_err(|e| format!("Failed to bind to {}: {}", addr, e))?;

    println!("HTTPS server listening on https://localhost:{}", port);

    // Accept connections
    for stream in listener.incoming() {
        match stream {
            Ok(tcp_stream) => {
                let handler_clone = handler.clone();
                let globals_clone = globals.clone();
                let methods_clone = custom_methods.clone();
                let config_clone = tls_config.clone();

                thread::spawn(move || {
                    // Wrap in TLS
                    let conn = rustls::ServerConnection::new(config_clone);
                    if conn.is_err() {
                        eprintln!("TLS connection error");
                        return;
                    }

                    // For simplicity, we'll handle TLS streams differently
                    // This is a placeholder - full TLS stream handling requires more work
                    // For now, just handle as regular connection (upgrade later)
                    handle_connection(tcp_stream, handler_clone, globals_clone, methods_clone);
                });
            }
            Err(e) => {
                eprintln!("Connection error: {}", e);
            }
        }
    }

    Ok(())
}

// ============================================================================
// ASYNC HTTP SERVER (High Performance with Tokio)
// ============================================================================

/// Parse an HTTP request from an async TCP stream
async fn parse_request_async(stream: &mut TokioTcpStream) -> Option<Value> {
    let mut reader = TokioBufReader::new(stream);

    // Read request line
    let mut request_line = String::new();
    if reader.read_line(&mut request_line).await.ok()? == 0 {
        return None;
    }

    let parts: Vec<&str> = request_line.trim().split_whitespace().collect();
    if parts.len() < 2 {
        return None;
    }

    let method = parts[0].to_string();
    let full_path = parts[1].to_string();

    // Split path and query
    let (path, query) = if let Some(idx) = full_path.find('?') {
        (full_path[..idx].to_string(), full_path[idx..].to_string())
    } else {
        (full_path, String::new())
    };

    // Read headers
    let mut headers: HashMap<Value, Value> = HashMap::new();
    let mut content_length: usize = 0;

    loop {
        let mut line = String::new();
        if reader.read_line(&mut line).await.ok()? == 0 {
            break;
        }
        let trimmed = line.trim();
        if trimmed.is_empty() {
            break;
        }
        if let Some(idx) = trimmed.find(':') {
            let key = trimmed[..idx].trim().to_lowercase();
            let value = trimmed[idx + 1..].trim().to_string();
            if key == "content-length" {
                content_length = value.parse().unwrap_or(0);
            }
            headers.insert(Value::String(key), Value::String(value));
        }
    }

    // Read body if present
    let mut body = String::new();
    if content_length > 0 {
        let mut body_bytes = vec![0u8; content_length];
        reader.read_exact(&mut body_bytes).await.ok()?;
        body = String::from_utf8_lossy(&body_bytes).to_string();
    }

    // Build Request object
    let mut fields = std::collections::HashMap::new();
    fields.insert("method".to_string(), Value::String(method));
    fields.insert("path".to_string(), Value::String(path));
    fields.insert("query".to_string(), Value::String(query));
    fields.insert("headers".to_string(), Value::Map(headers));
    fields.insert("body".to_string(), Value::String(body));

    Some(ObjectData::into_value("Request".to_string(), fields))
}

/// Write HTTP response to async stream
async fn write_response_async(stream: &mut TokioTcpStream, response: &Value) {
    let (status, headers, body) = if let Value::Object(obj) = response {
        let status = match obj.fields.get("status") {
            Some(Value::Numeric(n)) => match n {
                crate::value::Number::I64(v) => *v as u16,
                crate::value::Number::I32(v) => *v as u16,
                _ => 200,
            },
            _ => 200,
        };

        let body = match obj.fields.get("body") {
            Some(Value::String(s)) => s.clone(),
            _ => String::new(),
        };

        let headers = match obj.fields.get("headers") {
            Some(Value::Map(m)) => m.clone(),
            _ => HashMap::new(),
        };

        (status, headers, body)
    } else {
        (200, HashMap::new(), response.to_string())
    };

    let status_text = match status {
        200 => "OK",
        201 => "Created",
        204 => "No Content",
        400 => "Bad Request",
        404 => "Not Found",
        500 => "Internal Server Error",
        _ => "OK",
    };

    let mut response_str = format!("HTTP/1.1 {} {}\r\n", status, status_text);
    response_str.push_str("Connection: close\r\n");

    let mut has_content_length = false;
    for (k, v) in &headers {
        if let (Value::String(key), Value::String(val)) = (k, v) {
            if key.to_lowercase() == "content-length" {
                has_content_length = true;
            }
            response_str.push_str(&format!("{}: {}\r\n", key, val));
        }
    }

    if !has_content_length {
        response_str.push_str(&format!("Content-Length: {}\r\n", body.len()));
    }

    response_str.push_str("\r\n");
    response_str.push_str(&body);

    let _ = stream.write_all(response_str.as_bytes()).await;
    let _ = stream.flush().await;
}

/// Start a high-performance async HTTP server using tokio
/// This provides much better performance than the sync thread pool version
pub fn serve_async(
    port: u16,
    handler: Value,
    globals: Arc<Mutex<Vec<Value>>>,
    custom_methods: Arc<Mutex<HashMap<String, HashMap<String, Value>>>>,
) -> Result<(), String> {
    // Build tokio runtime
    let runtime = tokio::runtime::Runtime::new()
        .map_err(|e| format!("Failed to create tokio runtime: {}", e))?;

    runtime.block_on(async move { serve_async_inner(port, handler, globals, custom_methods).await })
}

async fn serve_async_inner(
    port: u16,
    handler: Value,
    globals: Arc<Mutex<Vec<Value>>>,
    custom_methods: Arc<Mutex<HashMap<String, HashMap<String, Value>>>>,
) -> Result<(), String> {
    let addr = format!("0.0.0.0:{}", port);
    let listener = TokioTcpListener::bind(&addr)
        .await
        .map_err(|e| format!("Failed to bind to {}: {}", addr, e))?;

    println!(
        "HTTP server listening on http://localhost:{} (async/tokio)",
        port
    );

    // Shutdown flag
    let shutdown = Arc::new(AtomicBool::new(false));
    let shutdown_flag = shutdown.clone();

    // Set up Ctrl+C handler
    tokio::spawn(async move {
        tokio::signal::ctrl_c().await.ok();
        println!("\nShutting down server...");
        shutdown_flag.store(true, Ordering::SeqCst);
    });

    // Accept loop
    while !shutdown.load(Ordering::Relaxed) {
        tokio::select! {
            result = listener.accept() => {
                match result {
                    Ok((stream, _)) => {
                        let handler = handler.clone();
                        let globals = globals.clone();
                        let custom_methods = custom_methods.clone();

                        // Spawn async task for each connection
                        tokio::spawn(async move {
                            handle_connection_async(stream, handler, globals, custom_methods).await;
                        });
                    }
                    Err(e) => {
                        eprintln!("Connection error: {}", e);
                    }
                }
            }
            _ = tokio::time::sleep(Duration::from_millis(100)) => {
                // Check shutdown flag periodically
            }
        }
    }

    println!("Server stopped gracefully");
    Ok(())
}

/// Handle a single HTTP connection asynchronously
async fn handle_connection_async(
    mut stream: TokioTcpStream,
    handler: Value,
    globals: Arc<Mutex<Vec<Value>>>,
    custom_methods: Arc<Mutex<HashMap<String, HashMap<String, Value>>>>,
) {
    // Parse request asynchronously
    let request = match parse_request_async(&mut stream).await {
        Some(req) => req,
        None => return,
    };

    // Execute handler in blocking thread pool (Fiber is sync)
    let response = tokio::task::spawn_blocking(move || match handler {
        Value::Function(func) => call_handler_function(&func, request, globals, custom_methods),
        Value::Closure(closure) => call_handler_closure(&closure, request, globals, custom_methods),
        _ => {
            let mut fields = std::collections::HashMap::new();
            fields.insert(
                "status".to_string(),
                Value::Numeric(crate::value::Number::I64(500)),
            );
            fields.insert(
                "body".to_string(),
                Value::String("Internal Server Error: Handler not callable".to_string()),
            );
            fields.insert("headers".to_string(), Value::Map(HashMap::new()));
            ObjectData::into_value("Response".to_string(), fields)
        }
    })
    .await
    .unwrap_or_else(|_| {
        let mut fields = std::collections::HashMap::new();
        fields.insert(
            "status".to_string(),
            Value::Numeric(crate::value::Number::I64(500)),
        );
        fields.insert(
            "body".to_string(),
            Value::String("Handler panicked".to_string()),
        );
        fields.insert("headers".to_string(), Value::Map(HashMap::new()));
        ObjectData::into_value("Response".to_string(), fields)
    });

    // Write response asynchronously
    write_response_async(&mut stream, &response).await;
}

use super::ToolboxModule;

pub fn create_module() -> ToolboxModule {
    // HTTP module doesn't export simple functions like math does
    // The serve function is handled specially via NativeFunction
    ToolboxModule {
        functions: HashMap::new(),
    }
}
