//! HTTP Server module for ngn toolbox
//!
//! Provides a high-performance async HTTP/HTTPS server.
//!
//! The ngn-level API is exposed via the VM (NativeFunction) and supports:
//! - `serve(handler, config?)` (HTTP by default, HTTPS if `config.tls` is present)
//!
//! Internally, the Rust toolbox supports both serve() and serve_tls() variants.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use tokio::io::{AsyncBufReadExt, AsyncReadExt, BufReader};
use tokio::net::{TcpListener, TcpStream};

use crate::value::{Closure, ObjectData, Value};
use crate::vm::{Fiber, FiberStatus};

/// Async handler execution with cooperative yielding.
/// Runs fiber in batches, yielding to tokio between batches for fair scheduling.
async fn call_handler_async(
    closure: &Closure,
    request: Value,
    globals: Arc<Vec<Value>>,
    shared_methods: Arc<Mutex<HashMap<String, HashMap<String, Value>>>>,
) -> Value {
    const BATCH_SIZE: usize = 100; // Run 100 instructions before yielding

    // Create a new fiber for this request
    let mut fiber = Fiber::new(Box::new(closure.clone()));

    // Set the request as the first argument (register 0)
    fiber.stack[0] = request;

    // Clone globals for this handler (each handler gets its own copy)
    let mut handler_globals = (*globals).clone();
    // Reuse the shared methods Arc directly (no clone needed!)

    // Run fiber with cooperative yielding
    loop {
        let (status, _steps) = fiber.run_steps(&mut handler_globals, &shared_methods, BATCH_SIZE);
        match status {
            FiberStatus::Finished => break,
            FiberStatus::Running => {
                // Yield to tokio, let other tasks run
                tokio::task::yield_now().await;
                continue;
            }
            FiberStatus::Suspended => {
                // Fiber yielded voluntarily, give other tasks time
                tokio::task::yield_now().await;
                continue;
            }
            FiberStatus::Sleeping(ms) => {
                // Async sleep
                tokio::time::sleep(std::time::Duration::from_millis(ms)).await;
                continue;
            }
            FiberStatus::Spawning(new_fiber) => {
                // Spawn the new fiber in a background async task
                let new_fiber = *new_fiber;
                let bg_globals = handler_globals.clone();
                let bg_methods = shared_methods.clone();

                tokio::spawn(async move {
                    let mut bg_fiber = new_fiber;
                    let mut bg_globals_mut = bg_globals;

                    loop {
                        // Run in smaller batches for better interleaving
                        let (status, _) = bg_fiber.run_steps(&mut bg_globals_mut, &bg_methods, 50);
                        match status {
                            FiberStatus::Finished => break,
                            FiberStatus::Running | FiberStatus::Suspended => {
                                tokio::task::yield_now().await;
                                continue;
                            }
                            FiberStatus::Spawning(nested) => {
                                // For nested spawns, just continue (rare case)
                                let _ = *nested;
                                continue;
                            }
                            FiberStatus::Sleeping(ms) => {
                                // Async sleep
                                tokio::time::sleep(std::time::Duration::from_millis(ms)).await;
                                continue;
                            }
                            FiberStatus::Panicked(msg) => {
                                eprintln!("[ngn] Background thread panicked: {}", msg);
                                if let Some(chan) = &bg_fiber.completion_channel {
                                    let error = crate::value::EnumData::into_value(
                                        "Result".to_string(),
                                        "Error".to_string(),
                                        Some(Box::new(Value::String(format!(
                                            "Thread panicked: {}",
                                            msg
                                        )))),
                                    );
                                    chan.buffer.lock().unwrap().push_back(error);
                                }
                                break;
                            }
                        }
                    }

                    // If fiber has a completion channel, send result
                    if let Some(chan) = &bg_fiber.completion_channel {
                        let result = bg_fiber.return_value.clone().unwrap_or(Value::Void);
                        chan.buffer.lock().unwrap().push_back(result);
                    }
                });

                continue;
            }
            FiberStatus::Panicked(msg) => {
                // Handler panicked - log and return 500 error
                eprintln!("[ngn] HTTP handler panicked: {}", msg);
                break;
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

/// Start a high-performance HTTP server on the given port
/// This function blocks until Ctrl+C is pressed, then gracefully shuts down
/// Uses async tokio runtime for maximum performance
#[derive(Debug, Clone, Copy)]
pub struct HttpServerOptions {
    pub keep_alive_timeout: std::time::Duration,
    pub max_requests_per_connection: usize,
}

impl Default for HttpServerOptions {
    fn default() -> Self {
        Self {
            keep_alive_timeout: std::time::Duration::from_secs(30),
            max_requests_per_connection: 1000,
        }
    }
}

pub fn serve(
    port: u16,
    handler: Value,
    globals: Arc<Mutex<Vec<Value>>>,
    custom_methods: Arc<Mutex<HashMap<String, HashMap<String, Value>>>>,
) -> Result<(), String> {
    serve_with_options(
        port,
        handler,
        HttpServerOptions::default(),
        globals,
        custom_methods,
    )
}

pub fn serve_with_options(
    port: u16,
    handler: Value,
    options: HttpServerOptions,
    globals: Arc<Mutex<Vec<Value>>>,
    custom_methods: Arc<Mutex<HashMap<String, HashMap<String, Value>>>>,
) -> Result<(), String> {
    // Build tokio runtime with optimized settings
    let runtime = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(num_cpus::get())
        .max_blocking_threads(1024) // Large pool for fiber execution
        .enable_all()
        .build()
        .map_err(|e| format!("Failed to create tokio runtime: {}", e))?;

    // Clone globals once - handlers shouldn't mutate global state
    let globals_snapshot = globals.lock().unwrap().clone();
    let methods_snapshot = custom_methods.lock().unwrap().clone();

    runtime.block_on(async move {
        serve_inner(port, handler, options, globals_snapshot, methods_snapshot).await
    })
}

async fn serve_inner(
    port: u16,
    handler: Value,
    options: HttpServerOptions,
    globals: Vec<Value>,
    custom_methods: HashMap<String, HashMap<String, Value>>,
) -> Result<(), String> {
    let addr = format!("0.0.0.0:{}", port);
    let listener = TcpListener::bind(&addr)
        .await
        .map_err(|e| format!("Failed to bind to {}: {}", addr, e))?;

    println!(
        "HTTP server listening on http://localhost:{} (async, {} workers)",
        port,
        num_cpus::get()
    );

    // Wrap in Arc for sharing across tasks
    let globals = Arc::new(globals);
    // Wrap in Arc<Mutex> once - shared by ALL requests (no per-request clone!)
    let custom_methods = Arc::new(Mutex::new(custom_methods));

    // Accept loop with graceful shutdown
    loop {
        tokio::select! {
            result = listener.accept() => {
                match result {
                    Ok((stream, _)) => {
                        let handler = handler.clone();
                        let globals = globals.clone();
                        let custom_methods = custom_methods.clone();

                        // Spawn async task for each connection
                        let options = options;
                        tokio::spawn(async move {
                            handle_connection_async_fast(stream, handler, options, globals, custom_methods).await;
                        });
                    }
                    Err(e) => {
                        eprintln!("Connection error: {}", e);
                    }
                }
            }
            _ = tokio::signal::ctrl_c() => {
                println!("\nShutting down server...");
                break;
            }
        }
    }

    println!("Server stopped gracefully");
    Ok(())
}

/// Handle a single HTTP connection asynchronously with Keep-Alive support
async fn handle_connection_async_fast(
    stream: TcpStream,
    handler: Value,
    options: HttpServerOptions,
    globals: Arc<Vec<Value>>,
    custom_methods: Arc<Mutex<HashMap<String, HashMap<String, Value>>>>,
) {
    use tokio::time::timeout;

    // Disable Nagle's algorithm for real-time streaming responses
    // This ensures each write is sent immediately without buffering
    let _ = stream.set_nodelay(true);

    // Get peer IP address before splitting
    let peer_ip = stream
        .peer_addr()
        .map(|addr| addr.ip().to_string())
        .unwrap_or_else(|_| "unknown".to_string());

    // Split stream into read and write halves - this allows us to persist the BufReader
    let (read_half, mut write_half) = stream.into_split();
    let mut reader = BufReader::new(read_half);

    // Keep-Alive: handle multiple requests on the same connection
    let keep_alive_timeout = options.keep_alive_timeout;
    let max_requests_per_connection = options.max_requests_per_connection;

    let mut request_count = 0;

    loop {
        request_count += 1;

        // Parse request with timeout for idle connections
        let request = match timeout(
            keep_alive_timeout,
            parse_request_from_reader(&mut reader, peer_ip.clone(), "http"),
        )
        .await
        {
            Ok(Some((req, should_close))) => {
                if should_close {
                    // Client requested connection close, handle this last request
                    let response = execute_handler(&handler, req, &globals, &custom_methods).await;
                    let _ = write_response_to_stream(&mut write_half, &response, true).await;
                    break;
                }
                req
            }
            Ok(None) => break, // Connection closed or parse error
            Err(_) => break,   // Timeout - close idle connection
        };

        // Execute handler
        let response = execute_handler(&handler, request, &globals, &custom_methods).await;

        // Check if we're at the connection limit - if so, tell client we're closing
        let should_close = request_count >= max_requests_per_connection;

        // Write response (with close flag if at limit)
        if write_response_to_stream(&mut write_half, &response, should_close)
            .await
            .is_err()
        {
            break; // Write failed, close connection
        }

        if should_close {
            break; // Gracefully end after sending close header
        }
    }
}

/// Execute the handler for a request
async fn execute_handler(
    handler: &Value,
    request: Value,
    globals: &Arc<Vec<Value>>,
    custom_methods: &Arc<Mutex<HashMap<String, HashMap<String, Value>>>>,
) -> Value {
    match handler {
        Value::Function(func) => {
            let closure = Closure {
                function: func.clone(),
                upvalues: vec![],
            };
            call_handler_async(&closure, request, globals.clone(), custom_methods.clone()).await
        }
        Value::Closure(closure) => {
            call_handler_async(closure, request, globals.clone(), custom_methods.clone()).await
        }
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
    }
}

/// Start a high-performance HTTPS server with TLS
pub fn serve_tls(
    port: u16,
    handler: Value,
    cert_path: &str,
    key_path: &str,
    globals: Arc<Mutex<Vec<Value>>>,
    custom_methods: Arc<Mutex<HashMap<String, HashMap<String, Value>>>>,
) -> Result<(), String> {
    serve_tls_with_options(
        port,
        handler,
        cert_path,
        key_path,
        HttpServerOptions::default(),
        globals,
        custom_methods,
    )
}

pub fn serve_tls_with_options(
    port: u16,
    handler: Value,
    cert_path: &str,
    key_path: &str,
    options: HttpServerOptions,
    globals: Arc<Mutex<Vec<Value>>>,
    custom_methods: Arc<Mutex<HashMap<String, HashMap<String, Value>>>>,
) -> Result<(), String> {
    use rustls::pki_types::CertificateDer;
    use std::fs::File;
    use std::io::BufReader as StdBufReader;

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

    // Build tokio runtime
    let runtime = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(num_cpus::get())
        .max_blocking_threads(1024)
        .enable_all()
        .build()
        .map_err(|e| format!("Failed to create tokio runtime: {}", e))?;

    // Clone globals for async
    let globals_snapshot = globals.lock().unwrap().clone();
    let methods_snapshot = custom_methods.lock().unwrap().clone();

    runtime.block_on(async move {
        serve_tls_inner(
            port,
            handler,
            options,
            config,
            globals_snapshot,
            methods_snapshot,
        )
        .await
    })
}

async fn serve_tls_inner(
    port: u16,
    handler: Value,
    options: HttpServerOptions,
    config: rustls::ServerConfig,
    globals: Vec<Value>,
    custom_methods: HashMap<String, HashMap<String, Value>>,
) -> Result<(), String> {
    use tokio_rustls::TlsAcceptor;

    let addr = format!("0.0.0.0:{}", port);
    let listener = TcpListener::bind(&addr)
        .await
        .map_err(|e| format!("Failed to bind to {}: {}", addr, e))?;

    // Create TLS acceptor
    let tls_acceptor = TlsAcceptor::from(Arc::new(config));

    println!(
        "HTTPS server listening on https://localhost:{} (async, {} workers)",
        port,
        num_cpus::get()
    );

    // Wrap in Arc for sharing across tasks
    let globals = Arc::new(globals);
    // Wrap in Arc<Mutex> once - shared by ALL requests
    let custom_methods = Arc::new(Mutex::new(custom_methods));

    // Accept loop with graceful shutdown
    loop {
        tokio::select! {
            result = listener.accept() => {
                match result {
                    Ok((tcp_stream, addr)) => {
                        // Disable Nagle's algorithm for real-time streaming (SSE/streaming)
                        let _ = tcp_stream.set_nodelay(true);
                        let handler = handler.clone();
                        let globals = globals.clone();
                        let custom_methods = custom_methods.clone();
                        let acceptor = tls_acceptor.clone();
                        let peer_ip = addr.ip().to_string();

                        // Spawn async task for TLS handshake + handling
                        let options = options;
                        tokio::spawn(async move {
                            // Perform TLS handshake
                            match acceptor.accept(tcp_stream).await {
                                Ok(tls_stream) => {
                                    handle_tls_connection_async(tls_stream, handler, options, globals, custom_methods, peer_ip).await;
                                }
                                Err(e) => {
                                    eprintln!("TLS handshake error: {}", e);
                                }
                            }
                        });
                    }
                    Err(e) => {
                        eprintln!("Connection error: {}", e);
                    }
                }
            }
            _ = tokio::signal::ctrl_c() => {
                println!("\nShutting down HTTPS server...");
                break;
            }
        }
    }

    println!("HTTPS server stopped gracefully");
    Ok(())
}

/// Handle a single HTTPS connection asynchronously
async fn handle_tls_connection_async<S>(
    stream: tokio_rustls::server::TlsStream<S>,
    handler: Value,
    options: HttpServerOptions,
    globals: Arc<Vec<Value>>,
    custom_methods: Arc<Mutex<HashMap<String, HashMap<String, Value>>>>,
    peer_ip: String,
) where
    S: tokio::io::AsyncRead + tokio::io::AsyncWrite + Unpin,
{
    use tokio::time::timeout;

    // Keep-Alive: handle multiple requests on the same connection
    let keep_alive_timeout = options.keep_alive_timeout;
    let max_requests_per_connection = options.max_requests_per_connection;

    let (read_half, mut write_half) = tokio::io::split(stream);
    let mut reader = BufReader::new(read_half);

    let mut request_count = 0;

    loop {
        request_count += 1;

        // Parse request with timeout for idle connections
        let request = match timeout(
            keep_alive_timeout,
            parse_request_from_reader(&mut reader, peer_ip.clone(), "https"),
        )
        .await
        {
            Ok(Some((req, should_close))) => {
                if should_close {
                    let response = execute_handler(&handler, req, &globals, &custom_methods).await;
                    let _ = write_response_to_stream(&mut write_half, &response, true).await;
                    break;
                }
                req
            }
            Ok(None) => break,
            Err(_) => break,
        };

        let response = execute_handler(&handler, request, &globals, &custom_methods).await;

        let should_close = request_count >= max_requests_per_connection;

        if write_response_to_stream(&mut write_half, &response, should_close)
            .await
            .is_err()
        {
            break;
        }

        if should_close {
            break;
        }
    }
}

// ============================================================================
// Async Helper Functions (Keep-Alive)
// ============================================================================

use tokio::io::AsyncWrite;

/// Parse HTTP request from a persistent BufReader (used for keep-alive connections)
/// Returns (Request, should_close) where should_close indicates if client wants to close connection
async fn parse_request_from_reader<R>(
    reader: &mut BufReader<R>,
    peer_ip: String,
    protocol: &str,
) -> Option<(Value, bool)>
where
    R: tokio::io::AsyncRead + Unpin,
{
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

    // Detect HTTP version for default keep-alive behavior
    let http_version = if parts.len() >= 3 {
        parts[2]
    } else {
        "HTTP/1.0"
    };
    let default_keepalive = http_version == "HTTP/1.1";

    // Split path and query
    let (path, query) = if let Some(idx) = full_path.find('?') {
        (full_path[..idx].to_string(), full_path[idx..].to_string())
    } else {
        (full_path.clone(), String::new())
    };

    // Parse query parameters into a map
    let mut params: HashMap<Value, Value> = HashMap::new();
    if !query.is_empty() {
        let query_str = query.strip_prefix('?').unwrap_or(&query);
        for pair in query_str.split('&') {
            if let Some(eq_idx) = pair.find('=') {
                let key = pair[..eq_idx].to_string();
                let value = pair[eq_idx + 1..].to_string();
                params.insert(Value::String(key), Value::String(value));
            } else if !pair.is_empty() {
                params.insert(
                    Value::String(pair.to_string()),
                    Value::String(String::new()),
                );
            }
        }
    }

    // Read headers
    let mut headers: HashMap<Value, Value> = HashMap::new();
    let mut content_length: usize = 0;
    let mut connection_close = !default_keepalive;
    let mut host = String::new();
    let mut cookie_header = String::new();

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
            if key == "connection" {
                connection_close = value.eq_ignore_ascii_case("close");
            }
            if key == "host" {
                host = value.clone();
            }
            if key == "cookie" {
                cookie_header = value.clone();
            }
            headers.insert(Value::String(key), Value::String(value));
        }
    }

    // Parse cookies into a map
    let mut cookies: HashMap<Value, Value> = HashMap::new();
    if !cookie_header.is_empty() {
        for cookie in cookie_header.split(';') {
            let cookie = cookie.trim();
            if let Some(eq_idx) = cookie.find('=') {
                let key = cookie[..eq_idx].trim().to_string();
                let value = cookie[eq_idx + 1..].trim().to_string();
                cookies.insert(Value::String(key), Value::String(value));
            }
        }
    }

    // Read body if present
    let mut body = String::new();
    if content_length > 0 {
        let mut body_bytes = vec![0u8; content_length];
        reader.read_exact(&mut body_bytes).await.ok()?;
        body = String::from_utf8_lossy(&body_bytes).to_string();
    }

    // Build url field
    let url = full_path.clone();

    // Build Request object with all fields
    let mut fields = std::collections::HashMap::new();
    fields.insert("method".to_string(), Value::String(method));
    fields.insert("path".to_string(), Value::String(path));
    fields.insert("query".to_string(), Value::String(query));
    fields.insert("headers".to_string(), Value::Map(headers));
    fields.insert("body".to_string(), Value::String(body));
    fields.insert("params".to_string(), Value::Map(params));
    fields.insert("ip".to_string(), Value::String(peer_ip));
    fields.insert("url".to_string(), Value::String(url));
    fields.insert("cookies".to_string(), Value::Map(cookies));
    fields.insert("protocol".to_string(), Value::String(protocol.to_string()));
    fields.insert("host".to_string(), Value::String(host));

    Some((
        ObjectData::into_value("Request".to_string(), fields),
        connection_close,
    ))
}

/// Write HTTP response to split write half with proper error handling
async fn write_response_to_stream<W>(
    stream: &mut W,
    response: &Value,
    close_connection: bool,
) -> std::io::Result<()>
where
    W: AsyncWrite + Unpin,
{
    use std::fmt::Write;
    use tokio::io::AsyncWriteExt;

    // Check if this is a StreamingResponse
    if let Value::StreamingResponse(streaming) = response {
        return write_streaming_response_to_stream(stream, streaming, close_connection).await;
    }

    // Check if this is an SseResponse
    if let Value::SseResponse(sse) = response {
        return write_sse_response_to_stream(stream, sse, close_connection).await;
    }

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

    // Pre-allocate with estimated capacity
    let estimated_size = 128 + headers.len() * 64 + body.len();
    let mut response_str = String::with_capacity(estimated_size);

    let _ = write!(response_str, "HTTP/1.1 {} {}\r\n", status, status_text);

    // Set Connection header based on whether we should close
    if close_connection {
        response_str.push_str("Connection: close\r\n");
    } else {
        response_str.push_str("Connection: keep-alive\r\n");
        response_str.push_str("Keep-Alive: timeout=30\r\n");
    }

    let mut has_content_length = false;
    for (k, v) in &headers {
        if let (Value::String(key), Value::String(val)) = (k, v) {
            if key.eq_ignore_ascii_case("content-length") {
                has_content_length = true;
            }
            // Skip if user tried to set Connection header (we control it)
            if key.eq_ignore_ascii_case("connection") || key.eq_ignore_ascii_case("keep-alive") {
                continue;
            }
            let _ = write!(response_str, "{}: {}\r\n", key, val);
        }
    }

    if !has_content_length {
        let _ = write!(response_str, "Content-Length: {}\r\n", body.len());
    }

    response_str.push_str("\r\n");
    response_str.push_str(&body);

    stream.write_all(response_str.as_bytes()).await?;
    stream.flush().await?;
    Ok(())
}

/// Write a streaming HTTP response using chunked transfer encoding
/// Each message from the body_channel becomes an HTTP chunk
async fn write_streaming_response_to_stream<W>(
    stream: &mut W,
    response: &crate::value::StreamingResponseData,
    close_connection: bool,
) -> std::io::Result<()>
where
    W: AsyncWrite + Unpin,
{
    use std::fmt::Write;
    use tokio::io::AsyncWriteExt;

    let status = response.status;
    let status_text = match status {
        200 => "OK",
        201 => "Created",
        204 => "No Content",
        400 => "Bad Request",
        404 => "Not Found",
        500 => "Internal Server Error",
        _ => "OK",
    };

    // Build headers
    let mut header_str = String::with_capacity(256);
    let _ = write!(header_str, "HTTP/1.1 {} {}\r\n", status, status_text);

    // Chunked transfer encoding for streaming
    header_str.push_str("Transfer-Encoding: chunked\r\n");

    // Connection handling
    if close_connection {
        header_str.push_str("Connection: close\r\n");
    } else {
        header_str.push_str("Connection: keep-alive\r\n");
        header_str.push_str("Keep-Alive: timeout=30\r\n");
    }

    // Add user-provided headers
    for (key, val) in &response.headers {
        // Skip headers we control
        if key.eq_ignore_ascii_case("transfer-encoding")
            || key.eq_ignore_ascii_case("content-length")
            || key.eq_ignore_ascii_case("connection")
            || key.eq_ignore_ascii_case("keep-alive")
        {
            continue;
        }
        let _ = write!(header_str, "{}: {}\r\n", key, val);
    }

    // End headers
    header_str.push_str("\r\n");

    // Write headers
    stream.write_all(header_str.as_bytes()).await?;
    stream.flush().await?;

    // Stream chunks from the channel
    let channel = &response.body_channel;
    loop {
        // Try to receive a chunk from the channel
        let chunk = {
            let mut buffer = channel.buffer.lock().unwrap();
            buffer.pop_front()
        };

        match chunk {
            Some(value) => {
                // Convert value to string
                let chunk_data = match value {
                    Value::String(s) => s,
                    _ => value.to_string(),
                };

                // Write chunk in chunked transfer encoding format:
                // <hex size>\r\n<data>\r\n
                let chunk_header = format!("{:x}\r\n", chunk_data.len());
                stream.write_all(chunk_header.as_bytes()).await?;
                stream.write_all(chunk_data.as_bytes()).await?;
                stream.write_all(b"\r\n").await?;
                stream.flush().await?;
            }
            None => {
                // Check if channel is closed
                let is_closed = *channel.is_closed.lock().unwrap();
                if is_closed {
                    // Send final chunk (0-length) to signal end of stream
                    stream.write_all(b"0\r\n\r\n").await?;
                    stream.flush().await?;
                    break;
                } else {
                    // Channel is open but empty, avoid busy spinning
                    tokio::time::sleep(std::time::Duration::from_millis(10)).await;
                }
            }
        }
    }

    Ok(())
}

/// Write a Server-Sent Events (SSE) response using chunked transfer encoding
///
/// The body_channel can send either strings (treated as event data) or objects shaped like SseEvent.
async fn write_sse_response_to_stream<W>(
    stream: &mut W,
    response: &crate::value::SseResponseData,
    close_connection: bool,
) -> std::io::Result<()>
where
    W: AsyncWrite + Unpin,
{
    use std::fmt::Write;
    use tokio::io::AsyncWriteExt;

    fn status_text(status: u16) -> &'static str {
        match status {
            200 => "OK",
            201 => "Created",
            204 => "No Content",
            400 => "Bad Request",
            404 => "Not Found",
            500 => "Internal Server Error",
            _ => "OK",
        }
    }

    fn sse_data_lines(out: &mut String, data: &str) {
        // SSE requires each line to be prefixed with "data: ".
        // Also normalize CRLF/newlines coming from user input.
        let normalized = data.replace("\r\n", "\n").replace('\r', "\n");
        for line in normalized.split('\n') {
            let _ = write!(out, "data: {}\n", line);
        }
    }

    // Build headers
    let mut header_str = String::with_capacity(256);
    let _ = write!(
        header_str,
        "HTTP/1.1 {} {}\r\n",
        response.status,
        status_text(response.status)
    );

    header_str.push_str("Transfer-Encoding: chunked\r\n");
    header_str.push_str("Content-Type: text/event-stream\r\n");
    header_str.push_str("Cache-Control: no-cache\r\n");
    header_str.push_str("X-Accel-Buffering: no\r\n");

    if close_connection {
        header_str.push_str("Connection: close\r\n");
    } else {
        header_str.push_str("Connection: keep-alive\r\n");
        header_str.push_str("Keep-Alive: timeout=30\r\n");
    }

    for (key, val) in &response.headers {
        if key.eq_ignore_ascii_case("transfer-encoding")
            || key.eq_ignore_ascii_case("content-length")
            || key.eq_ignore_ascii_case("connection")
            || key.eq_ignore_ascii_case("keep-alive")
            || key.eq_ignore_ascii_case("content-type")
        {
            continue;
        }
        let _ = write!(header_str, "{}: {}\r\n", key, val);
    }

    header_str.push_str("\r\n");

    stream.write_all(header_str.as_bytes()).await?;
    stream.flush().await?;

    let channel = &response.body_channel;
    let keep_alive_ms = response.keep_alive_ms;
    let mut last_write = std::time::Instant::now();

    loop {
        let next = {
            let mut buffer = channel.buffer.lock().unwrap();
            buffer.pop_front()
        };

        match next {
            Some(value) => {
                let mut event_buf = String::new();

                match value {
                    Value::Enum(ref e) if e.enum_name == "SseMessage" => {
                        match (e.variant_name.as_str(), e.data.as_ref()) {
                            ("Data", Some(v)) => {
                                if let Value::String(s) = v.as_ref() {
                                    sse_data_lines(&mut event_buf, s);
                                    event_buf.push('\n');
                                } else {
                                    sse_data_lines(&mut event_buf, &v.to_string());
                                    event_buf.push('\n');
                                }
                            }
                            ("Event", Some(v)) => {
                                if let Value::Object(o) = v.as_ref() {
                                    // SseEvent fields: data/event/id/retryMs/comment
                                    let get_str = |name: &str| -> Option<String> {
                                        o.fields.get(name).and_then(|vv| {
                                            if let Value::String(s) = vv {
                                                Some(s.clone())
                                            } else {
                                                None
                                            }
                                        })
                                    };
                                    let get_i64 = |name: &str| -> Option<i64> {
                                        o.fields.get(name).and_then(|vv| match vv {
                                            Value::Numeric(crate::value::Number::I64(n)) => {
                                                Some(*n)
                                            }
                                            Value::Numeric(crate::value::Number::I32(n)) => {
                                                Some(*n as i64)
                                            }
                                            _ => None,
                                        })
                                    };

                                    if let Some(c) = get_str("comment") {
                                        if !c.is_empty() {
                                            let _ = write!(event_buf, ": {}\n", c);
                                        }
                                    }
                                    if let Some(id) = get_str("id") {
                                        if !id.is_empty() {
                                            let _ = write!(event_buf, "id: {}\n", id);
                                        }
                                    }
                                    if let Some(ev) = get_str("event") {
                                        if !ev.is_empty() {
                                            let _ = write!(event_buf, "event: {}\n", ev);
                                        }
                                    }
                                    if let Some(ms) = get_i64("retryMs") {
                                        if ms > 0 {
                                            let _ = write!(event_buf, "retry: {}\n", ms);
                                        }
                                    }

                                    let data = get_str("data").unwrap_or_default();
                                    sse_data_lines(&mut event_buf, &data);
                                    event_buf.push('\n');
                                } else {
                                    // If it's not an object for some reason, stringify and treat as data.
                                    sse_data_lines(&mut event_buf, &v.to_string());
                                    event_buf.push('\n');
                                }
                            }
                            _ => {
                                sse_data_lines(&mut event_buf, &value.to_string());
                                event_buf.push('\n');
                            }
                        }
                    }
                    _ => {
                        // If user code somehow sends a non-SseMessage value, stringify and treat as data.
                        sse_data_lines(&mut event_buf, &value.to_string());
                        event_buf.push('\n');
                    }
                }

                let chunk_header = format!("{:x}\r\n", event_buf.len());
                stream.write_all(chunk_header.as_bytes()).await?;
                stream.write_all(event_buf.as_bytes()).await?;
                stream.write_all(b"\r\n").await?;
                stream.flush().await?;

                last_write = std::time::Instant::now();
            }
            None => {
                let is_closed = *channel.is_closed.lock().unwrap();
                if is_closed {
                    stream.write_all(b"0\r\n\r\n").await?;
                    stream.flush().await?;
                    break;
                }

                if keep_alive_ms > 0
                    && last_write.elapsed() >= std::time::Duration::from_millis(keep_alive_ms)
                {
                    let keepalive = ": keepalive\n\n";
                    let chunk_header = format!("{:x}\r\n", keepalive.len());
                    stream.write_all(chunk_header.as_bytes()).await?;
                    stream.write_all(keepalive.as_bytes()).await?;
                    stream.write_all(b"\r\n").await?;
                    stream.flush().await?;
                    last_write = std::time::Instant::now();
                } else {
                    tokio::time::sleep(std::time::Duration::from_millis(10)).await;
                }
            }
        }
    }

    Ok(())
}

use super::ToolboxModule;

pub fn create_module() -> ToolboxModule {
    // HTTP module doesn't export simple functions like math does
    // The serve function is handled specially via NativeFunction
    ToolboxModule {
        functions: HashMap::new(),
    }
}
