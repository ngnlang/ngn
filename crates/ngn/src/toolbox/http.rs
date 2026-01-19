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
                    let request = req;
                    let response =
                        execute_handler(&handler, request.clone(), &globals, &custom_methods).await;

                    if let Value::WebSocketResponse(ws) = &response {
                        // Upgrade (handshake) + websocket session
                        let sec_key = match is_valid_websocket_upgrade_request(&request) {
                            Ok(k) => k,
                            Err(msg) => {
                                let body = format!("Bad Request: {}", msg);
                                let mut fields = std::collections::HashMap::new();
                                fields.insert(
                                    "status".to_string(),
                                    Value::Numeric(crate::value::Number::I64(400)),
                                );
                                fields.insert("body".to_string(), Value::String(body));
                                fields.insert(
                                    "headers".to_string(),
                                    Value::Map(std::collections::HashMap::new()),
                                );
                                let resp = ObjectData::into_value("Response".to_string(), fields);
                                let _ = write_response_to_stream(
                                    &mut write_half,
                                    &request,
                                    &resp,
                                    true,
                                )
                                .await;
                                break;
                            }
                        };

                        let accept = websocket_accept_key(&sec_key);
                        let _ =
                            write_websocket_handshake(&mut write_half, &accept, &ws.headers).await;

                        let recv = ws.recv_channel.clone();
                        let send = ws.send_channel.clone();

                        // Run websocket session and then close.
                        let _ = run_websocket_session(reader, write_half, recv, send).await;
                        break;
                    }

                    let _ =
                        write_response_to_stream(&mut write_half, &request, &response, true).await;
                    break;
                }
                req
            }
            Ok(None) => break, // Connection closed or parse error
            Err(_) => break,   // Timeout - close idle connection
        };

        // Execute handler
        let response = execute_handler(&handler, request.clone(), &globals, &custom_methods).await;

        if let Value::WebSocketResponse(ws) = &response {
            // Upgrade (handshake) + websocket session
            let sec_key = match is_valid_websocket_upgrade_request(&request) {
                Ok(k) => k,
                Err(msg) => {
                    let body = format!("Bad Request: {}", msg);
                    let mut fields = std::collections::HashMap::new();
                    fields.insert(
                        "status".to_string(),
                        Value::Numeric(crate::value::Number::I64(400)),
                    );
                    fields.insert("body".to_string(), Value::String(body));
                    fields.insert(
                        "headers".to_string(),
                        Value::Map(std::collections::HashMap::new()),
                    );
                    let resp = ObjectData::into_value("Response".to_string(), fields);
                    let _ = write_response_to_stream(&mut write_half, &request, &resp, true).await;
                    break;
                }
            };

            let accept = websocket_accept_key(&sec_key);
            let _ = write_websocket_handshake(&mut write_half, &accept, &ws.headers).await;

            let recv = ws.recv_channel.clone();
            let send = ws.send_channel.clone();

            let _ = run_websocket_session(reader, write_half, recv, send).await;
            break;
        }

        // Check if we're at the connection limit - if so, tell client we're closing
        let should_close = request_count >= max_requests_per_connection;

        // Write response (with close flag if at limit)
        if write_response_to_stream(&mut write_half, &request, &response, should_close)
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
    S: tokio::io::AsyncRead + tokio::io::AsyncWrite + Unpin + Send + 'static,
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
                    let request = req;
                    let response =
                        execute_handler(&handler, request.clone(), &globals, &custom_methods).await;

                    if let Value::WebSocketResponse(ws) = &response {
                        let sec_key = match is_valid_websocket_upgrade_request(&request) {
                            Ok(k) => k,
                            Err(msg) => {
                                let body = format!("Bad Request: {}", msg);
                                let mut fields = std::collections::HashMap::new();
                                fields.insert(
                                    "status".to_string(),
                                    Value::Numeric(crate::value::Number::I64(400)),
                                );
                                fields.insert("body".to_string(), Value::String(body));
                                fields.insert(
                                    "headers".to_string(),
                                    Value::Map(std::collections::HashMap::new()),
                                );
                                let resp = ObjectData::into_value("Response".to_string(), fields);
                                let _ = write_response_to_stream(
                                    &mut write_half,
                                    &request,
                                    &resp,
                                    true,
                                )
                                .await;
                                break;
                            }
                        };

                        let accept = websocket_accept_key(&sec_key);
                        let _ =
                            write_websocket_handshake(&mut write_half, &accept, &ws.headers).await;

                        let recv = ws.recv_channel.clone();
                        let send = ws.send_channel.clone();

                        let _ = run_websocket_session(reader, write_half, recv, send).await;
                        break;
                    }

                    let _ =
                        write_response_to_stream(&mut write_half, &request, &response, true).await;
                    break;
                }
                req
            }
            Ok(None) => break,
            Err(_) => break,
        };

        let response = execute_handler(&handler, request.clone(), &globals, &custom_methods).await;

        if let Value::WebSocketResponse(ws) = &response {
            let sec_key = match is_valid_websocket_upgrade_request(&request) {
                Ok(k) => k,
                Err(msg) => {
                    let body = format!("Bad Request: {}", msg);
                    let mut fields = std::collections::HashMap::new();
                    fields.insert(
                        "status".to_string(),
                        Value::Numeric(crate::value::Number::I64(400)),
                    );
                    fields.insert("body".to_string(), Value::String(body));
                    fields.insert(
                        "headers".to_string(),
                        Value::Map(std::collections::HashMap::new()),
                    );
                    let resp = ObjectData::into_value("Response".to_string(), fields);
                    let _ = write_response_to_stream(&mut write_half, &request, &resp, true).await;
                    break;
                }
            };

            let accept = websocket_accept_key(&sec_key);
            let _ = write_websocket_handshake(&mut write_half, &accept, &ws.headers).await;

            let recv = ws.recv_channel.clone();
            let send = ws.send_channel.clone();

            let _ = run_websocket_session(reader, write_half, recv, send).await;
            break;
        }

        let should_close = request_count >= max_requests_per_connection;

        if write_response_to_stream(&mut write_half, &request, &response, should_close)
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

fn get_lowercase_request_header(req: &Value, name: &str) -> Option<String> {
    let name = name.to_ascii_lowercase();
    if let Value::Object(o) = req {
        if o.model_name != "Request" {
            return None;
        }
        if let Some(Value::Map(headers)) = o.fields.get("headers") {
            for (k, v) in headers {
                if let (Value::String(key), Value::String(val)) = (k, v) {
                    if key.eq_ignore_ascii_case(&name) {
                        return Some(val.clone());
                    }
                }
            }
        }
    }
    None
}

fn connection_header_has_token(connection_value: &str, token: &str) -> bool {
    let token = token.to_ascii_lowercase();
    for part in connection_value.split(',') {
        if part.trim().to_ascii_lowercase() == token {
            return true;
        }
    }
    false
}

fn is_valid_websocket_upgrade_request(req: &Value) -> Result<String, String> {
    let upgrade = get_lowercase_request_header(req, "upgrade").unwrap_or_default();
    if upgrade.to_ascii_lowercase() != "websocket" {
        return Err("Missing/invalid Upgrade: websocket".to_string());
    }

    let connection = get_lowercase_request_header(req, "connection").unwrap_or_default();
    if !connection_header_has_token(&connection, "upgrade") {
        return Err("Missing/invalid Connection: Upgrade".to_string());
    }

    let version = get_lowercase_request_header(req, "sec-websocket-version").unwrap_or_default();
    if version.trim() != "13" {
        return Err("Missing/invalid Sec-WebSocket-Version (expected 13)".to_string());
    }

    let key = get_lowercase_request_header(req, "sec-websocket-key")
        .ok_or_else(|| "Missing Sec-WebSocket-Key".to_string())?;

    Ok(key)
}

fn websocket_accept_key(sec_websocket_key: &str) -> String {
    use base64::Engine;
    use sha1::Digest;

    const GUID: &str = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";

    let mut hasher = sha1::Sha1::new();
    hasher.update(sec_websocket_key.as_bytes());
    hasher.update(GUID.as_bytes());
    let digest = hasher.finalize();

    base64::engine::general_purpose::STANDARD.encode(digest)
}

async fn write_websocket_handshake<W>(
    stream: &mut W,
    accept_key: &str,
    user_headers: &std::collections::HashMap<String, String>,
) -> std::io::Result<()>
where
    W: AsyncWrite + Unpin,
{
    use std::fmt::Write;
    use tokio::io::AsyncWriteExt;

    let mut resp = String::with_capacity(256);
    resp.push_str("HTTP/1.1 101 Switching Protocols\r\n");
    resp.push_str("Upgrade: websocket\r\n");
    resp.push_str("Connection: Upgrade\r\n");
    let _ = write!(resp, "Sec-WebSocket-Accept: {}\r\n", accept_key);

    for (k, v) in user_headers {
        if k.eq_ignore_ascii_case("upgrade")
            || k.eq_ignore_ascii_case("connection")
            || k.eq_ignore_ascii_case("sec-websocket-accept")
            || k.eq_ignore_ascii_case("sec-websocket-protocol")
        {
            continue;
        }
        let _ = write!(resp, "{}: {}\r\n", k, v);
    }

    resp.push_str("\r\n");

    stream.write_all(resp.as_bytes()).await?;
    stream.flush().await?;
    Ok(())
}

fn close_channel(chan: &crate::value::Channel) {
    let mut closed = chan.is_closed.lock().unwrap();
    *closed = true;
}

#[derive(Debug)]
enum WsControl {
    Pong(Vec<u8>),
    Close(u16),
}

async fn read_ws_frame<R>(reader: &mut R) -> std::io::Result<Option<(u8, bool, Vec<u8>)>>
where
    R: tokio::io::AsyncRead + Unpin,
{
    use tokio::io::AsyncReadExt;

    let mut hdr = [0u8; 2];
    match reader.read_exact(&mut hdr).await {
        Ok(_) => {}
        Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => return Ok(None),
        Err(e) => return Err(e),
    }

    let b1 = hdr[0];
    let b2 = hdr[1];

    let fin = (b1 & 0x80) != 0;
    let opcode = b1 & 0x0f;

    let masked = (b2 & 0x80) != 0;
    let mut len: u64 = (b2 & 0x7f) as u64;

    if len == 126 {
        let mut ext = [0u8; 2];
        reader.read_exact(&mut ext).await?;
        len = u16::from_be_bytes(ext) as u64;
    } else if len == 127 {
        let mut ext = [0u8; 8];
        reader.read_exact(&mut ext).await?;
        len = u64::from_be_bytes(ext);
    }

    if !masked {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "WebSocket protocol error: unmasked client frame",
        ));
    }

    let mut mask = [0u8; 4];
    reader.read_exact(&mut mask).await?;

    let mut payload = vec![0u8; len as usize];
    if len > 0 {
        reader.read_exact(&mut payload).await?;
        for i in 0..payload.len() {
            payload[i] ^= mask[i % 4];
        }
    }

    Ok(Some((opcode, fin, payload)))
}

async fn write_ws_frame<W>(writer: &mut W, opcode: u8, payload: &[u8]) -> std::io::Result<()>
where
    W: AsyncWrite + Unpin,
{
    use tokio::io::AsyncWriteExt;

    let fin_opcode = 0x80u8 | (opcode & 0x0f);

    // Server-to-client frames are never masked.
    let mut header = Vec::with_capacity(14);
    header.push(fin_opcode);

    let len = payload.len() as u64;
    if len <= 125 {
        header.push(len as u8);
    } else if len <= 65535 {
        header.push(126);
        header.extend_from_slice(&(len as u16).to_be_bytes());
    } else {
        header.push(127);
        header.extend_from_slice(&(len as u64).to_be_bytes());
    }

    writer.write_all(&header).await?;
    if !payload.is_empty() {
        writer.write_all(payload).await?;
    }
    writer.flush().await?;
    Ok(())
}

async fn write_ws_text<W>(writer: &mut W, text: &str) -> std::io::Result<()>
where
    W: AsyncWrite + Unpin,
{
    write_ws_frame(writer, 0x1, text.as_bytes()).await
}

async fn write_ws_close<W>(writer: &mut W, code: u16) -> std::io::Result<()>
where
    W: AsyncWrite + Unpin,
{
    let payload = code.to_be_bytes();
    write_ws_frame(writer, 0x8, &payload).await
}

async fn write_ws_pong<W>(writer: &mut W, payload: &[u8]) -> std::io::Result<()>
where
    W: AsyncWrite + Unpin,
{
    write_ws_frame(writer, 0xA, payload).await
}

async fn run_websocket_session<R, W>(
    mut reader: R,
    mut writer: W,
    recv_channel: crate::value::Channel,
    send_channel: crate::value::Channel,
) -> std::io::Result<()>
where
    R: tokio::io::AsyncRead + Unpin + Send + 'static,
    W: tokio::io::AsyncWrite + Unpin + Send + 'static,
{
    use tokio::sync::mpsc;

    let (ctrl_tx, mut ctrl_rx) = mpsc::channel::<WsControl>(16);

    let send_for_writer = send_channel.clone();
    let writer_task = tokio::spawn(async move {
        loop {
            tokio::select! {
                cmd = ctrl_rx.recv() => {
                    match cmd {
                        Some(WsControl::Pong(payload)) => {
                            let _ = write_ws_pong(&mut writer, &payload).await;
                        }
                        Some(WsControl::Close(code)) => {
                            let _ = write_ws_close(&mut writer, code).await;
                            break;
                        }
                        None => {
                            // reader ended
                            break;
                        }
                    }
                }
                _ = tokio::time::sleep(std::time::Duration::from_millis(10)) => {
                    // Drain application sends
                    loop {
                        let next = {
                            let mut buf = send_for_writer.buffer.lock().unwrap();
                            buf.pop_front()
                        };
                        match next {
                            Some(v) => {
                                match v {
                                    Value::String(s) => {
                                        let _ = write_ws_text(&mut writer, &s).await;
                                    }
                                    Value::Bytes(b) => {
                                        let _ = write_ws_frame(&mut writer, 0x2, b.as_slice()).await;
                                    }
                                    other => {
                                        let _ = write_ws_text(&mut writer, &other.to_string()).await;
                                    }
                                }
                            }
                            None => break,
                        }
                    }

                    let is_closed = *send_for_writer.is_closed.lock().unwrap();
                    let is_empty = send_for_writer.buffer.lock().unwrap().is_empty();
                    if is_closed && is_empty {
                        let _ = write_ws_close(&mut writer, 1000).await;
                        break;
                    }
                }
            }
        }
    });

    // Reader loop runs here so it is not cancel-interrupted.
    loop {
        let frame = read_ws_frame(&mut reader).await;
        let frame = match frame {
            Ok(v) => v,
            Err(_) => {
                let _ = ctrl_tx.send(WsControl::Close(1002)).await;
                break;
            }
        };

        let (opcode, fin, payload) = match frame {
            Some(x) => x,
            None => break,
        };

        // v1: reject fragmentation
        if !fin {
            let _ = ctrl_tx.send(WsControl::Close(1002)).await;
            break;
        }

        match opcode {
            0x1 => {
                match String::from_utf8(payload) {
                    Ok(text) => {
                        recv_channel
                            .buffer
                            .lock()
                            .unwrap()
                            .push_back(Value::String(text));
                    }
                    Err(_) => {
                        let _ = ctrl_tx.send(WsControl::Close(1007)).await; // invalid frame payload data
                        break;
                    }
                }
            }
            0x2 => {
                // binary
                recv_channel
                    .buffer
                    .lock()
                    .unwrap()
                    .push_back(Value::Bytes(std::sync::Arc::new(payload)));
            }
            0x8 => {
                let _ = ctrl_tx.send(WsControl::Close(1000)).await;
                break;
            }
            0x9 => {
                let _ = ctrl_tx.send(WsControl::Pong(payload)).await;
            }
            0xA => {
                // pong - ignore
            }
            _ => {
                let _ = ctrl_tx.send(WsControl::Close(1002)).await;
                break;
            }
        }
    }

    // Close both channels (per v1 decision)
    close_channel(&recv_channel);
    close_channel(&send_channel);

    // Ensure writer task finishes
    drop(ctrl_tx);
    let _ = writer_task.await;

    Ok(())
}

/// Write HTTP response to split write half with proper error handling
async fn write_response_to_stream<W>(
    stream: &mut W,
    _request: &Value,
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

    // WebSocketResponse is handled at the connection loop level (upgrade + session)
    // because it needs access to both the read and write halves.

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

                fn is_sse_event_shaped_object(o: &crate::value::ObjectData) -> bool {
                    let mut has_any = false;
                    for k in o.fields.keys() {
                        match k.as_str() {
                            "data" | "event" | "id" | "retryMs" | "comment" => {
                                has_any = true;
                            }
                            _ => return false,
                        }
                    }
                    has_any
                }

                fn get_stringified_field(
                    o: &crate::value::ObjectData,
                    name: &str,
                ) -> Option<String> {
                    o.fields.get(name).map(|v| match v {
                        Value::String(s) => s.clone(),
                        other => other.to_string(),
                    })
                }

                fn get_retry_ms(o: &crate::value::ObjectData) -> Option<i64> {
                    o.fields.get("retryMs").and_then(|v| match v {
                        Value::Numeric(n) => match n {
                            crate::value::Number::I64(x) => Some(*x),
                            crate::value::Number::I32(x) => Some(*x as i64),
                            crate::value::Number::I16(x) => Some(*x as i64),
                            crate::value::Number::I8(x) => Some(*x as i64),
                            crate::value::Number::U64(x) => i64::try_from(*x).ok(),
                            crate::value::Number::U32(x) => Some(*x as i64),
                            crate::value::Number::U16(x) => Some(*x as i64),
                            crate::value::Number::U8(x) => Some(*x as i64),
                            _ => None,
                        },
                        _ => None,
                    })
                }

                fn write_sse_event_object(out: &mut String, o: &crate::value::ObjectData) {
                    use std::fmt::Write;

                    if let Some(c) = get_stringified_field(o, "comment") {
                        if !c.is_empty() {
                            let _ = write!(
                                out,
                                ": {}\n",
                                c.replace("\r\n", " ").replace(['\r', '\n'], " ")
                            );
                        }
                    }
                    if let Some(id) = o.fields.get("id").and_then(|v| {
                        if let Value::String(s) = v {
                            Some(s.clone())
                        } else {
                            None
                        }
                    }) {
                        if !id.is_empty() {
                            let _ = write!(
                                out,
                                "id: {}\n",
                                id.replace("\r\n", " ").replace(['\r', '\n'], " ")
                            );
                        }
                    }
                    if let Some(ev) = o.fields.get("event").and_then(|v| {
                        if let Value::String(s) = v {
                            Some(s.clone())
                        } else {
                            None
                        }
                    }) {
                        if !ev.is_empty() {
                            let _ = write!(
                                out,
                                "event: {}\n",
                                ev.replace("\r\n", " ").replace(['\r', '\n'], " ")
                            );
                        }
                    }
                    if let Some(ms) = get_retry_ms(o) {
                        if ms > 0 {
                            let _ = write!(out, "retry: {}\n", ms);
                        }
                    }

                    let data = get_stringified_field(o, "data").unwrap_or_default();
                    sse_data_lines(out, &data);
                    out.push('\n');
                }

                match value {
                    Value::Object(o)
                        if o.model_name == "SseEvent"
                            || (o.model_name == "__anon__" && is_sse_event_shaped_object(&o)) =>
                    {
                        write_sse_event_object(&mut event_buf, &o);
                    }
                    Value::String(s) => {
                        sse_data_lines(&mut event_buf, &s);
                        event_buf.push('\n');
                    }
                    // best effort for sending data
                    _ => {
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
