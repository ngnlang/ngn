//! HTTP Server module for ngn toolbox
//!
//! Provides a high-performance async HTTP/HTTPS server that can be started with `serve(port, handler)`
//! where handler is a function or closure that takes a Request and returns a Response.
//!
//! Both serve() and serve_tls() use async tokio-based architecture with cooperative yielding.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader as TokioBufReader};
use tokio::net::{TcpListener as TokioTcpListener, TcpStream as TokioTcpStream};

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
            FiberStatus::Spawning(_new_fiber) => {
                // For HTTP handlers, spawned fibers are rare and not critical
                // In the future, could run them in background tasks
                // For now, just continue with the main handler
                continue;
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
pub fn serve(
    port: u16,
    handler: Value,
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

    runtime.block_on(
        async move { serve_inner(port, handler, globals_snapshot, methods_snapshot).await },
    )
}

async fn serve_inner(
    port: u16,
    handler: Value,
    globals: Vec<Value>,
    custom_methods: HashMap<String, HashMap<String, Value>>,
) -> Result<(), String> {
    let addr = format!("0.0.0.0:{}", port);
    let listener = TokioTcpListener::bind(&addr)
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
                        tokio::spawn(async move {
                            handle_connection_async_fast(stream, handler, globals, custom_methods).await;
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
    stream: TokioTcpStream,
    handler: Value,
    globals: Arc<Vec<Value>>,
    custom_methods: Arc<Mutex<HashMap<String, HashMap<String, Value>>>>,
) {
    use tokio::time::{Duration, timeout};

    // Get peer IP address before splitting
    let peer_ip = stream
        .peer_addr()
        .map(|addr| addr.ip().to_string())
        .unwrap_or_else(|_| "unknown".to_string());

    // Split stream into read and write halves - this allows us to persist the BufReader
    let (read_half, mut write_half) = stream.into_split();
    let mut reader = TokioBufReader::new(read_half);

    // Keep-Alive: handle multiple requests on the same connection
    const KEEP_ALIVE_TIMEOUT: Duration = Duration::from_secs(30);
    const MAX_REQUESTS_PER_CONNECTION: usize = 100;

    let mut request_count = 0;

    loop {
        request_count += 1;

        // Parse request with timeout for idle connections
        let request = match timeout(
            KEEP_ALIVE_TIMEOUT,
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
        let should_close = request_count >= MAX_REQUESTS_PER_CONNECTION;

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
        serve_tls_inner(port, handler, config, globals_snapshot, methods_snapshot).await
    })
}

async fn serve_tls_inner(
    port: u16,
    handler: Value,
    config: rustls::ServerConfig,
    globals: Vec<Value>,
    custom_methods: HashMap<String, HashMap<String, Value>>,
) -> Result<(), String> {
    use tokio_rustls::TlsAcceptor;

    let addr = format!("0.0.0.0:{}", port);
    let listener = TokioTcpListener::bind(&addr)
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
                        let handler = handler.clone();
                        let globals = globals.clone();
                        let custom_methods = custom_methods.clone();
                        let acceptor = tls_acceptor.clone();
                        let peer_ip = addr.ip().to_string();

                        // Spawn async task for TLS handshake + handling
                        tokio::spawn(async move {
                            // Perform TLS handshake
                            match acceptor.accept(tcp_stream).await {
                                Ok(tls_stream) => {
                                    handle_tls_connection_async(tls_stream, handler, globals, custom_methods, peer_ip).await;
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
    mut stream: tokio_rustls::server::TlsStream<S>,
    handler: Value,
    globals: Arc<Vec<Value>>,
    custom_methods: Arc<Mutex<HashMap<String, HashMap<String, Value>>>>,
    peer_ip: String,
) where
    S: tokio::io::AsyncRead + tokio::io::AsyncWrite + Unpin,
{
    use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader};

    // Parse request - similar to parse_request_async but for TlsStream
    let mut reader = BufReader::new(&mut stream);

    // Read request line
    let mut request_line = String::new();
    if reader.read_line(&mut request_line).await.unwrap_or(0) == 0 {
        return;
    }

    let parts: Vec<&str> = request_line.trim().split_whitespace().collect();
    if parts.len() < 2 {
        return;
    }

    let method = parts[0].to_string();
    let full_path = parts[1].to_string();

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
    let mut host = String::new();
    let mut cookie_header = String::new();

    loop {
        let mut line = String::new();
        if reader.read_line(&mut line).await.unwrap_or(0) == 0 {
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
        if reader.read_exact(&mut body_bytes).await.is_ok() {
            body = String::from_utf8_lossy(&body_bytes).to_string();
        }
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
    fields.insert("protocol".to_string(), Value::String("https".to_string()));
    fields.insert("host".to_string(), Value::String(host));
    let request = ObjectData::into_value("Request".to_string(), fields);

    // Execute handler with cooperative yielding
    let response = match handler {
        Value::Function(func) => {
            let closure = Closure {
                function: func.clone(),
                upvalues: vec![],
            };
            call_handler_async(&closure, request, globals, custom_methods).await
        }
        Value::Closure(closure) => {
            call_handler_async(&closure, request, globals, custom_methods).await
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
    };

    // Write response
    let (status, resp_headers, resp_body) = if let Value::Object(obj) = &response {
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
        let headers = obj
            .fields
            .get("headers")
            .cloned()
            .unwrap_or(Value::Map(HashMap::new()));
        (status, headers, body)
    } else {
        (200, Value::Map(HashMap::new()), String::new())
    };

    let status_text = match status {
        200 => "OK",
        201 => "Created",
        204 => "No Content",
        400 => "Bad Request",
        401 => "Unauthorized",
        403 => "Forbidden",
        404 => "Not Found",
        500 => "Internal Server Error",
        _ => "OK",
    };

    // Pre-allocate with estimated capacity
    let header_count = if let Value::Map(h) = &resp_headers {
        h.len()
    } else {
        0
    };
    let estimated_size = 128 + header_count * 64 + resp_body.len();
    let mut response_str = String::with_capacity(estimated_size);

    use std::fmt::Write;
    let _ = write!(response_str, "HTTP/1.1 {} {}\r\n", status, status_text);

    // Add headers
    if let Value::Map(h) = resp_headers {
        for (k, v) in h {
            if let (Value::String(key), Value::String(val)) = (k, v) {
                let _ = write!(response_str, "{}: {}\r\n", key, val);
            }
        }
    }

    // Add content-length and body
    let _ = write!(response_str, "Content-Length: {}\r\n", resp_body.len());
    response_str.push_str("Connection: close\r\n");
    response_str.push_str("\r\n");
    response_str.push_str(&resp_body);

    let _ = stream.write_all(response_str.as_bytes()).await;
    let _ = stream.flush().await;
}

// ============================================================================
// Async Helper Functions (Keep-Alive)
// ============================================================================

use tokio::net::tcp::OwnedWriteHalf;

/// Parse HTTP request from a persistent BufReader (used for keep-alive connections)
/// Returns (Request, should_close) where should_close indicates if client wants to close connection
async fn parse_request_from_reader<R>(
    reader: &mut TokioBufReader<R>,
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
async fn write_response_to_stream(
    stream: &mut OwnedWriteHalf,
    response: &Value,
    close_connection: bool,
) -> std::io::Result<()> {
    use std::fmt::Write;
    use tokio::io::AsyncWriteExt;

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

/// Parse HTTP request with Keep-Alive awareness (legacy - creates its own BufReader)
/// Returns (Request, should_close) where should_close indicates if client wants to close connection
async fn parse_request_async_keepalive(
    stream: &mut TokioTcpStream,
    peer_ip: String,
    protocol: &str,
) -> Option<(Value, bool)> {
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

    // Detect HTTP version for default keep-alive behavior
    let http_version = if parts.len() >= 3 {
        parts[2]
    } else {
        "HTTP/1.0"
    };
    let default_keepalive = http_version == "HTTP/1.1"; // HTTP/1.1 defaults to keep-alive

    // Split path and query
    let (path, query) = if let Some(idx) = full_path.find('?') {
        (full_path[..idx].to_string(), full_path[idx..].to_string())
    } else {
        (full_path.clone(), String::new())
    };

    // Parse query parameters into a map
    let mut params: HashMap<Value, Value> = HashMap::new();
    if !query.is_empty() {
        // Remove leading '?' if present
        let query_str = query.strip_prefix('?').unwrap_or(&query);
        for pair in query_str.split('&') {
            if let Some(eq_idx) = pair.find('=') {
                let key = pair[..eq_idx].to_string();
                let value = pair[eq_idx + 1..].to_string();
                // URL decode would be nice here, but keep it simple for now
                params.insert(Value::String(key), Value::String(value));
            } else if !pair.is_empty() {
                // Key with no value
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

    // Build url field (path + query)
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

/// Write HTTP response with Keep-Alive header
async fn write_response_async_keepalive(
    stream: &mut TokioTcpStream,
    response: &Value,
    close_connection: bool,
) {
    use std::fmt::Write;

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
    }

    let mut has_content_length = false;
    for (k, v) in &headers {
        if let (Value::String(key), Value::String(val)) = (k, v) {
            if key.eq_ignore_ascii_case("content-length") {
                has_content_length = true;
            }
            // Skip if user tried to set Connection header (we control it)
            if key.eq_ignore_ascii_case("connection") {
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

    let _ = stream.write_all(response_str.as_bytes()).await;
    let _ = stream.flush().await;
}

use super::ToolboxModule;

pub fn create_module() -> ToolboxModule {
    // HTTP module doesn't export simple functions like math does
    // The serve function is handled specially via NativeFunction
    ToolboxModule {
        functions: HashMap::new(),
    }
}
