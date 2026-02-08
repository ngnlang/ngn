---
title: Web Servers
description: Build APIs with serve() or export default fetch handlers.
---

ngn includes a built-in web server, as well as Request and Response models for APIs and streaming. Default port is 3000.

:::note
ngn does not currently have an API framework.
:::

## Request
### Properties
- `method`: The HTTP method of the request
- `url`: The URL of the request
- `protocol`: Whether HTTP or HTTPs was used
- `host`: The host the client used to make the request
- `path`: The path of the request
- `query`: The query string of the request
- `params`: Query parameters as a `Map<string, string>`
- `headers`: The headers of the request
- `body`: The body of the request
- `ip`: The client's IP address
- `cookies`: The cookies sent with the request

### Methods
- `clone()`: Creates a new `Request` object with the same properties
- `text()`: Parses the body as a string, returns a `string`
- `json()`: Parses the body as JSON, returns a Result enum
- `formData()`: Parses URL-encoded body, returns a `Map<string, string>`

## Response
### Properties
- `status`: The HTTP status code - default is 200
- `statusText`: The HTTP status text - default is ""
- `ok`: A boolean indicating whether the response status code is in the range 200-299
- `headers`: The headers to include in the response
- `body`: The body of the response - default is ""

### Methods
- `text()`: Parses the body as a string, returns a `string`
- `json()`: Parses the body as JSON, returns a Result enum
- `formData()`: Parses URL-encoded body, returns a `Map<string, string>`

## export default

```ngn
fn handler(req: Request): Response {
  return Response {
    body: "Hello from ngn!"
  }
}

export default { fetch: handler }
```

```bash
ngn run api.ngn
```

## serve

```ngn
import { serve } from "tbx::http"

fn handler(req: Request): Response {
  match (req.path) {
    "/" => return Response { body: "Hello!" },
    "/api" => return Response { body: json.stringify({ ok: true }) },
    _ => return Response { status: 404, body: "Not found" }
  }
}

fn main() {
  serve(handler, { port: 5173 })
}
```

## StreamingResponse
Send HTTP responses with chunked transfer encoding, allowing data to be streamed to the client as it becomes available. Perfect for LLM inference, large file downloads, or any scenario where you want to send data incrementally.

```ngn
fn handler(req: Request): StreamingResponse {
  const chunks = channel<string>()
  
  // Background thread produces data
  thread(|| {
    chunks <- "First chunk\n"
    sleep(500)
    chunks <- "Second chunk\n"
    sleep(500)
    chunks <- "Done!\n"
    chunks.close()  // Signals end of stream
  })
  
  return StreamingResponse {
    headers: { "Content-Type": "text/plain" },
    body: chunks
  }
}

export default { fetch: handler }
```

### Properties
- `status`: The HTTP status code - default is 200
- `headers`: The headers to include in the response
- `body`: A `channel<string>` that produces chunks. Close the channel to end the stream.

## SseResponse (Server-Sent Events)
Server-Sent Events (SSE) streams a sequence of events over a single HTTP response. This is useful for realtime updates (notifications, progress updates, model inference tokens, etc.).

SSE works over both HTTP and HTTPS in ngn.

```ngn
fn handler(req: Request): SseResponse {
  const events = channel<SseMessage>()

  thread(|| {
    events <- SseEvent { data: "Hello", event: "hello", id: "", retryMs: 0, comment: "" }
    sleep(500)

    // Send raw strings as event data
    events <- "World"
    sleep(500)

    // Send raw objects shaped like SseEvent
    events <- { data: "Hello", event: "hello", id: "", retryMs: 0, comment: "" }

    events.close()
  })

  return SseResponse {
    status: 200,
    headers: { "Access-Control-Allow-Origin": "*" },
    body: events,
    keepAliveMs: 15000,
  }
}

export default { fetch: handler }
```

### Properties
- `status`: The HTTP status code - default is 200
- `headers`: The headers to include in the response
- `body`: A `channel<SseMessage>` that can send either a `string` (treated as event data), an `SseEvent`, or a raw object that represents an `SseEvent`
- `keepAliveMs`: Optional keepalive interval (in milliseconds). If > 0, the server periodically sends `: keepalive` comments while idle.

### SseEvent
- `data`: Event payload (string). Newlines are sent as multiple `data:` lines.
- `event`: Optional event name (maps to the SSE `event:` field)
- `id`: Optional event id (maps to the SSE `id:` field)
- `retryMs`: Optional client reconnection hint (maps to the SSE `retry:` field)
- `comment`: Optional comment line (maps to the SSE `: ...` field)

## WebSocketResponse
WebSockets provides a full-duplex channel between a client and your server over a single upgraded HTTP connection.

In ngn, a WebSocket connection is represented by two channels:
- `recv`: messages from the client (client -> server)
- `send`: messages to the client (server -> client)

Notes:
- supports `string` (text frames) and `bytes` (binary frames) - i.e. `WsMessage` type
- no subprotocol selection

```ngn
fn handler(req: Request): WebSocketResponse {
  const recv = channel<WsMessage>()
  const send = channel<WsMessage>()

  // Echo everything back
  thread(|| {
    for (msg in <-? recv) {
      send <- msg
    }
    send.close()
  })

  return WebSocketResponse { recv, send }
}

export default { fetch: handler }
```

### Properties
- `headers`: The headers to include in the 101 Switching Protocols response (optional)
- `recv`: A `channel<WsMessage>` that receives client messages. It is closed when the client disconnects.
- `send`: A `channel<WsMessage>` used to send messages to the client. Close it to close the websocket.
