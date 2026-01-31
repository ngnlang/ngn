---
title: HTTP Servers
description: Build APIs with serve() or export default fetch handlers.
---

ngn includes a built-in HTTP server, as well as Request and Response types for APIs and streaming. Default port is 3000.

## Option 1: export default

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

## Option 2: serve()

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

## Streaming responses

```ngn
fn handler(req: Request): StreamingResponse {
  const chunks = channel<string>()

  thread(|| {
    chunks <- "Loading"
    sleep(250)
    chunks <- "..."
    sleep(250)
    chunks <- "Done"
    chunks.close()
  })

  return StreamingResponse {
    headers: { "Content-Type": "text/plain" },
    body: chunks
  }
}

export default { fetch: handler }
```
