---
title: MCP Servers
description: Build an MCP server
---

Build a basic MCP server.

```ngn
import { serve } from "tbx::http"

fn handleMcpWs(): WebSocketResponse {
  const recv = channel<WsMessage>()
  const send = channel<WsMessage>()
  thread(|| {
    for (raw in <-? recv) {
      const parsed = json.parse(raw)

      // only continue if we successfully parsed
      check parsed?, err? {
        send <- json.stringify({
          jsonrpc: "2.0",
          error: { code: -32601, message: "Invalid message. Could not parse json: ${err.message}" }
        })
        send.close()
        return
      }

      const method = parsed.method
      const id = parsed.id

      match (method) {
        "initialize" => {
          const payload = json.stringify({
            jsonrpc: "2.0",
            id,
            result: {
              protocolVersion: "2024-11-05",
              serverInfo: { name: "ngn-mcp", version: "0.1.0" },
              capabilities: {
                tools: { listChanged: false },
                resources: { listChanged: false },
                prompts: { listChanged: false }
              }
            }
          })
          send <- payload
        },
        "tools/list" => {
          const payload = json.stringify({
            jsonrpc: "2.0",
            id,
            result: {
              tools: [
                {
                  name: "echo",
                  description: "Echo back text",
                      inputSchema: {
                        "type": "object",
                        properties: { text: { "type": "string" } },
                        required: ["text"]
                      }
                }
              ]
            }
          })
          send <- payload
        },
        "tools/call" => {
          const { name, arguments: args } = parsed.params
          check name? {
            const payload = json.stringify({
              jsonrpc: "2.0",
              id,
              error: { code: -32601, message: "Invalid message. Missing tool name." }
            })
            send <- payload
            return
          }

          const text = args?.text ?? "NO TEXT SENT"

          if {
            (name == "echo")
              const payload = json.stringify({
                jsonrpc: "2.0",
                id,
                result: { content: [{ "type": "text", text }] }
              })
              send <- payload
            :
              const payload = json.stringify({
                jsonrpc: "2.0",
                id,
                error: { code: -32601, message: "Unknown tool" }
              })
              send <- payload
          }
        },
        _ => {
          // Ignore notifications or unknown methods
          if {
            (id)
              const payload = json.stringify({
                jsonrpc: "2.0",
                id,
                error: { code: -32601, message: "Method not found" }
              })
              send <- payload
          }
        }
      }
    }
    send.close()
  })
  return WebSocketResponse {
    headers: { "Access-Control-Allow-Origin": "*" },
    recv: recv,
    send: send,
  }
}

fn handler(req: Request): Response | WebSocketResponse {
  if (req.path == "/mcp") return handleMcpWs()
  return Response { status: 404, body: "Not found" }
}

export default { fetch: handler }
```
