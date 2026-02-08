# ngn

*Pronounced "engine"*

A modern, expressive programming language designed for **building APIs** and **AI applications** — where you'd usually reach for Go, TypeScript, or Python.

### The Highlights

| Feature | What makes it special |
|---------|----------------------|
| **No async/await contagion.** | ngn uses channels and threads for concurrency — your code stays clean and readable. |
| **Channels & Threads** | Beautiful `<-` syntax for async: `<- fetch(url)`, `<-3 channel`, `<-? stream` |
| **Shared Atomic State** | Safe mutation across threads with `state()` — no mutexes required |
| **Built-in HTTP Server** | Deploy APIs with `serve(handler)` or `export default { fetch }` |
| **First-class LLM Support** | Stream inference with `tbx::llm` — perfect for AI applications |
| **Type Extensions** | Add methods to certain types with `extend` |
| **Powerful Control Flow** | Inline if syntax, pattern matching, `check` guards |

---

## 🚀 Quick Start

### Install

```bash
# NOT AVAILABLE YET
curl -fsSL https://ngnlang.com/install | sh

# Or install to a custom directory
NGN_INSTALL_DIR=$HOME/bin curl -fsSL https://ngnlang.com/install | sh

ngn --version
```

### Hello, World

```bash
mkdir ngn && cd ngn
# Open the ngn directory in your code editor of choice
# Create a new file named `main.ngn`
```

Every ngn program starts with a `main()` function:

```ngn
fn main() {
  print("Hello, ngn!")
}
```

### Variables

```ngn
fn main() {
  var name = "Alice"      // mutable
  const age = 30          // immutable
  name = "Bob"            // ✅ works
  // age = 31             // ❌ error: immutable
}
```

### Functions

```ngn
// Traditional
fn add(a: i64, b: i64): i64 {
  return a + b
}

// Concise (implicit return)
fn multiply(a: i64, b: i64): i64 a * b

fn main() {
  print(add(2, 3))       // 5
  print(multiply(4, 5))  // 20
}
```

---

## 🎯 What Makes ngn Special

### 1. Async Without the Chaos

In many languages, one async function forces every caller to become async. Not in ngn:

```ngn
fn main() {
  // fetch() returns a channel — await with <-
  const response = <- fetch("https://api.example.com")
  
  // That's it. No async keyword. No .then(). No await propagation.
  print(response.body)
}
```

### 2. Channels: Your Async Superpower

Channels are first-class citizens for sending and receiving data:

```ngn
fn main() {
  const ch = channel<string>()
  
  // Send
  ch <- "Hello"
  ch <- "World"
  ch.close()
  
  // Receive
  for (msg in <-? ch) {
    print(msg)
  }
}
```

**The `<-` operator is expressive:**

```ngn
<- channel        // wait for 1 message
<-3 channel       // wait for 3 messages
<-tasks.size() ch // wait for N messages (dynamic)
<-? channel       // stream until closed (for loops)
```

### 3. Threads That Just Work

Spawn concurrent work without ceremony:

```ngn
fn main() {
  // thread() returns a channel for awaiting
  const done = thread(|| {
    print("Working in background...")
    sleep(1000)
    return "Done!"
  })
  
  print("Main continues immediately")
  
  // Wait when you need the result
  const result = <- done
  print(result)
}
```

### 4. Safe Shared State

When multiple threads need to mutate the same data, use `state()`:

```ngn
fn main() {
  var counter = state(0)
  const done = channel<bool>()
  
  // Two threads racing to update
  thread(|| {
    counter.update(|n| n + 10)
    done <- true
  })
  
  thread(|| {
    counter.update(|n| n + 5)
    done <- true
  })
  
  <-2 done  // wait for both
  print(counter)  // Always 15 — atomic!
}
```

---

## 🌐 Build APIs in Minutes

### Option 1: export default

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
# Server running at http://localhost:3000
```

### Option 2: serve()

```ngn
import { serve } from "tbx::http"

fn handler(req: Request): Response {
  match (req.path) {
    "/" => return Response { body: "Home" },
    "/api" => return Response { body: json.stringify({ ok: true }) },
    _ => return Response { status: 404, body: "Not found" }
  }
}

fn main() {
  serve(handler, { port: 5173 })
}
```

### Streaming Responses

Perfect for LLM output or large files:

```ngn
fn handler(req: Request): StreamingResponse {
  const chunks = channel<string>()
  
  thread(|| {
    chunks <- "Loading"
    sleep(500)
    chunks <- "..."
    sleep(500)
    chunks <- "Done!"
    chunks.close()
  })
  
  return StreamingResponse {
    headers: { "Content-Type": "text/plain" },
    body: chunks
  }
}

export default { fetch: handler }
```

### WebSockets

```ngn
fn handler(req: Request): WebSocketResponse {
  const recv = channel<WsMessage>()
  const send = channel<WsMessage>()

  // Echo server
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

---

## 🤖 AI-Native: Built-in LLM Support

ngn includes llama.cpp bindings for local model inference:

```ngn
import { load, stream } from "tbx::llm"

fn main() {
  const model = load("./model.gguf")
  
  match (model) {
    Ok(m) => {
      // Stream tokens as they generate
      for (token in <-? stream(m, "Explain ngn in one sentence:")) {
        echo(token)
      }
    },
    Error(e) => print("Failed to load: ${e}")
  }
}
```

### Combine with HTTP for an AI API

```ngn
import { load, stream } from "tbx::llm"

global MODEL = load("./model.gguf")

fn handler(req: Request): StreamingResponse {
  const prompt = req.body
  const chunks = channel<string>()
  
  thread(|| {
    match (MODEL) {
      Ok(m) => {
        for (token in <-? stream(m, prompt)) {
          chunks <- token
        }
      },
      Error(e) => chunks <- "Error: ${e}"
    }
    chunks.close()
  })
  
  return StreamingResponse { body: chunks }
}

export default { fetch: handler }
```

---

## 🛡️ Type Safety That Helps, Not Hinders

### Union Types

```ngn
var x: string | i64 = "hello"
x = 42  // ✅ valid
```

### Result & Maybe (Option)

```ngn
fn divide(a: i64, b: i64): Result<i64, string> {
  if b == 0 return Error("Cannot divide by zero")
  return Ok(a / b)
}

fn main() {
  match (divide(10, 2)) {
    Ok(result) => print("Result: ${result}"),
    Error(msg) => print("Error: ${msg}")
  }
}
```
```ngn
fn findUser(id: u64): Maybe<string> {
  if (id == 1) return Value("Jason")
  if (id == 2) return Value("Brad")
  return Null
}

const user1 = findUser(1)

match (user1) {
  Value(name) => print("Found: {name}"), // matches
  Null => print("User not found"),
}
```

### The `check` Guard

Clean error handling without nesting:

```ngn
fn getUser(id?: i64): Result<string, string> {
  // `err?` only available for Result<T, E> not Maybe<T>
  check id?, err? {
    print("Error! ${err}")
    return Error("ID required")
  }
  // id is now guaranteed to have a value
  return Ok("User ${id}")
}
```

### Optional Chaining

```ngn
var user: User? = null
const name = user?.name ?? "Anonymous" // null coalescing operator unwraps Maybe::Value
```

---

## 🔧 Extend Certain Types

Add methods to built-in types:

```ngn
extend number {
  fn isEven(): bool {
    return this % 2 == 0
  }
}

extend string {
  fn isBlank(): bool {
    return this.trim().length() == 0
  }
}

fn main() {
  print((42).isEven())     // true
  print("  ".isBlank())    // true
}
```

---

## 📦 Models & Composition

### Define Structured Types

```ngn
model User {
  name: string,
  age: i64
}

extend User {
  fn greet(): void {
    print("Hi, I'm ${this.name}!")
  }
}

fn main() {
  const user = User { name: "Alice", age: 30 }
  user.greet()  // Hi, I'm Alice!
}
```

### Roles (Interfaces)

```ngn
role Animal {
  fn speak(): void
}

model Dog {
  name: string
}

extend Dog with Animal {
  fn speak(): void {
    print("Woof!")
  }
}
```

### Generic Models

```ngn
model Container<T> {
  value: T
}

fn main() {
  const intBox = Container { value: 42 }
  const strBox = Container { value: "hello" }
}
```

---

## 🎨 Elegant Control Flow

### Inline If

```ngn
const status = if (x > 10) "high" : (x > 5) "medium" : "low"

// Or with side effects
if (x > 10) print("big") : print("small")
```

### Pattern Matching

```ngn
match (value) {
  1 | 2 => print("one or two"),
  3 => print("three"),
  _ => print("something else")
}
```

### Destructuring

```ngn
const user = { name: "Alice", age: 30, city: "NYC" }
const { name, age, ...rest } = user
check name? { return }
check age? { return }
check rest? { return }

const numbers = [1, 2, 3, 4, 5]
const [first, second, ...tail] = numbers

const data = ("Fred", 27, "Accountant", true)
const (name, age, ...other) = data
```

---

## 📚 Standard Library (Toolbox)

```ngn
import { abs, floor, ceil } from "tbx::math"
import { assert } from "tbx::test"
import { serve } from "tbx::http"
import { load, stream } from "tbx::llm"
import { run, stream } from "tbx::process"
import { read, write } from "tbx::io"
```

---

## 🏃 Running Your Code

```bash
# Run directly (requires ngnr in PATH)
ngn run main.ngn

# Build a mod
ngn build main.ngn
ngnr main.mod

# Build a standalone binary
ngn build main.ngn --bundle
./main
```

---

## 📖 Learn More

This guide covers some basics. For the complete reference, see the [full documentation](README.md).

### Key Concepts to Explore Next

- **Closures** — capture values and close over state
- **Spawn utilities** — `spawn.all`, `spawn.race`, `spawn.try` for parallel task execution
- **SSE responses** — real-time streaming APIs
- **Environment variables** — automatic `.env` file loading

---

## 🎯 TL;DR

**ngn is for developers who want:**

✅ Async code without async/await ceremony  
✅ Thread-safe concurrency with channels  
✅ APIs that deploy in seconds  
✅ Local LLM inference as a first-class feature  
✅ A type system that helps without getting in the way  

```ngn
fn main() {
  print("Welcome to ngn!")
}
```

*Start building.* 🚀
