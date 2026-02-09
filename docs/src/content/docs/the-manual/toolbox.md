---
title: Toolbox
---

ngn ships a "standard library" called the toolbox. It has a module namespace of `tbx::`

You can import in different ways:
- `import { abs } from "tbx::math"`
- `import { abs as ABS } from "tbx::math"`
- `import Math from "tbx::math"`
- `import * as Math from "tbx::math"`

## tbx::math

- `abs`: return the absolute value of a number. `abs(-5) // 5`
- `round`: return the number rounded to the nearest integer `round(4.5) // 5`
- `floor`: return the largest integer less than or equal to a number `floor(3.9) // 3`
- `ceil`: return the smallest integer greater than or equal to a number `ceil(3.2) // 4`
- `trunc`: return the integer part of a number (towards zero) `trunc(-3.9) // -3`
- `sign`: return `-1`, `0`, or `1` depending on the sign of the number `sign(-5) // -1`
- `min`: return the smallest of 2+ numbers `min(5, 3) // 3`
- `max`: return the largest of 2+ numbers `max(5, 3) // 5`
- `clamp`: clamp a value to `[min, max]` `clamp(15, 0, 10) // 10`

- `sqrt`: return the square root of a number `sqrt(9) // 3`
- `pow`: raise `base` to `exponent` `pow(2, 3) // 8`
- `exp`: return `e^x` `exp(1) // ~2.718`
- `log`: natural logarithm (base `e`) `log(2.718281828) // ~1`
- `log10`: base-10 logarithm `log10(1000) // 3`
- `log2`: base-2 logarithm `log2(8) // 3`

- `PI`: return the value of pi `PI() // ~3.141592653589793`

- `sin`: sine `sin(0) // 0`
- `cos`: cosine `cos(0) // 1`
- `tan`: tangent `tan(0) // 0`
- `asin`: arcsine `asin(1) // ~1.5707963267948966`
- `acos`: arccosine `acos(0) // ~1.5707963267948966`
- `atan`: arctangent `atan(1) // ~0.7853981633974483`
- `atan2`: arctangent of `y/x` using signs to determine quadrant `atan2(1, 1) // ~0.7853981633974483`

## tbx::encoding

Encoding helpers for bytes and strings.

```ngn
import { hexEncode, hexDecode, base64Encode, base64Decode } from "tbx::encoding"
```

- `hexEncode`: encode bytes to a lowercase hex string
- `hexDecode`: decode a hex string into bytes
- `base64Encode`: encode bytes to a base64 string (standard alphabet)
- `base64Decode`: decode a base64 string into bytes

```ngn
const data = "hello".toBytes()

const hex = hexEncode(data)         // "68656c6c6f"
const roundtrip = hexDecode(hex)    // bytes
print(roundtrip.toStringStrict())   // "hello"

const b64 = base64Encode(data)      // "aGVsbG8="
const decoded = base64Decode(b64)   // bytes
print(decoded.toStringStrict())     // "hello"
```

`hexDecode` and `base64Decode` return a type error if the input string is not valid for the encoding.

## tbx::test
`assert`: assert that a condition is true

```ngn
import { assert } from "tbx::test"

fn main() {
  assert(1 + 1 == 2)
  // ✅ Assertion passed

  // with optional description
  assert(1 + 1 == 2, "1 + 1 should equal 2")
  // ✅ 1 + 1 should equal 2
}
```

## tbx::http
Create an HTTP server.

`serve(handler, config?)`: create an HTTP/HTTPS server

If `config.tls` is present, ngn starts an HTTPS server. Otherwise it starts HTTP.

```ngn
import { serve } from "tbx::http"

fn handleRequest(req: Request): Response {
  return Response {
    status: 200,
    headers: map<string, string>(),
    body: "Hello from ngn HTTP server! Path: ${req.path}"
  }
}

fn main() {
  serve(handleRequest)
}
```

`serve` config (2nd arg)

```ngn
serve(handleRequest, {
  port: 3000,
  keepAliveTimeoutMs: 30000,
  maxRequestsPerConnection: 1000,

  // If present, server starts in HTTPS mode.
  tls: {
    cert: "./cert.pem",
    key: "./key.pem",
  },
})
```

default export with fetch method (serve called under the hood)

```ngn
fn handler(req: Request): Response {
  return Response {
    status: 200,
    body: "Hello from export-based API!",
    headers: map<string, string>()
  }
}

export default { fetch: handler }
```

export default `config`

If you export an object with a `config` field, ngn reads it when booting the server:

```ngn
export default {
  config: {
    port: 3000,
    keepAliveTimeoutMs: 30000,
    maxRequestsPerConnection: 1000,

    // Optional TLS config. If present, the server starts in HTTPS mode.
    tls: {
      cert: "./cert.pem",
      key: "./key.pem",
    },
  },

  fetch: handler
}
```

## tbx::llm

Language-level LLM APIs.

ngn vendors llama.cpp at `vendor/llama.cpp` and builds it into the runtime. `tbx::llm` uses llama.cpp directly. Provide a local `.gguf` model file.

```ngn
import { load, generate, stream } from "tbx::llm"
```

### load()
Load a model from disk. Returns `Result<LlmModel, string>`.

`load(path, opts?)`

Options:
- `context: i64` context window size. Defaults to `0`.
- `threads: i64` number of CPU threads. Defaults to `0`.
- `mmap: bool` use memory-mapped model loading. Defaults to `true`.

### generate()
Run a one-shot generation. Returns `Result<string, string>`.

`generate(model, prompt, opts?)`

Options:
- `max_tokens: i64` maximum tokens to generate. Defaults to `128`.
- `temperature: f64` sampling temperature. Defaults to `0.8`.
- `top_p: f64` nucleus sampling probability. Defaults to `0.95`.
- `top_k: i64` top-k sampling. Defaults to `40`.
- `seed: i64` RNG seed. Defaults to `0`.

### stream()
Stream generated text chunks. Returns `channel<string>`.

`stream(model, prompt, opts?)`

Options:
- `max_tokens: i64` maximum tokens to generate. Defaults to `128`.
- `temperature: f64` sampling temperature. Defaults to `0.8`.
- `top_p: f64` nucleus sampling probability. Defaults to `0.95`.
- `top_k: i64` top-k sampling. Defaults to `40`.
- `seed: i64` RNG seed. Defaults to `0`.

If the consumer closes the channel (e.g. client disconnects during `StreamingResponse`), generation should stop early.

```ngn
import { load, stream } from "tbx::llm"

fn main() {
  const r = load("./model.gguf")
  match (r) {
    Ok(m) => {
      const ch = stream(m, "hello")
      for (chunk in <-? ch) {
        match (chunk) {
          Value(s) => echo(s),
          Null => break,
        }
      }
    },
    Error(e) => print(e)
  }
}
```

## tbx::process

OS process execution utilities.

```ngn
import { run, stream, streamRaw } from "tbx::process"
```

### run()
Run a shell command (`/bin/sh -c`) and return a channel that produces a single `Result<ProcessOutput, string>`.

`run(cmd, opts?)`

```ngn
const result = <- run("printf 'hi'")
match (result) {
  Ok(out) => {
    print(out.code)   // exit code
    print(out.stdout) // captured stdout
    print(out.stderr) // captured stderr
  },
  Error(e) => print(e)
}
```

Options:
- `cwd: string`
- `timeoutMs: i64`

### stream()
Stream a command's stdout/stderr while it runs. Returns `Result<ProcessStream, string>`.

`stream(cmd, opts?)`

`ProcessStream` fields:
- `stdout: channel<string>` (line-based)
- `stderr: channel<string>` (line-based)
- `done: channel<Result<ProcessOutput, string>>`

```ngn
const r = stream("printf 'a\\nb\\n'")
match (r) {
  Ok(p) => {
    const a = <- p.stdout
    const b = <- p.stdout

    // Stop reading once you're done.
    p.stdout.close()
    p.stderr.close()

    const done = <- p.done
    match (done) {
      Ok(out) => print(out.code),
      Error(e) => print(e)
    }
  },
  Error(e) => print(e)
}
```

Options:
- `cwd: string`
- `timeoutMs: i64`

### streamRaw()
Like `stream()`, but `stdout`/`stderr` are `channel<bytes>` chunks instead of lines.

`streamRaw(cmd, opts?)`

```ngn
const r = streamRaw("printf 'xyz'")
match (r) {
  Ok(p) => {
    const chunk = <- p.stdout
    print(chunk.toStringStrict())

    p.stdout.close()
    p.stderr.close()

    <- p.done
  },
  Error(e) => print(e)
}
```

Options:
- `cwd: string`
- `timeoutMs: i64`

## tbx::io

File I/O operations.

```ngn
import { read, write, append, exists, delete } from "tbx::io"
```

### read()
Read entire file contents as a string. Returns `Result<string, string>`.

`read(path)`

```ngn
const result = read("config.txt")
match (result) {
  Ok(content) => print(content),
  Error(e) => print("Failed: ${e}"),
}
```

### write()
Write content to a file. Creates the file if it doesn't exist, or overwrites if it does. Returns `Result<void, string>`.

`write(path, content)`

```ngn
const result = write("output.txt", "Hello, file!")
match (result) {
  Ok(v) => print("Saved!"),
  Error(e) => print("Failed: ${e}"),
}
```

### append()
Append content to a file. Creates the file if it doesn't exist. Returns `Result<void, string>`.

`append(path, content)`

```ngn
append("log.txt", "New line\n")
```

### exists()
Check if a file exists. Returns `bool`.

`exists(path)`

```ngn
if (exists("config.txt")) {
  print("Config found!")
}
```

### delete()
Delete a file. Returns `Result<void, string>`.

`delete(path)`

```ngn
delete("temp.txt")
```
