---
title: Types
description: Primitive types, unions, and common patterns.
---

ngn has a compact, explicit type system with helpful inference for literals.

## Primitive types

- `string`
- `i64`, `i32`, `i16`, `i8`, `u64`, `u32`, `u16`, `u8`, `f64`, `f32`
- `bool`
- `bytes`
- `void`

## Collections and functions

- `array<type>`
- `map<key_type, value_type>`
- `set<value_type>`
- `channel<type>`
- `fn<...paramN, return_type>`

## Union types

```ngn
var x: string | i64 = "hello"
x = 42

const ch = channel<string | i64>()
ch <- "a"
ch <- 2
```

## Maybe and Result

```ngn
fn find(id: i64): i64? {
  if (id == 1) return Value(100)
  return null // syntactic sugar for Null
}

fn divide(a: i64, b: i64): Result<i64, string> {
  if b == 0 return Error("Division by zero")
  return Ok(a / b)
}
```
