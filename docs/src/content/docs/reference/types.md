---
title: Types
description: Primitive types, unions, and common patterns.
---

## Primitive types

`i64` is the default type for numbers.

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
- `fn<...paramN, return_type>` - also used for closures

## Union types

```ngn
var x: string | i64 = "hello"
x = 42

const ch = channel<string | i64>()
ch <- "a"
ch <- 2
```

## Maybe and Result enums

```ngn
// returns a Maybe<i64>, which can be either Maybe::Value<T> or Maybe::Null
fn find(id: i64): i64? {
  if (id == 1) return Value(100)
  return null // syntactic sugar for Null
}

// returns either a Result::Ok<T> or Result::Error<T>
fn divide(a: i64, b: i64): Result<i64, string> {
  if b == 0 return Error("Division by zero")
  return Ok(a / b)
}
```
