---
title: Types
description: Primitive types, unions, and common patterns.
---

## Primitive types

- `string`
- `i64`, `i32`, `i16`, `i8`, `u64`, `u32`, `u16`, `u8`, `f64`, `f32`
- `bool`
- `bytes`
- `void`

`i64` is the default type for numbers.

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

## Type aliases

```ngn
type UserId = i64
type Token = string | i64

type Token2 = Token

fn main() {
  var id: UserId = 123
  var t: Token = "abc"
  t = 99
}
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

## Custom Type Methods

If you want to add methods to built-in types, you can use the `extend` keyword. This feature applies to following types:

- number (generic that applies to all numeric types)
- f64, i32, u8, etc (for specific numeric types)
- string
- bool
- array
- tuple
- map
- set

```ngn
extend array {
  fn isEmpty(): bool {
    return this.size() == 0
  }
}

extend number {
  fn isEven(): bool {
    return this % 2 == 0
  }

  fn double(): number {
    return this * 2
  }
}

extend string {
  fn isBlank(): bool {
    return this.trim().length() == 0
  }
}

fn main() {
  [1, 2, 3].isEmpty() // false

  // if using a number directly, wrap in parenthesis
  (2).isEven() // true

  const x = 2
  x.isEven() // true

  // if a number method returns the generic `number` type, you should explicitly set the result type
  const y: i32 = x.double()

  "   ".isBlank() // true
}
```
