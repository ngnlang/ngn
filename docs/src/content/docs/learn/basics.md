---
title: Basics
description: Back to it!
---

### Defining simple data

```ngn
global VERSION = "1.0.0" // immutable

fn main() {
  var name = "Alice"      // mutable
  const age = 30          // immutable
  name = "Bob"            // ✅ works
  // age = 31             // ❌ error: immutable
}
```

Notice we wrap most of the above code in the `main` function. The entrypoint file of your program must have this, unless you're creating an API server.

### Functions

Types for params must be set explcitly. The same is true for the return type, unless you're not returning anything - i.e. you don't have to put `: void` unless you want to.

```ngn
// Traditional
fn add(a: i64, b: i64): i64 {
  return a + b
}

// Implicit return
fn multiply(a: i64, b: i64): i64 a * b

// Side-effects
fn say(thing: string) {
  print(thing)
}

fn main() {
  print(add(2, 3))       // 5
  print(multiply(4, 5))  // 20
}
```

Notice that you can define other functions at the top-level of a file, besides `main()`. Any of these other top-level functions can be called or referenced from anywhere within `main()` - even nested functions.

Functions are mostly isolated environments; meaning you can't reference or use outside data declared with `var` or `const`. But you can access things defined at the top-level of a file, aka "globals".

```ngn
global VERSION = "1.0.0"

fn add(a: i64, b: i64): i64 {
  return a + b
}

fn main() {
  const greeting = "Say hello to my little friend."

  fn subtract(a: i64, b: i64): i64 {
    return a - b
  }

  fn mock() {
    print(greeting) // ❌ error: access not allowed

    // but you can access data defined with `global`
    print(VERSION)

    // and you can access top-level functions
    const added = add(4, 5)

    // you can also access sibling functions
    const subtracted = subtract(10, 2)
  }

  mock()
}
```
