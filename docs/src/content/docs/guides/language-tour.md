---
title: Language Tour
description: A quick walk through ngn syntax and core features.
---

This guide is a fast, tutorial-style tour. Copy sections into `main.ngn` and run them as you go.

## Variables

```ngn
fn main() {
  var name = "Thor"      // mutable
  name = "Point Break"   // ok

  const species = Asgardian // immutable
  // species = Frost Giant  // error: immutable
}
```

## Functions

```ngn
fn add(a: i64, b: i64): i64 {
  return a + b
}

// implicit return
fn multiply(a: i64, b: i64): i64 a * b

fn main() {
  print(add(2, 3))
  print(multiply(4, 5))
}
```

## Types and unions

```ngn
var x: string | i64 = "answer"
x = 42

const ch = channel<string | i64>()
ch <- "a"
ch <- 1
```

## Control flow

```ngn
fn main() {
  const score = 87

  if (score >= 90) print("A") : (score >= 80) print("B") : print("C")

  match (score) {
    100 => print("perfect"),
    90..95 => print("great"),
    _ => print("keep going")
  }
}
```

## Data structures

```ngn
fn main() {
  const tags = ["api", "ai", "lang"]
  const point = (10, 20) // tuple
  const person = { name: "Rae", age: 29 }

  print(tags.size())
  print(point[0])
  print(person.name)
}
```
