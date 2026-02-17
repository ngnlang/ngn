---
title: Basics
description: Back to it!
---

## Defining simple data

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

Aside from strings and numbers, ngn has booleans, objects, arrays, tuples, and bytes. As you can guess, there are methods for most of these data types. You can learn more about these in The Manual.

## Types

- `string`
- `i64`, `i32`, `i16`, `i8`, `u64`, `u32`, `u16`, `u8`, `f64`, `f32`
- `bool`
- `array<type>`
- `bytes`
- `void`
- `map<key_type, value_type>`
- `set<value_type>`
- `channel<type>`
- `fn<...paramN, return_type>`

### Implicit
Supported for literals and expressions, as well as inside functions.

```ngn
const thing = "one" // inferred as `string`
const answer = 42 // inferred as `i64`
const pi = 3.14 // inferred as `f64`

const result = 3 + 2 // inferred as `i64`
```

### Unions
Union types let you declare that a value may be one of several types.

```ngn
var x: string | i64 = "hello"
x = 42
```

### Aliases
Type aliases name a type expression.

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

## Printing to the console
Line logging to the console. Implicit `\n`.
```ngn
print("Hello")
print("World")
// Hello
// World
```

## Runtime errors
Use to stop execution and show a runtime error.

```ngn
print("before")
panic("Something went wrong")
print("after") // not reached
```

## String Interpolation
```ngn
const x = 5
print("x plus 1 is ${x + 1}") // x plus 1 is 6

const greeting = "world"
print("Hello, ${greeting}!") // Hello, world!

// you can escape if needed
print("hello \${x}") // hello ${x}
```

## Functions

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

## Closures
Closures are similar to functions, but have important differences:
- assign them with `const` then call like a function
- access to external values, even ones outside its environment
- use a pair of `|` to wrap params, or an empty pair if none
- param ownership transfer is the same as functions
- to mutate the value of a variable from within a closure, use `state()` to declare the variable.

1. The closure captures outside values at creation.
    ```ngn
    var count = 0

    const incrementBy = |a: i64| count + a // captures `count` at 0

    print(incrementBy(10)) // 10

    print(incrementBy(5)) // 5

    count = 100
    print(incrementBy(7)) // still 7

    const incrementCount = |a: i64| count + a // captures `count` at 100
    print(incrementCount(7)) // 107
    ```

2. You can mimic classic "close over" behavior by returning a closure from a function.
    ```ngn
    fn main() {
      var count = 10

      fn adder(c) {
        return |m| {
          return c + m
        }
      }

      const add_it = adder(count)
      // add_it becomes the returned closure from adder,
      // and the value of `c` is locked-in as 10
      // since that was the value of `count` when it was passed

      print(add_it(3)) // 13
      count = add_it(5) // sets count to 15

      const add_me = adder(count) // param `c` is 15 for this closure 
      print(add_me(5)) // 20
    }
    ```
    Or, the closed over value can be within the function. In this case, we use `state()` to declare the variable since we need to mutate it from within the closure.
    ```ngn
    fn main() {
      fn make_counter() {
        var count = state(0)
        
        return || {
          count.update(|c| c + 1)
          print(count)
        }
      }

      const counter = make_counter()
      counter()  // 1
      counter()  // 2
      counter()  // 3
    }
    ```

## Typed Objects and Composability
You can create typed objects using models, then create a new instance of a model.

### model
Create object structures.

```ngn
model Dog {
  name: string,
  breed: string
}
```

#### Optional Model Fields
Mark optional fields with `?`. Optional fields return `Maybe<T>` when accessed and can have defaults.

```ngn
model User {
  name: string,
  age?: i64 = 42,
  nickname?: string
}

fn main() {
  const user = User { name: "Alice" }

  const { age, nickname } = user
  check age? { return }
  print(age) // 42

  // Missing optional fields become Maybe::Null
  assert(!nickname)
}
```

#### Generic Models
Models can have type parameters for creating reusable container types:

```ngn
model Container<T> {
  value: T
}

model Pair<K, V> {
  key: K,
  val: V
}

fn main() {
  const intBox = Container { value: 42 }
  const strBox = Container { value: "hello" }
  const pair = Pair { key: "age", val: 25 }
}
```

#### Type Inference and Enforcement
When you instantiate a generic model, ngn infers the concrete type from the field values:

```ngn
model Box<T> {
  value: T
}

var box = Box { value: 42 }  // Inferred as Box<i64>
print(box.value)             // 42

// Type is enforced on reassignment:
box.value = 100              // ✓ OK - same type (i64)
box.value = "hello"          // ✗ Type Error: Cannot assign String to field 'value' of type I64
```

This ensures type safety even with generic types - once a type parameter is bound to a concrete type, it remains consistent.

### role
You can extend a model's functionality with groups of methods via roles. Declare one or more method signatures and/or method implementations. Use this to group methods into roles in order to define their functionality for models.

```ngn
role Animal {
  fn speak(): void
}
```

### extend
Extend a model's functionality with methods. You can implement custom methods, apply one or more roles, or a mix of both.

```ngn
// extend with custom methods
extend Dog {
  fn fetch(thing: string): bool {
    return attemptToFetch(thing)
  }
}
```

```ngn
// extend with role
extend Dog with Animal {
  fn speak(): void {
    print("Woof, woof!")
  }
}
```

Now, putting it all together:
```ngn
const my_dog = Dog {
  name: "Apollo",
  breed: "Labrador"
}
print(my_dog) // { name: Apollo, breed: Labrador }
print(my_dog.name) // Apollo

const fetched = my_dog.fetch("stick")
print(fetched) // either true or false

my_dog.speak() // Woof, woof!
```

### Alternative for instantiating models
You may also choose to create a constructor method and use it to create a new instance of a model.

```ngn
model User {
  name: string,
  age: u32
}

extend User {
  fn new(name: string, age: u32): User {
    return User { name, age }
  }
}

fn main() {
  var user = User.new("Chloe", 27)
}
```

### Mutating model data
When you create an instance of a model, it's essentially an object - although it can have methods attached to it as well.

The general rule is that you can mutate based on how the variable was declared (`var`, `const`). However, you can't change a field's type.

Here are the ways to manipulate an object's fields, based on the above example code:
- direct assignment: `user.age = 7`
- entire object: `user = { name: "Ben", age: 56 }`
- method: `user.changeName("Stacie")`
- by `const`, `global` variables: ❌ not allowed, as these are all strictly immutable

## this
There's no need to fear `this` in ngn. It's an implicit reference to the instance that a method is called on.

For models, it gives you access to the instance's fields and other methods.
```ngn
model User {
  name: string,
  age: u32
}

extend User {
  fn greet(): string {
    print("Hello, I'm {this.name}")
  }

  fn changeName(name: string): void {
    this.name = name
  }
}

var user = User { name: "Jason", age: 47 }
user.greet()  // "Hello, I'm Jason"
```
For custom type methods, it gives you access to the type's value.
```ngn
extend string {
  fn isBlank(): bool {
    return this.trim().length() == 0
  }
}

const name = ""
print(name.isBlank()) // true
```

## Null Coalescing Operator

Returns the left-hand side if non-null, otherwise returns the right-hand side. If there's a `Maybe::Value<T>`, it's automatically unwrapped.

```ngn
const u = getUser() // Maybe<User>
const user = u ?? "anonymous"
```

## Logical Operators

ngn supports industry-standard short-circuit boolean operators:

- `&&`: logical AND
- `||`: logical OR

```ngn
fn main() {
  const a = 15
  if (a > 10 && a < 20) print("in range")

  const ok = true || false
  assert(ok == true)
}
```

Precedence:

- `&&` binds tighter than `||`
- both bind tighter than `??` (null coalescing)

Notes on `|`:

- `|` is still used for union types: `type IntOrString = i64 | string`
- `|` is still used in `match` arms to match multiple patterns: `2 | 3 => ...`

## Control Flow

### if
Run a statement based on if a condition is true.

For blocks with only a single statement, you can use the following syntax:
```ngn
if (condtion) statement : (condition) statement : statement

if (condition)
  statement
: (condition)
  statement
: statement
```

The below syntax is required if any of your blocks have multiple statements. Note the first brace comes directly after the `if` keyword.
```ngn
if {
  (condition)
    statement
    statement
  : (condition)
    statement
  :
    statement
}
```

### match
Match a value against one test case; optionally, provide a default case.

If a match is found:
- that branch's statement block is run.
- other cases are skipped, including the default, unless a matched statement block contains the `next` keyword. `next` will only try to match the very next case.

```ngn
const value = 3
match (value) {
  1 => statement,
  2 | 3 => statement, // matches 2 or 3
  4 => {
    statement
    statement
    next
  }, // matches 4, runs both statements, then tries to match the next case
  _ => statement // matches any other value
}
```

## Loops

### loop
Run the statement block indefinitely. Use `break` to exit the loop.
```ngn
loop {
  statement
  statement
}
```

### while
Run the statement block while the condition is true. Not guaranteed to run at all.
```ngn
while (condition) {
  statement
  statement
}
```

Can be inlined if only using a single statement.
```ngn
while (condition) statement
```

#### once variant
To always run the statement block once, before checking the condition.
```ngn
while once (condition) {
  statement
}
```

### for
Run a statement block for each message in a channel or items in a collection.

> Arrays have an `each` method, so you don't need to use a for loop with them unless you want to.

```ngn
for (msg in <-? channel) {
  print(msg)
}

for (item in items) {
  print(item)
}
```

## Enums

ngn provides two built-in enums for common patterns: `Result` and `Maybe`

### Result

`Result<T, E>` represents an operation that can either succeed or fail.

#### Variants

- `Ok(T)` — The operation succeeded with a value of type `T`
- `Error(E)` — The operation failed with an error of type `E`

#### Examples

```ngn
fn divide(a: i64, b: i64): Result<i64, string> {
  if b == 0 return Error("Division by zero not allowed")
  return Ok(a / b)
}

fn main() {
  const result = divide(10, 2)
  match (result) {
    Ok(value) => print("Ok: {value}"), // Ok: 5
    Error(msg) => print("Error: {msg}"), // Error: Division by zero not allowed
  }
}
```

### Maybe<T>
`Maybe` represents a value that may or may not exist. You can write `Maybe<T>` or use the shorthand `T?` syntax.

#### Variants
- `Value(T)` — The value exists
- `Null` (or `null`) — The value does not exist

#### Type syntax
```ngn
// These are equivalent:
var x: Maybe<i64> = null
var y: i64? = null

// Function return types:
fn find(id: i64): i64? {    // Same as Maybe<i64>
  if (id == 1) return Value(100)
  return null
}

// Complex types:
var arr: array<string>? = null  // Optional array
```

#### Examples
```ngn
fn findUser(id: u64): Maybe<string> {
  if (id == 1) return Value("Jason")
  if (id == 2) return Value("Brad")
  return Null
}

const user1 = findUser(1)
const user2 = findUser(99)

match (user1) {
  Value(name) => print("Found: {name}"), // matches
  Null => print("User not found"),
}

match (user2) {
  Value(name) => print("Found: {name}"),
  Null => print("User not found"), // matches
}
```

### Custom Enums

You can define your own enums for domain-specific types.

```ngn
enum Color { Red, Green, Blue }

enum Status {
  Active,
  Inactive(string)  // With associated data
}

fn main() {
  const color = Red
  print(color)  // Color::Red
  
  const status = Inactive("maintenance")
  print(status)  // Status::Inactive (maintenance)

  match (status) {
    Active => print("Status: Active!"),
    Inactive(value) => print("Status: Inactive with reason, {value}")
  }
}
```

### Generic Enums

Custom enums can also have generic type parameters:
```ngn
enum Option<T> {
  Some(T),
  None
}

fn main() {
  const value = Some(42)  // Inferred as Option<i64>
  
  match (value) {
    Some(v) => print("Got: {v}"),  // v has type i64
    None => print("Got nothing")
  }
}
```

When you use `Some(42)`, ngn infers that this is an `Option<i64>`. In match patterns, bindings like `v` in `Some(v)` are given the concrete type (i64), not the type parameter (T).
