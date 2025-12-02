# ngn

Pronounced "engine".

An expressive and easy to use programming language.

## Status

Extremely early development.

## `main()`

Your entrypoint file must define a `main()` function. It's found and run automatically. Most of your code will live inside of this function, but not everything.

## Declaring identifiers

| example | scope | binding | value | ownership |
|-------|-------|-------|-------|-------|
| `var z = "world"` | local | mutable | mutable | owned |
| `const status = "go"` | local | immutable | immutable | owned |
| `static DATA = [1..=1000]` | global | immutable | immutable | borrowed |

> The `static` example uses pseudocode to mimic creating an array of numbers from 1 to 1000, inclusively.

### `var`

```ngn
var x = "hello" // declares `x` as an owned string
x = "goodbye" ✅ // value is mutable since it's owned
```
#### Owned params
Marking a param as owned is necessary if you want to mutate the param within the function. Despite `const` being owned (since its data _could_ live on the heap), remember that it's not mutable. Marking a param as needing to be owned also moves ownership of the var or const into the function - which means it's no longer available after the function is called.

```ngn
var x = "hello" // owned string

fn doThing(thing: <string) { // the `<` means it requires an owned string
  // do thing
}

doThing(x) ✅ // moves ownership of `x` to the function

print(x) ❌ // `x` is no longer available, since it's ownership was moved
```

Instantiate a model with `var` if you want to mutate fields. But even though it's owned, you can still pass it to a function that doesn't have an owned param (doesn't mutate). In this case, we "downgrade" what you pass, so that it doesn't transfer ownership.
```ngn
model User {
  name: string,
  role: string
}

var user = User {
  name: "Sam",
  role: "Developer"
  years: 3
}

fn readUser(u: User) { // only require a borrowed User
  // take a read action on the reference to `u`; cannot mutate
}

// Can pass a variable to a function that expects a borrowed param;
// ngn ensures it's downgraded to "borrowed" within the function
// and doesn't transfer ownership.
readUser(user) ✅

print(user) ✅ // can still do things with `user` here
```

### `const`
Data could live on the heap, if only known at runtime, or in binary if it's a literal. You cannot mutate the data - even for objects and arrays.

```ngn
const x = "hello" // owned, even if ngn stores it in binary (which doesn't need memory cleanup)
x = "goodbye" ❌ // value is immutable
```

### `static`

Used for global declarations, which can only exist at the top-level of a file, not inside functions.

```ngn
static VERSION = "v3" // inlined at compile time

// pseudocode to create an array of numbers from 1 to 1000
static DATA = [1..=1000] // stored in binary, not inlined, since data is large

fn main() {
  print(VERSION) // v3
  print(DATA) // (I'll spare you; you get it.)
}
```

## Types

- `string`
- `i64`, `i32`, `u64`, `u32`, `f64`, `f32`
- `boolean`
- `array`
- `array<type>`
- `void`

### explicit
```ngn
const thing: string = "one"
var answer: u64 = 42
var truth: boolean = false
const things: array = [1, 2, 3]
const stuff: array<string> = ["shirt", "hat", "coat"]

fn sideEffects(): void {
  // do something
}
```

### implicit
Supported for literals and expressions, as well as inside functions (requires explict types for fn params and return).

```ngn
const thing = "one" // inferred as `string`
const answer = 42 // inferred as `i64`
const pi = 3.14 // inferred as `f64`

const result = 3 + 2 // inferred as `i64`

fn add(a: i32, b: i32): i32 {
  return a + b
}
```

## `echo`
Log to the console, without formatting.
```ngn
const name = "ngn"
echo(name)
// ngn

echo("Hello")
echo("World")
// HelloWorld
```

## `print`
Line logging to the console. Implicit `\n`.
```ngn
print("Hello")
print("World")
// Hello
// World
```

## String Interpolation
```ngn
const x = 5
print("x plus 1 is {x + 1}") // x plus 1 is 6

const greeting = "world"
print("Hello, {greeting}!") // Hello, world!

// you can escape the first brace if you need to actually print them
print("hello \{x}") // hello {x}
```

## Strings

### `length()`
Return the length of a string.

### `index(pattern, start?)`
Search a string for a given pattern, and return the index number of the first instance found. If no pattern is found, returns `-1`. You can pass an optional start index.

```ngn
const sent = "I learned to draw today."
const ind = sent.index("to") // 10
```

### `includes(pattern)`
Determine if a string includes a given pattern. Returns a boolean.

```ngn
const weather = "sunny"
const inc = weather.includes("sun") // true
```

### `starts(pattern)`
Determine if a string starts with a given pattern. Returns a boolean.

```ngn
var process = "complete"
const beg = process.starts("c") // true
```

### `ends(pattern)`
Determine if a string ends with a given pattern. Returns a boolean.

```ngn
var process = "working"
const end = process.ends("ing") // true
```

### `split(pattern?)`
Create an array of strings by splitting on a pattern of characters within a string. If you do not pass a pattern, each character in the string is split individually. Preserves the original string.

```ngn
const sent = "What. On. Earth."
const split_sent = sent.split(".") // ["What", " On", " Earth", ""]

var greeting = "Hello"
const split_greeting = greeting.split() // ["H", "e", "l", "l", "o"]
```

### `replace(search, replacement)`
Replace a pattern with a string. `search` can be a string or a RegEx; but if a string is passed, only the first occurrence is replaced. Preserves the original string and returns a new one.

```ngn
var plain = "Forge ahead"
const fancy = plain.replace("a", "@") // "Forge @head"
```

```ngn
var plain = "Forge ahead"
const fancy = plain.replace(/a/g, "@") // "Forge @he@d"
```

### `copy(start?, stop?)`
Copies an entire string or a section of it, based on indices. This does not change the string you copied from, but returns the copied value as a new string.

- If `start` is provided but `stop` is not, it copies everything upto and including the end of the string.
- If `stop` is provided (implies `start`), the copy excludes the item at that index.
- If neither is provided, the entire string is copied.

```
const some = "Some Stuff"
const copied = some.copy(5)

print(copied) // "Stuff"
print(some) // "Some Stuff"

var all = some.copy()

print(all) // "Some Stuff"
print(some) // "Some Stuff"
```

### `slice(start, stop?)`
Remove a section of a string by providing a start index and an optional stop index. This changes the original string and returns the sliced section as a new string.

- If `stop` is provided, the slice excludes the item at that index.
- If `stop` is not provided, it removes everything upto and including the last item.
- Since you're mutating the original string, it must be owned.
```
var quote = "I flew too close to the sun on wings of pastrami."
const sliced = orig.slice(24, 31)

print(orig) // I flew too close to the wings of pastrami.
print(sliced) // "sun on "
```

### `upper()`
Transform a string to all uppercase; preserves original string.

```ngn
var version = "one"
print(version.upper()) // ONE
```

### `lower()`
Transform a string to all lowercase; preserves original string.

```ngn
var version = "ONE"
print(version.lower()) // one
```

### `trim()`
Remove whitespace from both ends of a string; preserves original string.

```ngn
var = thing = " strong "
print(thing.trim()) // "strong"
```

### `repeat(num)`
Repeat a string some number of times.

```ngn
const ending = "goodbye"
print(greeting.repeat(2)) // goodbyegoodbye
```

## Numbers
These will be moved into a math module.

### `abs(number)`
Get the absolute value of a number.
```ngn
print((-5).abs()) // 5
```

### `round(number)`
Round a number to the nearest integer.
```ngn
print((3.7).round()) // 4
print((5.5).round()) // 6
```

### `ceil(number)`
Round a number up, no matter the decimal value.
```ngn
print((7.6).ceil()) // 8
print((3.2).ceil())  // 4
```

### `floor(number)`
Round a number down, no matter the decimal value.
```ngn
print((1.3).floor()) // 1
print((3.7).floor()) // 3
```

## Arrays
If you want to mutate arrays, be sure to declare them with `var`

```ngn
var stuff = ["hat", "coat", "gloves"]
const ages = [3, 8, 15, 23]

const mixed = ["hat", true, 7] ❌ // cannot mix types
```

### `size()`
Return the size of the array.

### `push(item, index?)`
Push, i.e. add, an item into an array. By default, it pushes at the end. To push into another location, provide the index number. Returns the new size of the array as an `i64`.
```
var stuff = ["guitar", "shirt"]
const size = stuff.push("hat")

print(size) // 3
print(stuff) // ["guitar", "shirt", "hat"]

stuff.push("coat", 0)
print(stuff) // ["coat", "guitar", "shirt", "hat"]
```

### `pull(index?)`
Pull, i.e. remove, an item from an array. By default, it removes from the end. To pull from another location, provide the index number. Returns the removed item's value.
```
var stuff = ["coat", "guitar", "shirt", "hat"]
const pulled = stuff.pull()

print(pulled) // hat
print(stuff) // ["coat", "guitar", "shirt"]

const pulled_one = stuff.pull(1)

print(pulled_one) // ["guitar"]
print stuff // ["coat", "shirt", "hat"]
```

### `copy(start?, stop?)`
Copies an entire array or a section of it, based on indices. This does not change the array you copied from, but returns the copied items as a new array.

- If `start` is provided but `stop` is not, it copies everything upto and including the last item.
- If `stop` is provided (implies `start`), the copy excludes the item at that index.
- If neither is provided, the entire array is copied.

```
const stuff = [10, 20, 30, 40, 50] // borrowed
const copied = stuff.copy(3) // you can copy borrowed arrays

print(copied) // [40, 50]
print(stuff) // [10, 20, 30, 40, 50]

var all = stuff.copy()

print(all) // [10, 20, 30, 40, 50]
print(stuff) // [10, 20, 30, 40, 50]
```

### `slice(start, stop?)`
Remove a section of the array by providing a start index and an optional stop index. This changes the array and returns the item(s) as a new array.

- If `stop` is provided, the slice excludes the item at that index.
- If `stop` is not provided, it removes everything upto and including the last item.
```
var stuff = [10, 20, 30, 40, 50]
const sliced = stuff.slice(1, 3)

print(sliced) = [20, 30]
print(stuff) // [10, 40, 50]
```

### `splice(item[], start?)`
Add multiple items to an array; optionally, at a specific index. Returns the new size of the array.

- If `start` is not provided, it adds the items at the end.
```
var stuff = [10, 20, 30]
stuff.splice([40, 50]) // [10, 20, 30, 40, 50]

const size = stuff.splice([45, 47], 4)

print(stuff) // [10, 20, 30, 40, 45, 47, 50]
print(size) // 7
```

## Enums

ngn provides two built-in enums for common patterns: `Result` and `Maybe`

### Result<T, E>

`Result` represents an operation that can either succeed or fail.

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

`Maybe` represents a value that may or may not exist.

#### Variants

- `Value(T)` — The value exists
- `Null` — The value does not exist

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

## `while`
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

### `once` variant
To always run the statement block once, before checking the condition.
```ngn
while once (condition) {
  statement
}
```

## `until`
Run the statement block until the condition is true. Not guaranteed to run at all.
```ngn
until (condition) {
  statement
  statement
}
```

Can be inlined if only using a single statement.
```ngn
until (condition) statement
```

### `once` variant
To always run the statement block once, before checking the condition.
```ngn
until once (condition) {
  statement
}
```

## `if`
Run a statement based on if a condition is true.

The full syntax is a bit out of the ordinary, but helps reduce boilerplate "else if" and "else" wording, along with the extra braces. This style is required if any of your blocks have multiple statements.
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

For single statement blocks, you can drop the braces and go inline; or even go multiline if desired.
```ngn
if (condtion) statement : (condition) statement : statement

if (condition)
  statement
: (condition)
  statement
: statement
```

### `not` variant
If you're concerned about code readability in certain situations, you can use the `not` operator in place of `!`.

So, instead of
```ngn
if (!browser) print("not browser")
```
you can do
```ngn
if not (browser) print("not browser")

// or

if {
  not (browser) print("not browser")
  : not (thing) print("not it either")
  : print("something else")
}
```

## `match`
Match a value against one test case; optionally, provide a default case.

If a match is found:
- that branch's statement block is run.
- other cases are skipped, including the default, unless a matched statement block contains the `next` keyword.

```
const value = 3
match (value) {
  1 => statement,
  2 | 3 => statement,
  4 => {
    statement
    statement
    next
  },
  => statement
}
```

### `any` variant
Even if a match is found, continue checking cases unless a matched statement block contains the `break` keyword.

```
match any (value) {
  test => statement,
  test1 | test2 => {
    statement
    statement
    break
  },
  => statement
}
```

## `fn`
You can write functions in various ways; along with passing them as arguments to other functions.

### Traditional block, explicit return
```ngn
fn add(a, b) {
  return a + b
}
```

### Traditional block, explicit multiline return
```ngn
fn add(a, b) {
  return (
    a + 
    b
  )
}
```

### Implicit return
```ngn
fn add(a, b) a + b
```

### Side-effects only
```ngn
fn doThing() {
  print("something")
}
```

## Closures
Closures are similar to functions, but you can assign them to an identifier, then call it like a function. You can define any params within a pair of pipes, or have an empty set of pipes if not using params.
```ngn
const add = |a: i64, b: i64| a + b

const sum = add(3, 4)

const doThing = || {
  // do thing!
  print("Hello")
}
doThing()
```

Unlike functions, closures give you access to the state of their surrounding scope.
```ngn
const base = 10

const add_base = |a: i64| base + a
print(add_base(3)) // 13
```

The value within the closure can be kept in sync with the outside environment when using vars.
```ngn
var count = 0

const incrementBy = |a: i64| count + a // calculate and return

print(incrementBy(10)) // 10

print(incrementBy(5)) // 5

count = 100
print(incrementBy(7)) // 107, not 7
```

The closure can even mutate the outside var, if you'd like.
```ngn
var count = 0

const incrementBy = |a: i64| count = count + a // calculate, mutate, return

incrementBy(10) // we're not capturing the returned value here, but you could
print(count) // 10

incrementBy(5)
print(count) // 15

count = 100
incrementBy(7)
print(count) // 107
```

## Objects and Composability
You can create typed objects using models, then create a new instance of a model.

### `model`
Create object structures.

```ngn
model Dog {
  name: string,
  breed: string
}
```

You can also extend a model's functionality with direct methods or groups of methods via roles.
### `role`
Declare one or more method signatures and/or method implementations. Group methods into roles in order to implement their functionality for models.

```ngn
role Animal {
  fn speak(): void
}
```

### `extend`
Extend a model's functionality with methods. You can implement custom methods, apply one or more roles, or a mix of both.

```ngn
// extend with custom methods
extend Dog with {
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
const dog = Dog {
  name: "Apollo",
  breed: "Labrador"
}
print(dog) // { name: Apollo, breed: Labrador }
print(dog.name) // Apollo

const fetched = dog.fetch("stick")
print(fetched) // either true or false

dog.speak() // Woof, woof!
```

### Alternative for instantiating models
You may also choose to create a constructor method and use it to create a new instance of a model.

```ngn
model User {
  name: string,
  age: u32
}

extend User with {
  fn new(name: string, age: u32): User {
    return User { name, age }
  }
}

fn main() {
  var user = User.new("Chloe", 27)
}
```

## `this`
There's no need to fear `this` in ngn. It's an implicit reference to the instance that a method is called on. It gives you access to the instance's fields and other methods.

```ngn
model User {
  name: string,
  age: u32
}

extend User with {
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

### Mutating "object" data
When you create an instance of a model, it's essentially an object - although it can have methods attached to it as well.

The general rule is that you can mutate based on how the variable was declared (`var`, `const`). However, you can't change a field's type.

Here are the ways to manipulate an object's fields, based on the above example code:
- direct assignment, by `var`s: `user.age = 7`
- entire object, by `var`s: `user = { name: "Ben", age: 56 }`
- method, by `var`s: `user.changeName("Stacie")`
- by `const`, `static` variables: ❌ not allowed, as these are all strictly immutable

## Channels
Send and receive data. You must provide a data type for the channel.

```ngn
fn main() {
    // You can only assign channels with "const"
    const c = channel(): string

    // Send a message
    c <- "first"

    // Close the channel
    c.close()

    // Assign channel output to a variable
    // Receiving "first" will still work here, because of buffering
    const msg = <- c
    print("Received: {msg}")

    // This should FAIL because the channel is closed and empty.
    const fail = <- c
}
```

Channels can even contain other channels, and you can send/receive data within those inner channels.
```ngn
fn main() {
    const request_line = channel(): channel<string>

    // (See next section for details on threads)
    thread(|| {
        // Thread waits for inbound data on the request_line channel,
        // which happens to be another channel that we assign to a constant.
        const reply_channel = <- request_line
        
        // Reply back on the private channel
        reply_channel <- "Your order is ready!"
    })

    // Create a private response channel
    const private_channel = channel(): string
    
    // Send private channel, which the worker is waiting for
    request_line <- private_channel
    
    // Wait for the private reply
    print(<- private_channel)
}
```

You can even send a closure to a channel:
```ngn
fn main() {
    const job_queue = channel(): fn
    const done = channel(): bool

    thread(|| {
        print("Worker started")
        while (true) {
            match (<-? job_queue) {
                Value(task) => task(42), 
                Null => break
            }
        }
        print("Worker finished")
        
        done <- true
    })

    job_queue <- |n: i64| { print("Task A executing with {n}") }
    
    job_queue <- |n: i64| { 
        const res = n * 2
        print("Task B executing: {n} * 2 = {res}") 
    }
    
    job_queue.close()
    print("Jobs sent")
    
    <- done
    print("Main exiting")
}
```

## Threads - Concurrency and Parallelism
Allows you to do async work while, optionally, continuing to do work in the main thread. Threads take a closure with no parameters, but have access to their surrounding scope.

Standalone threads are risky because as soon as the main program ends, all unfinished threads are killed. In the below example, `setData(value)` may never finish.
```ngn
fn main() {
    const value = 100

    thread(|| {
        setData(value)
    })

    // Continue doing other work while the thread runs
    print(value)
}
```
In such cases, use a channel to await the thread.
```ngn
fn main() {
    const value = 100
    const done = channel(): bool

    thread(|| {
        setData(value)
        done <- true
    })

    // Continue doing other work while the thread runs
    print("Value: {value}")

    // Now wait until we receive a message, indicating thread work is done
    <- done
}
```
Threads may run in parallel or sequentially unordered, but you can control the order in which you wait on their results.
```ngn
fn main() {
    print("1. Main started")
    
    // Create a channel for each thread
    const c = channel(): Result<string, string>
    const d = channel(): Result<string, string>
    
    // Create a thread, to do some async work
    thread(|| {
        print("  2c. Thread c started (sleeping...)")
        sleep(2000)
        
        print("  3c. Thread c sending message")
        const result = Ok("Hello from channel c!")
        c <- result

        print("  4c. Thread c finished")
    })

    // Create a thread, to do some async work
    thread(|| {
        print("  2d. Thread d started (sleeping...)")
        sleep(2000)
        
        print("  3d. Thread d sending message")

        d <- Error("Oh this is bad channel d!")

        print("  4d. Thread d finished")
    })
    
    print("5. Main doing other work while thread runs...")

    // This should block until the "c" thread sends a message
    const msgc = <- c
    print("6c. Main received, from thread c: {msgc}")

    // This should block until the "d" thread sends a message
    const msgd = <- d
    print("6d. Main received, from thread d: {msgd}")
}
```

If you're unsure how much data is coming, use a `while` loop, and then close the channel at the end of the input in order to indicate there are no more messages coming. Here, we're simulating "unknown" amounts of data.

```ngn
fn main() {
    const c = channel(): string

    thread(|| {
        c <- "A"
        c <- "B"
        c <- "C"
        c.close()
    })

    // Loop forever until break
    // because we don't know how much data is coming
    while (true) {
        // adding the ? means we might receive data
        match (<-? c) {
            Value(msg) => print("Got: {msg}"),
            Null => {
                print("Channel closed. Exiting loop.")
                break
            },
        }
    }
    
    print("Done")
}
```

### Shared, mutable state
It's safe to sequentially mutate shared data outside of threads or within a single thread. However, if more than one thread can mutate shared data, use `state()` to declare the variable. This gives you safe, atomic operations within your threads by using state variable methods.
```ngn
fn main() {
    var counter = state(0)
    const done = channel(): bool
    
    thread(|| {
        // Pass a closure that mutates the data.
        // The closure receives the current value of the state variable via a param.
        counter.update(|n| n + 10)
        print("added 10")
        done <- true
    })
    
    thread(|| {
        counter.update(|n| n + 5)
        print("added 5")
        done <- true
    })
    
    <-done
    <-done
    print(counter)  // Always 15
}
```
If needed, you also have access to these variable methods when using `state()`:
- `.get()`, gets the current value
- `.set()`, sets the current value - which replaces the existing one. Be careful, as it can be tricky to ensure proper mutation order when coupled with `.update()`.
