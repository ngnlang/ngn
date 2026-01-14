# ngn

Pronounced "engine".

An expressive and easy to use high-level programming language.

## Status

Make it work - extremely early development.

## `main()`

Your entrypoint file must define a `main()` function. It's found and run automatically. Most of your code will live inside of this function, but not everything.

## Declaring identifiers

| example | scope | value |
|-------|-------|-------|
| `var z = "world"` | local | mutable |
| `const status = "go"` | local | immutable |
| `static DATA = [1, 2, 3, 4, 5]` | global | immutable |

### `var`
Defines a variable who's value can be changed.

```ngn
var x = "hello"
x = "goodbye" ✅
```

### `const`
Defines a constant who's value cannot be changed.

```ngn
const x = "hello"
x = "goodbye" ❌ // value is immutable
```

### `static`
Used for global declarations, which can only exist at the top-level of a file, not inside functions.

- usually inlined at compile time
- strings not inlined if longer than 32 bytes
- arrays and tuples not inlined if size is greater than 4 items or if any item is not a primitive type

```ngn
static VERSION = "v3" // inlined at compile time
static DATA = [1, 2, 3, 4, 5] // not inlined

fn main() {
  print(VERSION)
  print(DATA)
}
```

## Types

- `string`
- `i64`, `i32`, `i16`, `i8`, `u64`, `u32`, `u16`, `u8`, `f64`, `f32`
- `bool`
- `array<type>`
- `void`
- `map<key_type, value_type>`
- `set<value_type>`
- `channel<type>`
- `fn<...paramN, return_type>`

### explicit
```ngn
const thing: string = "one"
var answer: u64 = 42
var truth: bool = false
const things: array<i64> = [1, 2, 3]
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
print("x plus 1 is ${x + 1}") // x plus 1 is 6

const greeting = "world"
print("Hello, ${greeting}!") // Hello, world!

// you can escape if needed
print("hello \${x}") // hello ${x}
```

## json

### `parse()`
You can parse a JSON string or an array.
```ngn
const data = json.parse('{"name": "ngn"}')
print(data.name) // ngn
```

### `stringify()`
You can stringify an object or an array.
```ngn
const data = { name: "ngn" }
const str = json.stringify(data)
print(str) // {"name": "ngn"}
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
Determine if a string includes a given pattern. Returns a bool.

```ngn
const weather = "sunny"
const inc = weather.includes("sun") // true
```

### `starts(pattern)`
Determine if a string starts with a given pattern. Returns a bool.

```ngn
var process = "complete"
const beg = process.starts("c") // true
```

### `ends(pattern)`
Determine if a string ends with a given pattern. Returns a bool.

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
- Since you're mutating the original string, it must be declared with `var`.
```
var quote = "I flew too close to the sun on wings of pastrami."
const sliced = quote.slice(24, 31)

print(orig) // I flew too close to the wings of pastrami.
print(sliced) // "sun on "
```

### `upper()`
Transform a string to all uppercase, returning a new string. Preserves original string.

```ngn
const version = "one"
print(version.upper()) // ONE
```

### `lower()`
Transform a string to all lowercase, returning a new string. Preserves original string.

```ngn
var version = "ONE"
print(version.lower()) // one
```

### `trim()`
Remove whitespace from both ends of a string, returning a new string. Preserves original string.

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
There are currently no number methods, but we do have a math mod (see [below](#standard-library)), or you can use the `extend` keyword to add your own (see [below](#custom-methods)).

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

### `pop(index?)`
Pop, i.e. remove, an item from an array. By default, it removes from the end. To pop from another location, provide the index number. Returns the removed item's value.
```
var stuff = ["coat", "guitar", "shirt", "hat"]
const popped = stuff.pop()

print(popped) // hat
print(stuff) // ["coat", "guitar", "shirt"]

const popped_one = stuff.pop(1)

print(popped_one) // ["guitar"]
print stuff // ["coat", "shirt"]
```

### `copy(start?, stop?)`
Copies an entire array or a section of it, based on indices. This does not change the array you copied from, but returns the copied items as a new array.

- If `start` is provided but `stop` is not, it copies everything upto and including the last item.
- If `stop` is provided (implies `start`), the copy excludes the item at that index.
- If neither is provided, the entire array is copied.

```
const stuff = [10, 20, 30, 40, 50]
const copied = stuff.copy(3)

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

### `each(|item, index| {})`
For each item in an array, execute a closure.

```ngn
var things = ["hat", "gloves", "coat"]

things.each(|t, i| {
  print("{i}: {t}")
})
```

## Tuples
Similar to arrays, but can contain mixed types. However, they are fixed-size and immutable.

```ngn
const point = (10, 20)

// they can be indexed like arrays
const x = point[0] // 10
const y = point[1] // 20

const tup = (10, "hello", true)
```

### `size()`
Return the size of the tuple.

### `includes(item)`
Check if a tuple contains a specific item.

```ngn
const tup = (10, "hello", true)
const has_hello = tup.includes("hello")

print(has_hello) // true
```

### `index(item)`
Search a tuple for a given item, and return the index number of the first instance found. If no item is found, returns `-1`.

```ngn
const tup = (10, "hello", true)
const ind = tup.index("hello") // 1
```

### `copy(start?, stop?)`
Copies an entire tuple or a section of it, based on indices. This does not change the tuple you copied from, but returns the copied items as a new tuple.

- If `start` is provided but `stop` is not, it copies everything upto and including the last item.
- If `stop` is provided (implies `start`), the copy excludes the item at that index.
- If neither is provided, the entire tuple is copied.

```ngn
const tup = (10, "hello", true)
const copied = tup.copy(1)

print(copied) // ("hello", true)
print(tup) // (10, "hello", true)
```

### `toArray()`
Convert a tuple to an array. Items must be of the same type.

```ngn
const tup = (10, 20, 30)
const arr = tup.toArray()

print(arr) // [10, 20, 30]
```

### `join(delimiter)`
Join a tuple into a string, separated by a given delimiter.

```ngn
const tup = (10, 20, 30)
const joined = tup.join(",")

print(joined) // "10,20,30"
```

## Objects
You can create raw objects using the `{}` syntax and access their properties using the dot notation.
```ngn
const person = {
  name: "John",
  age: 30,
  isStudent: false
}

print(person.name) // John
print(person.age) // 30
print(person.isStudent) // false
```

You can also use shorthand syntax for assigning values to object fields.
```ngn
const name = "John"
const age = 30
const isStudent = false

const person = { name, age, isStudent }

print(person.name) // John
print(person.age) // 30
print(person.isStudent) // false
```

## Custom TypeMethods

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

#### `check`
`check` is a way of guarding logic that requires a value. It can only be used for variables of type `Maybe<T>` or `Result<T, E>`.

- If it evaluates to `Null`, the statement block is run and it must either `return` or `break`.
- If it evaluates to a value, the declared variable (`u` in the example) is assigned the value and the statement block is skipped.
```ngn
fn getUser(user?: string): Result<User, string> {
  check var u = user {
    // failure case
    return Error("User not found")
  }
  // success case
  return Ok(u)
}

const user = getUser("jason")
match (user) {
  Ok(user) => print("User: ${user}"),
  Error(msg) => print("Error: ${msg}"),
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

## `loop`
Run the statement block indefinitely. Use `break` to exit the loop.
```ngn
loop {
  statement
  statement
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

## `for`
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

## `if`
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

## `match`
Match a value against one test case; optionally, provide a default case.

If a match is found:
- that branch's statement block is run.
- other cases are skipped, including the default, unless a matched statement block contains the `next` keyword. `next` will only try to match the very next case.

```
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

## `fn` (functions)
Functions create an isolated environment, meaning it can't access values outside of itself. If you need access to a value outside the environment, pass it as a parameter; but there are exceptions, which you can always access:

- globals (imports, statics, models, enums, functions)
- sibling functions

If passing a function as a param, you can mark the param like `fn<param1_type, param2_type, paramN_type, return_type>`. `return_type` is always last in the list, even if that means it's the only type listed.

Function params must be explicitly typed - otherwise ngn will show a console warning.

### Traditional block, explicit return
```ngn
fn add(a: i64, b: i64): i64 {
  return a + b
}
```

### Traditional block, explicit multiline return
```ngn
fn add(a: i64, b: i64): i64 {
  return (
    a + 
    b
  )
}
```

### Implicit return
```ngn
fn add(a: i64, b: i64): i64 a + b
```

### Side-effects only
Functions that only perform side-effects don't need a return type, but you can declare `void` if you want.
```ngn
fn doThing() {
  print("something")
}
```

### Owned params
When you mark a function param as owned, here is what happens:
- the value is mutable within the function, if it was declared with `var`
- ownership of the passed data is moved to the function
- the var or const is no longer accessible outside of the function
- ngn will cleanup any associated memory after the function finishes

```ngn
var x = "hello"

// the `<` means it requires an owned string
fn createRockMusic(stuff: <string) {
  // do stuff: read and/or mutate
}

createRockMusic(x) ✅ // moves ownership of `x` to the function

print(x) ❌ // `x` is no longer available, since it's ownership was moved
```

### Borrowed params
This is the default for all params.

```ngn
var x = "hello"

fn readThing(thing: string) {
  // do thing: but cannot mutate
}

readThing(x) ✅ // does not move ownership of `x` to the function

print(x) ✅ // `x` is still available
```

### Optional params
In this example, `suffix` is optional. Inside the function, it is either `Maybe::Value<T>` or `Maybe::Null`. There are a couple of ways to handle checking which variant it is:
```ngn
fn greet(name: string, suffix?: string): string {
  // explicit match
  match (suffix) {
    Value(s) => return "Hello ${name}${s}"
    Null => return "Hello ${name}"
  }
}
print(greet("Bob")) // "Hello Bob"
print(greet("Bob", "!")) // "Hello Bob!"
```
```ngn
fn greet(name: string, suffix?: string): string {
  // Check if a value can be unwrapped from the enum variant. If so, assign to local variable and run the statement block; otherwise, it's `Maybe::Null`.
  if (let s = suffix) {
    return "Hello ${name}${s}"
  }
  return "Hello ${name}"
}
print(greet("Bob")) // "Hello Bob"
print(greet("Bob", "!")) // "Hello Bob!"
```

### Default params
Default params are implicitly optional.
```ngn
fn greet(name: string, suffix: string = "!") {
  print("Hello ${name}${suffix}")
}
print(greet("Bob")) // "Hello Bob!"
print(greet("Bob", ",")) // "Hello Bob,"
```

## `map`
Create a key, value map. Type declartion is required.

```ngn
const m = map<i64, string>()

// add an entry
m.set(1, "one") // returns the map

// chain set
m.set(2, "two").set(3, "three")

// checks if an entry exists, based on key
m.has(1) // returns a bool

// get an entry
m.get(1) // returns the value, or void if not found

// remove an entry
m.remove(1) // returns the removed value

// get the size
m.size() // returns the number of entries in the map
```

## `set`
Create a set of values.
- Type declartion is required
- Values are deduplicated

```ngn
const s = set<string>()

// add a value
s.add("one") // returns the set

// chain add
s.add("two").add("three")

// checks if a value exists
s.has("one') // returns a bool

// remove a value
s.remove("one") // returns a bool of whether the value was removed

// get the size
s.size() // returns the number of values in the set

// clear all values
s.clear()

// iterate over values
for (v in s) {
  print(v)
}

// iterate over values with index
for (v, i in s) {
  print("index: ${i}, value: ${v}")
}
```

## Closures
Closures are similar to functions, but have important differences:
- assign them with `const` then call like a function
- access to external values, even ones outside its environment
- uses pipe syntax to wrap params
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

### `model`
Create object structures.

```ngn
model Dog {
  name: string,
  breed: string
}
```

### `role`
You can extend a model's functionality with groups of methods via roles. Declare one or more method signatures and/or method implementations. Use this to group methods into roles in order to define their functionality for models.

```ngn
role Animal {
  fn speak(): void
}
```

### `extend`
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
- by `const`, `static` variables: ❌ not allowed, as these are all strictly immutable

## `this`
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

## Channels
Send and receive data.
- You must declare a channel with `const`
- You must provide a data type for the channel's messages.

### <- syntax
Use `<-` to both send and receive messages. Let's assume we have a channel called `line`.
- `line <- "hello"` would send a string message to the channel.
- `<- line` would cause the program to stop and wait for a single message.

Regarding the last point: if you had multiple things sending messages, you have the following options:
```ngn
// Would wait on two messages before continuing the program.
<- line
<- line

// If you know how many messages to wait on, here's a shorthand version of the above:
<-2 line

// You can even base it off of other things:
<-tasks.size() line  // array size
<-(x + y) line

// If you don't know how many messages you'll receive.
// You'll need to close the channel for this to work (example futher below).
<-? line
```

```ngn
fn main() {
    const c = channel(): string

    // Send a message
    c <- "first"

    // Optionally close the channel for this example
    c.close()

    // Assign channel output to a variable
    // Receiving "first" will still work here, because of buffering
    const msg = <- c
    print("Received: ${msg}")

    // This will fail because the channel is closed and empty.
    const fail = <- c
}
```

You can send a closure to a channel:
```ngn
fn main() {
    const job_queue = channel(): fn<i64, void>

    // (See next section for details on threads)
    const done = thread(|| {
        print("Worker started")
        loop {
            match (<-? job_queue) {
                Value(task) => task(42), 
                Null => break
            }
        }
        print("Worker finished")
        
        return
    })

    job_queue <- |n: i64| { print("Task A executing with ${n}") }
    
    job_queue <- |n: i64| { 
        const res = n * 2
        print("Task B executing: ${n} * 2 = ${res}") 
    }
    
    // must close the channel to break out of `while` loop
    job_queue.close()

    print("Jobs sent")
    
    // wait for jobs to complete
    <- done

    print("Jobs complete")
}
```
```txt
Jobs sent
Worker started
Task A executing with 42
Task B executing: 42 * 2 = 84
Worker finished
Jobs complete
```

Channels can even contain other channels, and you can send/receive data within those inner channels.
```ngn
fn main() {
    const request_line = channel(): channel<string>

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

## Threads - Concurrency and Parallelism
Allows you to do work while, optionally, continuing to do work in the main thread. Threads take a closure with no parameters, but have access to their surrounding scope. They also return a channel, if that fits your usecase.

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
In such cases, use the returned channel to await the thread.
```ngn
fn main() {
    const value = 100

    // "done" is a channel we can use for signaling
    const done = thread(|| {
        setData(value)
        return // returning from a thread sends that data to the created channel
    })

    // Continue doing other work while the thread runs

    // Now wait until we receive a message,
    // indicating thread work is done
    <- done
}
```
Threads may run in parallel or sequentially but unordered; however, you can control the order in which you wait on their results.
```ngn
fn main() {
    print("1. Main started")
    
    // Create a thread, to do some async work
    const c = thread(|| {
        print("  2c. Thread c started (sleeping...)")
        sleep(2000)
        
        print("  3c. Thread c sending message")
        return Ok("Hello from channel c!")
    })

    // Create a thread, to do some async work
    const d = thread(|| {
        print("  2d. Thread d started (sleeping...)")
        sleep(2000)
        
        print("  3d. Thread d sending message")
        return Error("Oh this is bad channel d!")
    })
    
    print("4. Main doing other work while thread runs...")

    // This should block until the "c" thread sends a message
    const msgc = <- c
    print("5c. Main received, from thread c: ${msgc}")

    // This should block until the "d" thread sends a message
    const msgd = <- d
    print("5d. Main received, from thread d: ${msgd}")
}
```
```txt
1. Main started
4. Main doing other work while thread runs...
  2c. Thread c started (sleeping...)
  3c. Thread c sending message
  2d. Thread d started (sleeping...)
  3d. Thread d sending message
5c. Main received, from thread c: Result::Ok (Hello from channel c!)
5d. Main received, from thread d: Result::Error (Oh this is bad channel d!)
```

If you're unsure how much data is coming, use a `for` loop, and then close the channel at the end of the input in order to indicate that no more messages can be sent. Below, we're simulating "unknown" amounts of data.

```ngn
fn main() {
    // In this example, we can't use the thread's returned channel,
    // because we need to close the channel from within the thread
    // in order to signal the `for` loop to stop.
    const c = channel(): string

    thread(|| {
        c <- "A"
        c <- "B"
        c <- "C"
        c.close()
    })

    for (msg in <-? c) {
      print("Got: {msg}")
    }
    
    print("Done")
}
```

### Shared, mutable state
It's safe to sequentially mutate shared data outside of threads or within a single thread. However, if one or more threads might mutate data, use `state()` to declare the variable. This gives you safe, atomic operations for mutating the data by using state variable methods.

You'd also use `state()` if you need to mutate data from within a closure.
```ngn
fn main() {
    var counter = state(0)
    const done = channel(): bool
    
    thread(|| {
        // Pass a closure that mutates the data.
        // The closure receives the current value of `counter` via a param.
        counter.update(|n| n + 10) // implicit return used
        print("added 10")
        done <- true
    })
    
    thread(|| {
        counter.update(|n| n + 5)
        print("added 5")
        done <- true
    })
    
    // If number of awaited messages is known, you can declare that here.
    // They'll be returned as an array, if you need to assign them.
    <-2 done

    print(counter)  // Always 15
}
```
If needed, you also have access to these variable methods when using `state()`:
- `.read()`, gets the current value
- `.write()`, sets the current value - which replaces the existing one. Be careful, as it can be tricky to ensure proper mutation order when coupled with `.update()`.

### Spawning threads
If you have multiple tasks, here is one way you can spawn each of them in their own thread.
```ngn
fn main() {
  const done = channel(): bool
  var results = state([])

  fn task1(): string {
    print("Doing task 1")
    return "Task 1 done"
  }
  fn task2(): string {
    print("Doing task 2")
    return "Task 2 done"
  }
  fn task3():string {
    print("Doing task 3")
    return "Task 3 done"
  }
  const tasks = [task1, task2, task3]

  // Spawn threads
  // Optional index value as the second closure param
  for (task in tasks) {
    thread(|| {
      const result = task()
      results.update(|r| {
        r.push(result)
        return r
      })

      done <- true
    })
  }

  <-tasks.size() done
  
  print("Spawned results: ${results}")
}
```

## `fetch()`
Use `fetch` to make HTTP requests, such as to external APIs. It returns a channel, so you await it with the `<-` operator.

```ngn
const response = <- fetch("https://example.com")
print(response)
```
```ngn
const response = <- fetch("https://example.com", {
  method: "POST",
  headers: {
    "Accept": "application/json",
    "Content-Type": "application/json",
  },
  body: json.stringify({
    "name": "John Doe",
    "email": "john.doe@example.com",
  }),
  timeout: 30000,
})
print(response)
```

### `fetch` properties
- `url`: The URL of the request
- `options`: An object containing the options for the request
  - `method`: The HTTP method of the request - defaults to GET
  - `headers`: The headers of the request
  - `body`: The body of the request
  - `timeout`: The timeout of the request, in milliseconds - defaults to 10 seconds

## `Request`
You can handle `Request` objects.

```ngn
fn handler(req: Request) {
  const path = req.path
  const method = req.method
  const headers = req.headers
  const body = req.body

  return Response {
    body: "Hello, world!",
  }
}

export default {
  fetch: handler
}
```
### `Request` properties
- `method`: The HTTP method of the request
- `url`: The URL of the request
- `protocol`: Whether HTTP or HTTPs was used
- `host`: The host the client used to make the request
- `path`: The path of the request
- `query`: The query string of the request
- `params`: Query parameters as a `Map<string, string>`
- `headers`: The headers of the request
- `body`: The body of the request
- `ip`: The client's IP address
- `cookies`: The cookies sent with the request

### `Request` methods
- `clone()`: Creates a new `Request` object with the same properties
- `text()`: Parses the body as a string, returns a `string`
- `json()`: Parses the body as JSON, returns a Result enum
- `formData()`: Parses URL-encoded body, returns a `Map<string, string>`

## `Response`
You can create a `Response` object to send HTTP responses.

```ngn
fn handler(req: Request) {
  return Response {
    body: "Hello, world!",
  }
}

export default {
  fetch: handler
}
```

### `Response` properties
- `status`: The HTTP status code - default is 200
- `statusText`: The HTTP status text - default is ""
- `ok`: A boolean indicating whether the response status code is in the range 200-299
- `headers`: The headers to include in the response
- `body`: The body of the response - default is ""

### `Response` methods
- `text()`: Parses the body as a string, returns a `string`
- `json()`: Parses the body as JSON, returns a Result enum
- `formData()`: Parses URL-encoded body, returns a `Map<string, string>`

## Modules
You can use `export` and `import` to create modules in your project. This is a functions-only feature.

```ngn
// math.ngn
export fn add(a, b) a + b

export fn subtract(a, b) a - b
```
Here, we look for an `add` function from a `math.ngn` file within the same directory that your `main.ngn` file is in.
```ngn
// main.ngn
import { add } from "math.ngn"

fn main() {
  print(add(21, 3)) // 24
}
```

### Relative path imports
```ngn
import { add } from "./lib/math.ngn"
```

### Aliased imports
```ngn
import { add as adder } from "math.ngn"
```

### Module imports
```ngn
// main.ngn
import * as Math from "math.ngn"

print(Math.subtract(10, 2)) // 5
```

### Default export
```ngn
// math.ngn
fn add(a, b) a + b

export default add
```
```ngn
// main.ngn
import add from "math.ngn"
```

## "Standard Library"
We call this the toolbox.

You can import in different ways:
- `import { abs } from "tbx::math"`; use functions directly (best for tree-shaking)
- `import { abs as ABS } from "tbx::math"`; aliased functions
- `import Math from "tbx::math"`; use functions via alias `Math.abs`
- `import * as Math from "tbx::math"`; (use same as above)

### tbx::math
- `abs`: return the absolute value of a number. `abs(-5) // 5`
- `ceil`: return the smallest integer greater than or equal to `ceil(3.2) // 4`
- `floor`: return the smallest integer less than or equal to `floor(3.9) // 3`
- `round`: return the number rounded to the nearest integer `round(4.5) // 5`
- `sin`: (Trigonometry? If you know, you know.) `sin(0) // 0`

### tbx::test
- `assert`: assert that a condition is true
```ngn
import { assert } from "tbx::test"

fn main() {
  assert(1 + 1 == 2)
}
```

### tbx::http
Create an HTTP server.

- `serve`: create an HTTP server
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
  print("Starting HTTP server on port 3000...")
  serve(3000, handleRequest)
}
```

- default export with fetch method (serve called under the hood)
```ngn
fn fetch(req: Request): Response {
  return Response {
    status: 200,
    body: "Hello from export-based API!",
    headers: map<string, string>()
  }
}

export default { fetch: fetch }
```
