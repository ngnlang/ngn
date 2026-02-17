---
title: API
description: API reference
---

## main()

Your entrypoint file must define a `main()` function. It's found and run automatically. Most of your code will live inside of this function. The exceptions are:

- `global` definitions
- `import` statements
- `enum` definitions
- `role` definitions
- `extend` statements
- `type` definitions
- other functions can be defined at the global level

## var
Defines a variable who's value can be changed.

```ngn
var x = "hello"
x = "goodbye" ✅
```

## const
Defines a constant who's value cannot be changed.

```ngn
const x = "hello"
x = "goodbye" ❌ // value is immutable
```

## global
Used for global declarations, which can only exist at the top-level of a file, not inside functions.

- usually inlined at compile time
- strings not inlined if longer than 32 bytes
- arrays and tuples not inlined if size is greater than 4 items or if any item is not a primitive type

```ngn
global VERSION = "v3" // inlined at compile time
global DATA = [1, 2, 3, 4, 5] // not inlined

fn main() {
  print(VERSION)
  print(DATA)
}
```

## echo
Log to the console, without formatting.

```ngn
const name = "ngn"
echo(name)
// ngn

echo("Hello")
echo("World")
// HelloWorld
```

## print
Line logging to the console. Implicit `\n`.

```ngn
print("Hello")
print("World")
// Hello
// World
```

## json

### parse()
You can parse a JSON string or an array. `json.parse()` returns a `Result<any, { message: string, line: i64, column: i64 }>`.

```ngn
const data = json.parse('{"name": "ngn"}')

// unwrap the Result
check data?, err? { 
  print("Parse error at line ${err.line}: ${err.message}")
  return 
}
print(data.name) // ngn
```

### stringify()
You can stringify an object or an array.

```ngn
const data = { name: "ngn" }
const str = json.stringify(data)
print(str) // {"name": "ngn"}
```

## Strings

### length()
Return the length of a string.

### index()
Search a string for a given pattern, and return the index number of the first instance found. If no pattern is found, returns `-1`. You can pass an optional start index.

`index(pattern, start?)`

```ngn
const sent = "I learned to draw today."
const ind = sent.index("to") // 10
```

### includes()
Determine if a string includes a given pattern. Returns a bool.

`includes(pattern)`

```ngn
const weather = "sunny"
const inc = weather.includes("sun") // true
```

### starts()
Determine if a string starts with a given pattern. Returns a bool.

`starts(pattern)`

```ngn
var process = "complete"
const beg = process.starts("c") // true
```

### ends()
Determine if a string ends with a given pattern. Returns a bool.

`ends(pattern)`

```ngn
var process = "working"
const end = process.ends("ing") // true
```

### split()
Create an array of strings by splitting on a pattern of characters within a string. If you do not pass a pattern, each character in the string is split individually. Preserves the original string.

`split(pattern?)`

```ngn
const sent = "What. On. Earth."
const split_sent = sent.split(".") // ["What", " On", " Earth", ""]

var greeting = "Hello"
const split_greeting = greeting.split() // ["H", "e", "l", "l", "o"]
```

### replace()
Replace a pattern with a string. `search` can be a string or a RegEx; but if a string is passed, only the first occurrence is replaced. Preserves the original string and returns a new one.

`replace(search, replacement)`

```ngn
var plain = "Forge ahead"
const fancy = plain.replace("a", "@") // "Forge @head"
```

```ngn
var plain = "Forge ahead"
const fancy = plain.replace(/a/g, "@") // "Forge @he@d"
```

### copy()
Copies an entire string or a section of it, based on indices. This does not change the string you copied from, but returns the copied value as a new string.

`copy(start?, stop?)`

- If `start` is provided but `stop` is not, it copies everything upto and including the end of the string.
- If `stop` is provided (implies `start`), the copy excludes the item at that index.
- If neither is provided, the entire string is copied.

```ngn
const some = "Some Stuff"
const copied = some.copy(5)

print(copied) // "Stuff"
print(some) // "Some Stuff"

var all = some.copy()

print(all) // "Some Stuff"
print(some) // "Some Stuff"
```

### slice()
Remove a section of a string by providing a start index and an optional stop index. This changes the original string and returns the sliced section as a new string.

`slice(start, stop?)`

- If `stop` is provided, the slice excludes the item at that index.
- If `stop` is not provided, it removes everything upto and including the last item.
- Since you're mutating the original string, it must be declared with `var`.

```ngn
var quote = "I flew too close to the sun on wings of pastrami."
const sliced = quote.slice(24, 31)

print(orig) // I flew too close to the wings of pastrami.
print(sliced) // "sun on "
```

### upper()
Transform a string to all uppercase, returning a new string. Preserves original string.

```ngn
const version = "one"
print(version.upper()) // ONE
```

### lower()
Transform a string to all lowercase, returning a new string. Preserves original string.

```ngn
var version = "ONE"
print(version.lower()) // one
```

### trim()
Remove whitespace from both ends of a string, returning a new string. Preserves original string.

```ngn
var thing = " strong "
print(thing.trim()) // "strong"
```

### repeat()
Repeat a string some number of times.

`repeat(num)`

```ngn
const ending = "goodbye"
print(greeting.repeat(2)) // goodbyegoodbye
```

## Numbers
There are currently no number methods, but we do have a math mod in the Toolbox, or you can use the `extend` keyword to add your own.

## Arrays
If you want to mutate arrays, be sure to declare them with `var`

```ngn
var stuff = ["hat", "coat", "gloves"]
const ages = [3, 8, 15, 23]

const mixed = ["hat", true, 7] ❌ // cannot mix types
```

### Destructuring

Extract elements from an array into variables:

```ngn
const arr = [10, 20, 30, 40]
const [first, second] = arr

print(first) // 10
print(second) // 20
```

### ...rest

Collect remaining elements into a new array:

```ngn
const nums = [1, 2, 3, 4, 5]
const [head, ...tail] = nums

print(head) // 1
print(tail) // [2, 3, 4, 5]
print(tail.size()) // 4
```

### size()
Return the size of the array.

### push()
Push, i.e. add, an item into an array. By default, it pushes at the end. To push into another location, provide the index number. Returns the new size of the array as an `i64`.

`push(item, index?)`

```ngn
var stuff = ["guitar", "shirt"]
const size = stuff.push("hat")

print(size) // 3
print(stuff) // ["guitar", "shirt", "hat"]

stuff.push("coat", 0)
print(stuff) // ["coat", "guitar", "shirt", "hat"]
```

### pop()
Pop, i.e. remove, an item from an array. By default, it removes from the end. To pop from another location, provide the index number. Returns the removed item's value.

`pop(index?)`

```ngn
var stuff = ["coat", "guitar", "shirt", "hat"]
const popped = stuff.pop()

print(popped) // hat
print(stuff) // ["coat", "guitar", "shirt"]

const popped_one = stuff.pop(1)

print(popped_one) // ["guitar"]
print(stuff) // ["coat", "shirt"]
```

### copy()
Copies an entire array or a section of it, based on indices. This does not change the array you copied from, but returns the copied items as a new array.

`copy(start?, stop?)`

- If `start` is provided but `stop` is not, it copies everything upto and including the last item.
- If `stop` is provided (implies `start`), the copy excludes the item at that index.
- If neither is provided, the entire array is copied.

```ngn
const stuff = [10, 20, 30, 40, 50]
const copied = stuff.copy(3)

print(copied) // [40, 50]
print(stuff) // [10, 20, 30, 40, 50]

var all = stuff.copy()

print(all) // [10, 20, 30, 40, 50]
print(stuff) // [10, 20, 30, 40, 50]
```

### slice()
Remove a section of the array by providing a start index and an optional stop index. This changes the array and returns the item(s) as a new array.

`slice(start, stop?)`

- If `stop` is provided, the slice excludes the item at that index.
- If `stop` is not provided, it removes everything upto and including the last item.

```ngn
var stuff = [10, 20, 30, 40, 50]
const sliced = stuff.slice(1, 3)

print(sliced) // [20, 30]
print(stuff) // [10, 40, 50]
```

### splice()
Add multiple items to an array; optionally, at a specific index. Returns the new size of the array.

`splice(item[], start?)`

- If `start` is not provided, it adds the items at the end.

```ngn
var stuff = [10, 20, 30]
stuff.splice([40, 50]) // [10, 20, 30, 40, 50]

const size = stuff.splice([45, 47], 4)

print(stuff) // [10, 20, 30, 40, 45, 47, 50]
print(size) // 7
```

### each()
For each item in an array, execute a closure.

`each(|item, index| {})`

```ngn
var things = ["hat", "gloves", "coat"]

things.each(|t, i| {
  print("${i}: ${t}")
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

### Destructuring

Extract elements from a tuple into variables:

```ngn
const point = (10, 20)
const (x, y) = point

print(x) // 10
print(y) // 20
```

Tuples preserve the type of each element:

```ngn
const mixed = (42, "hello", true)
const (num, str, flag) = mixed

print(num)  // 42 (i64)
print(str)  // hello (string)
print(flag) // true (bool)
```

### ...rest

Collect remaining elements into a new tuple:

```ngn
const values = (1, 2, 3, 4, 5)
const (first, second, ...rest) = values

print(first)  // 1
print(second) // 2
print(rest)   // (3, 4, 5)
```

### size()
Return the size of the tuple.

### includes()
Check if a tuple contains a specific item.

`includes(item)`

```ngn
const tup = (10, "hello", true)
const has_hello = tup.includes("hello")

print(has_hello) // true
```

### index()
Search a tuple for a given item, and return the index number of the first instance found. If no item is found, returns `-1`.

`index(item)`

```ngn
const tup = (10, "hello", true)
const ind = tup.index("hello") // 1
```

### copy()
Copies an entire tuple or a section of it, based on indices. This does not change the tuple you copied from, but returns the copied items as a new tuple.

`copy(start?, stop?)`

- If `start` is provided but `stop` is not, it copies everything upto and including the last item.
- If `stop` is provided (implies `start`), the copy excludes the item at that index.
- If neither is provided, the entire tuple is copied.

```ngn
const tup = (10, "hello", true)
const copied = tup.copy(1)

print(copied) // ("hello", true)
print(tup) // (10, "hello", true)
```

### toArray()
Convert a tuple to an array. Items must be of the same type.

```ngn
const tup = (10, 20, 30)
const arr = tup.toArray()

print(arr) // [10, 20, 30]
```

### join()
Join a tuple into a string, separated by a given delimiter.

`join(delimiter)`

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

:::caution
When working with unknown object data, using dot notation will throw a runtime error if the value isn't there. Use `json.parse()` when possible, since the returned value is a `Maybe` that you can check.
:::

### Destructuring

Extract fields from an object into variables. Destructured fields are `Maybe` values.

```ngn
const person = { name: "Alice", age: 30, city: "NYC" }
const { name, age } = person

check name? { return }
check age? { return }

print(name) // Alice
print(age) // 30
```

### Aliasing

Use a different variable name than the field name:

```ngn
const user = { id: 42, email: "test@example.com" }
const { id, email: userEmail } = user

check id? { return }
check userEmail? { return }

print(id) // 42
print(userEmail) // test@example.com
```

### ...rest

Collect remaining fields into a new object:

```ngn
const data = { a: 1, b: 2, c: 3, d: 4 }
const { a, b, ...rest } = data

check a? { return }
check b? { return }
check rest? { return }

print(a) // 1
print(b) // 2
print(rest.c) // 3
print(rest.d) // 4
```

## model
Create object structures.

```ngn
model Dog {
  name: string,
  breed: string
}
```

### Generic Models
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

### Alternative for instantiating
You may also choose to create a constructor method and use it to create a new instance of a model.

```ngn
model User {
  name: string,
  age: u32
}

fn main() {
  var user = User.new("Chloe", 27)
}
```

### Destructuring

```ngn
model Point {
  x: i64,
  y: i64
}

fn main() {
  const p = Point { x: 5, y: 10 }
  const { x: px, y: py } = p
  
  print(px) // 5
  print(py) // 10
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

### Type Inference
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

## this
There's no need to fear `this` in ngn. It's an implicit reference to the instance or type that a method is called on.

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

## role
You can extend a model's functionality with groups of methods via roles. Declare one or more method signatures and/or method implementations. Use this to group methods into roles in order to define their functionality for models.

```ngn
role Animal {
  fn speak(): void
}
```

## extend
Extend a model or built-in type with methods.

### Models

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

### Built-in methods

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

## bytes()
A built-in binary data type. It represents an arbitrary sequence of raw bytes (0..255).

You will most commonly use `bytes` for binary WebSocket frames, encoding/decoding, and other I/O-style APIs.

- `bytes()`, Create an empty bytes value.

- `bytes(string)`, Create bytes from a UTF-8 string.

    ```ngn
    const b = bytes("hello")
    print(b.length()) // 5
    ```

- `bytes(array<u8>)`, Create bytes from an array of numeric byte values. Each element must be in the range 0..255.

    ```ngn
    const raw: array<u8> = [0, 255, 16]
    const b = bytes(raw)
    print(b.length()) // 3
    ```

### length()
Return the number of bytes.

### copy()
Copy an entire bytes value or a section of it, based on indices. This does not change the bytes you copied from.

`copy(start?, stop?)`

- If `start` is provided but `stop` is not, it copies everything upto and including the end.
- If `stop` is provided (implies `start`), the copy excludes the item at that index.
- If neither is provided, the entire bytes is copied.

### slice()
Remove a section of bytes by providing a start index and an optional stop index. This changes the original bytes value and returns the sliced bytes.

`slice(start, stop?)`

- If `stop` is provided, the slice excludes the item at that index.
- If `stop` is not provided, it removes everything upto and including the last byte.
- Since you're mutating the original bytes, it must be declared with `var`.

```ngn
var b = bytes("abcd")
const sliced = b.slice(1, 3)

print(sliced.toStringStrict()) // bc
print(b.toStringStrict()) // ad
```

### toString()
Decode bytes as UTF-8 using a lossy conversion. Invalid sequences are replaced.

This is useful for logging/debugging or when you are working with "mostly" UTF-8 data.

### toStringStrict()
Decode bytes as UTF-8 using a strict conversion.

If the bytes are not valid UTF-8, this throws a runtime error.

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

### Maybe
`Maybe<T>` represents a value that may or may not exist. You can write `Maybe<T>` or use the shorthand `T?` syntax.

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

#### `null` keyword
The `null` keyword is syntactic sugar for `Maybe::Null`. You can use it in any context where `Null` would be used:

```ngn
// These are equivalent
var m1 = null
var m2 = Null

// Using null in return statements
fn maybeValue(flag: bool): Maybe<i64> {
  if (flag) return Value(42)
  return null  // Syntactic sugar for Maybe::Null
}

// null works with the ?? (null-coalescing) operator
var x: Maybe<i64> = null
var result = x ?? 100  // result is 100
```

#### Null checks with `!`
You can use the `!` operator on `Maybe` values to check if they are null:

```ngn
fn describe(x?: i64): string {
  if (!x) return "is null"  // !null is true, !Value(_) is false
  return "has value"
}
print(describe())    // "is null"
print(describe(42))  // "has value"
```

:::caution
`!` working for `null` may get removed.

This approach might seem more efficient than the guards below. The difference is that `x` doesn't unwrap here, it's just a short-circuit guard check - likely best used when you don't want the code to proceed if it's null, but you also don't care about the value even if it's not null (or you have to unwrap it if you do need the value; which, at that point, it would be better to use `if (x?)`).
:::

#### Optional chaining (`?.`)
Use `?.` to safely access fields or call methods on `Maybe` values. If the value is null, the entire expression short-circuits to `null`.

```ngn
model User {
  name: string,
  age: i64
}

var user: User? = null
var name = user?.name        // null (short-circuited)
var safeName = user?.name ?? "Unknown"  // "Unknown"

var user2 = Value(User { name: "Alice", age: 30 })
var name2 = user2?.name      // Value("Alice")

// Chaining
model Address { city: string }
model Person { address: Address }

var p: Person? = null
var city = p?.address?.city  // null (multiple short-circuits)
```

### if ?
Use the postfix `?` guard to unwrap a `Maybe<T>` or `Result<T, E>` inside an `if` branch.

```ngn
fn greet(name: string, suffix?: string): string {
  if (suffix?) {
    // inside this branch, `suffix` is a `string`
    return "Hello ${name}${suffix}"
  }
  return "Hello ${name}"
}
```

### check ?
`check` is a guard for `Maybe<T>` / `Result<T, E>` that reduces ceremony.

Syntax:

```ngn
check value? { /* failure */ }
check value?, err? { /* failure */ }
```

Semantics:

- If `value` is `Null` or `Error`, the failure block runs and must exit (`return`/`break`).
- If `value` is `Value(T)` or `Ok(T)`, the failure block is skipped and `value` is upgraded to the unwrapped `T` for the rest of the scope.
- `err` (if provided) is only available inside the failure block.

```ngn
fn getUser(user?: string): Result<User, string> {
  check user? {
    return Error("User not found")
  }
  // after check, `user` is a `string`
  return Ok(User { name: user, age: 0 })
}
```

### check error binding

```ngn
fn fetchData(): Result<string, string> {
  const result: Result<string, string> = Error("network timeout")

  check result?, err? {
    print("Failed: ${err}")
    return Error(err)
  }

  // after check, `result` is a `string`
  return Ok(result)
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

## Ranges
Ranges are integer sequences. Use `..` for inclusive ranges and `..<` for exclusive upper bounds.

```ngn
const arr<i32> = [1..5]
print(arr) // [1, 2, 3, 4, 5]

for (i in 1..5) {
  print(i)
}

match (score) {
  94..99 => print("great"),
  90..<94 => print("good"),
  _ => print("keep striving")
}
```

- Range bounds must be integers (no floats).
- Ranges are empty when the start is greater than the end.
- Array range literals only allow a single range (e.g. `[1..5]`).

## loop
Run the statement block indefinitely. Use `break` to exit the loop.
```ngn
loop {
  statement
  statement
}
```

## while
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

### once
To always run the statement block once, before checking the condition.
```ngn
while once (condition) {
  statement
}
```

## for
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

## if
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

## match
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

## fn (functions)
Functions create an isolated environment, meaning it can't access values outside of itself. If you need access to a value outside the environment, pass it as a parameter; but there are exceptions, which you can always access:

- globals (imports, models, enums, functions)
- sibling functions

If passing a function as a param, you can mark the param like `fn<param1_type, param2_type, paramN_type, return_type>`. `return_type` is always last in the list, even if that means it's the only type listed.

Function params must be explicitly typed - otherwise ngn will show a console warning.

#### Explicit return
```ngn
fn add(a: i64, b: i64): i64 {
  return a + b
}
```

#### Explicit multiline return
```ngn
fn add(a: i64, b: i64): i64 {
  return (
    a + 
    b
  )
}
```

#### Implicit return
```ngn
fn add(a: i64, b: i64): i64 a + b
```

#### Side-effects only
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
In this example, `suffix` is optional. Inside the function, it is either `Maybe::Value<T>` or `Maybe::Null`; so they must be checked or unwrapped (see Enums section for more options).

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
  // If the optional has a value, unwrap it for this block.
  if (suffix?) {
    // inside this branch, `suffix` is a `string` (unwrapped)
    return "Hello ${name}${suffix}"
  }
  return "Hello ${name}"
}
print(greet("Bob")) // "Hello Bob"
print(greet("Bob", "!")) // "Hello Bob!"
```

### Default params
Default params are implicitly optional, but are never a "maybe" since it's guaranteed to have a value.

```ngn
fn greet(name: string, suffix: string = "!") {
  print("Hello ${name}${suffix}")
}
print(greet("Bob")) // "Hello Bob!"
print(greet("Bob", ",")) // "Hello Bob,"
```

## Closures
Closures are similar to functions, but have important differences:

- assign them with `const` then call like a function
- access to external values, even ones outside its environment
- wrap params with `|`

Param ownership transfer is the same as functions. To mutate the value of a variable from within a closure, use `state()` to declare the variable.

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

## map
Create a key, value map.
- You can seed a map with an array of 2-tuples
- Types are inferred from the seed
- Empty seeds require explicit type args

```ngn
const m = map([("one", 1), ("two", 2)])

// add an entry
m.set("three", 3) // returns the map

// chain set
m.set("four", 4).set("five", 5)

// checks if an entry exists, based on key
m.has("one") // returns a bool

// get an entry
m.get("one") // returns the value, or void if not found

// remove an entry
m.remove("one") // returns the removed value

// get the size
m.size() // returns the number of entries in the map

// empty seed requires explicit types
const empty = map<string, i64>()
```

## set
Create a set of values.
- You can seed a set with an array
- Types are inferred from the seed
- Empty seeds require explicit type args
- Values are deduplicated

```ngn
const s = set(["one", "two", "three"])

// add a value
s.add("four") // returns the set

// chain add
s.add("five").add("six")

// checks if a value exists
s.has("one") // returns a bool

// remove a value
s.remove("one") // returns a bool of whether the value was removed

// get the size
s.size() // returns the number of values in the set

// clear all values
s.clear()

// empty seed requires explicit types
const empty = set<string>()

// iterate over values
for (v in s) {
  print(v)
}

// iterate over values with index
for (v, i in s) {
  print("index: ${i}, value: ${v}")
}
```

## channel
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
  const c = channel<string>()

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
  const job_queue = channel<fn<i64, void>>()

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
  const request_line = channel<channel<string>>()

  thread(|| {
    // Thread waits for inbound data on the request_line channel,
    // which happens to be another channel that we assign to a constant.
    const reply_channel = <- request_line
    
    // Reply back on the private channel
    reply_channel <- "Your order is ready!"
  })

  // Create a private response channel
  const private_channel = channel<string>()
  
  // Send private channel, which the worker is waiting for
  request_line <- private_channel
  
  // Wait for the private reply
  print(<- private_channel)
}
```

## thread
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
    print("  2c. Thread c started (sleeping...")
    sleep(2000)
    
    print("  3c. Thread c sending message")
    return Ok("Hello from channel c!")
  })

  // Create a thread, to do some async work
  const d = thread(|| {
    print("  2d. Thread d started (sleeping...")
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
  const c = channel<string>()

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

### Returned Channels
Creating a thread returns a channel, which can be used to await the thread's result. To send data to the channel, just `return` from the thread - either empty or with a value.
```ngn
fn main() {
  const done = thread(|| {
    setData(value)
    return Ok("Done")
  })

  <- done
}
```

## state()
It's safe to sequentially mutate shared data outside of threads or within a single thread. However, if one or more threads might mutate data, use `state()` to declare the variable. This gives you safe, atomic operations for mutating the data by using state variable methods.

You'd also use `state()` if you need to mutate data from within a closure.
```ngn
fn main() {
  var counter = state(0)
  const done = channel<bool>()
  
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

## spawn
The `spawn` global provides two related tools:

- single-task offloading (`spawn.cpu`, `spawn.block`) which returns a `channel` immediately
- multi-task parallel helpers (`spawn.all`, `spawn.try`, `spawn.race`) which return results directly

Tasks should return `Result<T, E>`. If a task returns a non-`Result` value, it will be wrapped as `Ok(value)`. Thread panics are automatically converted to `Error("Thread panicked: <error message>")`.

### cpu()
Run a single task on the CPU worker pool.

`spawn.cpu(task)`

This call does not block immediately; it returns a channel right away, and you await it with `<-`.

- accepts either a function or a closure
- returns `channel<Result<any, string>>`
- if the CPU pool queue is full, the returned channel will contain `Error("spawn.cpu() queue is full")`

```ngn
fn expensive(): i64 {
  // some CPU-heavy work
  return 123 // automatically wrapped with `Ok()`
}

fn main() {
  const ch = spawn.cpu(expensive)
  // do other work?

  const result = <- ch
  print(result)
}
```

### block()
Run a single task on the blocking worker pool (for work that may stall the current thread, e.g. filesystem operations, subprocess waits, etc.).

`spawn.block(task)`

The name `block` refers to the *kind of work* (it may block internally). The call itself returns a channel immediately, and you use `<-` when you want to await it.

Note: `fetch()` already runs on the blocking pool internally, so there's no need to wrap `fetch()` in `spawn.block()`.

- accepts either a function or a closure
- returns `channel<Result<any, string>>`
- if the blocking pool queue is full, the returned channel will contain `Error("spawn.block() queue is full")`

```ngn
import { file } from "tbx::io"

fn main() {
  // File I/O is blocking. Offload it so it doesn't stall other work.
  const ch = spawn.block(|| file.read("./big.txt"))

  const result = <- ch
  print(result)
}
```

### all()
Returns an array of results (including any errors).

`spawn.all(tasks, options?)`

```ngn
fn task1(): Result<string, string> { return Ok("Task 1 done") }
fn task2(): Result<string, string> { return Error("Task 2 failed") }
fn task3(): Result<string, string> { return Ok("Task 3 done") }

const results = spawn.all([task1, task2, task3])
// [Result::Ok (Task 1 done), Result::Error (Task 2 failed), Result::Ok (Task 3 done)]

for (result in results) {
  match (result) {
    Ok(msg) => print(msg),
    Err(msg) => print(msg)
  }
}
```

With concurrency limit:
```ngn
const results = spawn.all(tasks, { concurrency: 2 })  // Max 2 concurrent threads at a time
```

If `concurrency` is omitted, ngn schedules as many workers as needed for the task list. If `concurrency <= 0`, ngn falls back to `1`.

### try()
Stop launching new tasks on first error. Returns partial results up to and including the error.

`spawn.try(tasks, options?)`

```ngn
const results = spawn.try([task1, task2, task3], { concurrency: 4 })
// Stops when first error occurs
// [Result::Ok (Task 1 done), Result::Error (Task 2 failed)]
```

Notes:
- Already-running tasks are cancelled on a best-effort basis (cooperative checks between VM steps).
- Tasks doing blocking/native work may still finish even after fail-fast is triggered.

### race()
Returns the first successful result, or the first error if all tasks fail.

`spawn.race(tasks)`

```ngn
const result = spawn.race([task1, task2, task3])
// Result::Ok (Task 1 done)  - whichever completes first with success
```

## fetch()
Use `fetch` to make HTTP requests, such as to external APIs. It returns a channel, so you await it with the `<-` channel operator.

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

### Properties
- `url`: The URL of the request
- `options`: An object containing the options for the request
  - `method`: The HTTP method of the request - defaults to GET
  - `headers`: The headers of the request
  - `body`: The body of the request
  - `timeout`: The timeout of the request, in milliseconds - defaults to 10 seconds

## Request
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
### Properties
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

### Methods
- `clone()`: Creates a new `Request` object with the same properties
- `text()`: Parses the body as a string, returns a `string`
- `json()`: Parses the body as JSON, returns a Result enum
- `formData()`: Parses URL-encoded body, returns a `Map<string, string>`

## Response
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

### Properties
- `status`: The HTTP status code - default is 200
- `statusText`: The HTTP status text - default is ""
- `ok`: A boolean indicating whether the response status code is in the range 200-299
- `headers`: The headers to include in the response
- `body`: The body of the response - default is ""

### Methods
- `text()`: Parses the body as a string, returns a `string`
- `json()`: Parses the body as JSON, returns a Result enum
- `formData()`: Parses URL-encoded body, returns a `Map<string, string>`

## StreamingResponse
Send HTTP responses with chunked transfer encoding, allowing data to be streamed to the client as it becomes available. Perfect for LLM inference, large file downloads, or any scenario where you want to send data incrementally.

```ngn
fn handler(req: Request): StreamingResponse {
  const chunks = channel<string>()
  
  // Background thread produces data
  thread(|| {
    chunks <- "First chunk\n"
    sleep(500)
    chunks <- "Second chunk\n"
    sleep(500)
    chunks <- "Done!\n"
    chunks.close()  // Signals end of stream
  })
  
  return StreamingResponse {
    headers: { "Content-Type": "text/plain" },
    body: chunks
  }
}

export default { fetch: handler }
```

### Properties
- `status`: The HTTP status code - default is 200
- `headers`: The headers to include in the response
- `body`: A `channel<string>` that produces chunks. Close the channel to end the stream.

## SseResponse (Server-Sent Events)
Server-Sent Events (SSE) streams a sequence of events over a single HTTP response. This is useful for realtime updates (notifications, progress updates, model inference tokens, etc.).

SSE works over both HTTP and HTTPS in ngn.

```ngn
fn handler(req: Request): SseResponse {
  const events = channel<SseMessage>()

  thread(|| {
    events <- SseEvent { data: "Hello", event: "hello", id: "", retryMs: 0, comment: "" }
    sleep(500)

    // Send raw strings as event data
    events <- "World"
    sleep(500)

    // Send raw objects shaped like SseEvent
    events <- { data: "Hello", event: "hello", id: "", retryMs: 0, comment: "" }

    events.close()
  })

  return SseResponse {
    status: 200,
    headers: { "Access-Control-Allow-Origin": "*" },
    body: events,
    keepAliveMs: 15000,
  }
}

export default { fetch: handler }
```

### Properties
- `status`: The HTTP status code - default is 200
- `headers`: The headers to include in the response
- `body`: A `channel<SseMessage>` that can send either a `string` (treated as event data), an `SseEvent`, or a raw object that represents an `SseEvent`
- `keepAliveMs`: Optional keepalive interval (in milliseconds). If > 0, the server periodically sends `: keepalive` comments while idle.

### SseEvent properties
- `data`: Event payload (string). Newlines are sent as multiple `data:` lines.
- `event`: Optional event name (maps to the SSE `event:` field)
- `id`: Optional event id (maps to the SSE `id:` field)
- `retryMs`: Optional client reconnection hint (maps to the SSE `retry:` field)
- `comment`: Optional comment line (maps to the SSE `: ...` field)

## WebSocketResponse
WebSockets provides a full-duplex channel between a client and your server over a single upgraded HTTP connection.

In ngn, a WebSocket connection is represented by two channels:
- `recv`: messages from the client (client -> server)
- `send`: messages to the client (server -> client)

v1 notes:
- supports `string` (text frames) and `bytes` (binary frames) - i.e. `WsMessage` type
- no subprotocol selection

```ngn
fn handler(req: Request): WebSocketResponse {
  const recv = channel<WsMessage>()
  const send = channel<WsMessage>()

  // Echo everything back
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

### Properties
- `headers`: The headers to include in the 101 Switching Protocols response (optional)
- `recv`: A `channel<WsMessage>` that receives client messages. It is closed when the client disconnects.
- `send`: A `channel<WsMessage>` used to send messages to the client. Close it to close the websocket.

## Time

Work with dates and times using the `time` global.

### DateTime Model

All time functions return or work with the `DateTime` model, which has the following fields:

| Field | Type | Description |
|-------|------|-------------|
| `year` | `i64` | Full year (e.g., 2026) |
| `month` | `i64` | Month (1-12) |
| `day` | `i64` | Day of month (1-31) |
| `hour` | `i64` | Hour (0-23) |
| `minute` | `i64` | Minute (0-59) |
| `second` | `i64` | Second (0-59) |
| `weekday` | `i64` | Day of week (0=Monday, 6=Sunday) |
| `timestamp` | `i64` | Unix timestamp in seconds |
| `timestampMs` | `i64` | Unix timestamp in milliseconds |

### now()

Get the current local time as a `DateTime` model.

```ngn
const now = time.now()
print("Current year: ${now.year}")
print("Current month: ${now.month}")
print("Current day: ${now.day}")
print("Hour: ${now.hour}:${now.minute}:${now.second}")
print("Weekday: ${now.weekday}") // 0=Monday
print("Unix timestamp: ${now.timestamp}")
```

### utc()

Get the current UTC time as a `DateTime` model.

```ngn
const utc = time.utc()
print("UTC hour: ${utc.hour}")
```

### unix()

Get the current Unix timestamp in seconds as an `i64`.

```ngn
const timestamp = time.unix()
print("Seconds since epoch: ${timestamp}")
```

### unixMs()

Get the current Unix timestamp in milliseconds as an `i64`.

```ngn
const timestampMs = time.unixMs()
print("Milliseconds since epoch: ${timestampMs}")
```

### parse()

Parse a date string into a `DateTime` model using a format string. Returns `Result<DateTime, string>`.

`parse(dateString, formatString)`

```ngn
const result = time.parse("2026-01-21 15:30:45", "%Y-%m-%d %H:%M:%S")
match (result) {
  Ok(dt) => {
    print("Year: ${dt.year}")
    print("Month: ${dt.month}")
    print("Day: ${dt.day}")
  },
  Error(e) => print("Parse error: ${e}")
}
```

#### Format Specifiers

| Specifier | Description | Example |
|-----------|-------------|---------|
| `%Y` | Full year | 2026 |
| `%m` | Month (01-12) | 01 |
| `%d` | Day of month (01-31) | 21 |
| `%H` | Hour (00-23) | 15 |
| `%M` | Minute (00-59) | 30 |
| `%S` | Second (00-59) | 45 |
| `%B` | Full month name | January |
| `%b` | Abbreviated month | Jan |
| `%A` | Full weekday name | Wednesday |
| `%a` | Abbreviated weekday | Wed |

## import/export

See the [Modules](../modules) page for details.
