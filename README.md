# ngn

Pronounced "engine".

An expressive and easy to use programming language, that's strongly and statically typed.

## Status

Extremely early development.

## `main()`

Your entrypoint file must define a `main()` function. It's found and run automatically. Most of your code will live inside of this function, but not everything.

## Declaring identifiers

ngn follows in the footsteps of Rust's ownership model, but tries to make it easier to use and reason about.

| example | scope | binding | value | ownership |
|-------|-------|-------|-------|-------|
| `var x = "hello"` | local | mutable | immutable | borrowed |
| `var z =< "world"` | local | mutable | mutable | owned |
| `const status = "go"` | local | immutable | immutable | borrowed |
| `lit VERSION = "2"` | global | immutable | immutable | borrowed |
| `static DATA = [1..=1000]` | global | immutable | immutable | borrowed |

> The `static` example uses psuedocode to mimic creating an array of numbers from 1 to 1000, inclusively.

### `var`

```ngn
var x = "hello" // declares `x` as a borrowed string
x = "goodbye" ❌ // value is immutable since it's borrowed
rebind x = 0 ✅ // is rebindable, which allows you to change the value and type
```
```ngn
var x =< "hello" // declares `x` as an owned string
x = "goodbye" ✅ // value is mutable since it's owned
rebind x = "cya" ✅ // is rebindable
```
```ngn
var x = "hello" // borrowed

fn doThing(thing: <string) { // the `<` means it requires an owned string
  // do thing
}

doThing(x) ❌ // function requires an owned argument
```
```ngn
var x =< "hello" // owned string

// separate with a space, if that's more readable for you: `(thing: < string)`
fn doThing(thing: <string) {
  // do thing
}

doThing(x) ✅ // moves ownership of `x` to the function

print(x) ❌ // `x` is no longer available, since it's ownership was moved
```
```ngn
model User {
  name: string,
  role: string
}

// use `=<` to ensure all relevant properties are owned
var user =< User { // owned
  name: "Sam",
  role: "Developer"
  years: 3
}

fn readUser(u: User) { // only require a borrowed User
  // take a read action on the reference to `u`; cannot mutate
}

// Can pass an owned variable to a function that expects a borrowed param
// ngn ensures it's "downgraded" to borrowed within the function
readUser(user) ✅

print(user) ✅ // can still do things with `user` here
```

### `const`

```ngn
const x = "hello" // borrowed
x = "goodbye" ❌ // value is immutable since it's borrowed
rebind x = "goodbye" ❌ // is not rebindable since it's a constant
```

### `lit` vs `static`

Used for global declarations. Must be delcared at the top-level of a file, not inside `main()`.

```ngn
lit VERSION = "v3"
static DATA = [1..=1000] // pseudocode to create an array of numbers from 1 to 1000

fn main() {
  print(VERSION) // v3
  print(DATA) // (I'll spare you; you get it.)
}
```

- With `lit`, all instances are replaced with the literal value at compile time.
- With `static`, all instances are a reference to a single memory address, of the stored data, at runtime.

Therefore, it's best to use `lit` when assigning a small amount of data. If you're declaring a large amount of data - like an array with 1000 entries - then use `static`.

Think about it this way: if you had an array of 1000 numbers defined using `lit`, every single use of it in your code would be replaced with the entire array of data. This would be an inefficient use of memory. Whereas if you define the large array with `static`, ngn stores a single instance of the data in memory and any code references to it are just that, a reference, not the entire data set.

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
print("x plus 1 is {x + 1}")

const greeting = "world"
print("Hello, {greeting}!")
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
Create an array of strings by splitting a string on a pattern of characters. If you do not pass a pattern, each character in the string is split individually. Preserves the original string.

```ngn
const sent = "The quick brown fox jumped over the fence."
const arr = sent.split("o") // ["The quick br", "wn f", "x jumped ", "ver the fence."]

var greeting = "Hello"
const arr = greeting.split() // ["H", "e", "l", "l", "o"]
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
const some = "Some Stuff" // borrowed
const copied = some.copy(5) // you can copy borrowed strings

print(copied) // "Stuff"
print(some) // "Some Stuff"

var all = some.copy()

print(all) // "Some Stuff"
print(some) // "Some Stuff"
```

### `slice(start, stop?)`
Remove a section of a string by providing a start index and an optional stop index. This changes the original string and returns the item(s) as a new string.

- If `stop` is provided, the slice excludes the item at that index.
- If `stop` is not provided, it removes everything upto and including the last item.
- Since you're mutating the original string, it must be owned.
```
var stuff =< [10, 20, 30, 40, 50]
const sliced = stuff.slice(1, 3)

print(sliced) = [20, 30]
print(stuff) // [10, 40, 50]
```

### `upper()`
Transform a string to all uppercase. String must be owned.

```ngn
var version =< "one"
print(version.upper()) // ONE
```

### `lower()`
Transform a string to all lowercase. String must be owned.

```ngn
var version =< "ONE"
print(version.lower()) // one
```

### `trim()`
Remove whitespace from both ends of a string. String must be owned.

```ngn
var =< thing = " strong "
print(thing.trim()) // "strong"
```

### `repeat(num)`
Repeat a string some number of times.

```ngn
const ending = "goodbye"
print(greeting.repeat(2)) // goodbyegoodbye
```

## Numbers

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
If you want to mutate arrays, be sure to declare them with `=<`

```ngn
var stuff = ["hat", "coat", "gloves"]
const ages = [3, 8, 15, 23]

const mixed = ["hat", true, 7] ❌ // cannot mix types
```

### `size()`
Return the size of the array.

### `push(item)`
Push, i.e. add, an item to the end of an array. Returns the new size of the array as a `number`.
```
var stuff =< ["guitar", "shirt"]
const size = stuff.push("hat")

print(size) // 3
print(stuff) // ["guitar", "shirt", "hat"]
```

### `pop()`
Pop, i.e. remove, the last item of an array. Returns the removed item's value.
```
var stuff =< ["guitar", "shirt", "hat"]
const popped = stuff.pop()

print(popped) // hat
print(stuff) // ["guitar", "shirt"]
```

### `put(item, index?)`
Put, i.e. add, an item into an array. By default, it puts at the beginning. To put in another location, provide the index number. Returns the new size of the array as a `number`.
```
var stuff =< ["guitar", "shirt"]
const size = stuff.put("hat")

print(size) // 3
print(stuff) // ["hat", "guitar", "shirt"]

stuff.put("coat", 2)
print(stuff) // ["hat", "guitar", "coat", "shirt"]
```

### `pull(index?)`
Pull, i.e. remove, an item from an array. By default, it removes from the beginning. To pull from another location, provide the index number. Returns the removed item's value.
```
var stuff =< ["guitar", "shirt", "coat", "hat"]
const pulled = stuff.pull()

print(pulled) // guitar
print(stuff) // ["shirt", "coat", "hat"]

const pulled_one = stuff.pull(1)

print(pulled_one) // ["coat"]
print stuff // ["shirt", "hat"]
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
- Array must be owned.
```
var stuff =< [10, 20, 30, 40, 50]
const sliced = stuff.slice(1, 3)

print(sliced) = [20, 30]
print(stuff) // [10, 40, 50]
```

### `splice(item[], start?)`
Add multiple items to an array; optionally, at a specific index. Returns the new size of the array.

- If `start` is not provided, it adds the items at the end.
- Array must be owned.
```
var stuff =< [10, 20, 30]
stuff.splice([40, 50]) // [10, 20, 30, 40, 50]

const size = stuff.splice([45, 47], 4)

print(stuff) // [10, 20, 30, 40, 45, 47, 50]
print(size) // 7
```

## Enums: Result and Maybe

ngn provides two built-in enums for common patterns: `Result` for error handling and `Maybe` for possible values.

### Result<T, E>

`Result` represents an operation that can either succeed or fail.

#### Variants

- `Ok(T)` — The operation succeeded with a value of type `T`
- `Error(E)` — The operation failed with an error of type `E`

#### Examples

```ngn
fn divide(a: number, b: number): Result<number, string> {
    if b == 0 return Error("Division by zero")
    return Ok(a / b)
}

fn main() {
    const result = divide(10, 2)
    print(result)  // Result::Ok (5)
}
```

### Maybe<T>

`Maybe` represents a value that may or may not exist.

#### Variants

- `Value(T)` — The value exists
- `Null` — The value does not exist

#### Examples

```ngn
model User {
    name: string,
    email: Optional<string>  // May be Null
}

fn findUser(id: number): Maybe<User> {
    if user_exists return Value(user)
    return Null
}

fn main() {
    const user = findUser(1)
    print(user)  // Maybe::Value (...) or Maybe::Null
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
    const c = Red
    print(c)  // Color::Red
    
    const s = Inactive("maintenance")
    print(s)  // Status::Inactive (maintenance)
}
```

### Pattern Matching

Use `match` to work with enum values:

```ngn
match result {
    Ok(value) => print("Success: {value}"),
    Error(msg) => print("Failed: {msg}"),
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
const add = |a: number, b: number| a + b

const sum = add(3, 4)

const doThing = || {
  // do thing!
  print("Hello")
}
doThing()
```

Unlike functions, closures give you access to the state of their surrounding scope.
```ngn
var base = 10

const tally = |a: number| base + a
print(tally(3)) // 13
```

With borrowed variables, this access is a snapshot of the variable's value when the closure is created; so, changing the value afterwards does not change it inside the closure.
```ngn
var base = 10

const tally = |a: number| base + a
print(tally(3)) // 13

rebind base = 100 // does not change the value of `base` within the closure
print(tally(3)) // still 13
```

However, the value within the closure can be kept in sync when using owned variables or using `rebind`. This also mutates the variable outside the closure.

- implicit mutability for owned variables
- explicit mutability for rebindable variables, via `rebind`

```ngn
var count =< 0 // owned, mutable by default

const incrementBy = |a: number| count = count + a // `count`'s value is synced with the outside variable

incrementBy(10)
print(count) // 10

incrementBy(5)
print(count) // 15

rebind count = 100
incremenBy(7)
print(count) // 107
```
```ngn
var count = 0 // borrrowed, but rebindable

const incrementBy = |a: number| rebind count = count + a

incrementBy(10)
print(count) // 10

incrementBy(5)
print(count) // 15

rebind count = 100
incremenBy(7)
print(count) // 107
```

## Objects and Composability
You can create typed objects using models, then create a new instance of a model. You can also extend a model's functionality with direct methods or groups of methods via roles.

### `model`
Create object structures.

```ngn
model Dog {
  name: string,
  breed: string
}
```

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
  age: number
}

extend User with {
  fn new(name: string, age: number): User {
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
  age: number
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

The general rule is that you can mutate based on how the variable was declared (`var =`, `var =<`, `const`, etc). However, you can't change a field's type - which is typically allowed with `rebind`.

Here are the ways to manipulate an object's fields, based on the above example code:
- direct assignment, by owned `var`s: `user.age = 7`
- rebind, by owned or borrowed `var`s: `rebind user.age = 7`
- rebind entire object, by owned or borrowed `var`s: `rebind user = User { name: "Ben", age: 56 }`
- method, by owned `var`s: `user.changeName("Stacie")`
- method, by borrowed `var`s: `user.changeName("Stacie")` only if the method mutates via `rebind this.name = name`
- by `const`, `lit`, `static` variables: ❌ not allowed, as these are all strictly immutable

## Types

- `string`
- `number`
- `boolean`
- `array`
- `array<type>`
- `void`


### explicit
```ngn
const thing: string = "one"
var answer: number = 42
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
const thing = "one" // inferred to `string`
const answer = 42 // inferred to `number`

const result = 3 + 2 // `result` inferred as `number`

fn add(a: number, b: number): number {
  return a + b // inferred as numbers
}
```
