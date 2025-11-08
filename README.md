# ngn

An expressive, strongly and statically typed programming language. Built with Rust.

Pronounced "engine".

## Status

Extremely early development.

## `main()`

Your entrypoint file must define a `main()` function, which is run automatically; you do not have to call it.

Most of your code will live inside of this function, but things like `model`s, `role`s, `extend`s, and global delcarations will not.

## Declaring identifiers
> You cannot assign functions using this syntax. Use `fn` instead.

ngn follows in the footsteps of Rust's ownership model, but tries to make it easier to use and reason about.

| keyword | scope | binding | value | ownership | example | type |
|-------|-------|-------|-------|-------|-------|-------|
| var | local | mutable | immutable | borrowed | `var x = "hello"` | string |
| var | local | mutable | mutable | owned | `var z =< "world"` | string |
| const | local | immutable | immutable | borrowed | `const status = "go"` | string |
| lit | global | immutable | immutable | borrowed | `lit VERSION = "2"` | string |
| static | global | immutable | immutable | borrowed | `static DATA = [1..=1000]` | [i32] |

> The `static` example uses psuedocode to mimic creating an array of numbers from 1 to 1000, inclusively.

### `var`

```ngn
var x = "hello" // declares `x` as a borrowed `string`
x = "hello!" ❌ // value is immutable since it's borrowed
rebind x = 0 ✅ // is rebindable, which allows you to change the value and type
```
```ngn
var x =< "hello" // declares `x` as an owned `string`
x = "hello!!" ✅ // value is mutable since it's owned
rebind x = "goodbye" ✅ // is rebindable
```
```ngn
var x = "hello" // borrowed `string`

fn doThing(thing: <string) { // requires owned string via `<` prefix
  // consume thing
}

doThing(x) ❌ // cannot use a borrowed arg for a function that expects an owned param
```
```ngn
model User {
  name: string,
  role: string
}

// use `=<` to ensure all relevant properties are owned, per types in the model
var user =< User {
  name: "Sam",
  role: "Developer"
}

fn readUser(u: User) {
  // only read u, not consume
}

// Can pass an owned variable to a function that expects a borrowed param
// ngn handles this for you
readUser(user)

// can still do things with `user` here
```

### `const`

```ngn
const x = "hello" // declare x as a borrowed `string`
x = "hello!!" ❌ // value is immutable since it's borrowed
rebind x = "goodbye" ❌ // is not rebindable since it's a constant
```
```ngn
const x =< "hello" // owned `string`

// separate with a space, if that's more readable for you: `(thing: < string)`
fn doThing(thing: <string) { // requires an owned string, using owned (`<`) syntax
  // consume thing
}

doThing(x) ✅

print(x) ❌ // can no longer use `x` in this context
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

## Working API

> All code examples assume you're placing them in the correct context within the file.

```ngn
const version = 2
version = 3 ❌

var age = 41
age = 42 ✅
```

### `echo`
Log to the console, without formatting.
```ngn
const name = "ngn"
echo(name)
// ngn

echo("Hello")
echo("World")
// HelloWorld
```

### `print`
Line logging to the console. Implicit `\n`.
```ngn
print("Hello")
print("World")
// Hello
// World
```

### String Interpolation
```ngn
const x = 5
print("x plus 1 is {x + 1}")

const greeting = "world"
print("Hello, {greeting}!")
```

### Arrays
```ngn
var stuff = ["hat", "coat", "gloves"]
const ages = [3, 8, 15, 23]

// cannot mix types
const stuff = ["hat", true, 3] ❌
```

### `while`
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

#### `once` variant
To always run the statement block once, before checking the condition.
```ngn
while once (condition) {
  statement
}
```

### `until`
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

#### `once` variant
To always run the statement block once, before checking the condition.
```ngn
until once (condition) {
  statement
}
```

### `if`
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

Can also be inlined:
```ngn
if (condtion) statement : (condition) statement : statement
```

#### `not` variant
If you're concerned about code readability in certain situations, you can use the `not` operator in place of `!`.

So, instead of
```ngn
if (!browser) print("not browser")
```
you can do
```ngn
if not (browser) print("not browser")
```

### `match`
Match a value (Number, String, Boolean) against one test case. Optionally provide a default case.

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
  },
  => statement
}
```

#### `any` variant
Even if a match is found, continue checking cases unless a matched statement block contains the `break` keyword.

```
match any (value) {
  test => statement,
  test1 | test2 => {
    statement
    statement
  },
  => statement
}
```

### `fn`
You can write functions in various ways; along with passing them as arguments to other functions.

#### Traditional block, explicit return
```ngn
fn add(a, b) {
  return a + b
}
```

#### Traditional block, explicit multiline return
```ngn
fn add(a, b) {
  return (
    a + 
    b
  )
}
```

#### Implicit return
```ngn
fn add(a, b) a + b
```

#### Side-effects only
```ngn
fn doThing() {
  print("something")
}
```

### Closures
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

Unlike functions, closures allow you to access the state of their surrounding scope.
```ngn
var base = 10

const tally = |a: number| base + a
print(tally(3)) // 13
```

However, by default, this access is a read-only snapshot of the variable's state when the closure is created; so, changing the state afterwards does not change the state inside the closure.
```ngn
var base = 10

const tally = |a: number| base + a
print(tally(3)) // 13

rebind base = 100
print(tally(3)) // still 13
```

If you want to mutate state from a closure, use `rebind` on the variables the closure uses. This approach helps you avoid accidentally mutating outside state when you didn't intend to. Using `rebind` in this way also syncs the variable's state within the closure, no matter where the variable is updated from.
```ngn
var count = 0

const incrementBy = |a: number| rebind count = count + a

incrementBy(10)
print(count) // 10

incrementBy(5)
print(count) // 15

rebind count = 100
incremenBy(7)
print(count) // 107
```

### `model`
Create object structures.

```ngn
model Dog {
  name: string,
  breed: string
}
```

#### Instantiate a model
```ngn
const dog = Dog {
  name: "Apollo",
  breed: "Labrador"
}
print(dog) // { name: Apollo, breed: Labrador }
print(dog.name) // Apollo
```

### `role`
Declare one or more method signatures and/or method implementations. Create roles in order to later implement their functionality for models.

```ngn
role Animal {
  fn speak(): void
}
```

See below on how to use roles.

### `extend`
Extend a model's functionality with methods. You can implement custom methods or base them off of a role.

```ngn
extend Dog with {
  fn fetch(thing: string): bool {
    return attemptToFetch(thing)
  }
}
```

```ngn
extend Dog with Animal {
  fn speak(): void {
    print("Woof, woof!")
  }
}
```

Now, putting it all together:
```ngn
const fetched = dog.fetch("stick")
print(fetched) // either true or false

dog.speak() // Woof, woof!
```

### Types

- `string`
- `number`
- `boolean`
- `array`
- `array<type>`
- `void`


#### explicit
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

#### implicit
Supported for literals and expressions, as well as inside functions (requires explict types for fn params and return).

```ngn
const thing = "one" // inferred to `string`
const answer = 42 // inferred to `number`

const result = 3 + 2 // `result` inferred as `number`

fn add(a: number, b: number): number {
  return a + b // inferred as numbers
}
```
