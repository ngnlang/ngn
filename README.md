# ngn

An expressive, strongly and statically typed programming language. Built with Rust.

Pronounced "engine".

## Status

Extremely early development.

## Declaring identifiers
> You cannot assign functions using this syntax. Use `fn` instead.

ngn follows in the footsteps of Rust's ownership model, but tries to make it easier to use and reason about.

| keyword | scope | binding | value | ownership | example | type |
|-------|-------|-------|-------|-------|-------|-------|
| var | local | mutable | immutable | borrowed | `var x = "hello"` | string |
| var | local | mutable | mutable | owned | `var z <- "world"` | string |
| const | local | immutable | immutable | borrowed | `const status = "go"` | string |
| lit | global | immutable | immutable | borrowed | `lit VERSION = "2"` | string |
| static | global | immutable | immutable | borrowed | `static DATA = [1..=1000]` | [i32] |

> The `static` example uses psuedocode to mimic creating an array of numbers from 1 to 1000, inclusively.

### `var`

```ngn
var x = "hello" // declares `x` as a borrowed `string`
x = "hello!" ❌ // value is immutable since it's borrowed
rebind x = "goodbye" ✅ // is rebindable, which allows you to change the value
```
```ngn
var x <- "hello" // declares `x` as an owned `string`
x = "hello!!" ✅ // value is mutable since it's owned
rebind x = "goodbye" ✅ // is rebindable
```
```ngn
var x = "hello" // borrowed `string`

fn doThing(thing: string) // requires owned string
  // consume thing
end

doThing(x) ❌ // cannot use a borrowed arg for a function that expects an owned param
```
```ngn
model User {
  name: string,
  role: string
}

// use `<-` to ensure all relevant properties are owned, per types in the model
var user <- User {
  name: "Sam",
  role: "Developer"
}

fn readUser(u: User)
  // only read u, cannot consume
end

// Pass `user` as borrowed
readUser(user)

// can still do things with `user` here
```

### `const`

```ngn
const x = "hello" // declare x as a borrowed `&string`
x = "hello!!" ❌ // value is immutable since it's borrowed
rebind x = "goodbye" ❌ // is not rebindable since it's a constant
```
```ngn
const x <- "hello" // owned `string`

fn doThing(thing: <string) // requires owned string, using `<` syntax
  // consume thing
end

doThing(x) ✅

print(x) ❌ // can no longer use `x` in this context
```

### `lit` vs `static`

- With `lit`, all instances are replaced with the literal value at compile time.
- With `static`, all instances are a reference to a single memory address, of the stored data, at runtime.

Therefore, it's best to use `lit` when assigning a small amount of data. If you're declaring a large amount of data - like an array with 1000 entries - then use `static`.

Think about it this way: if you had an array of 1000 numbers defined using `lit`, every single use of it in your code would be replaced with the entire array of data. This would be an inefficient use of memory. Whereas if you define the large array with `static`, ngn stores a single instance of the data in memory and any code references to it are just that, a reference, not the entire data set.

## `main()`

Your entrypoint file must define a `main()` function, which is run automatically; you do not have to call it.

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

### `while`
Run the statement block while the condition is true. Not guaranteed to run at all.
```ngn
while (condition)
  statement
  statement
end
```

Can be inlined if only using a single statement.
```ngn
while (condition) statement
```

#### `once` variant
To always run the statement block once, before checking the condition.
```ngn
while once (condition)
  statement
end
```

### `until`
Run the statement block until the condition is true. Not guaranteed to run at all.
```ngn
until (condition)
  statement
  statement
end
```

Can be inlined if only using a single statement.
```ngn
until (condition) statement
```

#### `once` variant
To always run the statement block once, before checking the condition.
```ngn
until once (condition)
  statement
end
```

### `if`
```ngn
if (condition)
  statement
  statement
: (condition)
  statement
:
  statement
end
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

### `match one`
Match a value (Number, String, Boolean) against one test case. Optionally provide a default case.

If a match is found:
- that branch's statement block is run.
- other cases are skipped, including the default, unless a matched statement block contains the `next` keyword.

```
const value = 3
match one (value)
  : 1
    statement
  : 2 || 3
    statement
  : 
    statement
end
```

### `match any`
Match a value (Number, String, Boolean) against one or more test cases. Optionally provide a default case.

If a match is found:
- that branch's statement block is run.
- other cases are also checked, including default, unless a matched statement block contains the `break` keyword.

```
match any (value)
  : test
    statement
  : test1 || test2
    statement
  : 
    statement
end
```

### `fn`
You can write functions in various ways; along with passing them as arguments to other functions.

#### Traditional block, explicit return
```ngn
fn add(a, b)
  return a + b
end
```

#### Traditional block, explicit multiline return
```ngn
fn add(a, b)
  return (
    a + 
    b
  )
end
```

#### Implicit return
```ngn
fn add(a, b) a + b
```

#### Side-effects only
```ngn
fn doThing()
  print("something")
end
```

### Types

- `string`
- `number`
- `boolean`
- `array`
- `array<type>` of type
- `void`


#### explicit
```ngn
const thing: string = "one"
var answer: number = 42
var truth: boolean = false
const things: array = [1, 2, 3]
const stuff: array<string> = ["shirt", "hat", "coat"]

fn sideEffects(): void
  // do something
end
```

#### implicit
Supported for literals and expressions, as well as inside functions (requires explict types for fn params and return).

```ngn
const thing = "one" // inferred to `string`
const answer = 42 // inferred to `number`

const result = 3 + 2 // `result` inferred as `number`

fn add(a: number, b: number): number
  return a + b // inferred as numbers
end
```
