# ngn

An expressive, strongly and statically typed programming language. Built with Rust.

Pronounced "engine"

## Status

Extremely early development.

## Working API

### Identifiers
All identifiers are locally scoped.

| keyword | binding | value | type | example | result type |
|------|-------|------|------|-------|-------|
| const | immutable | immutable | literal | `const engine = "ngn"` | ngn |
| var | mutable | mutable | widened | `var status = "go"` | String |

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
> This may change.
```ngn
fn add(a, b) => a + b
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
- `array` of numbers
- `array<type>` of type


#### explicit
```ngn
const thing: string = "one"
var answer: number = 42
var truth: boolean = false
const things: array = [1, 2, 3]
const stuff: array<string> = ["shirt", "hat", "coat"]
```

#### implicit
Currently only supports declarations, not function parameters or return types, nor expression results.

```ngn
const thing = "one" // inferred to `string`
const answer = 42 // inferred to `number`

const result = 3 + 2 // `result` not inferred as `number`

// `a`, `b`, and return type are not inferred
fn add(a, b)
  return a + b
end
```
