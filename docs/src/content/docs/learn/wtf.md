---
title: WTF?
description: Things to know about ngn
---

What would a language be without some fun? You're welcome.

## we use `var` not `let`

Locally scoped, `var` is more congruent with `const` than `let` is. I suppose we could've gone with `let` and `make`? Nah.

## `if` statements

I've never liked the look of all the braces and else/if verbiage of an `if` statement. Why not get rid of them whenever possible?

I took some inspiration from ternaries and landed on this:
```ngn
if (x > 10)
  print("high")
: (x > 5)
  print("normal")
: print("low")

if (y == 10) print("perfect!") : (y <= 6) print("danger zone") : print("not bad!")
```

Damn that's beautiful.

However, there's a catch. The parser expects only one statement per block. If any of them have more, we just wrap everything with braces. So, at most, you'll only need two.

```ngn
if {
  (x > 10)
    print("high")
  : (x > 5)
    print("medium")
  : 
    print("low")
    print("so be it")
}
```
This is necessary so the parser knows where the `if` statement ends and where the next statement begins. Otherwise, is `print("so be it")` part of the "else" block or just the next statement after `if`? Indentation has no meaning in ngn.

So, we sacrifice occassional weirdness for concise, powerful inline syntax.

## `slice` behavior

The `slice()` method on strings and arrays mutates the original. Why? Because ngn follows the thinking that if you take a slice of something, it's, well, gone from the thing. That's how pizza works, right? And I love pizza.

If you don't want to mutate the original, use `copy()` - it has the same API as `slice()`.

## Functions

Functions are isolated data environments; i.e. you can't reference outside `var` or `const` data, only pass them in. However, you can reference/call sibling functions and globals - literally data defined with `global` and other top-level things (imports, enums, models, functions, etc).

## Function params

You know how in Rust you use `&`, as in `(name: &str)`, to borrow data for a function, otherwise you transfer ownership of the data to the function? ngn essentially does the opposite - function params are borrowed by default, so they get a non-mutable copy.

This means that if you want to mutate a passed-in `var` within a function, you need to mark the param as "owned". Optionally, you could also do this if you're not going to use the data after the function - for a smidge of early memory cleanup.
```ngn
var x = "hello"

// the `<` in front of the type assumes ownership
// and enables mutation inside the function
fn doSomething(stuff: <string) {
  // read and/or mutate "stuff".
  // memory is automatically cleaned up
}

doSomething(x) ✅ // moves ownership of `x` to the function

print(x) ❌ // `x` is no longer available, since it's ownership was moved
```

In contrast, you can continue using passed borrowed data, but it can't be mutated inside the function.
```ngn
var x = "hello"

fn readThing(thing: string) {
  // can read "thing", but not mutate.
  // the copy's memory is automatically cleaned up
}

readThing(x) ✅ // does not move ownership of `x` to the function

x = "goodbye" ✅ // `x` is still available
```

:::note

If you move ownership of data declared with `const`, it's not mutable, since... you know, constants aren't mutable in ngn*
:::

## Handling `Maybe`

ngn doesn't have a `null` value. Instead, we have a `Maybe` enum with `Value<T>` and `Null` variants. There are various ways of handling this, to get access to the data.

### match

```ngn
match (maybe_value) {
  Ok(val) => print(val),
  Null => print("no value")
}
```

### nullish coalescing operator

If there's a value, it's unwrapped and assigned.

```ngn
const thing = maybe_value ?? "fallback"
```

### if
Unwrap the value and make it available in the block.

```ngn
if (maybe_value?) {
  // success scenario
  // "maybe_value" is the actual data here
}
```

### check
If it's a `Maybe::Null`, run the failure block. You must either return or break within the block.

```ngn
check maybe_value? {
  // failure scenario
  // must return or break
}

// "maybe_value" is the actual data here
```

### Optional chaining

This unwraps the `Maybe` for further testing of nested values; the assigned value is wrapped in `Maybe`.

```ngn
const maybe_location = user?.meta_data?.location
```

## Parsing json

The `json.parse()` method returns a `Maybe` for unknown data. So, you need to check it using one of the above methods. However, this unknown data (from a user, for example), could throw a runtime error if you start using dot notation to access fields.

```ngn
const maybe_data = json.parse(data)
const name = maybe_data?.name // might error if "name" isn't there
```

It is safer to destructure the data, which returns `Maybe` for each field, which you can then test against.

```ngn
const { name } = maybe_data

check name? {
  // any error handling
  return
}

// "name" is actual data here
```

:::note
Using `json.parse()` on known model data returns the raw data, instead of a `Maybe`.
:::

## "standard library"

Yeah, it's not called that in ngn, but you can use the phrase if you'd like. We call it the toolbox; after all, anyone who works on an engine has a toolbox, right?

```ngn
import { ceil } from "tbx::math"
```

idk, there's just something about having `std` in code that's a bit cringe for me. Perhaps I'm just nitpicking.

