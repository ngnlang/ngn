---
title: wtf?
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

However, there's a catch. The parser expects only one statement per block. If any of them have more than one, we just wrap things two braces:

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

So, we sacrifice a little weirdness for concise, powerful inline syntax.

## `slice` behavior

The `slice()` method on strings and arrays mutates the original. Why? Because ngn follows the thinking that if you take a slice of something, it's, well, gone from the thing. That's how pizza works, right? And I love pizza.

If you don't want to mutate the original, use `copy()` - it has the same API as `slice()`.

## Function params

You know how in Rust you would do `(name: &str)` to borrow data for a function, otherwise you transfer ownership of the data to the function? ngn essentially does the opposite - function params are borrowed by default, so they get a non-mutable copy.

This means that if you want to mutate a `var` inside of a function, you need to mark the param as "owned". Optionally, you could also do this if you're not going to use the data after it's passed in - for a smidge of early memory cleanup.
```ngn
var x = "hello"

// the `<` in front of the type assumes ownership
// and enables mutation inside the function
fn doSomething(stuff: <string) {
  // read and/or mutate "stuff",
  // then cleanup the memory
}

doSomething(x) ✅ // moves ownership of `x` to the function

print(x) ❌ // `x` is no longer available, since it's ownership was moved
```

In contrast, you can continue using passed borrowed data, but it can't be mutated inside the function.
```ngn
var x = "hello"

fn readThing(thing: string) {
  // can read "thing", but not mutate;
  // then cleanup the memory
}

readThing(x) ✅ // does not move ownership of `x` to the function

x = "goodbye" ✅ // `x` is still available
```

:::note

If you move ownership of data declared with `const`, it's not mutable, since... you know, constants aren't mutable in ngn*
:::

## "standard library"

Yeah, it's not called that in ngn, but you can use the phrase if you'd like. We call it the toolbox; after all, anyone who works on an engine has a toolbox, right?

```ngn
import { ceil } from "tbx::math"
```

idk, there's just something about having `std` in code that's a bit cringe.

