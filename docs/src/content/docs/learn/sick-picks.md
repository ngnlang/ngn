---
title: Sick Picks
description: You gotta try these!
---

Some of our favs.

## Piping

Pipe data through functions and expressions.

You don't need to call the functions, but you can if you really want to. ngn automatically takes the previous output and calls the functions for you, passing the data as the first argument.

```ngn
var x = -10

// these do the same thing
const piped = x |> abs |> sqrt
const nested = sqrt(abs(x))

const res = 5 |> add(7) // 12
```

You can destructure arrays and tuples with the `$n` syntax. Notice that `$1` would be the 0 index.

```ngn
const pair = (2, 3)
const res = pair |> add($1, $2) // 5
```

You can also access the entire piped value with `$`.

```ngn
// $1 above is not the same as what we do below
// with index-based access, since $ is referencing the entire array
const key = 1
const picked = ["a", "b", "c"] |> $[key] // grabs second element, at index 1
assert(picked == "b") ✅

const tuple_joined = ("b", "a") |> $.join(",")

// use expressions too
const price = 100 |> $ * 1.05 |> round() // explicit fn call syntax
```

## State

Create mutable, atomic state; most useful when mutating data from multiple places, like threads.

```ngn
const counter = state(0)

counter.update(|n| n + 1) // we pass a closure
// "n" is a snapshot of the current value of "counter",
// internally, we write back the new value
```

You can also call `.read()`; as well as `.write()`, which is an overwrite of the current value and should not be used in combination with `.update()` unless you can guarantee order of operations - otherwise, you may get unexpected results.

:::note
Yep, you saw that correctly: you can use `const` to declare state variables; because the methods are mutating the value it holds, not the state variable itself.

However, you can mutate a state variable into another one, using `var`; although the same effect could be achieved with the `.write()` method. I wouldn't recommend the below pattern.
```ngn
var counter = state(0)
counter = state(100)
```
:::

## Closures

Similar to functions, but with two key differences:
- they capture data from outside their environment
- you can assign them to a constant, then call them

1. Values are captured at closure creation time.
    ```ngn
    var count = 0

    // captures `count` as 0
    const incrementBy = |a: i64| count + a

    print(incrementBy(10)) // 10

    print(incrementBy(5)) // 5

    count = 100
    print(incrementBy(7)) // still 7

    // captures `count` as 100
    const incrementCount = |a: i64| count + a
    print(incrementCount(7)) // 107
    ```

2. You can mimic classic "close over" behavior by returning a closure from a function.
    ```ngn
    fn main() {
      fn make_counter() {
        var count = state(0)
        
        return || {
          count.update(|c| c + 1)
          print(count)
        }
      }

      // we're assigning the returned closure here
      const counter = make_counter()
      counter()  // 1
      counter()  // 2
      counter()  // 3
    }
    ```

:::note
As you get to know ngn, you'll notice it's common to pass a closure to various things.
:::

## Channels

Send and receive data. You must set a data type for the messages.

```ngn
fn main() {
  const ch = channel<string>()

  // simulate sending an "unknown" amount of messages
  ch <- "Hello"
  ch <- "World"

  // close the channel, so the `for` loop can break
  // once the channel is empty (channels buffer messages)
  ch.close()

  // <- is used to stop and wait for one message.
  // Here, we use the `?` to indicate we are to receive
  // an unknown number of messages.
  for (msg in <-? ch) {
    print(msg)
  }

  /*
    Hello
    World
  */
}
```

:::note
You can explicitly set the number of messages you're expecting.
```ngn
<- ch // one message
<-3 ch // three messages
<-tasks.size() ch // expression-based
```
:::

## Threads

Do some work in another thread while the main thread continues. The thread returns a channel to make it easier to send data back, using a simple `return`.

```ngn
fn main() {
  const done = thread(|| {
    sleep(200)
    return "Thread work complete!"
  })

  print("Main thread continues immediately")

  // stop and wait on on a single message in the channel
  const result = <- done
  print(result)

  /*
    Main thread continues immediately
    Thread work complete!
  */
}
```

## Extend ngn data types

Not finding a native method for these? Create your own!

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

  // if a number method applies to the generic `number` type,
  // you should explicitly set the result type
  const y: i32 = x.double()

  "   ".isBlank() // true
}
```
