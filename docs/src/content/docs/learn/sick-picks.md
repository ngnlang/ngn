---
title: Sick Picks
description: You gotta try these!
---

Here are some features you should know about.

## State

Used for mutable, atomic state; most useful when mutating data from multiple places, like threads.

```ngn
var counter = state(0)

// "n" is a snapshot of the current value of "counter",
// internally, we write back the new value
counter.update(|n| n + 1) // we pass a closure
```

You can also call `.read()` and `.write()`. Writing sets the state variable's value, and should not be used in combination with `.update()` unless you can guarantee order of operations - otherwise, you may get unexpected results.

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
