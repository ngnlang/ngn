---
title: Concurrency Basics
description: Learn channels, threads, and shared state.
---

ngn uses channels and threads for concurrency. There is no async keyword and no await chain.

## Channels

```ngn
fn main() {
  const ch = channel<string>()
  ch <- "Hello"
  ch <- "World"
  ch.close()

  for (msg in <-? ch) {
    print(msg)
  }
}
```

## Threads

```ngn
fn main() {
  const done = thread(|| {
    sleep(200)
    return "done"
  })

  // optionally, do other work while thread runs

  // stop and wait on on a single message in the channel
  const result = <- done
  print(result)
}
```

## Shared state

Use `state()` when multiple threads might mutate data.

```ngn
fn main() {
  var counter = state(0)
  const done = channel<bool>()

  thread(|| {
    // "n" is a reference to the current value of the state
    counter.update(|n| n + 10)
    done <- true
  })

  thread(|| {
    counter.update(|n| n + 5)
    done <- true
  })

  <-2 done
  print(counter) // always 15
}
```
