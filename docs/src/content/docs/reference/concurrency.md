---
title: Concurrency
description: Channels, threads, and shared state.
---

Concurrency in ngn is explicit, readable, and composable.

## Channels

```ngn
const ch = channel<string>()
ch <- "hello"
const msg = <- ch
```

The `<-` operator can wait for multiple messages:

```ngn
<-2 ch
<-tasks.size() ch
<-? ch // must be used in a "for" loop or "match" statement
```

## Threads

```ngn
const done = thread(|| {
  sleep(250)
  return "finished"
})

const result = <- done
```

## Shared state

```ngn
var counter = state(0)
counter.update(|n| n + 1)
```
