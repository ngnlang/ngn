---
title: Concurrency
description: Channels, threads, and shared state.
---

Concurrency in ngn is explicit, readable, and composable.

## Channels

```ngn
const ch = channel<string>()
ch <- "hello" // send a message to the channel
const msg = <- ch // wait for a single message and assign it
```

The `<-` operator can also wait for multiple messages:

```ngn
<-2 ch
<-tasks.size() ch
<-? ch // must be used in a "for" loop or "match" statement
```

## Threads
Creating a thread also returns a channel to you; then simply return data from the thread closure in order to send to the channel.

```ngn
const done = thread(|| {
  sleep(250)
  return "finished"
})

const result = <- done // finished
```

## Shared state


