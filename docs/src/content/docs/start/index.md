---
title: Start
description: What is ngn?
---

Pronounced "engine", it's a high-level programming language for the backend - APIs, MCP servers, and to serve AI purposes like LLM inference. It's intended to be easily learned and dearly loved; and draws inspiration from many other languages, with some unique modifications here and there.

ngn is composed of a cli (`ngn`) and a VM runtime (`ngnr`). When you `ngn build main.ngn`, a `main.mod` file is created (bytecode). So, in production, you'd do `ngnr run main.mod`

## What you can expect

A few highlights of ngn:

- concurrency and parallelism
- straight-forward async code via threads and channels
- concise await syntax, with no contagion
- shared, atomic state when needed
- built-in http/s server

:::note
I've driven the vision for ngn, and put a lot of thinking time into it's features, naming conventions, and syntax, but AI has written nearly 100% of the code - which I've reviewed _nearly_ all of.

Fear? That's someone else's problem.
:::