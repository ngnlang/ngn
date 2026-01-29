---
title: Hello World
description: Write and run your first ngn program.
---

Every ngn program starts with a `main()` function in the entrypoint file.

## Create a project folder

```bash
mkdir ngn
cd ngn
```

## Create an entrypoint

Create a file named `main.ngn` with the following:

```ngn
fn main() {
  print("Hello, ngn!")
}
```

## Run it

```bash
ngn run main.ngn
```

You should see:

```txt
Hello, ngn!
```
