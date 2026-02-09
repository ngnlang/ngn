---
title: Hello World
description: Write and run your first ngn program.
---

## Create a project folder

```bash
mkdir ngn && cd ngn
```

## Create an entrypoint

Create a file named `main.ngn` with the following:

```ngn
fn main() {
  print("Hello, ngn!")
}
```
> Your entrypoint file's name does not have to be main.ngn

## Run it

```bash
ngn run main.ngn
```

You should see:

```txt
Hello, ngn!
```
