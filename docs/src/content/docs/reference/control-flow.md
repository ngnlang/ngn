---
title: Control Flow
description: Conditionals, loops, and pattern matching.
---

## Inline if

Inspired by ternaries, so we avoid the tendency for braces and "else/if" to become verbose. Each statement block can only have one statement.

```ngn
if (x > 10) print("high") : (x > 5) print("medium") : print("low")
```

You can go multiline if that's better for readability.

```ngn
if (x > 10)
  print("high")
: (x > 5)
  print("medium")
:
  print("low")
```

## Block if

Needed if one or more blocks have multiple statements.

```ngn
if {
  (ready)
    print("go")
  : (wait)
    print("hold")
  :
    print("unknown state")
    print("stop")
}
```

## Loops

```ngn
// infinite loop; manual break needed
loop {
  print("running")
  break
}

while (count < 3) count += 1

for (i in items) {
  print(i)
}
```

## Match

```ngn
match (value) {
  0 => print("invalid"),
  1 | 2 => print("low"),
  3..10 => print("in range")
  _ => print("high")
}
```
