---
title: Control Flow
description: Conditionals, loops, and pattern matching.
---

ngn combines classic control flow with compact inline forms.

## Inline if

```ngn
const label = if (x > 10) "high" : (x > 5) "medium" : "low"
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
loop {
  print("running")
  break
}

while (count < 3) count = count + 1
```

## Match

```ngn
match (value) {
  1 => print("one"),
  2 | 3 => print("two or three"),
  _ => print("other")
}
```
