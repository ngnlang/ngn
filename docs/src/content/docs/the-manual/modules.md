---
title: Modules
---

You can use `export` and `import` to create modules in your project. This is a functions-only feature.

```ngn
// math.ngn
export fn add(a, b) a + b

export fn subtract(a, b) a - b
```
Here, we look for an `add` function from a `math.ngn` file within the same directory that your `main.ngn` file is in.
```ngn
// main.ngn
import { add } from "math.ngn"

fn main() {
  print(add(21, 3)) // 24
}
```

## Relative path imports
```ngn
import { add } from "./lib/math.ngn"
```

## Aliased imports
```ngn
import { add as adder } from "math.ngn"
```

## Module imports
```ngn
// main.ngn
import * as Math from "math.ngn"

print(Math.subtract(10, 2)) // 5
```

## Default export
```ngn
// math.ngn
fn add(a, b) a + b

export default add
```
```ngn
// main.ngn
import add from "math.ngn"
```
