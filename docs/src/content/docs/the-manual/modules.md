---
title: Modules
---

You can use `export` and `import` to create modules in your project.

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

## Exporting types and models
```ngn
// types.ngn
export type UserId = i64

export model User {
  id: UserId,
  name: string
}
```

## Exporting roles and enums
```ngn
// domain.ngn
export role Named {
  fn name(): string
}

export enum Status {
  Active,
  Disabled,
  Error(string),
}
```

```ngn
// main.ngn
import { Named, Status } from "domain.ngn"

fn describe(status: Status): string {
  match status {
    Status::Active => "active"
    Status::Disabled => "disabled"
    Status::Error(message) => "error: ${message}"
  }
}
```

```ngn
// main.ngn
import { User, UserId } from "types.ngn"

fn format_id(id: UserId): string {
  return "id=${id}"
}

fn main() {
  const user = User { id: 7, name: "Ari" }
  print(format_id(user.id))
}
```
