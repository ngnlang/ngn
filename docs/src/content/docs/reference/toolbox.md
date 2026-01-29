---
title: Toolbox
description: ngn's "standard library".
---

ngn ships a "standard library" called the toolbox. It has module namespace of `tbx::`.

## Imports

Non-exhaustive list. See API reference for more.

```ngn
import { abs, floor, ceil } from "tbx::math"
import { assert } from "tbx::test"
import { serve } from "tbx::http"
import { load, stream } from "tbx::llm"
import { run, stream } from "tbx::process"
import { read, write } from "tbx::io"
```
