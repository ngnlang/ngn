---
title: Environment
---

Access environment variables from both process environment and `.env` files.

## Automatic .env loading

ngn automatically loads `.env` files at startup in this order (later overrides earlier):
1. `.env` - Base configuration
2. `.env.{mode}` - Mode-specific (production, development, test)
3. `.env.local` - Local overrides (should be gitignored)

The mode is determined by the `NGN_MODE` environment variable (default: `development`).

## Direct key access

Access environment variables directly as properties. Returns `Maybe<string>`:

```ngn
// Direct property access
const db = env.DATABASE_URL ?? "localhost"
const port = env.PORT ?? "3000"

// With pattern matching
match (env.API_KEY) {
  Value(key) => print("Key: ${key}"),
  Null => panic("API_KEY is required!"),
}
```

## get()
Get by dynamic key. Returns `Maybe<string>`.

```ngn
const key = "DATABASE_URL"
const value = env.get(key) ?? "default"
```

## has()
Check if a variable exists. Returns `bool`.

```ngn
if (env.has("DEBUG")) {
  print("Debug mode enabled")
}
```
