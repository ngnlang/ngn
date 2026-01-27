---
title: Install
description: Install the ngn CLI and verify your setup.
---

ngn ships as a single CLI you can install with curl.

:::note
The installer will add ngn and the runtime, ngnr, to your `PATH` when using bash or zsh.
:::

## Install

```bash
curl -fsSL https://ngnlang.com/install | sh
```

## Custom install directory

```bash
NGN_INSTALL_DIR=$HOME/bin curl -fsSL https://ngnlang.com/install | sh
```

## Verify

```bash
ngn --version
```

Once the CLI is available, you are ready to build your first program.
