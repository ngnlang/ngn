---
title: Let's Go
description: Install the ngn CLI and verify your setup.
---

ngn is only supported on Linux

:::note
If using bash or zsh, the installer will add ngn and it's runtime, ngnr, to your `PATH`.
:::

## Install

By default, ngn installs at `~/.local/bin`. To customize this, prefix the below command with `NGN_INSTALL_DIR=custom/dir/path`

```bash
curl -fsSL https://ngnlang.com/install | sh
```

## Verify

If this doesn't work, you may need to add `ngn` and `ngnr` to your `PATH`, or restart your terminal.

```bash
ngn --version
```
