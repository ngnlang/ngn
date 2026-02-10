---
title: Install
description: Install the ngn CLI and verify your setup.
---

ngn is only supported on Linux

:::note
If using bash or zsh, the installer will add ngn and it's runtime, ngnr, to your `PATH`.
:::

By default, ngn installs at `~/.local/bin`. To customize this, prefix the below command with `NGN_INSTALL_DIR=/custom/dir/path`

```bash
curl -fsSL https://ngnlang.com/install | bash
```

Architecture is auto-detected, but you can be explicit (x86_64, amd64, arm64, aarch64)
```bash
curl -fsSL https://ngnlang.com/install -a aarch64 | bash
```

## Upgrade

Upgrade ngn in place (uses the current install directory):

```bash
ngn upgrade
```

Overrides:

```bash
ngn upgrade --version latest --arch x86_64 --dir ~/.local/bin
```

## Verify

If this doesn't work, you may need to add `ngn` and `ngnr` to your `PATH`, or restart your terminal.

```bash
ngn --version
```
