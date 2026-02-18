# Releasing

This repository now has two independent release tracks:

- NGN runtime/CLI releases (`v*` tags)
- NGN LSP releases (`lsp-v*` tags)

## 1) NGN release (runtime/CLI)

Use this when shipping `ngn` changes.

1. Bump the workspace version in `Cargo.toml` (`[workspace.package].version`).
2. Commit the version bump.
3. Tag the commit as `v<version>` (example: `v0.3.0`).
4. Push commit + tag.

The `release-ngn` workflow (`.github/workflows/release.yml`) will:

- build NGN artifacts,
- publish versioned assets,
- publish rolling `ngn-latest-*` assets.

## 2) LSP-only release

Use this when shipping `crates/lsp` changes without a new NGN runtime release.

1. Bump `version` in `crates/lsp/Cargo.toml`.
2. Commit the version bump.
3. Tag the commit as `lsp-v<version>` (example: `lsp-v0.2.2`).
4. Push commit + tag.

The `release-lsp` workflow (`.github/workflows/release-lsp.yml`) will:

- validate the tag matches `crates/lsp/Cargo.toml` version,
- build `ngn-lsp`,
- publish `ngn-lsp-v<version>-linux-x86_64.tar.gz` to the `lsp-v<version>` release,
- update rolling `lsp-latest` release asset `ngn-lsp-latest-linux-x86_64.tar.gz`.

## Notes

- VS Code extension auto-download uses the rolling `lsp-latest` asset.
- LSP releases do not require an NGN `v*` tag.
- NGN releases do not publish LSP artifacts.
