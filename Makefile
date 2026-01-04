# Build ngn with embedded runtime
# Usage: make release

.PHONY: release clean runtime lsp all bench

# Build everything
all: release lsp

# Run benchmarks
bench:
	cargo run --release -p ngn --bin bench

# Build the minimal runtime first, then the full ngn with embedded runtime
release: runtime
	cargo build --release -p ngn --bin ngn

# Build runtime binary separately (must be done first)
runtime:
	cargo build --release -p ngn --bin runtime
	mkdir -p target/embed
	cp target/release/runtime target/embed/runtime_binary

# Build the LSP server
lsp:
	cargo build --release -p ngn-lsp

clean:
	cargo clean
	rm -rf target/embed
