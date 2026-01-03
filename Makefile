# Build ngn with embedded runtime
# Usage: make release

.PHONY: release clean runtime

# Build the minimal runtime first, then the full ngn with embedded runtime
release: runtime
	cargo build --release --bin ngn

# Build runtime binary separately (must be done first)
runtime:
	cargo build --release --bin runtime
	mkdir -p target/embed
	cp target/release/runtime target/embed/runtime_binary

clean:
	cargo clean
	rm -rf target/embed
