# Build ngn with embedded runtime
# Usage: make release

.PHONY: release clean runtime lsp all bench dist release-aarch64

# Build everything
all: release lsp

# Run benchmarks
bench:
	cargo run --release -p ngn --bin bench

# Build the minimal runtime first, then the full ngn with embedded runtime
release: runtime
	cargo build --release -p ngn --bin ngn
	$(MAKE) release-aarch64

# Build runtime binary separately (must be done first)
runtime:
	cargo build --release -p ngn --bin runtime
	mkdir -p target/embed
	cp target/release/runtime target/embed/ngnr
	cp target/release/runtime target/embed/runtime_binary
	cp target/release/runtime target/release/ngnr

# Build aarch64 binaries if cross toolchain is available
release-aarch64:
	@if rustup target list --installed | grep -q aarch64-unknown-linux-gnu; then \
		if command -v aarch64-linux-gnu-gcc >/dev/null 2>&1; then \
			cargo build --release -p ngn --bin runtime --target aarch64-unknown-linux-gnu; \
			cargo build --release -p ngn --bin ngn --target aarch64-unknown-linux-gnu; \
			cp target/aarch64-unknown-linux-gnu/release/runtime target/aarch64-unknown-linux-gnu/release/ngnr; \
		else \
			echo "Skipping aarch64 release: missing gcc-aarch64-linux-gnu"; \
		fi; \
	else \
		echo "Skipping aarch64 release: rust target aarch64-unknown-linux-gnu not installed"; \
	fi

# Build the LSP server
lsp:
	cargo build --release -p ngn-lsp

clean:
	cargo clean
	rm -rf target/embed

# Build release binaries and package a tarball in dist/
# Usage: make dist VERSION=0.1.0
dist: release
	@if [ -z "${VERSION}" ]; then \
		VERSION="$$(git describe --tags --always --dirty 2>/dev/null || echo dev)"; \
	fi; \
	OS_NAME="$$(uname -s)"; \
	case "$$OS_NAME" in \
		Linux) OS=linux ;; \
		*) echo "Unsupported OS: $$OS_NAME" >&2; exit 1 ;; \
	esac; \
	mkdir -p dist; \
	package() { \
		ARCH="$$1"; \
		BIN_DIR="$$2"; \
		TMP_DIR="$$(mktemp -d)"; \
		trap 'rm -rf "$$TMP_DIR"' EXIT; \
		cp "$$BIN_DIR/ngn" "$$TMP_DIR/ngn"; \
		cp "$$BIN_DIR/ngnr" "$$TMP_DIR/ngnr"; \
		tar -czf "dist/ngn-$$VERSION-$$OS-$$ARCH.tar.gz" -C "$$TMP_DIR" ngn ngnr; \
	}; \
	ARCH_NAME="$$(uname -m)"; \
	case "$$ARCH_NAME" in \
		x86_64|amd64) HOST_ARCH=x86_64 ;; \
		arm64|aarch64) HOST_ARCH=arm64 ;; \
		*) echo "Unsupported architecture: $$ARCH_NAME" >&2; exit 1 ;; \
	esac; \
	package "$$HOST_ARCH" "target/release"; \
	if [ -f "target/aarch64-unknown-linux-gnu/release/ngn" ] && [ -f "target/aarch64-unknown-linux-gnu/release/ngnr" ]; then \
		package "arm64" "target/aarch64-unknown-linux-gnu/release"; \
	fi
