# Build ngn with embedded runtime
# Usage: make release

.PHONY: release release-llm release-all clean runtime runtime-llm embed-runtime lsp all bench dist dist-llm release-aarch64 release-aarch64-llm

CARGO := cargo
EMBED_DIR := target/embed

# Build everything
all: release-llm release lsp

# Build both runtime flavors and ngn binaries (no LSP)
release-all: release-llm release

# Build distributions for both runtimes and binaries (no LSP)
dist-all: dist-llm dist

# Run benchmarks
bench:
	$(CARGO) run --release -p ngn --bin bench

# Build the minimal runtime first, then the full ngn with embedded runtime
release: runtime
	NGN_EMBED_RUNTIME=ngnr $(CARGO) build --release -p ngn --bin ngn
	$(MAKE) release-aarch64

# Build ngn with LLM support and embedded LLM runtime
release-llm: runtime-llm
	NGN_EMBED_RUNTIME=ngnr-llm $(CARGO) build --release -p ngn --bin ngn --features llm
	cp target/release/ngn target/release/ngn-llm
	$(MAKE) release-aarch64-llm

# Build runtime binary separately (must be done first)
runtime:
	$(CARGO) build --release -p ngn --bin runtime
	$(MAKE) embed-runtime RUNTIME=target/release/runtime EMBED_NAME=ngnr
	cp target/release/runtime target/release/ngnr

# Build runtime with LLM support (temporarily replaces embedded runtime)
runtime-llm:
	$(CARGO) build --release -p ngn --bin runtime --features llm
	$(MAKE) embed-runtime RUNTIME=target/release/runtime EMBED_NAME=ngnr-llm
	cp target/release/runtime target/release/ngnr-llm

embed-runtime:
	mkdir -p $(EMBED_DIR)
	@if [ -z "$(EMBED_NAME)" ]; then \
		echo "EMBED_NAME is required (e.g. ngnr or ngnr-llm)" >&2; \
		exit 1; \
	fi
	cp $(RUNTIME) $(EMBED_DIR)/$(EMBED_NAME)

# Build aarch64 binaries if cross toolchain is available
release-aarch64:
	@if rustup target list --installed | grep -q aarch64-unknown-linux-gnu; then \
		if command -v aarch64-linux-gnu-gcc >/dev/null 2>&1; then \
			$(CARGO) build --release -p ngn --bin runtime --target aarch64-unknown-linux-gnu; \
			$(CARGO) build --release -p ngn --bin ngn --target aarch64-unknown-linux-gnu; \
			cp target/aarch64-unknown-linux-gnu/release/runtime target/aarch64-unknown-linux-gnu/release/ngnr; \
		else \
			echo "Skipping aarch64 release: missing gcc-aarch64-linux-gnu"; \
		fi; \
	else \
		echo "Skipping aarch64 release: rust target aarch64-unknown-linux-gnu not installed"; \
	fi

# Build aarch64 binaries with LLM support if cross toolchain is available
release-aarch64-llm:
	@if rustup target list --installed | grep -q aarch64-unknown-linux-gnu; then \
		if command -v aarch64-linux-gnu-gcc >/dev/null 2>&1; then \
			NGN_EMBED_RUNTIME=ngnr-llm $(CARGO) build --release -p ngn --bin runtime --features llm --target aarch64-unknown-linux-gnu; \
			NGN_EMBED_RUNTIME=ngnr-llm $(CARGO) build --release -p ngn --bin ngn --features llm --target aarch64-unknown-linux-gnu; \
			cp target/aarch64-unknown-linux-gnu/release/runtime target/aarch64-unknown-linux-gnu/release/ngnr-llm; \
			cp target/aarch64-unknown-linux-gnu/release/ngn target/aarch64-unknown-linux-gnu/release/ngn-llm; \
		else \
			echo "Skipping aarch64-llm release: missing gcc-aarch64-linux-gnu"; \
		fi; \
	else \
		echo "Skipping aarch64-llm release: rust target aarch64-unknown-linux-gnu not installed"; \
	fi

# Build the LSP server
lsp:
	$(CARGO) build --release -p ngn-lsp

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

# Build LLM release binaries and package a tarball in dist/
# Usage: make dist-llm VERSION=0.1.0
dist-llm: release-llm
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
		cp "$$BIN_DIR/ngn-llm" "$$TMP_DIR/ngn"; \
		cp "$$BIN_DIR/ngnr-llm" "$$TMP_DIR/ngnr"; \
		tar -czf "dist/ngn-llm-$$VERSION-$$OS-$$ARCH.tar.gz" -C "$$TMP_DIR" ngn ngnr; \
	}; \
	ARCH_NAME="$$(uname -m)"; \
	case "$$ARCH_NAME" in \
		x86_64|amd64) HOST_ARCH=x86_64 ;; \
		arm64|aarch64) HOST_ARCH=arm64 ;; \
		*) echo "Unsupported architecture: $$ARCH_NAME" >&2; exit 1 ;; \
	esac; \
	package "$$HOST_ARCH" "target/release"; \
	if [ -f "target/aarch64-unknown-linux-gnu/release/ngn-llm" ] && [ -f "target/aarch64-unknown-linux-gnu/release/ngnr-llm" ]; then \
		package "arm64" "target/aarch64-unknown-linux-gnu/release"; \
	fi
