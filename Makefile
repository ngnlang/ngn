# Build ngn with embedded runtime
# Usage: make release

.PHONY: release clean runtime runtime_full runtime_variants lsp all bench

comma := ,

RUNTIME_VARIANTS := \
	core$(comma)http$(comma)os$(comma)fetch$(comma)llm:runtime_full \
	:runtime_min \
	fetch:runtime_min_fetch \
	os:runtime_min_os \
	os$(comma)fetch:runtime_min_os_fetch \
	http:runtime_min_http \
	http$(comma)fetch:runtime_min_http_fetch \
	os$(comma)http:runtime_min_os_http \
	os$(comma)http$(comma)fetch:runtime_min_os_http_fetch \
	llm:runtime_min_llm \
	llm$(comma)fetch:runtime_min_llm_fetch \
	os$(comma)llm:runtime_min_os_llm \
	os$(comma)llm$(comma)fetch:runtime_min_os_llm_fetch \
	http$(comma)llm:runtime_min_http_llm \
	http$(comma)llm$(comma)fetch:runtime_min_http_llm_fetch \
	os$(comma)http$(comma)llm:runtime_min_os_http_llm \
	os$(comma)http$(comma)llm$(comma)fetch:runtime_min_os_http_llm_fetch \
	core:runtime_core \
	core$(comma)fetch:runtime_core_fetch \
	core$(comma)os:runtime_core_os \
	core$(comma)os$(comma)fetch:runtime_core_os_fetch \
	core$(comma)http:runtime_core_http \
	core$(comma)http$(comma)fetch:runtime_core_http_fetch \
	core$(comma)os$(comma)http:runtime_core_os_http \
	core$(comma)os$(comma)http$(comma)fetch:runtime_core_os_http_fetch \
	core$(comma)llm:runtime_core_llm \
	core$(comma)llm$(comma)fetch:runtime_core_llm_fetch \
	core$(comma)os$(comma)llm:runtime_core_os_llm \
	core$(comma)os$(comma)llm$(comma)fetch:runtime_core_os_llm_fetch \
	core$(comma)http$(comma)llm:runtime_core_http_llm \
	core$(comma)http$(comma)llm$(comma)fetch:runtime_core_http_llm_fetch \
	core$(comma)os$(comma)http$(comma)llm:runtime_core_os_http_llm \
	core$(comma)os$(comma)http$(comma)llm$(comma)fetch:runtime_core_os_http_llm_fetch

# Build everything
all: release lsp

# Run benchmarks
bench:
	cargo run --release -p ngn --bin bench

# Build the minimal runtime first, then the full ngn with embedded runtime
release: runtime
	cargo build --release -p ngn --bin ngn

define build_runtime
	cargo build --release -p ngn --bin runtime --no-default-features $(if $(1),--features "$(1)",)
	mkdir -p target/embed
	cp target/release/runtime target/embed/$(2)
endef

# Build runtime binaries (all variants)
runtime: runtime_variants

runtime_variants:
	set -e; \
	for v in $(RUNTIME_VARIANTS); do \
		features=$${v%%:*}; \
		name=$${v##*:}; \
		if [ -n "$$features" ]; then \
			NGN_SKIP_RUNTIME_COPY=1 cargo build --release -p ngn --bin runtime --no-default-features --features "$$features"; \
		else \
			NGN_SKIP_RUNTIME_COPY=1 cargo build --release -p ngn --bin runtime --no-default-features; \
		fi; \
		mkdir -p target/embed; \
		cp target/release/runtime target/embed/$$name; \
	done
	cp target/embed/runtime_full target/embed/runtime_binary

# Build only the full runtime (legacy target)
runtime_full:
	$(call build_runtime,core$(comma)http$(comma)os$(comma)fetch$(comma)llm,runtime_full)
	cp target/embed/runtime_full target/embed/runtime_binary

# Build the LSP server
lsp:
	cargo build --release -p ngn-lsp

clean:
	cargo clean
	rm -rf target/embed
