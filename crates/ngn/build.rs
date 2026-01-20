// Build script
//
// 1) Copies the pre-built runtime binary into OUT_DIR (current packaging model)
// 2) Builds the vendored llama.cpp as a static library for tbx::llm

use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    build_llama_cpp();
    copy_runtime_binary();
}

fn repo_root(manifest_dir: &str) -> PathBuf {
    PathBuf::from(manifest_dir)
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
}

fn build_llama_cpp() {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let root = repo_root(&manifest_dir);

    println!("cargo:rerun-if-env-changed=NGN_LLAMA_BACKEND");

    let llama_src = root.join("vendor").join("llama.cpp");
    let ggml_src = llama_src.join("ggml").join("src");
    let llama_include = llama_src.join("include");
    let ggml_include = llama_src.join("ggml").join("include");

    if !llama_src.exists() {
        println!("cargo:warning=vendor/llama.cpp not found; tbx::llm will not link");
        return;
    }

    // Rebuild if vendored source changes
    println!("cargo:rerun-if-changed={}", llama_src.display());

    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let bridge_cpp = PathBuf::from(&manifest_dir).join("src").join("llama_bridge.cpp");
    let bridge_h = PathBuf::from(&manifest_dir).join("src").join("llama_bridge.h");

    println!("cargo:rerun-if-changed={}", bridge_cpp.display());
    println!("cargo:rerun-if-changed={}", bridge_h.display());

    // Always compile the C ABI bridge into a static lib.
    // We disable cc crate's cargo metadata so we can control link ordering.
    compile_llama_bridge(
        &bridge_cpp,
        &llama_src,
        &llama_include,
        &ggml_include,
        &ggml_src,
    );

    let backend = env::var("NGN_LLAMA_BACKEND").unwrap_or_else(|_| "cpu".to_string());
    match backend.as_str() {
        "cpu" => {
            build_llama_cpu(&llama_src, &llama_include, &ggml_include, &ggml_src);

            // Link our libs as a group to avoid order-dependent failures.
            println!("cargo:rustc-link-search=native={}", out_dir.display());
            println!("cargo:rustc-link-lib=dylib=stdc++");
            println!("cargo:rustc-link-arg=-Wl,--start-group");
            println!("cargo:rustc-link-lib=static=llama_bridge");
            println!("cargo:rustc-link-lib=static=llama_cpp");
            println!("cargo:rustc-link-lib=static=ggml_c");
            println!("cargo:rustc-link-arg=-Wl,--end-group");
        }
        "cuda" => {
            // CUDA build is opt-in.
            // It requires CUDA tooling (nvcc) and CUDA runtime libraries at link time.
            ensure_tool("cmake");
            ensure_tool("nvcc");

            let lib_dir = build_llama_cuda(&llama_src);

            println!("cargo:rustc-link-search=native={}", out_dir.display());
            println!("cargo:rustc-link-search=native={}", lib_dir.display());
            println!("cargo:rustc-link-lib=dylib=stdc++");
            println!("cargo:rustc-link-arg=-Wl,--start-group");
            println!("cargo:rustc-link-lib=static=llama_bridge");
            for lib in detect_static_libs(&lib_dir) {
                println!("cargo:rustc-link-lib=static={}", lib);
            }
            println!("cargo:rustc-link-arg=-Wl,--end-group");

            // CUDA runtime dependencies
            println!("cargo:rustc-link-lib=dylib=cudart");
            println!("cargo:rustc-link-lib=dylib=cublas");
            println!("cargo:rustc-link-lib=dylib=cublasLt");
            println!("cargo:rustc-link-lib=dylib=cuda");
        }
        other => {
            println!(
                "cargo:warning=Unknown NGN_LLAMA_BACKEND='{}' (expected 'cpu' or 'cuda'); using cpu",
                other
            );
            build_llama_cpu(&llama_src, &llama_include, &ggml_include, &ggml_src);
        }
    }
}

fn compile_llama_bridge(
    bridge_cpp: &Path,
    llama_src: &Path,
    llama_include: &Path,
    ggml_include: &Path,
    ggml_src: &Path,
) {
    let mut bridge = cc::Build::new();
    bridge.cpp(true);
    bridge.flag_if_supported("-std=c++17");
    bridge.warnings(true);
    bridge.cargo_metadata(false);
    bridge.include(llama_include);
    bridge.include(ggml_include);
    bridge.include(&llama_src.join("src"));
    bridge.include(ggml_src);
    bridge.define("GGML_VERSION", "\"vendored\"");
    bridge.define("GGML_COMMIT", "\"vendored\"");
    bridge.file(bridge_cpp);
    bridge.compile("llama_bridge");
}

fn build_llama_cpu(llama_src: &Path, llama_include: &Path, ggml_include: &Path, ggml_src: &Path) {
    // llama.cpp contains both C and C++ sources.
    // Build them with their respective compilers.
    let mut c_files: Vec<PathBuf> = Vec::new();
    let mut cpp_files: Vec<PathBuf> = Vec::new();

    // llama.cpp sources have a couple important subdirectories (e.g. src/models)
    // that we need for CPU inference.
    collect_sources_recursive(&llama_src.join("src"), &mut c_files, &mut cpp_files);

    // ggml/src contains optional backends that require extra SDKs (SYCL, Metal, etc).
    // For the CPU build we only compile the top-level ggml/src files.
    collect_sources_shallow(ggml_src, &mut c_files, &mut cpp_files);

    // But we do need the CPU backend implementation so ggml can register a backend.
    // Only compile the top-level ggml-cpu sources. Arch-specific sources under
    // ggml-cpu/arch/* are not meant to be compiled as standalone translation units.
    collect_sources_shallow(&ggml_src.join("ggml-cpu"), &mut c_files, &mut cpp_files);

    // However, the top-level ggml-cpu sources rely on arch-specific implementations
    // for quantization kernels on some platforms. Compile the current target's arch
    // directory so the expected symbols exist.
    let target_arch = env::var("CARGO_CFG_TARGET_ARCH").unwrap_or_default();
    if target_arch == "x86_64" || target_arch == "x86" {
        collect_sources_shallow(
            &ggml_src.join("ggml-cpu").join("arch").join("x86"),
            &mut c_files,
            &mut cpp_files,
        );
    }

    let mut build_cpp = cc::Build::new();
    build_cpp.cpp(true);
    build_cpp.flag_if_supported("-std=c++17");
    build_cpp.warnings(true);
    build_cpp.cargo_metadata(false);
    build_cpp.include(llama_include);
    build_cpp.include(ggml_include);
    build_cpp.include(&llama_src.join("src"));
    build_cpp.include(ggml_src);
    build_cpp.include(&ggml_src.join("ggml-cpu"));
    build_cpp.define("GGML_VERSION", "\"vendored\"");
    build_cpp.define("GGML_COMMIT", "\"vendored\"");
    build_cpp.define("GGML_USE_CPU", None);
    build_cpp.define("_GNU_SOURCE", None);

    for f in &cpp_files {
        build_cpp.file(f);
    }
    build_cpp.compile("llama_cpp");

    let mut build_c = cc::Build::new();
    build_c.warnings(true);
    build_c.cargo_metadata(false);
    build_c.include(llama_include);
    build_c.include(ggml_include);
    build_c.include(&llama_src.join("src"));
    build_c.include(ggml_src);
    build_c.include(&ggml_src.join("ggml-cpu"));
    build_c.define("GGML_VERSION", "\"vendored\"");
    build_c.define("GGML_COMMIT", "\"vendored\"");
    build_c.define("GGML_USE_CPU", None);
    build_c.define("_GNU_SOURCE", None);
    for f in &c_files {
        build_c.file(f);
    }
    build_c.compile("ggml_c");
}

fn build_llama_cuda(llama_src: &Path) -> PathBuf {
    // Use llama.cpp's CMake build for CUDA to avoid re-implementing nvcc integration.
    let dst = cmake::Config::new(llama_src)
        .define("GGML_CUDA", "ON")
        .define("BUILD_SHARED_LIBS", "OFF")
        .define("LLAMA_BUILD_TESTS", "OFF")
        .define("LLAMA_BUILD_EXAMPLES", "OFF")
        .build();

    let lib = dst.join("lib");
    if lib.exists() {
        return lib;
    }
    let lib64 = dst.join("lib64");
    if lib64.exists() {
        return lib64;
    }

    panic!("CUDA build completed but no lib dir found under {}", dst.display());
}

fn detect_static_libs(lib_dir: &Path) -> Vec<String> {
    let mut libs: Vec<String> = Vec::new();
    if let Ok(entries) = std::fs::read_dir(lib_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if !path.is_file() {
                continue;
            }
            let Some(name) = path.file_name().and_then(|s| s.to_str()) else {
                continue;
            };
            if !name.starts_with("lib") || !name.ends_with(".a") {
                continue;
            }
            // Only link llama/ggml libs.
            if !(name.contains("llama") || name.contains("ggml")) {
                continue;
            }
            libs.push(name.trim_start_matches("lib").trim_end_matches(".a").to_string());
        }
    }

    // Keep order stable (and group-linking reduces sensitivity).
    libs.sort();
    libs
}

fn ensure_tool(name: &str) {
    let ok = Command::new(name)
        .arg("--version")
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .is_ok();

    if !ok {
        panic!(
            "NGN_LLAMA_BACKEND=cuda requires '{}' to be installed and in PATH",
            name
        );
    }
}

fn collect_sources_recursive(dir: &Path, c_files: &mut Vec<PathBuf>, cpp_files: &mut Vec<PathBuf>) {
    if let Ok(entries) = std::fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                collect_sources_recursive(&path, c_files, cpp_files);
                continue;
            }
            if !path.is_file() {
                continue;
            }
            if let Some(ext) = path.extension().and_then(|s| s.to_str()) {
                match ext {
                    "c" => c_files.push(path),
                    "cc" | "cpp" => cpp_files.push(path),
                    _ => {}
                }
            }
        }
    }
}

fn collect_sources_shallow(dir: &Path, c_files: &mut Vec<PathBuf>, cpp_files: &mut Vec<PathBuf>) {
    if let Ok(entries) = std::fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if !path.is_file() {
                continue;
            }
            if let Some(ext) = path.extension().and_then(|s| s.to_str()) {
                match ext {
                    "c" => c_files.push(path),
                    "cc" | "cpp" => cpp_files.push(path),
                    _ => {}
                }
            }
        }
    }
}

fn copy_runtime_binary() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();

    // Look for pre-built runtime in workspace target/embed/
    let runtime_path = PathBuf::from(&manifest_dir)
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("target")
        .join("embed")
        .join("runtime_binary");

    if runtime_path.exists() {
        let dest_path = PathBuf::from(&out_dir).join("runtime_binary");
        fs::copy(&runtime_path, &dest_path).expect("Failed to copy runtime binary to OUT_DIR");
    } else {
        // Create a placeholder if runtime not built yet (for initial cargo check)
        let dest_path = PathBuf::from(&out_dir).join("runtime_binary");
        fs::write(
            &dest_path,
            b"PLACEHOLDER - run 'make release' to build properly",
        )
        .expect("Failed to write placeholder");

        // This will cause a build error if someone tries to use it
        println!("cargo:warning=Runtime not found! Run 'make release' instead of 'cargo build'");
    }

    // Tell cargo to rerun if runtime changes
    println!("cargo:rerun-if-changed=target/embed/runtime_binary");
    println!("cargo:rerun-if-changed=src/bin/runtime.rs");
}
