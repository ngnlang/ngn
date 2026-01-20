// Build script
//
// 1) Copies the pre-built runtime binary into OUT_DIR (current packaging model)
// 2) Builds the vendored llama.cpp as a static library for tbx::llm

use std::env;
use std::fs;
use std::path::{Path, PathBuf};

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

    let bridge_cpp = PathBuf::from(&manifest_dir).join("src").join("llama_bridge.cpp");
    let bridge_h = PathBuf::from(&manifest_dir).join("src").join("llama_bridge.h");

    println!("cargo:rerun-if-changed={}", bridge_cpp.display());
    println!("cargo:rerun-if-changed={}", bridge_h.display());

    // llama.cpp contains both C and C++ sources.
    // Build them with their respective compilers and link static libs.
    let mut c_files: Vec<PathBuf> = Vec::new();
    let mut cpp_files: Vec<PathBuf> = Vec::new();
    // llama.cpp sources have a couple important subdirectories (e.g. src/models)
    // that we need for CPU inference.
    collect_sources_recursive(&llama_src.join("src"), &mut c_files, &mut cpp_files);

    // ggml/src contains optional backends that require extra SDKs (SYCL, Metal, etc).
    // For the CPU build we only compile the top-level ggml/src files.
    collect_sources_shallow(&ggml_src, &mut c_files, &mut cpp_files);

    // Small bridge library (depends on llama.cpp)
    {
        let mut bridge = cc::Build::new();
        bridge.cpp(true);
        bridge.flag_if_supported("-std=c++17");
        bridge.warnings(true);
        bridge.include(&llama_include);
        bridge.include(&ggml_include);
        bridge.include(&llama_src.join("src"));
        bridge.include(&ggml_src);
        bridge.define("GGML_VERSION", "\"vendored\"");
        bridge.define("GGML_COMMIT", "\"vendored\"");
        bridge.file(&bridge_cpp);
        bridge.compile("llama_bridge");
    }

    // C++ build (llama.cpp C++ and ggml C++ files)
    let mut build_cpp = cc::Build::new();
    build_cpp.cpp(true);
    build_cpp.flag_if_supported("-std=c++17");
    build_cpp.warnings(true);
    build_cpp.include(&llama_include);
    build_cpp.include(&ggml_include);
    build_cpp.include(&llama_src.join("src"));
    build_cpp.include(&ggml_src);
    build_cpp.define("GGML_VERSION", "\"vendored\"");
    build_cpp.define("GGML_COMMIT", "\"vendored\"");

    for f in &cpp_files {
        build_cpp.file(f);
    }
    build_cpp.compile("llama_cpp");

    // C build (ggml and support C files)
    let mut build_c = cc::Build::new();
    build_c.warnings(true);
    build_c.include(&llama_include);
    build_c.include(&ggml_include);
    build_c.include(&llama_src.join("src"));
    build_c.include(&ggml_src);

    // Build metadata normally injected by CMake.
    build_c.define("GGML_VERSION", "\"vendored\"");
    build_c.define("GGML_COMMIT", "\"vendored\"");

    for f in &c_files {
        build_c.file(f);
    }
    build_c.compile("ggml_c");
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
