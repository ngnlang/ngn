// Build script - just copies the pre-built runtime binary
// The runtime must be built first using: cargo build --release --bin runtime

use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
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
