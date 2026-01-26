// Minimal runtime for executing ngn bytecode
// This binary contains only the VM, value types, and bytecode deserializer
// It's embedded into the ngn compiler and extracted during `ngn build`

use ngn::bytecode::OpCode;
use ngn::value::Value;
use ngn::vm::VM;
use std::io::{Read, Seek, SeekFrom};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() == 2 {
        let flag = args[1].as_str();
        if flag == "--version" || flag == "-V" {
            println!("{}", env!("CARGO_PKG_VERSION"));
            return;
        }
    }
    let mod_arg = if args.len() > 2 && args[1] == "run" {
        Some(args[2].as_str())
    } else if args.len() > 1 {
        Some(args[1].as_str())
    } else {
        None
    };

    if let Some(mod_arg) = mod_arg {
        let path = std::path::Path::new(mod_arg);
        let bytes = std::fs::read(path).expect("Could not read bytecode file");
        let (instructions, constants): (Vec<OpCode>, Vec<Value>) =
            bincode::deserialize(&bytes).expect("Failed to deserialize bytecode");
        let working_dir = std::env::var("NGN_WORKDIR")
            .ok()
            .map(std::path::PathBuf::from)
            .or_else(|| path.parent().map(|p| p.to_path_buf()))
            .unwrap_or_else(|| std::path::PathBuf::from("."));
        ngn::env::init(&working_dir);
        let mut vm = VM::new(instructions, constants, 0);
        vm.run();
        return;
    }

    // Check for embedded bytecode (same logic as main ngn binary)
    if let Some((instructions, constants)) = check_for_embedded_bytecode() {
        let working_dir = std::env::var("NGN_WORKDIR")
            .ok()
            .map(std::path::PathBuf::from)
            .unwrap_or_else(|| std::path::PathBuf::from("."));
        ngn::env::init(&working_dir);
        let mut vm = VM::new(instructions, constants, 0);
        vm.run();
    } else {
        eprintln!("Error: This binary has no embedded bytecode.");
        std::process::exit(1);
    }
}

fn check_for_embedded_bytecode() -> Option<(Vec<OpCode>, Vec<Value>)> {
    let path = std::env::current_exe().ok()?;
    let mut file = std::fs::File::open(path).ok()?;
    let file_len = file.metadata().ok()?.len();

    // We need at least 16 bytes: 8 for magic number, 8 for length
    if file_len < 16 {
        return None;
    }

    // Read the last 16 bytes
    file.seek(SeekFrom::End(-16)).ok()?;
    let mut footer = [0u8; 16];
    file.read_exact(&mut footer).ok()?;

    let magic = u64::from_le_bytes(footer[0..8].try_into().ok()?);
    let size = u64::from_le_bytes(footer[8..16].try_into().ok()?);

    // Use unique magic number to identify ngn binaries (0x4E474E20 = "NGN ")
    if magic != 0x4E474E20 {
        return None;
    }

    // Seek back to where the bytecode starts
    file.seek(SeekFrom::End(-(16 + size as i64))).ok()?;
    let mut buffer = vec![0u8; size as usize];
    file.read_exact(&mut buffer).ok()?;

    bincode::deserialize(&buffer).ok()
}
