//! Environment variable module
//! Handles loading .env files and providing access to environment variables.
//!
//! At VM startup, loads .env files in precedence order:
//! 1. .env (base)
//! 2. .env.{mode} (production, development, test)
//! 3. .env.local (highest precedence, typically gitignored)

use std::collections::HashMap;
use std::path::Path;
use std::sync::OnceLock;

/// Global storage for loaded env vars (thread-safe, initialized once)
static ENV_STORE: OnceLock<HashMap<String, String>> = OnceLock::new();

/// Initialize the environment store by loading .env files
/// Called once at VM startup
pub fn init(working_dir: &Path) {
    ENV_STORE.get_or_init(|| load_env_files(working_dir));
}

/// Load .env files in precedence order
fn load_env_files(working_dir: &Path) -> HashMap<String, String> {
    let mut env_map = HashMap::new();

    // Determine mode from process environment
    let mode = std::env::var("NGN_MODE").unwrap_or_else(|_| "development".to_string());

    // Load in precedence order (later overrides earlier)
    let files = [
        ".env".to_string(),
        format!(".env.{}", mode),
        ".env.local".to_string(),
    ];

    for filename in files {
        let path = working_dir.join(&filename);
        if let Ok(content) = std::fs::read_to_string(&path) {
            parse_env_content(&content, &mut env_map);
        }
    }

    env_map
}

/// Parse .env file content into the map
fn parse_env_content(content: &str, map: &mut HashMap<String, String>) {
    for line in content.lines() {
        let line = line.trim();

        // Skip empty lines and comments
        if line.is_empty() || line.starts_with('#') {
            continue;
        }

        if let Some(eq_pos) = line.find('=') {
            let key = line[..eq_pos].trim().to_string();
            let mut value = line[eq_pos + 1..].trim();

            // Handle quoted values (both single and double quotes)
            if (value.starts_with('"') && value.ends_with('"'))
                || (value.starts_with('\'') && value.ends_with('\''))
            {
                if value.len() >= 2 {
                    value = &value[1..value.len() - 1];
                }
            } else if let Some(comment_pos) = value.find(" #") {
                // Remove inline comments (only for unquoted values)
                value = value[..comment_pos].trim();
            }

            if !key.is_empty() {
                map.insert(key, value.to_string());
            }
        }
    }
}

/// Get an environment variable (checks .env store first, then process env)
pub fn get(key: &str) -> Option<String> {
    // Check loaded .env files first (higher precedence)
    if let Some(store) = ENV_STORE.get() {
        if let Some(value) = store.get(key) {
            return Some(value.clone());
        }
    }
    // Fall back to process environment
    std::env::var(key).ok()
}

/// Check if an environment variable exists
pub fn has(key: &str) -> bool {
    get(key).is_some()
}
