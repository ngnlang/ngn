use crate::ast::EnumDef;
use std::collections::HashMap;

pub fn infer_enum_name(variant: &str, enums: &HashMap<String, EnumDef>) -> String {
    match variant {
        "Ok" | "Error" => "Result".to_string(),
        "Value" | "Null" => "Maybe".to_string(),
        _ => {
            for enum_def in enums.values() {
                if enum_def.variants.iter().any(|v| v.name == *variant) {
                    return enum_def.name.clone();
                }
            }
            panic!("Enum variant '{}' not found", variant)
        }
    }
}