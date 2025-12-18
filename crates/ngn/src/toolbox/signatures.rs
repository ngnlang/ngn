use crate::types::Type;

pub struct BuiltinSignature {
    pub name: &'static str,
    pub params: &'static [(&'static str, Type)],
    pub return_type: Type,
}

pub const MATH_BUILTINS: &[(&str, &[(&str, &str)], &str)] = &[
    ("abs",   &[("value", "T")],   "T"), 
    ("round", &[("value", "T")],   "T"),
    ("floor", &[("value", "T")],   "T"),
    ("ceil",  &[("value", "T")],   "T"),
    ("sin",   &[("value", "f64")], "f64"), 
];

pub fn get_builtin_signature(name: &str) -> Option<(&'static str, &'static [(&'static str, &'static str)], &'static str)> {
    MATH_BUILTINS.iter()
        .find(|(n, _, _)| *n == name)
        .copied()
}
