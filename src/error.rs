#[derive(Debug, Clone)]
pub enum RuntimeError {
    TypeError(String),
    ArityError(String),
    UndefinedVariable(String),
    UndefinedFunction(String),
    ImportError(String),
    AssertionError(String),
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::TypeError(msg) => write!(f, "Type error: {}", msg),
            RuntimeError::ArityError(msg) => write!(f, "Arity error: {}", msg),
            RuntimeError::UndefinedVariable(name) => write!(f, "Undefined variable: {}", name),
            RuntimeError::UndefinedFunction(name) => write!(f, "Undefined function: {}", name),
            RuntimeError::ImportError(msg) => write!(f, "Import error: {}", msg),
            RuntimeError::AssertionError(msg) => write!(f, "Assertion failed: {}", msg),
        }
    }
}

