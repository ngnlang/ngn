use crate::lexer::{Lexer, Span, Token};

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum Type {
    I64,
    I32,
    I16,
    I8,
    U64,
    U32,
    U16,
    U8,
    F64,
    F32,
    String,
    Bytes,
    Bool,
    Void,
    Array(Box<Type>),
    Tuple(Vec<Type>),
    Function {
        params: Vec<Type>,
        optional_count: usize, // Number of trailing optional params
        return_type: Box<Type>,
    },
    Any,
    Enum(String),
    Channel(Box<Type>),
    State(Box<Type>),
    Model(String),
    Role(String),
    Generic(String, Vec<Type>), // e.g., Container<i64> -> Generic("Container", [I64])
    Map(Box<Type>, Box<Type>),
    Set(Box<Type>),
    Regex,
    Number,            // Generic number type for extends
    Json,              // Built-in json module type
    Spawn,             // Built-in spawn module type
    Env,               // Built-in env module type for environment variables
    Time,              // Built-in time module type for date/time operations
    TypeParam(String), // Type parameter reference (e.g., T in model Container<T>)
    Union(Vec<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub is_owned: bool,
    pub ty: Option<Type>,
    pub is_optional: bool,           // true if `name?` syntax
    pub default_value: Option<Expr>, // Value if `= expr` syntax
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub patterns: Vec<Pattern>,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Literal(Expr),
    EnumVariant {
        enum_name: Option<String>,
        variant_name: String,
        binding: Option<String>,
    },
    Wildcard,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariantDef {
    pub name: String,
    pub data_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDef {
    pub name: String,
    pub type_params: Vec<String>, // Type parameters like T, U for generic enums
    pub variants: Vec<EnumVariantDef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    TypeAlias {
        name: String,
        target: Type,
    },
    Declaration {
        name: String,
        is_mutable: bool,
        is_global: bool,
        value: Expr,
        declared_type: Option<Type>,
    },
    // const { a, b: renamed, ...rest } = obj
    DestructureObject {
        fields: Vec<(String, Option<String>)>, // (field_name, optional_alias) - if alias is None, binds to field_name
        rest: Option<String>,                  // Optional ...rest binding
        is_mutable: bool,
        value: Expr,
    },
    // const [a, b, ...rest] = arr
    DestructureArray {
        bindings: Vec<String>, // Names to bind each element to
        rest: Option<String>,  // Optional ...rest binding
        is_mutable: bool,
        value: Expr,
    },
    // const (a, b, ...rest) = tuple
    DestructureTuple {
        bindings: Vec<String>, // Names to bind each element to
        rest: Option<String>,  // Optional ...rest binding
        is_mutable: bool,
        value: Expr,
    },
    Expression(Expr),
    Function {
        name: String,
        params: Vec<Parameter>,
        body: Vec<Statement>,
        return_type: Option<Type>,
        is_exported: bool,
    },
    // import { a, b as c } from "..."
    Import {
        names: Vec<(String, Option<String>)>, // (name, alias)
        source: String,
    },
    // import x from "..."
    ImportDefault {
        name: String,
        source: String,
    },
    // import * as X from "..."
    ImportModule {
        alias: String,
        source: String,
    },
    ExportDefault(Expr),
    Print(Expr),
    Echo(Expr),
    Sleep(Expr),
    Block(Vec<Statement>),
    If {
        condition: Expr,
        binding: Option<String>, // For `if (b = x)` syntax
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    Check {
        binding: String,
        error_binding: Option<String>,
        is_mutable: bool,
        source: Expr,
        failure_block: Box<Statement>,
    },
    While {
        condition: Expr,
        body: Box<Statement>,
        is_once: bool,
    },
    Loop(Box<Statement>),
    For {
        binding: String,
        index_binding: Option<String>,
        iterable: Expr,
        body: Box<Statement>,
    },
    Match {
        condition: Expr,
        arms: Vec<MatchArm>,
    },
    Next,
    Break,
    Return(Option<Expr>),
    Enum(EnumDef),
    Model(ModelDef),
    Role(RoleDef),
    Extend {
        target: Type,
        role: Option<String>,
        methods: Vec<Statement>,
    },
    Error(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModelDef {
    pub name: String,
    pub type_params: Vec<String>, // Type parameters like T, U for generic models
    pub fields: Vec<(String, Type)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RoleDef {
    pub name: String,
    pub methods: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Assign {
        name: String,
        value: Box<Expr>,
    },
    FieldAssign {
        object: Box<Expr>,
        field: String,
        value: Box<Expr>,
    },
    Bool(bool),
    Regex(String),
    Call {
        name: String,
        args: Vec<Expr>,
    },
    Number(i64),
    Float(f64),
    String(String),
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Variable(String),
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    InterpolatedString(Vec<Expr>),
    EnumVariant {
        enum_name: Option<String>,
        variant_name: String,
        args: Vec<Expr>,
    },
    Closure {
        params: Vec<Parameter>,
        body: Box<Statement>,
        return_type: Option<Type>,
    },
    Thread(Box<Expr>),
    Send(Box<Expr>, Box<Expr>),
    Receive(Box<Expr>),
    ReceiveCount(Box<Expr>, Box<Expr>),
    ReceiveMaybe(Box<Expr>),
    Channel(Option<Type>),
    State(Box<Expr>),
    Bytes(Option<Box<Expr>>),
    MethodCall(Box<Expr>, String, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Unary {
        op: Token,
        right: Box<Expr>,
    },
    ModelInstance {
        name: String,
        fields: Vec<(String, Expr)>,
    },
    FieldAccess {
        object: Box<Expr>,
        field: String,
    },
    OptionalFieldAccess {
        object: Box<Expr>,
        field: String,
    },
    OptionalMethodCall(Box<Expr>, String, Vec<Expr>),
    Map(Type, Type),
    Set(Type),
    This,
    Object(Vec<(String, Expr)>), // Anonymous object literal: { key: value }
    Null,                        // null literal - syntactic sugar for Maybe::Null
    Error(String),
}

impl Expr {
    pub fn is_primitive(&self) -> bool {
        match &self.kind {
            ExprKind::Number(_) | ExprKind::Float(_) | ExprKind::Bool(_) => true,

            ExprKind::String(s) => s.len() <= 32,

            ExprKind::Array(elements) => elements.is_empty(),
            ExprKind::Tuple(elements) => elements.is_empty(),

            _ => false,
        }
    }
}

pub struct Parser {
    pub lexer: Lexer,
    pub current_token: Token,
    pub current_span: Span,
    pub previous_span: Span,
    pub in_function: bool,
    pub paren_depth: usize,
    /// Type parameters currently in scope (for parsing generic model/enum definitions)
    pub type_params_in_scope: Vec<String>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let (first_token, first_span) = lexer.next_token_with_span();
        Self {
            lexer,
            current_token: first_token,
            current_span: first_span,
            previous_span: Span::default(),
            in_function: false,
            paren_depth: 0,
            type_params_in_scope: Vec::new(),
        }
    }

    // Move to the next token
    pub fn advance(&mut self) {
        self.previous_span = self.current_span;
        let (token, span) = self.lexer.next_token_with_span();
        self.current_token = token;
        self.current_span = span;
    }

    fn consume_newlines(&mut self) {
        while self.current_token == Token::Newline {
            self.advance();
        }
    }

    fn expect(&mut self, expected: Token) {
        if self.current_token == expected {
            self.advance();
        } else {
            eprintln!(
                "Syntax Error: Expected {:?}, got {:?}",
                expected, self.current_token
            );
            // We assume the token was missing and continue to prevent crashing
        }
    }

    fn expect_identifier(&mut self) -> String {
        if let Token::Identifier(name) = &self.current_token {
            let name_cloned = name.clone();

            self.advance();

            name_cloned
        } else if matches!(self.current_token, Token::Set | Token::Map) {
            // Allow keywords as identifiers in certain contexts (like method names)
            let name = match &self.current_token {
                Token::Set => "set".to_string(),
                Token::Map => "map".to_string(),
                _ => unreachable!(),
            };
            self.advance();
            name
        } else {
            eprintln!(
                "Syntax Error: Expected an identifier, but found {:?}",
                self.current_token
            );
            "__error_identifier__".to_string()
        }
    }

    fn parse_function(&mut self) -> Statement {
        self.parse_function_with_export(false)
    }

    fn parse_function_with_export(&mut self, is_exported: bool) -> Statement {
        let start = self.current_span.start;
        self.advance();

        let name = self.expect_identifier();
        self.expect(Token::LParen);

        let mut params = Vec::new();

        while self.current_token != Token::RParen {
            let param_name = self.expect_identifier();
            // Capture span start from the identifier we just parsed
            let start_param = self.previous_span.start;

            // Check for optional marker (?)
            let is_optional = if self.current_token == Token::Question {
                self.advance();
                true
            } else {
                false
            };

            let mut ty = None;
            let mut is_owned = false;

            if self.current_token == Token::Colon {
                self.advance();

                if self.current_token == Token::LessThan {
                    is_owned = true;
                    self.advance();
                }

                ty = Some(self.parse_type());
            }

            // Check for default value (= expr)
            let default_value = if self.current_token == Token::Equal {
                self.advance();
                Some(self.parse_expression())
            } else {
                None
            };

            let end_param = self.previous_span.end;

            params.push(Parameter {
                name: param_name,
                is_owned,
                ty,
                is_optional,
                default_value,
                span: Span::new(start_param, end_param),
            });

            if self.current_token == Token::Comma {
                self.advance();
            }
        }
        self.expect(Token::RParen);

        let mut return_type = None;
        if self.current_token == Token::Colon {
            self.advance();
            return_type = Some(self.parse_type());
        }

        let old_in_function = self.in_function;
        self.in_function = true;

        let body = if self.current_token == Token::LBrace {
            self.parse_block()
        } else if self.current_token == Token::Newline || self.current_token == Token::RBrace {
            // Empty body (signature)
            vec![]
        } else {
            // Implicit return
            vec![Statement {
                kind: StatementKind::Return(Some(self.parse_expression())),
                span: self.current_span, // Approx span
            }]
        };

        self.in_function = old_in_function;
        let end = self.previous_span.end;

        Statement {
            kind: StatementKind::Function {
                name,
                params,
                body,
                return_type,
                is_exported,
            },
            span: Span::new(start, end),
        }
    }

    fn parse_type_alias_stmt(&mut self) -> Statement {
        let start = self.current_span.start;
        self.advance(); // consume 'type'

        if self.in_function {
            let msg = "Syntax Error: 'type' declarations must be in global scope".to_string();
            eprintln!("{}", msg);
            return Statement {
                kind: StatementKind::Error(msg),
                span: Span::new(start, self.current_span.end),
            };
        }

        let name = self.expect_identifier();
        self.expect(Token::Equal);
        let target = self.parse_type();
        let end = self.previous_span.end;

        Statement {
            kind: StatementKind::TypeAlias { name, target },
            span: Span::new(start, end),
        }
    }

    fn parse_import_statement(&mut self) -> Statement {
        let start = self.current_span.start;
        self.advance(); // consume 'import'

        // 1. Module Import: import * as Name from "..."
        if self.current_token == Token::Star {
            self.advance(); // consume '*'
            self.expect(Token::As);
            let alias = self.expect_identifier();
            self.expect(Token::From);

            let source = self.parse_literal_string();
            let end = self.previous_span.end;
            return Statement {
                kind: StatementKind::ImportModule { alias, source },
                span: Span::new(start, end),
            };
        }

        // 2. Named Imports: import { a, b as c } from "..."
        if self.current_token == Token::LBrace {
            self.advance(); // consume '{'
            let mut names = Vec::new();
            while self.current_token != Token::RBrace && self.current_token != Token::EOF {
                let name = self.expect_identifier();
                let mut alias = None;
                if self.current_token == Token::As {
                    self.advance(); // consume 'as'
                    alias = Some(self.expect_identifier());
                }
                names.push((name, alias));
                if self.current_token == Token::Comma {
                    self.advance();
                }
            }
            self.expect(Token::RBrace);
            self.expect(Token::From);

            let source = self.parse_literal_string();
            let end = self.previous_span.end;
            return Statement {
                kind: StatementKind::Import { names, source },
                span: Span::new(start, end),
            };
        }

        // 3. Default Import: import Name from "..."
        let name = self.expect_identifier();
        self.expect(Token::From);

        let source = self.parse_literal_string();
        let end = self.previous_span.end;
        Statement {
            kind: StatementKind::ImportDefault { name, source },
            span: Span::new(start, end),
        }
    }

    pub fn parse_statement(&mut self) -> Statement {
        self.consume_newlines();

        let start = self.current_span.start;

        match self.current_token.clone() {
            Token::Global => {
                if self.in_function {
                    let msg = "Syntax Error: 'global' declarator can only be used in global scope"
                        .to_string();
                    eprintln!("{}", msg);
                    self.advance(); // consume token to proceed
                    return Statement {
                        kind: StatementKind::Error(msg),
                        span: Span::new(start, self.current_span.end),
                    };
                }
                self.parse_declaration()
            }
            Token::Const => {
                if !self.in_function {
                    let msg = "Syntax Error: 'const' declarator can only be used inside functions (use 'global' for global state)".to_string();
                    eprintln!("{}", msg);
                    self.advance();
                    return Statement {
                        kind: StatementKind::Error(msg),
                        span: Span::new(start, self.current_span.end),
                    };
                }
                self.parse_declaration()
            }
            Token::Var => {
                if !self.in_function {
                    let msg = "Syntax Error: 'var' declarator can only be used inside functions (use 'global' for global state)".to_string();
                    eprintln!("{}", msg);
                    self.advance();
                    return Statement {
                        kind: StatementKind::Error(msg),
                        span: Span::new(start, self.current_span.end),
                    };
                }
                self.parse_declaration()
            }
            Token::Type => self.parse_type_alias_stmt(),
            Token::Fn => self.parse_function(),
            Token::Export => {
                self.advance(); // consume 'export'
                if self.current_token == Token::Default {
                    self.advance(); // consume 'default'
                    // Expect an expression for now (typically an identifier)
                    let expr = self.parse_expression();
                    let end = self.previous_span.end;
                    Statement {
                        kind: StatementKind::ExportDefault(expr),
                        span: Span::new(start, end),
                    }
                } else if self.current_token == Token::Fn {
                    self.parse_function_with_export(true)
                } else {
                    panic!("Syntax Error: 'export' must be followed by 'fn' or 'default'");
                }
            }
            Token::Identifier(_) => {
                let expr = self.parse_expression();
                let end = expr.span.end; // Expression end
                Statement {
                    kind: StatementKind::Expression(expr),
                    span: Span::new(start, end),
                }
            }
            Token::Import => self.parse_import_statement(),
            Token::Enum => self.parse_enum_stmt(),
            Token::Model => self.parse_model_stmt(),
            Token::Role => self.parse_role_stmt(),
            Token::Extend => self.parse_extend_stmt(),
            Token::Print => {
                self.advance();
                self.expect(Token::LParen);
                let expr = self.parse_expression();
                self.expect(Token::RParen);
                let end = self.previous_span.end;
                Statement {
                    kind: StatementKind::Print(expr),
                    span: Span::new(start, end),
                }
            }
            Token::Echo => {
                self.advance();
                self.expect(Token::LParen);
                let expr = self.parse_expression();
                self.expect(Token::RParen);
                let end = self.previous_span.end;
                Statement {
                    kind: StatementKind::Echo(expr),
                    span: Span::new(start, end),
                }
            }
            Token::Sleep => {
                self.advance();
                self.expect(Token::LParen);
                let expr = self.parse_expression();
                self.expect(Token::RParen);
                let end = self.previous_span.end;
                Statement {
                    kind: StatementKind::Sleep(expr),
                    span: Span::new(start, end),
                }
            }
            Token::If => self.parse_if_stmt(),
            Token::While => self.parse_while_stmt(),
            Token::Loop => self.parse_loop_stmt(),
            Token::For => self.parse_for_stmt(),
            Token::Match => self.parse_match_stmt(),
            Token::Check => self.parse_check_stmt(),
            Token::LBrace => {
                let block = self.parse_block();
                // parse_block returns Vec<Statement>. We must wrap it in Statement::Block
                // But parse_block eats the braces?
                // Wait, parse_block returns Vec<Statement>.
                // We need to know where the closing brace was.
                // Assuming parse_block updates current/previous span.
                let end = self.previous_span.end;
                Statement {
                    kind: StatementKind::Block(block),
                    span: Span::new(start, end),
                }
            }
            Token::Break => {
                self.advance();
                let end = self.previous_span.end;
                Statement {
                    kind: StatementKind::Break,
                    span: Span::new(start, end),
                }
            }
            Token::Next => {
                self.advance();
                let end = self.previous_span.end;
                Statement {
                    kind: StatementKind::Next,
                    span: Span::new(start, end),
                }
            }
            Token::Return => {
                self.advance();
                let expr = if self.current_token != Token::Newline
                    && self.current_token != Token::RBrace
                    && self.current_token != Token::EOF
                {
                    Some(self.parse_expression())
                } else {
                    None
                };
                let end = self.previous_span.end;
                Statement {
                    kind: StatementKind::Return(expr),
                    span: Span::new(start, end),
                }
            }
            _ => {
                let expr = self.parse_expression();
                let end = expr.span.end;
                Statement {
                    kind: StatementKind::Expression(expr),
                    span: Span::new(start, end),
                }
            }
        }
    }

    fn parse_declaration(&mut self) -> Statement {
        let start = self.current_span.start;
        let declarator = self.current_token.clone();
        self.advance(); // consume 'var', 'const', or 'global'

        let (is_mutable, is_global) = match declarator {
            Token::Var => (true, false),
            Token::Const => (false, false),
            Token::Global => (false, true),
            _ => panic!("Expected declarator"),
        };

        // Check for destructuring patterns
        if self.current_token == Token::LBrace {
            return self.parse_object_destructure(start, is_mutable, is_global);
        }
        if self.current_token == Token::LBracket {
            return self.parse_array_destructure(start, is_mutable, is_global);
        }
        if self.current_token == Token::LParen {
            return self.parse_tuple_destructure(start, is_mutable, is_global);
        }

        let name = self.expect_identifier();

        let mut declared_type = None;
        if self.current_token == Token::Colon {
            self.advance();
            declared_type = Some(self.parse_type());
        }

        self.expect(Token::Equal);
        let value = self.parse_expression();
        let end = value.span.end; // Declaration ends at value end (or semicolon usually, but NGN has implicit semi)

        Statement {
            kind: StatementKind::Declaration {
                name,
                is_mutable,
                is_global,
                value,
                declared_type,
            },
            span: Span::new(start, end),
        }
    }

    /// Parse object destructuring: const { a, b: aliasB, ...rest } = expr
    fn parse_object_destructure(
        &mut self,
        start: usize,
        is_mutable: bool,
        _is_global: bool,
    ) -> Statement {
        self.advance(); // consume '{'
        self.consume_newlines();

        let mut fields: Vec<(String, Option<String>)> = Vec::new();
        let mut rest: Option<String> = None;

        while self.current_token != Token::RBrace && self.current_token != Token::EOF {
            self.consume_newlines();

            // Check for rest syntax ...rest
            if self.current_token == Token::DotDotDot {
                self.advance(); // consume '...'
                let rest_name = self.expect_identifier();
                rest = Some(rest_name);
                self.consume_newlines();
                // Rest must be last, so break out
                if self.current_token == Token::Comma {
                    self.advance();
                    self.consume_newlines();
                }
                break;
            }

            // Parse field name
            let field_name = self.expect_identifier();

            // Check for alias: a: b
            let alias = if self.current_token == Token::Colon {
                self.advance();
                Some(self.expect_identifier())
            } else {
                None
            };

            fields.push((field_name, alias));

            self.consume_newlines();
            if self.current_token == Token::Comma {
                self.advance();
                self.consume_newlines();
            }
        }

        self.expect(Token::RBrace);
        self.expect(Token::Equal);
        let value = self.parse_expression();
        let end = value.span.end;

        Statement {
            kind: StatementKind::DestructureObject {
                fields,
                rest,
                is_mutable,
                value,
            },
            span: Span::new(start, end),
        }
    }

    /// Parse array destructuring: const [a, b, ...rest] = expr
    fn parse_array_destructure(
        &mut self,
        start: usize,
        is_mutable: bool,
        _is_global: bool,
    ) -> Statement {
        self.advance(); // consume '['
        self.consume_newlines();

        let mut bindings: Vec<String> = Vec::new();
        let mut rest: Option<String> = None;

        while self.current_token != Token::RBracket && self.current_token != Token::EOF {
            self.consume_newlines();

            // Check for rest syntax ...rest
            if self.current_token == Token::DotDotDot {
                self.advance(); // consume '...'
                let rest_name = self.expect_identifier();
                rest = Some(rest_name);
                self.consume_newlines();
                // Rest must be last, so break out
                if self.current_token == Token::Comma {
                    self.advance();
                    self.consume_newlines();
                }
                break;
            }

            // Parse binding name
            let name = self.expect_identifier();
            bindings.push(name);

            self.consume_newlines();
            if self.current_token == Token::Comma {
                self.advance();
                self.consume_newlines();
            }
        }

        self.expect(Token::RBracket);
        self.expect(Token::Equal);
        let value = self.parse_expression();
        let end = value.span.end;

        Statement {
            kind: StatementKind::DestructureArray {
                bindings,
                rest,
                is_mutable,
                value,
            },
            span: Span::new(start, end),
        }
    }

    /// Parse tuple destructuring: const (a, b, ...rest) = expr
    fn parse_tuple_destructure(
        &mut self,
        start: usize,
        is_mutable: bool,
        _is_global: bool,
    ) -> Statement {
        self.advance(); // consume '('
        self.consume_newlines();

        let mut bindings: Vec<String> = Vec::new();
        let mut rest: Option<String> = None;

        while self.current_token != Token::RParen && self.current_token != Token::EOF {
            self.consume_newlines();

            // Check for rest syntax ...rest
            if self.current_token == Token::DotDotDot {
                self.advance(); // consume '...'
                let rest_name = self.expect_identifier();
                rest = Some(rest_name);
                self.consume_newlines();
                // Rest must be last, so break out
                if self.current_token == Token::Comma {
                    self.advance();
                    self.consume_newlines();
                }
                break;
            }

            // Parse binding name
            let name = self.expect_identifier();
            bindings.push(name);

            self.consume_newlines();
            if self.current_token == Token::Comma {
                self.advance();
                self.consume_newlines();
            }
        }

        self.expect(Token::RParen);
        self.expect(Token::Equal);
        let value = self.parse_expression();
        let end = value.span.end;

        Statement {
            kind: StatementKind::DestructureTuple {
                bindings,
                rest,
                is_mutable,
                value,
            },
            span: Span::new(start, end),
        }
    }

    fn parse_type_unit(&mut self) -> Type {
        let base_type = match &self.current_token {
            Token::Identifier(name) => match name.as_str() {
                "i64" => {
                    self.advance();
                    Type::I64
                }
                "i32" => {
                    self.advance();
                    Type::I32
                }
                "i16" => {
                    self.advance();
                    Type::I16
                }
                "i8" => {
                    self.advance();
                    Type::I8
                }
                "u64" => {
                    self.advance();
                    Type::U64
                }
                "u32" => {
                    self.advance();
                    Type::U32
                }
                "u16" => {
                    self.advance();
                    Type::U16
                }
                "u8" => {
                    self.advance();
                    Type::U8
                }
                "f64" => {
                    self.advance();
                    Type::F64
                }
                "f32" => {
                    self.advance();
                    Type::F32
                }
                "string" => {
                    self.advance();
                    Type::String
                }
                "bytes" => {
                    self.advance();
                    Type::Bytes
                }
                "bool" => {
                    self.advance();
                    Type::Bool
                }
                "void" => {
                    self.advance();
                    Type::Void
                }
                "number" => {
                    self.advance();
                    Type::Number
                }
                "array" => {
                    self.advance();
                    if self.current_token == Token::LessThan {
                        self.advance();
                        let inner = self.parse_type();
                        self.expect(Token::GreaterThan);
                        Type::Array(Box::new(inner))
                    } else {
                        Type::Array(Box::new(Type::I64))
                    }
                }
                "tuple" => {
                    self.advance();
                    if self.current_token == Token::LessThan {
                        self.advance();
                        let mut base_types = Vec::new();
                        let mut last_type = None;
                        let mut size: Option<i64> = None;

                        while self.current_token != Token::GreaterThan {
                            if let Token::Number(n) = self.current_token {
                                size = Some(n);
                                self.advance();
                            } else {
                                let ty = self.parse_type();
                                last_type = Some(ty.clone());
                                base_types.push(ty);
                            }
                            if self.current_token == Token::Comma {
                                self.advance();
                            }
                        }
                        self.expect(Token::GreaterThan);

                        if let Some(n) = size {
                            if let Some(t) = last_type {
                                let mut actual_types = Vec::new();
                                for _ in 0..n {
                                    actual_types.push(t.clone());
                                }
                                return Type::Tuple(actual_types);
                            }
                        }
                        Type::Tuple(base_types)
                    } else {
                        Type::Tuple(vec![])
                    }
                }
                _ => {
                    let n = name.clone();
                    self.advance();

                    if self.type_params_in_scope.contains(&n) {
                        return Type::TypeParam(n);
                    }

                    if self.current_token == Token::LessThan {
                        self.advance();
                        let mut args = Vec::new();
                        while self.current_token != Token::GreaterThan {
                            args.push(self.parse_type());
                            if self.current_token == Token::Comma {
                                self.advance();
                            }
                        }
                        self.expect(Token::GreaterThan);
                        Type::Generic(n, args)
                    } else {
                        Type::Model(n)
                    }
                }
            },
            Token::Fn => {
                self.advance();
                self.parse_fn_type()
            }
            Token::Channel => {
                self.advance();
                if self.current_token == Token::LessThan {
                    self.advance();
                    let inner = self.parse_type();
                    self.expect(Token::GreaterThan);
                    Type::Channel(Box::new(inner))
                } else {
                    panic!(
                        "Syntax Error: channel type requires a type argument, e.g. channel<i64>"
                    );
                }
            }
            Token::Map => {
                self.advance();
                self.expect(Token::LessThan);
                let key_type = self.parse_type();
                self.expect(Token::Comma);
                let value_type = self.parse_type();
                self.expect(Token::GreaterThan);
                Type::Map(Box::new(key_type), Box::new(value_type))
            }
            Token::Set => {
                self.advance();
                self.expect(Token::LessThan);
                let element_type = self.parse_type();
                self.expect(Token::GreaterThan);
                Type::Set(Box::new(element_type))
            }
            Token::LParen => {
                self.advance();
                let mut inner_types = Vec::new();
                while self.current_token != Token::RParen {
                    inner_types.push(self.parse_type());
                    if self.current_token == Token::Comma {
                        self.advance();
                    }
                }
                self.expect(Token::RParen);
                Type::Tuple(inner_types)
            }
            _ => panic!(
                "Syntax Error: Expected a type, but found {:?}",
                self.current_token
            ),
        };

        if self.current_token == Token::Question {
            self.advance();
            Type::Generic("Maybe".to_string(), vec![base_type])
        } else {
            base_type
        }
    }

    fn parse_type(&mut self) -> Type {
        // Union types: A | B | C
        let first = self.parse_type_unit();
        if self.current_token != Token::Pipe {
            return first;
        }

        let mut members = vec![first];
        while self.current_token == Token::Pipe {
            self.advance();
            members.push(self.parse_type_unit());
        }

        let mut flat: Vec<Type> = Vec::new();
        for t in members {
            match t {
                Type::Union(inner) => flat.extend(inner),
                other => flat.push(other),
            }
        }

        let mut out: Vec<Type> = Vec::new();
        for t in flat {
            if !out.contains(&t) {
                out.push(t);
            }
        }

        if out.len() == 1 {
            out.remove(0)
        } else {
            Type::Union(out)
        }
    }

    /// Parse function type generics: fn, fn<T>, or fn<T, E, ...>
    /// - fn (bare) -> Function { params: [], return_type: Void }
    /// - fn<T> -> Function { params: [], return_type: T }
    /// - fn<T, E> -> Function { params: [T], return_type: E }
    /// - fn<T, E, R> -> Function { params: [T, E], return_type: R }
    fn parse_fn_type(&mut self) -> Type {
        if self.current_token != Token::LessThan {
            // Bare `fn` with no type info
            return Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(Type::Void),
            };
        }

        self.advance(); // consume '<'

        let mut type_args = Vec::new();
        type_args.push(self.parse_type());

        while self.current_token == Token::Comma {
            self.advance(); // consume ','
            type_args.push(self.parse_type());
        }

        self.expect(Token::GreaterThan);

        if type_args.len() == 1 {
            // fn<return_type> means () -> return_type
            Type::Function {
                params: vec![],
                optional_count: 0,
                return_type: Box::new(type_args.remove(0)),
            }
        } else {
            // fn<arg1, arg2, ..., return_type> - last is return, rest are params
            let return_type = type_args.pop().unwrap();
            Type::Function {
                params: type_args,
                optional_count: 0,
                return_type: Box::new(return_type),
            }
        }
    }

    pub fn parse_expression(&mut self) -> Expr {
        self.parse_send()
    }

    fn parse_send(&mut self) -> Expr {
        let mut left = self.parse_assignment();

        while self.current_token == Token::LArrow {
            let start = left.span.start;
            self.advance();
            let right = self.parse_assignment();
            let end = right.span.end;
            left = Expr {
                kind: ExprKind::Send(Box::new(left), Box::new(right)),
                span: Span::new(start, end),
            };
        }
        left
    }

    fn parse_assignment(&mut self) -> Expr {
        let expr = self.parse_null_coalesce();

        let compound_op = match self.current_token {
            Token::PlusEqual => Some(Token::Plus),
            Token::MinusEqual => Some(Token::Minus),
            Token::StarEqual => Some(Token::Star),
            Token::SlashEqual => Some(Token::Slash),
            Token::PercentEqual => Some(Token::Percent),
            Token::StarStarEqual => Some(Token::Power),
            Token::CaretEqual => Some(Token::Power),
            _ => None,
        };

        if let Some(binary_op) = compound_op {
            let start = expr.span.start;
            self.advance();
            let value = self.parse_assignment();
            let end = value.span.end;

            if let ExprKind::Variable(ref name) = expr.kind {
                return Expr {
                    kind: ExprKind::Assign {
                        name: name.clone(),
                        value: Box::new(Expr {
                            kind: ExprKind::Binary {
                                left: Box::new(expr),
                                op: binary_op,
                                right: Box::new(value),
                            },
                            span: Span::new(start, end),
                        }),
                    },
                    span: Span::new(start, end),
                };
            } else {
                panic!("Syntax Error: Invalid assignment target");
            }
        }

        // If the next token is '=', this is an assignment
        if self.current_token == Token::Equal {
            let start = expr.span.start;
            self.advance(); // consume '='
            let value = self.parse_assignment(); // Recursive for things like x = y = 10
            let end = value.span.end;

            // Check if the left side is a valid assignment target
            match expr.kind {
                ExprKind::Variable(name) => {
                    return Expr {
                        kind: ExprKind::Assign {
                            name,
                            value: Box::new(value),
                        },
                        span: Span::new(start, end),
                    };
                }
                ExprKind::FieldAccess { object, field } => {
                    return Expr {
                        kind: ExprKind::FieldAssign {
                            object,
                            field,
                            value: Box::new(value),
                        },
                        span: Span::new(start, end),
                    };
                }
                _ => {
                    panic!("Syntax Error: Invalid assignment target");
                }
            }
        }
        expr
    }

    /// Parse null-coalescing operator (??) with right-to-left associativity
    fn parse_null_coalesce(&mut self) -> Expr {
        let mut left = self.parse_equality();

        while self.current_token == Token::QuestionQuestion {
            let start = left.span.start;
            let op = self.current_token.clone();
            self.advance();
            let right = self.parse_equality(); // Right-to-left: parse at same level
            let end = right.span.end;
            left = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                span: Span::new(start, end),
            };
        }
        left
    }

    fn parse_equality(&mut self) -> Expr {
        let mut left = self.parse_comparison();

        loop {
            if self.paren_depth > 0 {
                self.consume_newlines();
            }
            if self.current_token == Token::EqualEqual || self.current_token == Token::NotEqual {
                let start = left.span.start;
                let op = self.current_token.clone();
                self.advance();
                let right = self.parse_comparison();
                let end = right.span.end;
                left = Expr {
                    kind: ExprKind::Binary {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                    },
                    span: Span::new(start, end),
                };
            } else {
                break;
            }
        }
        left
    }

    fn parse_comparison(&mut self) -> Expr {
        let mut left = self.parse_addition();

        loop {
            if self.paren_depth > 0 {
                self.consume_newlines();
            }
            if self.current_token == Token::LessThan
                || self.current_token == Token::GreaterThan
                || self.current_token == Token::LessThanEqual
                || self.current_token == Token::GreaterThanEqual
            {
                let start = left.span.start;
                let op = self.current_token.clone();
                self.advance();
                let right = self.parse_addition();
                let end = right.span.end;
                left = Expr {
                    kind: ExprKind::Binary {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                    },
                    span: Span::new(start, end),
                };
            } else {
                break;
            }
        }
        left
    }

    fn parse_addition(&mut self) -> Expr {
        let mut left = self.parse_multiplication();

        loop {
            if self.paren_depth > 0 {
                self.consume_newlines();
            }
            if self.current_token == Token::Plus || self.current_token == Token::Minus {
                let start = left.span.start;
                let op = self.current_token.clone();
                self.advance();
                let right = self.parse_multiplication();
                let end = right.span.end;

                left = Expr {
                    kind: ExprKind::Binary {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                    },
                    span: Span::new(start, end),
                };
            } else {
                break;
            }
        }
        left
    }

    fn parse_multiplication(&mut self) -> Expr {
        let mut left = self.parse_power();

        loop {
            if self.paren_depth > 0 {
                self.consume_newlines();
            }
            if self.current_token == Token::Star
                || self.current_token == Token::Slash
                || self.current_token == Token::Percent
            {
                let start = left.span.start;
                let op = self.current_token.clone();
                self.advance();
                let right = self.parse_power();
                let end = right.span.end;
                left = Expr {
                    kind: ExprKind::Binary {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                    },
                    span: Span::new(start, end),
                };
            } else {
                break;
            }
        }
        left
    }

    fn parse_power(&mut self) -> Expr {
        let left = self.parse_unary();

        if self.current_token == Token::Power {
            let start = left.span.start;
            self.advance();
            let right = self.parse_power(); // Right-associative
            let end = right.span.end;
            return Expr {
                kind: ExprKind::Binary {
                    left: Box::new(left),
                    op: Token::Power,
                    right: Box::new(right),
                },
                span: Span::new(start, end),
            };
        }
        left
    }

    fn parse_unary(&mut self) -> Expr {
        if self.paren_depth > 0 {
            self.consume_newlines();
        }
        if self.current_token == Token::Minus || self.current_token == Token::Bang {
            let start = self.current_span.start;
            let op = self.current_token.clone();
            self.advance();
            let right = self.parse_unary();
            let end = right.span.end;
            return Expr {
                kind: ExprKind::Unary {
                    op,
                    right: Box::new(right),
                },
                span: Span::new(start, end),
            };
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Expr {
        if self.paren_depth > 0 {
            self.consume_newlines();
        }

        let mut expr = match self.current_token.clone() {
            Token::Bool(b) => {
                let start = self.current_span.start;
                self.advance();
                Expr {
                    kind: ExprKind::Bool(b),
                    span: Span::new(start, self.previous_span.end),
                }
            }
            Token::Null => {
                let start = self.current_span.start;
                self.advance();
                Expr {
                    kind: ExprKind::Null,
                    span: Span::new(start, self.previous_span.end),
                }
            }
            Token::LParen => {
                let start = self.current_span.start;
                self.advance();
                self.paren_depth += 1;
                self.consume_newlines();

                if self.current_token == Token::RParen {
                    self.paren_depth -= 1;
                    self.advance();
                    Expr {
                        kind: ExprKind::Tuple(Vec::new()),
                        span: Span::new(start, self.previous_span.end),
                    }
                } else {
                    let expression = self.parse_expression();
                    self.consume_newlines();

                    if self.current_token == Token::Comma {
                        self.advance();
                        self.consume_newlines();
                        let mut elements = vec![expression];

                        while self.current_token != Token::RParen {
                            elements.push(self.parse_expression());
                            self.consume_newlines();
                            if self.current_token == Token::Comma {
                                self.advance();
                                self.consume_newlines();
                            }
                        }
                        self.paren_depth -= 1;
                        self.expect(Token::RParen);
                        Expr {
                            kind: ExprKind::Tuple(elements),
                            span: Span::new(start, self.previous_span.end),
                        }
                    } else {
                        self.paren_depth -= 1;
                        self.expect(Token::RParen);
                        expression
                    }
                }
            }
            Token::Number(n) => {
                let start = self.current_span.start;
                self.advance();
                Expr {
                    kind: ExprKind::Number(n),
                    span: Span::new(start, self.previous_span.end),
                }
            }
            Token::Float(n) => {
                let start = self.current_span.start;
                self.advance();
                Expr {
                    kind: ExprKind::Float(n),
                    span: Span::new(start, self.previous_span.end),
                }
            }
            Token::Regex(pattern) => {
                let start = self.current_span.start;
                self.advance();
                Expr {
                    kind: ExprKind::Regex(pattern),
                    span: Span::new(start, self.previous_span.end),
                }
            }
            Token::StringStart => self.parse_interpolated_string(),
            Token::Identifier(name) => {
                let start = self.current_span.start;
                self.advance();
                if self.current_token == Token::DoubleColon {
                    self.advance();
                    let variant_name = self.expect_identifier();
                    Expr {
                        kind: ExprKind::EnumVariant {
                            enum_name: Some(name),
                            variant_name,
                            args: Vec::new(),
                        },
                        span: Span::new(start, self.previous_span.end),
                    }
                } else {
                    Expr {
                        kind: ExprKind::Variable(name),
                        span: Span::new(start, self.previous_span.end),
                    }
                }
            }
            Token::LBracket => self.parse_array_literal(),
            Token::LBrace => {
                // Anonymous object literal: { key: value, ... }
                let start = self.current_span.start;
                self.advance(); // consume '{'
                self.consume_newlines();

                let mut fields = Vec::new();
                while self.current_token != Token::RBrace && self.current_token != Token::EOF {
                    let field_start = self.current_span.start;

                    // Accept either identifier or string as field name
                    let field_name = if let Token::StringStart = self.current_token {
                        // String key: { "Content-Type": value }
                        self.parse_literal_string()
                    } else {
                        // Identifier key: { field: value }
                        self.expect_identifier()
                    };

                    // Check if this is shorthand syntax: { field } instead of { field: value }
                    let value = if self.current_token == Token::Colon {
                        self.advance(); // consume ':'
                        self.parse_expression()
                    } else {
                        // Shorthand: use the identifier as both field name and variable reference
                        Expr {
                            kind: ExprKind::Variable(field_name.clone()),
                            span: Span::new(field_start, self.previous_span.end),
                        }
                    };

                    fields.push((field_name, value));
                    if self.current_token == Token::Comma {
                        self.advance();
                    }
                    self.consume_newlines();
                }
                self.expect(Token::RBrace);
                let end = self.previous_span.end;

                Expr {
                    kind: ExprKind::Object(fields),
                    span: Span::new(start, end),
                }
            }
            Token::Pipe => self.parse_closure(),
            Token::Thread => {
                let start = self.current_span.start;
                self.advance();
                self.expect(Token::LParen);
                let expr = self.parse_expression();
                self.expect(Token::RParen);
                Expr {
                    kind: ExprKind::Thread(Box::new(expr)),
                    span: Span::new(start, self.previous_span.end),
                }
            }
            Token::LArrow => {
                let start = self.current_span.start;
                self.advance();

                // Case 1: <-? chan (MaybeReceive)
                if self.current_token == Token::Question {
                    self.advance();
                    let chan = self.parse_expression();
                    Expr {
                        kind: ExprKind::ReceiveMaybe(Box::new(chan)),
                        span: Span::new(start, self.previous_span.end),
                    }
                } else {
                    // We need to distinguish between <- chan and <- count chan
                    let first = self.parse_expression();

                    if self.can_start_expression() {
                        let chan = self.parse_expression();
                        Expr {
                            kind: ExprKind::ReceiveCount(Box::new(chan), Box::new(first)),
                            span: Span::new(start, self.previous_span.end),
                        }
                    } else {
                        Expr {
                            kind: ExprKind::Receive(Box::new(first)),
                            span: Span::new(start, self.previous_span.end),
                        }
                    }
                }
            }
            Token::Channel => {
                let start = self.current_span.start;
                self.advance(); // consume 'channel'

                // Parse type parameter: channel<type>()
                let ty = if self.current_token == Token::LessThan {
                    self.advance(); // consume '<'
                    let inner_type = self.parse_type();
                    self.expect(Token::GreaterThan);
                    Some(inner_type)
                } else {
                    None
                };

                self.expect(Token::LParen);
                self.expect(Token::RParen);

                Expr {
                    kind: ExprKind::Channel(ty),
                    span: Span::new(start, self.previous_span.end),
                }
            }
            Token::State => {
                let start = self.current_span.start;
                self.advance();
                self.expect(Token::LParen);
                let initial = self.parse_expression();
                self.expect(Token::RParen);
                Expr {
                    kind: ExprKind::State(Box::new(initial)),
                    span: Span::new(start, self.previous_span.end),
                }
            }
            Token::Bytes => {
                let start = self.current_span.start;
                self.advance();
                self.expect(Token::LParen);

                let arg = if self.current_token == Token::RParen {
                    None
                } else {
                    let expr = self.parse_expression();
                    Some(Box::new(expr))
                };

                if self.current_token == Token::Comma {
                    // bytes() takes 0 or 1 arguments
                    panic!("Parse Error: bytes() takes at most 1 argument");
                }

                self.expect(Token::RParen);
                Expr {
                    kind: ExprKind::Bytes(arg),
                    span: Span::new(start, self.previous_span.end),
                }
            }
            Token::Map => {
                let start = self.current_span.start;
                self.advance(); // consume 'map'
                self.expect(Token::LessThan);
                let key_type = self.parse_type();
                self.expect(Token::Comma);
                let value_type = self.parse_type();
                self.expect(Token::GreaterThan);
                self.expect(Token::LParen);
                self.expect(Token::RParen);
                Expr {
                    kind: ExprKind::Map(key_type, value_type),
                    span: Span::new(start, self.previous_span.end),
                }
            }
            Token::Set => {
                let start = self.current_span.start;
                self.advance(); // consume 'set'
                self.expect(Token::LessThan);
                let element_type = self.parse_type();
                self.expect(Token::GreaterThan);
                self.expect(Token::LParen);
                self.expect(Token::RParen);
                Expr {
                    kind: ExprKind::Set(element_type),
                    span: Span::new(start, self.previous_span.end),
                }
            }
            Token::This => {
                let start = self.current_span.start;
                self.advance();
                Expr {
                    kind: ExprKind::This,
                    span: Span::new(start, self.previous_span.end),
                }
            }
            _ => {
                let msg = format!("Expected expression, found {:?}", self.current_token);
                eprintln!("{}", msg); // Log to stderr for debugging
                let start = self.current_span.start;
                if self.current_token != Token::EOF {
                    self.advance(); // consume the bad token to make progress
                }
                Expr {
                    kind: ExprKind::Error(msg),
                    span: Span::new(start, self.previous_span.end),
                }
            }
        };

        loop {
            match self.current_token {
                Token::LParen => {
                    self.advance(); // consume '('
                    self.paren_depth += 1;
                    self.consume_newlines();
                    let mut args = Vec::new();
                    while self.current_token != Token::RParen && self.current_token != Token::EOF {
                        args.push(self.parse_expression());
                        self.consume_newlines();
                        if self.current_token == Token::Comma {
                            self.advance();
                            self.consume_newlines();
                        }
                    }
                    self.paren_depth -= 1;
                    self.expect(Token::RParen);

                    let end = self.previous_span.end;
                    let start = expr.span.start;

                    match expr.kind {
                        ExprKind::Variable(name) => {
                            expr = Expr {
                                kind: ExprKind::Call { name, args },
                                span: Span::new(start, end),
                            };
                        }
                        ExprKind::EnumVariant {
                            enum_name,
                            variant_name,
                            ..
                        } => {
                            expr = Expr {
                                kind: ExprKind::EnumVariant {
                                    enum_name,
                                    variant_name,
                                    args,
                                },
                                span: Span::new(start, end),
                            };
                        }
                        _ => {
                            let msg =
                                "Syntax Error: Current expression cannot be called".to_string();
                            eprintln!("{}", msg);
                            expr = Expr {
                                kind: ExprKind::Error(msg),
                                span: expr.span,
                            };
                        }
                    }
                }
                Token::Period => {
                    self.advance(); // consume '.'
                    let name = self.expect_identifier();
                    let start = expr.span.start;

                    // Check for method call: .method(args)
                    if self.current_token == Token::LParen {
                        self.advance();
                        self.paren_depth += 1;
                        self.consume_newlines();
                        let mut args = Vec::new();
                        while self.current_token != Token::RParen
                            && self.current_token != Token::EOF
                        {
                            args.push(self.parse_expression());
                            self.consume_newlines();
                            if self.current_token == Token::Comma {
                                self.advance();
                                self.consume_newlines();
                            }
                        }
                        self.paren_depth -= 1;
                        self.expect(Token::RParen);
                        let end = self.previous_span.end;

                        expr = Expr {
                            kind: ExprKind::MethodCall(Box::new(expr), name, args),
                            span: Span::new(start, end),
                        };
                    } else {
                        // Field access: .field
                        let end = self.previous_span.end;
                        expr = Expr {
                            kind: ExprKind::FieldAccess {
                                object: Box::new(expr),
                                field: name,
                            },
                            span: Span::new(start, end),
                        };
                    }
                }
                Token::QuestionDot => {
                    // Optional chaining: obj?.field or obj?.method()
                    self.advance(); // consume '?.'
                    let name = self.expect_identifier();
                    let start = expr.span.start;

                    // Check for optional method call: ?.method(args)
                    if self.current_token == Token::LParen {
                        self.advance();
                        self.paren_depth += 1;
                        self.consume_newlines();
                        let mut args = Vec::new();
                        while self.current_token != Token::RParen
                            && self.current_token != Token::EOF
                        {
                            args.push(self.parse_expression());
                            self.consume_newlines();
                            if self.current_token == Token::Comma {
                                self.advance();
                                self.consume_newlines();
                            }
                        }
                        self.paren_depth -= 1;
                        self.expect(Token::RParen);
                        let end = self.previous_span.end;

                        expr = Expr {
                            kind: ExprKind::OptionalMethodCall(Box::new(expr), name, args),
                            span: Span::new(start, end),
                        };
                    } else {
                        // Optional field access: ?.field
                        let end = self.previous_span.end;
                        expr = Expr {
                            kind: ExprKind::OptionalFieldAccess {
                                object: Box::new(expr),
                                field: name,
                            },
                            span: Span::new(start, end),
                        };
                    }
                }
                Token::LBrace => {
                    if let ExprKind::Variable(name) = &expr.kind {
                        let name = name.clone();
                        let start = expr.span.start;
                        self.advance(); // consume '{'
                        self.consume_newlines();
                        let mut fields = Vec::new();
                        while self.current_token != Token::RBrace
                            && self.current_token != Token::EOF
                        {
                            let field_name = self.expect_identifier();
                            self.expect(Token::Colon);
                            let value = self.parse_expression();
                            fields.push((field_name, value));
                            if self.current_token == Token::Comma {
                                self.advance();
                            }
                            self.consume_newlines();
                        }
                        self.expect(Token::RBrace);
                        let end = self.previous_span.end;

                        expr = Expr {
                            kind: ExprKind::ModelInstance { name, fields },
                            span: Span::new(start, end),
                        };
                    } else {
                        break;
                    }
                }
                Token::LBracket => {
                    self.advance(); // consume '['
                    let index_expr = self.parse_expression();
                    self.expect(Token::RBracket);
                    let start = expr.span.start;
                    let end = self.previous_span.end;

                    expr = Expr {
                        kind: ExprKind::Index(Box::new(expr), Box::new(index_expr)),
                        span: Span::new(start, end),
                    };
                }
                _ => break,
            }
        }

        expr
    }
    fn can_start_expression(&mut self) -> bool {
        matches!(
            self.current_token,
            Token::Identifier(_)
                | Token::Number(_)
                | Token::Float(_)
                | Token::StringStart
                | Token::LParen
                | Token::LBracket
                | Token::LBrace
                | Token::Thread
                | Token::LArrow
                | Token::Channel
                | Token::Bytes
                | Token::State
                | Token::Bool(_)
                | Token::Enum
                | Token::Underscore
        )
    }

    fn parse_interpolated_string(&mut self) -> Expr {
        let start = self.current_span.start;
        self.expect(Token::StringStart);
        let mut parts = Vec::new();

        while self.current_token != Token::StringEnd && self.current_token != Token::EOF {
            match self.current_token.clone() {
                Token::StringPart(s) => {
                    let p_start = self.current_span.start;
                    self.advance();
                    let p_end = self.previous_span.end;
                    parts.push(Expr {
                        kind: ExprKind::String(s),
                        span: Span::new(p_start, p_end),
                    });
                }
                Token::InterpolationStart => {
                    self.advance();
                    parts.push(self.parse_expression());
                    self.expect(Token::InterpolationEnd);
                }
                _ => break,
            }
        }

        self.expect(Token::StringEnd);
        let end = self.previous_span.end; // string end token
        Expr {
            kind: ExprKind::InterpolatedString(parts),
            span: Span::new(start, end),
        }
    }

    fn parse_literal_string(&mut self) -> String {
        match self.current_token.clone() {
            Token::StringStart => {
                self.advance();
                let mut result = String::new();
                while let Token::StringPart(s) = &self.current_token {
                    result.push_str(s);
                    self.advance();
                }
                if let Token::InterpolationStart = self.current_token {
                    panic!("Syntax Error: Interpolation not allowed in this context");
                }
                self.expect(Token::StringEnd);
                result
            }
            _ => panic!("Expected string literal, found {:?}", self.current_token),
        }
    }

    fn parse_array_literal(&mut self) -> Expr {
        let start = self.current_span.start;
        self.advance(); // consume '['
        let mut elements = Vec::new();

        if self.current_token == Token::RBracket {
            self.advance(); // empty array
            let end = self.previous_span.end;
            return Expr {
                kind: ExprKind::Array(elements),
                span: Span::new(start, end),
            };
        }

        while self.current_token != Token::RBracket && self.current_token != Token::EOF {
            elements.push(self.parse_expression());

            if self.current_token == Token::Comma {
                self.advance();
            } else if self.current_token != Token::RBracket {
                panic!("Expected ',' or ']' in array literal");
            }
        }

        self.expect(Token::RBracket);
        let end = self.previous_span.end;
        Expr {
            kind: ExprKind::Array(elements),
            span: Span::new(start, end),
        }
    }

    fn parse_block(&mut self) -> Vec<Statement> {
        self.expect(Token::LBrace);
        let mut statements = Vec::new();

        loop {
            // Skip incoming newlines
            while self.current_token == Token::Newline {
                self.advance();
            }

            if self.current_token == Token::RBrace || self.current_token == Token::EOF {
                break;
            }

            statements.push(self.parse_statement());
        }

        self.expect(Token::RBrace);
        statements
    }

    fn parse_if_stmt(&mut self) -> Statement {
        let start = self.current_span.start;
        self.advance(); // consume 'if'

        // Check for Block If: if { ... }
        if self.current_token == Token::LBrace {
            self.advance(); // consume '{'

            // First branch must have a condition
            if self.current_token == Token::Newline {
                self.advance();
            }

            self.expect(Token::LParen);

            // Check for binding pattern: (var n = expr)
            let (condition, binding) = if self.current_token == Token::Var {
                self.advance(); // consume 'var'
                let bind_name = self.expect_identifier();
                self.expect(Token::Equal);
                let expr = self.parse_expression();
                (expr, Some(bind_name))
            } else {
                (self.parse_expression(), None)
            };

            self.expect(Token::RParen);

            // Parse statements until we hit ':' or '}'
            let mut then_block = Vec::new();
            loop {
                while self.current_token == Token::Newline {
                    self.advance();
                }
                if self.current_token == Token::Colon
                    || self.current_token == Token::RBrace
                    || self.current_token == Token::EOF
                {
                    break;
                }
                then_block.push(self.parse_statement());
            }

            let mut else_branch = None;

            if self.current_token == Token::Colon {
                self.advance(); // consume ':'
                while self.current_token == Token::Newline {
                    self.advance();
                }

                // Check if it's an 'else if' or 'else'
                if self.current_token == Token::LParen {
                    else_branch = Some(Box::new(self.parse_if_inner()));
                } else {
                    // Final 'else' block
                    let mut else_stmts = Vec::new();
                    loop {
                        while self.current_token == Token::Newline {
                            self.advance();
                        }
                        if self.current_token == Token::RBrace || self.current_token == Token::EOF {
                            break;
                        }
                        else_stmts.push(self.parse_statement());
                    }
                    let block_span = if !else_stmts.is_empty() {
                        Span::new(
                            else_stmts[0].span.start,
                            else_stmts.last().unwrap().span.end,
                        )
                    } else {
                        self.current_span
                    };
                    else_branch = Some(Box::new(Statement {
                        kind: StatementKind::Block(else_stmts),
                        span: block_span,
                    }));
                }
            }

            if self.current_token == Token::RBrace {
                self.advance();
            }
            let end = self.previous_span.end;

            // Construct 'then' block span approx
            let then_span = if !then_block.is_empty() {
                Span::new(
                    then_block[0].span.start,
                    then_block.last().unwrap().span.end,
                )
            } else {
                Span::new(start, end)
            };

            Statement {
                kind: StatementKind::If {
                    condition,
                    binding,
                    then_branch: Box::new(Statement {
                        kind: StatementKind::Block(then_block),
                        span: then_span,
                    }),
                    else_branch,
                },
                span: Span::new(start, end),
            }
        } else {
            // Inline If: if (cond) stmt : ... OR if (var b = x) stmt : ...
            self.expect(Token::LParen);

            // Check for binding pattern: (var n = expr)
            let (condition, binding) = if self.current_token == Token::Var {
                self.advance(); // consume 'var'
                let bind_name = self.expect_identifier();
                self.expect(Token::Equal);
                let expr = self.parse_expression();
                (expr, Some(bind_name))
            } else {
                (self.parse_expression(), None)
            };

            self.expect(Token::RParen);

            let then_stmt = self.parse_statement();
            let mut else_branch = None;

            // Allow newlines before the else colon for multiline inline if
            self.consume_newlines();

            if self.current_token == Token::Colon {
                self.advance(); // consume ':'
                // Check for 'else if' vs 'else'
                if self.current_token == Token::LParen {
                    // Inline Else If (Implicit If)
                    else_branch = Some(Box::new(self.parse_implicit_if()));
                } else {
                    else_branch = Some(Box::new(self.parse_statement()));
                }
            }

            let end = match &else_branch {
                Some(stmt) => stmt.span.end,
                None => then_stmt.span.end,
            };

            Statement {
                kind: StatementKind::If {
                    condition,
                    binding,
                    then_branch: Box::new(then_stmt),
                    else_branch,
                },
                span: Span::new(start, end),
            }
        }
    }

    // Helper for Block If recursion
    fn parse_if_inner(&mut self) -> Statement {
        let start = self.current_span.start; // start at '('
        // Expect condition
        self.expect(Token::LParen);

        // Check for binding pattern: (var n = expr)
        let (condition, binding) = if self.current_token == Token::Var {
            self.advance(); // consume 'var'
            let bind_name = self.expect_identifier();
            self.expect(Token::Equal);
            let expr = self.parse_expression();
            (expr, Some(bind_name))
        } else {
            (self.parse_expression(), None)
        };

        self.expect(Token::RParen);

        let mut then_block = Vec::new();
        loop {
            while self.current_token == Token::Newline {
                self.advance();
            }
            if self.current_token == Token::Colon
                || self.current_token == Token::RBrace
                || self.current_token == Token::EOF
            {
                break;
            }
            then_block.push(self.parse_statement());
        }

        let mut else_branch = None;
        if self.current_token == Token::Colon {
            self.advance();
            while self.current_token == Token::Newline {
                self.advance();
            }

            if self.current_token == Token::LParen {
                else_branch = Some(Box::new(self.parse_if_inner()));
            } else {
                let mut else_stmts = Vec::new();
                loop {
                    while self.current_token == Token::Newline {
                        self.advance();
                    }
                    if self.current_token == Token::RBrace || self.current_token == Token::EOF {
                        break;
                    }
                    else_stmts.push(self.parse_statement());
                }
                let block_span = if !else_stmts.is_empty() {
                    Span::new(
                        else_stmts[0].span.start,
                        else_stmts.last().unwrap().span.end,
                    )
                } else {
                    self.current_span
                };
                else_branch = Some(Box::new(Statement {
                    kind: StatementKind::Block(else_stmts),
                    span: block_span,
                }));
            }
        }

        let end = self.previous_span.end; // May not be accurate if RBrace handled by parent?
        // Actually, parse_if_inner is called inside the braces loop of parent.
        // It consumes segments inside the block.

        let then_span = if !then_block.is_empty() {
            Span::new(
                then_block[0].span.start,
                then_block.last().unwrap().span.end,
            )
        } else {
            Span::new(start, end)
        };

        Statement {
            kind: StatementKind::If {
                condition,
                binding,
                then_branch: Box::new(Statement {
                    kind: StatementKind::Block(then_block),
                    span: then_span,
                }),
                else_branch,
            },
            span: Span::new(start, end),
        }
    }

    // Parse: check var binding = source { failure_block }
    // or:    check var binding, error_binding = source { failure_block }
    fn parse_check_stmt(&mut self) -> Statement {
        let start = self.current_span.start;
        self.advance(); // consume 'check'

        // Expect: var or const
        let is_mutable = if self.current_token == Token::Var {
            self.advance();
            true
        } else if self.current_token == Token::Const {
            self.advance();
            false
        } else {
            panic!("Syntax Error: Expected 'var' or 'const' after 'check'");
        };

        let binding = self.expect_identifier();

        // Check for optional error binding: check var val, err = ...
        let error_binding = if self.current_token == Token::Comma {
            self.advance(); // consume ','
            Some(self.expect_identifier())
        } else {
            None
        };

        self.expect(Token::Equal);

        // Parse source as simple identifier (to avoid { being consumed as object literal)
        let source_name = self.expect_identifier();
        let source_span = self.previous_span;
        let source = Expr {
            kind: ExprKind::Variable(source_name),
            span: source_span,
        };

        // Parse failure block
        let failure_block = self.parse_block();
        let end = self.previous_span.end;

        Statement {
            kind: StatementKind::Check {
                binding,
                error_binding,
                is_mutable,
                source,
                failure_block: Box::new(Statement {
                    kind: StatementKind::Block(failure_block),
                    span: Span::new(start, end),
                }),
            },
            span: Span::new(start, end),
        }
    }

    // Helper for Inline If recursion (implicit if)
    fn parse_implicit_if(&mut self) -> Statement {
        let start = self.current_span.start;
        self.expect(Token::LParen);

        // Check for binding pattern: (var n = expr)
        let (condition, binding) = if self.current_token == Token::Var {
            self.advance(); // consume 'var'
            let bind_name = self.expect_identifier();
            self.expect(Token::Equal);
            let expr = self.parse_expression();
            (expr, Some(bind_name))
        } else {
            (self.parse_expression(), None)
        };

        self.expect(Token::RParen);
        let then_stmt = self.parse_statement();
        let mut else_branch = None;

        // Allow newlines before the else colon for multiline inline if
        self.consume_newlines();

        if self.current_token == Token::Colon {
            self.advance();
            if self.current_token == Token::LParen {
                else_branch = Some(Box::new(self.parse_implicit_if()));
            } else {
                else_branch = Some(Box::new(self.parse_statement()));
            }
        }

        let end = match &else_branch {
            Some(stmt) => stmt.span.end,
            None => then_stmt.span.end,
        };

        Statement {
            kind: StatementKind::If {
                condition,
                binding,
                then_branch: Box::new(then_stmt),
                else_branch,
            },
            span: Span::new(start, end),
        }
    }

    fn parse_while_stmt(&mut self) -> Statement {
        let start = self.current_span.start;
        self.advance(); // consume 'while'

        let mut is_once = false;
        if self.current_token == Token::Once {
            is_once = true;
            self.advance();
        }

        self.expect(Token::LParen);
        let condition = self.parse_expression();
        self.expect(Token::RParen);

        let body = if self.current_token == Token::LBrace {
            let b_start = self.current_span.start;
            let stmts = self.parse_block();
            let b_end = self.previous_span.end;
            Statement {
                kind: StatementKind::Block(stmts),
                span: Span::new(b_start, b_end),
            }
        } else {
            self.parse_statement()
        };
        let end = body.span.end;

        Statement {
            kind: StatementKind::While {
                condition,
                body: Box::new(body),
                is_once,
            },
            span: Span::new(start, end),
        }
    }

    fn parse_loop_stmt(&mut self) -> Statement {
        let start = self.current_span.start;
        self.advance(); // consume 'loop'

        let body = if self.current_token == Token::LBrace {
            let b_start = self.current_span.start;
            let stmts = self.parse_block();
            let b_end = self.previous_span.end;
            Statement {
                kind: StatementKind::Block(stmts),
                span: Span::new(b_start, b_end),
            }
        } else {
            self.parse_statement()
        };
        let end = body.span.end;

        Statement {
            kind: StatementKind::Loop(Box::new(body)),
            span: Span::new(start, end),
        }
    }

    fn parse_for_stmt(&mut self) -> Statement {
        let start = self.current_span.start;
        self.advance(); // consume 'for'
        self.expect(Token::LParen);

        let binding = self.expect_identifier();
        let mut index_binding = None;

        if self.current_token == Token::Comma {
            self.advance(); // consume ','
            index_binding = Some(self.expect_identifier());
        }

        self.expect(Token::In);
        let iterable = self.parse_expression();
        self.expect(Token::RParen);

        let body = if self.current_token == Token::LBrace {
            let b_start = self.current_span.start;
            let stmts = self.parse_block();
            let b_end = self.previous_span.end;
            Statement {
                kind: StatementKind::Block(stmts),
                span: Span::new(b_start, b_end),
            }
        } else {
            self.parse_statement()
        };
        let end = body.span.end;

        Statement {
            kind: StatementKind::For {
                binding,
                index_binding,
                iterable,
                body: Box::new(body),
            },
            span: Span::new(start, end),
        }
    }

    fn parse_match_stmt(&mut self) -> Statement {
        let start = self.current_span.start;
        self.advance(); // consume 'match'

        self.expect(Token::LParen);
        let condition = self.parse_expression();
        self.expect(Token::RParen);

        self.expect(Token::LBrace);
        self.consume_newlines();

        let mut arms = Vec::new();

        while self.current_token != Token::RBrace && self.current_token != Token::EOF {
            let mut patterns = Vec::new();

            // Check for default case _ =>
            if self.current_token == Token::Underscore {
                self.advance();
                self.expect(Token::FatArrow);
                patterns.push(Pattern::Wildcard);
            } else {
                loop {
                    patterns.push(self.parse_pattern());
                    if self.current_token == Token::Pipe {
                        self.advance();
                    } else {
                        break;
                    }
                }
                self.expect(Token::FatArrow);
            }

            let body = if self.current_token == Token::LBrace {
                let b_start = self.current_span.start;
                let stmts = self.parse_block();
                let b_end = self.previous_span.end;
                Statement {
                    kind: StatementKind::Block(stmts),
                    span: Span::new(b_start, b_end),
                }
            } else {
                self.parse_statement()
            };

            arms.push(MatchArm {
                patterns,
                body: Box::new(body),
            });

            if self.current_token == Token::Comma {
                self.advance();
            }
            self.consume_newlines();
        }

        self.expect(Token::RBrace);
        let end = self.previous_span.end;

        Statement {
            kind: StatementKind::Match { condition, arms },
            span: Span::new(start, end),
        }
    }

    fn parse_pattern(&mut self) -> Pattern {
        match &self.current_token {
            Token::Underscore => {
                self.advance();
                Pattern::Wildcard
            }
            Token::Identifier(id) => {
                let name = id.clone();
                self.advance();

                if self.current_token == Token::DoubleColon {
                    self.advance();
                    let variant_name = self.expect_identifier();
                    let mut binding = None;
                    if self.current_token == Token::LParen {
                        self.advance();
                        binding = Some(self.expect_identifier());
                        self.expect(Token::RParen);
                    }
                    return Pattern::EnumVariant {
                        enum_name: Some(name),
                        variant_name,
                        binding,
                    };
                }

                // Binding search: Variant(bind)
                if self.current_token == Token::LParen {
                    self.advance();
                    let binding = Some(self.expect_identifier());
                    self.expect(Token::RParen);
                    return Pattern::EnumVariant {
                        enum_name: None,
                        variant_name: name,
                        binding,
                    };
                }

                Pattern::EnumVariant {
                    enum_name: None,
                    variant_name: name,
                    binding: None,
                }
            }
            _ => Pattern::Literal(self.parse_expression()),
        }
    }

    fn parse_closure(&mut self) -> Expr {
        let start = self.current_span.start;
        self.advance(); // consume first '|'

        let mut params = Vec::new();

        if self.current_token != Token::Pipe {
            loop {
                if self.current_token == Token::Pipe {
                    break;
                }

                let p_start = self.current_span.start;
                let param_name = self.expect_identifier();

                let mut ty = None;
                let mut is_owned = false;

                if self.current_token == Token::Colon {
                    self.advance();

                    if self.current_token == Token::LessThan {
                        is_owned = true;
                        self.advance();
                    }

                    // In closure param lists, `|` is also the parameter-list terminator.
                    // We therefore parse a single type unit here (including trailing `?`)
                    // rather than a full union type.
                    ty = Some(self.parse_type_unit());
                }
                let p_end = self.previous_span.end;

                params.push(Parameter {
                    name: param_name,
                    is_owned,
                    ty,
                    is_optional: false,
                    default_value: None,
                    span: Span::new(p_start, p_end),
                });

                if self.current_token == Token::Comma {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        self.expect(Token::Pipe);

        let return_type = if self.current_token == Token::Colon {
            self.advance();
            Some(self.parse_type())
        } else {
            None
        };

        // Closures are function-like contexts, so allow const/var declarations
        let old_in_function = self.in_function;
        self.in_function = true;

        // Parse body - expect block or expression
        let body_stmt = if self.current_token == Token::LBrace {
            let b_start = self.current_span.start;
            let stmts = self.parse_block();
            let b_end = self.previous_span.end;
            Box::new(Statement {
                kind: StatementKind::Block(stmts),
                span: Span::new(b_start, b_end),
            })
        } else {
            // Single expression implicit return
            let expr = self.parse_expression();
            let e_start = expr.span.start;
            let e_end = expr.span.end;
            Box::new(Statement {
                kind: StatementKind::Return(Some(expr)),
                span: Span::new(e_start, e_end),
            })
        };

        self.in_function = old_in_function;
        let end = body_stmt.span.end;

        Expr {
            kind: ExprKind::Closure {
                params,
                body: body_stmt,
                return_type,
            },
            span: Span::new(start, end),
        }
    }

    fn parse_enum_stmt(&mut self) -> Statement {
        let start = self.current_span.start;

        if self.in_function {
            panic!("Syntax Error: 'enum' can only be declared in global scope");
        }
        self.advance(); // consume 'enum'
        let name = self.expect_identifier();

        // Parse optional type parameters: enum MyEnum<T, U> { ... }
        let mut type_params = Vec::new();
        if self.current_token == Token::LessThan {
            self.advance(); // consume '<'
            while self.current_token != Token::GreaterThan {
                let param_name = self.expect_identifier();
                type_params.push(param_name.clone());
                if self.current_token == Token::Comma {
                    self.advance();
                }
            }
            self.expect(Token::GreaterThan);
        }

        // Add type params to scope for parsing variant data types
        let old_type_params =
            std::mem::replace(&mut self.type_params_in_scope, type_params.clone());

        self.expect(Token::LBrace);
        self.consume_newlines();

        let mut variants = Vec::new();
        while self.current_token != Token::RBrace && self.current_token != Token::EOF {
            let variant_name = self.expect_identifier();
            let mut data_type = None;

            if self.current_token == Token::LParen {
                self.advance();
                data_type = Some(self.parse_type());
                self.expect(Token::RParen);
            }

            variants.push(EnumVariantDef {
                name: variant_name,
                data_type,
            });

            if self.current_token == Token::Comma {
                self.advance();
            }
            self.consume_newlines();
        }
        self.expect(Token::RBrace);
        let end = self.previous_span.end;

        // Restore previous type params scope
        self.type_params_in_scope = old_type_params;

        Statement {
            kind: StatementKind::Enum(EnumDef {
                name,
                type_params,
                variants,
            }),
            span: Span::new(start, end),
        }
    }
    fn parse_model_stmt(&mut self) -> Statement {
        let start = self.current_span.start;

        if self.in_function {
            panic!("Syntax Error: 'model' can only be declared in global scope");
        }
        self.advance(); // consume 'model'
        let name = self.expect_identifier();

        // Parse optional type parameters: model Container<T, U> { ... }
        let mut type_params = Vec::new();
        if self.current_token == Token::LessThan {
            self.advance(); // consume '<'
            while self.current_token != Token::GreaterThan {
                let param_name = self.expect_identifier();
                type_params.push(param_name.clone());
                if self.current_token == Token::Comma {
                    self.advance();
                }
            }
            self.expect(Token::GreaterThan);
        }

        // Add type params to scope for parsing field types
        let old_type_params =
            std::mem::replace(&mut self.type_params_in_scope, type_params.clone());

        self.expect(Token::LBrace);
        self.consume_newlines();

        let mut fields = Vec::new();
        while self.current_token != Token::RBrace && self.current_token != Token::EOF {
            let field_name = self.expect_identifier();
            self.expect(Token::Colon);
            let field_type = self.parse_type();
            fields.push((field_name, field_type));

            if self.current_token == Token::Comma {
                self.advance();
            }
            self.consume_newlines();
        }
        self.expect(Token::RBrace);
        let end = self.previous_span.end;

        // Restore previous type params scope
        self.type_params_in_scope = old_type_params;

        Statement {
            kind: StatementKind::Model(ModelDef {
                name,
                type_params,
                fields,
            }),
            span: Span::new(start, end),
        }
    }

    fn parse_role_stmt(&mut self) -> Statement {
        let start = self.current_span.start;

        if self.in_function {
            panic!("Syntax Error: 'role' can only be declared in global scope");
        }
        self.advance(); // consume 'role'
        let name = self.expect_identifier();
        self.expect(Token::LBrace);
        self.consume_newlines();

        let mut methods = Vec::new();
        while self.current_token != Token::RBrace && self.current_token != Token::EOF {
            let method = self.parse_function();
            methods.push(method);
            self.consume_newlines();
        }
        self.expect(Token::RBrace);
        let end = self.previous_span.end;

        Statement {
            kind: StatementKind::Role(RoleDef { name, methods }),
            span: Span::new(start, end),
        }
    }

    fn parse_extend_target_type(&mut self) -> Type {
        // Special type parsing for extend statements
        // Allows `map` and `set` without type parameters to mean "all maps/sets"
        match &self.current_token {
            Token::Map => {
                self.advance();
                // Check if type params are provided
                if self.current_token == Token::LessThan {
                    self.advance();
                    let key_type = self.parse_type();
                    self.expect(Token::Comma);
                    let value_type = self.parse_type();
                    self.expect(Token::GreaterThan);
                    Type::Map(Box::new(key_type), Box::new(value_type))
                } else {
                    // Generic: extend all maps
                    Type::Map(Box::new(Type::Any), Box::new(Type::Any))
                }
            }
            Token::Set => {
                self.advance();
                // Check if type params are provided
                if self.current_token == Token::LessThan {
                    self.advance();
                    let element_type = self.parse_type();
                    self.expect(Token::GreaterThan);
                    Type::Set(Box::new(element_type))
                } else {
                    // Generic: extend all sets
                    Type::Set(Box::new(Type::Any))
                }
            }
            Token::Identifier(id) if id == "number" => {
                self.advance();
                Type::Number
            }
            _ => {
                // For all other types, use normal type parsing
                self.parse_type()
            }
        }
    }

    fn parse_extend_stmt(&mut self) -> Statement {
        let start = self.current_span.start;
        self.advance(); // consume 'extend'

        let target = self.parse_extend_target_type();

        let mut role = None;
        if self.current_token == Token::With {
            self.advance();
            if self.current_token != Token::LBrace {
                role = Some(self.expect_identifier());
            }
        }

        self.expect(Token::LBrace);
        self.consume_newlines();

        let mut methods = Vec::new();
        while self.current_token != Token::RBrace && self.current_token != Token::EOF {
            let method = self.parse_function();
            methods.push(method);
            self.consume_newlines();
        }

        self.expect(Token::RBrace);
        let end = self.previous_span.end;

        Statement {
            kind: StatementKind::Extend {
                target,
                role,
                methods,
            },
            span: Span::new(start, end),
        }
    }
}
