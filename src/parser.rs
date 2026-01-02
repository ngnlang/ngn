use crate::lexer::{Lexer, Token};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    I64, I32, I16, I8,
    U64, U32, U16, U8,
    F64, F32,
    String,
    Bool,
    Void,
    Array(Box<Type>),
    Tuple(Vec<Type>),
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    Any,
    Enum(String),
    Channel(Box<Type>),
    State(Box<Type>),
    Model(String),
    Role(String),
    Generic(String, Vec<Type>),
    Map(Box<Type>, Box<Type>),
    Set(Box<Type>),
    Regex,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub is_owned: bool,
    pub ty: Option<Type>,
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
    pub variants: Vec<EnumVariantDef>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Declaration {
        name: String,
        is_mutable: bool,
        is_static: bool,
        value: Expr,
        declared_type: Option<Type>,
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
        source: String 
    },
    // import x from "..."
    ImportDefault {
        name: String,
        source: String,
    },
    // import * as X from "..."
    ImportModule {
        alias: String,
        source: String
    },
    ExportDefault(Expr),
    Print(Expr),
    Echo(Expr),
    Sleep(Expr),
    Block(Vec<Statement>),
    If {
        condition: Expr,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModelDef {
    pub name: String,
    pub fields: Vec<(String, Type)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RoleDef {
    pub name: String,
    pub methods: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Assign { name: String, value: Box<Expr> },
    Bool(bool),
    Regex(String),
    Call { name: String, args: Vec<Expr> },
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
    MethodCall(Box<Expr>, String, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Unary {
        op: Token,
        right: Box<Expr>,
    },
    ModelInstance { name: String, fields: Vec<(String, Expr)> },
    FieldAccess { object: Box<Expr>, field: String },
    Map(Type, Type),
    Set(Type),
    This,
}

pub struct Parser {
    pub lexer: Lexer,
    pub current_token: Token,
    pub in_function: bool,
    pub paren_depth: usize,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let first_token = lexer.next_token();
        Self {
            lexer,
            current_token: first_token,
            in_function: false,
            paren_depth: 0,
        }
    }

    // Move to the next token
    pub fn advance(&mut self) {
        self.current_token = self.lexer.next_token();
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
            panic!("Expected {:?}, got {:?}", expected, self.current_token);
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
                _ => unreachable!()
            };
            self.advance();
            name
        } else {
            panic!(
                "Syntax Error: Expected an identifier, but found {:?}", 
                self.current_token
            );
        }
    }

    fn parse_function(&mut self) -> Statement {
        self.parse_function_with_export(false)
    }

    fn parse_function_with_export(&mut self, is_exported: bool) -> Statement {
        self.advance();

        let name = self.expect_identifier();
        self.expect(Token::LParen);

        let mut params = Vec::new();

        while self.current_token != Token::RParen {
            let param_name = self.expect_identifier();
            
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

            params.push(Parameter { name: param_name, is_owned, ty });

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
            vec![Statement::Return(Some(self.parse_expression()))]
        };
        
        self.in_function = old_in_function;

        Statement::Function { name, params, body, return_type, is_exported }
    }

    fn parse_import_statement(&mut self) -> Statement {
        self.advance(); // consume 'import'
        
        // 1. Module Import: import * as Name from "..."
        if self.current_token == Token::Star {
            self.advance(); // consume '*'
            self.expect(Token::As);
            let alias = self.expect_identifier();
            self.expect(Token::From);
            
            let source = self.parse_literal_string();
            return Statement::ImportModule { alias, source };
        }

        // 2. Named Imports: import { a, b as c } from "..."
        if self.current_token == Token::LBrace {
            self.advance(); // consume '{'
            let mut names = Vec::new();
            while self.current_token != Token::RBrace {
                let name = self.expect_identifier();
                let mut alias = None;
                if self.current_token == Token::As {
                    self.advance(); // consume 'as'
                    alias = Some(self.expect_identifier());
                }
                names.push((name, alias));
                if self.current_token == Token::Comma { self.advance(); }
            }
            self.expect(Token::RBrace);
            self.expect(Token::From);
            
            let source = self.parse_literal_string();
            return Statement::Import { names, source };
        }

        // 3. Default Import: import Name from "..."
        let name = self.expect_identifier();
        self.expect(Token::From);
        
        let source = self.parse_literal_string();
        Statement::ImportDefault { name, source }
    }

    pub fn parse_statement(&mut self) -> Statement {
        self.consume_newlines();

        let stmt = match self.current_token.clone() {
            Token::Static => {
                if self.in_function {
                    panic!("Syntax Error: 'static' declarator can only be used in global scope");
                }
                self.parse_declaration()
            }
            Token::Const => {
                if !self.in_function {
                    panic!("Syntax Error: 'const' declarator can only be used inside functions (global state must be 'static')");
                }
                self.parse_declaration()
            }
            Token::Var => {
                if !self.in_function {
                    panic!("Syntax Error: 'var' declarator can only be used inside functions (global state must be 'static')");
                }
                self.parse_declaration()
            }
            Token::Fn => self.parse_function(),
            Token::Export => {
                self.advance(); // consume 'export'
                if self.current_token == Token::Default {
                    self.advance(); // consume 'default'
                    // Expect an expression for now (typically an identifier)
                    let expr = self.parse_expression();
                    Statement::ExportDefault(expr)
                } else if self.current_token == Token::Fn {
                    self.parse_function_with_export(true)
                } else {
                    panic!("Syntax Error: 'export' must be followed by 'fn' or 'default'");
                }
            }
            Token::Identifier(_) => {
                let expr = self.parse_expression();
                Statement::Expression(expr)
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
                Statement::Print(expr)
            }
            Token::Echo => {
                self.advance();
                self.expect(Token::LParen);
                let expr = self.parse_expression();
                self.expect(Token::RParen);
                Statement::Echo(expr)
            }
            Token::Sleep => {
                self.advance();
                self.expect(Token::LParen);
                let expr = self.parse_expression();
                self.expect(Token::RParen);
                Statement::Sleep(expr)
            }
            Token::If => self.parse_if_stmt(),
            Token::While => self.parse_while_stmt(),
            Token::Loop => self.parse_loop_stmt(),
            Token::For => self.parse_for_stmt(),
            Token::Match => self.parse_match_stmt(),
            Token::LBrace => Statement::Block(self.parse_block()),
            Token::Break => {
                self.advance();
                Statement::Break
            }
            Token::Next => {
                self.advance();
                Statement::Next
            }
            Token::Return => {
                self.advance();
                let expr = if self.current_token != Token::Newline && self.current_token != Token::RBrace && self.current_token != Token::EOF {
                    Some(self.parse_expression())
                } else {
                    None
                };
                Statement::Return(expr)
            }
             _ => {
               let expr = self.parse_expression();
               Statement::Expression(expr)
            }
        };

        if self.current_token == Token::Newline {
             self.advance();
        }
        
        stmt
    }

    fn parse_declaration(&mut self) -> Statement {
        let declarator = self.current_token.clone();
        self.advance(); // consume 'var', 'const', or 'static'
        
        let (is_mutable, is_static) = match declarator {
            Token::Var => (true, false),
            Token::Const => (false, false),
            Token::Static => (false, true),
            _ => panic!("Expected declarator"),
        };

        let name = self.expect_identifier();
        
        let mut declared_type = None;
        if self.current_token == Token::Colon {
            self.advance();
            declared_type = Some(self.parse_type());
        }

        self.expect(Token::Equal);
        let value = self.parse_expression();
        
        Statement::Declaration {
            name,
            is_mutable,
            is_static,
            value,
            declared_type,
        }
    }

    fn parse_type(&mut self) -> Type {
        match &self.current_token {
            Token::Identifier(name) => {
                match name.as_str() {
                    "i64" => { self.advance(); Type::I64 }
                    "i32" => { self.advance(); Type::I32 }
                    "i16" => { self.advance(); Type::I16 }
                    "i8" => { self.advance(); Type::I8 }
                    "u64" => { self.advance(); Type::U64 }
                    "u32" => { self.advance(); Type::U32 }
                    "u16" => { self.advance(); Type::U16 }
                    "u8" => { self.advance(); Type::U8 }
                    "f64" => { self.advance(); Type::F64 }
                    "f32" => { self.advance(); Type::F32 }
                    "string" => { self.advance(); Type::String }
                    "bool" => { self.advance(); Type::Bool }
                    "void" => { self.advance(); Type::Void }
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
                    _ => { // assume model or generic
                        let n = name.clone();
                        self.advance();
                        
                        // Check for generics <T, U>
                        if self.current_token == Token::LessThan {
                             self.advance(); // consume <
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
                }
            }
            Token::Fn => {
                self.advance(); // consume 'fn'
                self.parse_fn_type()
            }
            Token::Channel => {
                self.advance(); // consume 'channel'
                if self.current_token == Token::LessThan {
                    self.advance(); // consume '<'
                    let inner = self.parse_type();
                    self.expect(Token::GreaterThan);
                    Type::Channel(Box::new(inner))
                } else {
                    panic!("Syntax Error: channel type requires a type argument, e.g. channel<i64>");
                }
            }
            Token::Map => {
                self.advance(); // consume 'map'
                self.expect(Token::LessThan);
                let key_type = self.parse_type();
                self.expect(Token::Comma);
                let value_type = self.parse_type();
                self.expect(Token::GreaterThan);
                Type::Map(Box::new(key_type), Box::new(value_type))
            }
            Token::Set => {
                self.advance(); // consume 'set'
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
            _ => panic!("Syntax Error: Expected a type, but found {:?}", self.current_token),
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
                return_type: Box::new(type_args.remove(0)),
            }
        } else {
            // fn<arg1, arg2, ..., return_type> - last is return, rest are params
            let return_type = type_args.pop().unwrap();
            Type::Function {
                params: type_args,
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
            self.advance();
            let right = self.parse_assignment();
            left = Expr::Send(Box::new(left), Box::new(right));
        }
        left
    }

    fn parse_assignment(&mut self) -> Expr {
        let expr = self.parse_equality();

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
            self.advance();
            let value = self.parse_assignment();
            if let Expr::Variable(ref name) = expr {
                return Expr::Assign {
                    name: name.clone(),
                    value: Box::new(Expr::Binary {
                        left: Box::new(expr),
                        op: binary_op,
                        right: Box::new(value),
                    }),
                };
            } else {
                panic!("Syntax Error: Invalid assignment target");
            }
        }

        // If the next token is '=', this is an assignment
        if self.current_token == Token::Equal {
            self.advance(); // consume '='
            let value = self.parse_assignment(); // Recursive for things like x = y = 10

            // We check if the left side was actually a variable
            if let Expr::Variable(name) = expr {
                return Expr::Assign {
                    name,
                    value: Box::new(value),
                };
            } else {
                panic!("Syntax Error: Invalid assignment target");
            }
        }
        expr
    }

    fn parse_equality(&mut self) -> Expr {
        let mut left = self.parse_comparison();

        loop {
            if self.paren_depth > 0 { self.consume_newlines(); }
            if self.current_token == Token::EqualEqual || self.current_token == Token::NotEqual {
                let op = self.current_token.clone();
                self.advance();
                let right = self.parse_comparison();
                left = Expr::Binary { left: Box::new(left), op, right: Box::new(right) };
            } else {
                break;
            }
        }
        left
    }

    fn parse_comparison(&mut self) -> Expr {
        let mut left = self.parse_addition();

        loop {
            if self.paren_depth > 0 { self.consume_newlines(); }
            if self.current_token == Token::LessThan || self.current_token == Token::GreaterThan || 
                self.current_token == Token::LessThanEqual || self.current_token == Token::GreaterThanEqual {
                let op = self.current_token.clone();
                self.advance();
                let right = self.parse_addition();
                left = Expr::Binary { left: Box::new(left), op, right: Box::new(right) };
            } else {
                break;
            }
        }
        left
    }

    fn parse_addition(&mut self) -> Expr {
        let mut left = self.parse_multiplication();

        loop {
            if self.paren_depth > 0 { self.consume_newlines(); }
            if self.current_token == Token::Plus || self.current_token == Token::Minus {
                let op = self.current_token.clone();
                self.advance();
                let right = self.parse_multiplication();
                
                left = Expr::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
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
            if self.paren_depth > 0 { self.consume_newlines(); }
            if self.current_token == Token::Star || self.current_token == Token::Slash || self.current_token == Token::Percent {
                let op = self.current_token.clone();
                self.advance();
                let right = self.parse_power();
                left = Expr::Binary { left: Box::new(left), op, right: Box::new(right) };
            } else {
                break;
            }
        }
        left
    }

    fn parse_power(&mut self) -> Expr {
        let left = self.parse_unary();

        if self.current_token == Token::Power {
            self.advance();
            let right = self.parse_power(); // Right-associative
            return Expr::Binary {
                left: Box::new(left),
                op: Token::Power,
                right: Box::new(right),
            };
        }
        left
    }

    fn parse_unary(&mut self) -> Expr {
        if self.paren_depth > 0 { self.consume_newlines(); }
        if self.current_token == Token::Minus || self.current_token == Token::Bang {
            let op = self.current_token.clone();
            self.advance();
            let right = self.parse_unary();
            return Expr::Unary { op, right: Box::new(right) };
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Expr {
        if self.paren_depth > 0 {
            self.consume_newlines();
        }
        
        let mut expr = match self.current_token.clone() {
            Token::Bool(b) => {
                self.advance();
                Expr::Bool(b)
            }
            Token::LParen => {
                self.advance();
                self.paren_depth += 1;
                self.consume_newlines();
                
                if self.current_token == Token::RParen {
                    self.paren_depth -= 1;
                    self.advance();
                    Expr::Tuple(Vec::new())
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
                        Expr::Tuple(elements)
                    } else {
                        self.paren_depth -= 1;
                        self.expect(Token::RParen);
                        expression
                    }
                }
            }
            Token::Number(n) => {
                self.advance();
                Expr::Number(n)
            }
            Token::Float(n) => {
                self.advance();
                Expr::Float(n)
            }
            Token::Regex(pattern) => {
                self.advance();
                Expr::Regex(pattern)
            }
            Token::StringStart => self.parse_interpolated_string(),
            Token::Identifier(name) => {
                self.advance();
                if self.current_token == Token::DoubleColon {
                    self.advance();
                    let variant_name = self.expect_identifier();
                    Expr::EnumVariant {
                        enum_name: Some(name),
                        variant_name,
                        args: Vec::new(),
                    }
                } else {
                    Expr::Variable(name)
                }
            }
            Token::LBracket => self.parse_array_literal(),
            Token::Pipe => self.parse_closure(),
            Token::Thread => {
                self.advance();
                self.expect(Token::LParen);
                let expr = self.parse_expression();
                self.expect(Token::RParen);
                Expr::Thread(Box::new(expr))
            }
            Token::LArrow => {
                self.advance();
                
                // Case 1: <-? chan (MaybeReceive)
                if self.current_token == Token::Question {
                    self.advance();
                    let chan = self.parse_expression();
                    Expr::ReceiveMaybe(Box::new(chan))
                } else {
                    // We need to distinguish between <- chan and <- count chan
                    // In ngn2's current parser, we can try to parse one expression
                    let first = self.parse_expression();
                    
                    // If the NEXT token can start a channel expression (like an identifier, bracket, etc.)
                    // then 'first' was likely a count.
                    // This is slightly ambiguous but let's follow the simple heuristic: 
                    // if it's a number literal, it's a count if followed by another expression.
                    
                    if self.can_start_expression() {
                        let chan = self.parse_expression();
                        Expr::ReceiveCount(Box::new(chan), Box::new(first))
                    } else {
                        Expr::Receive(Box::new(first))
                    }
                }
            }
            Token::Channel => {
                self.advance();
                self.expect(Token::LParen);
                self.expect(Token::RParen);
                let ty = if self.current_token == Token::Colon {
                    self.advance();
                    Some(self.parse_type())
                } else {
                    None
                };
                Expr::Channel(ty)
            }
            Token::State => {
                self.advance();
                self.expect(Token::LParen);
                let initial = self.parse_expression();
                self.expect(Token::RParen);
                Expr::State(Box::new(initial))
            }
            Token::Map => {
                self.advance(); // consume 'map'
                self.expect(Token::LessThan);
                let key_type = self.parse_type();
                self.expect(Token::Comma);
                let value_type = self.parse_type();
                self.expect(Token::GreaterThan);
                self.expect(Token::LParen);
                self.expect(Token::RParen);
                Expr::Map(key_type, value_type)
            }
            Token::Set => {
                self.advance(); // consume 'set'
                self.expect(Token::LessThan);
                let element_type = self.parse_type();
                self.expect(Token::GreaterThan);
                self.expect(Token::LParen);
                self.expect(Token::RParen);
                Expr::Set(element_type)
            }
            Token::This => {
                self.advance();
                Expr::This
            }
            _ => panic!("Expected expression, found {:?}", self.current_token),
        };

        loop {
            match self.current_token {
                Token::LParen => {
                    self.advance(); // consume '('
                    let mut args = Vec::new();
                    while self.current_token != Token::RParen {
                        args.push(self.parse_expression());
                        if self.current_token == Token::Comma { self.advance(); }
                    }
                    self.expect(Token::RParen);
                    
                    if let Expr::Variable(name) = expr {
                        expr = Expr::Call { name, args };
                    } else if let Expr::EnumVariant { enum_name, variant_name, .. } = expr {
                        expr = Expr::EnumVariant { enum_name, variant_name, args };
                    } else {
                        panic!("Syntax Error: Current expression {:?} cannot be called", expr);
                    }
                }
                Token::Period => {
                    self.advance(); // consume '.'
                    let name = self.expect_identifier();
                    
                    // Check for method call: .method(args)
                    if self.current_token == Token::LParen {
                        self.advance();
                        let mut args = Vec::new();
                        while self.current_token != Token::RParen {
                            args.push(self.parse_expression());
                            if self.current_token == Token::Comma { self.advance(); }
                        }
                        self.expect(Token::RParen);
                        expr = Expr::MethodCall(Box::new(expr), name, args);
                    } else {
                        // Field access: .field
                        expr = Expr::FieldAccess { object: Box::new(expr), field: name };
                    }
                }
                Token::LBrace => {
                    if let Expr::Variable(name) = expr {
                        self.advance(); // consume '{'
                        self.consume_newlines();
                        let mut fields = Vec::new();
                        while self.current_token != Token::RBrace && self.current_token != Token::EOF {
                            let field_name = self.expect_identifier();
                            self.expect(Token::Colon);
                            let value = self.parse_expression();
                            fields.push((field_name, value));
                            if self.current_token == Token::Comma { self.advance(); }
                            self.consume_newlines();
                        }
                        self.expect(Token::RBrace);
                        expr = Expr::ModelInstance { name, fields };
                    } else {
                        break;
                    }
                }
                Token::LBracket => {
                    self.advance(); // consume '['
                    let index_expr = self.parse_expression();
                    self.expect(Token::RBracket);
                    expr = Expr::Index(Box::new(expr), Box::new(index_expr));
                }
                _ => break,
            }
        }

        expr
    }
    fn can_start_expression(&mut self) -> bool {
        matches!(
            self.current_token,
            Token::Identifier(_) | Token::Number(_) | Token::Float(_) | 
            Token::StringStart | Token::LParen | Token::LBracket | 
            Token::Thread | Token::LArrow | Token::Channel | Token::State |
            Token::Bool(_) | Token::Enum | Token::Underscore
        )
    }

    fn parse_interpolated_string(&mut self) -> Expr {
        self.expect(Token::StringStart);
        let mut parts = Vec::new();

        while self.current_token != Token::StringEnd && self.current_token != Token::EOF {
            match self.current_token.clone() {
                Token::StringPart(s) => {
                    self.advance();
                    parts.push(Expr::String(s));
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
        Expr::InterpolatedString(parts)
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
        self.advance(); // consume '['
        let mut elements = Vec::new();

        if self.current_token == Token::RBracket {
            self.advance(); // empty array
            return Expr::Array(elements);
        }

        while self.current_token != Token::RBracket {
            elements.push(self.parse_expression());
            
            if self.current_token == Token::Comma {
                self.advance();
            } else if self.current_token != Token::RBracket {
                panic!("Expected ',' or ']' in array literal");
            }
        }

        self.expect(Token::RBracket);
        Expr::Array(elements)
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
        self.advance(); // consume 'if'

        // Check for Block If: if { ... }
        if self.current_token == Token::LBrace {
             self.advance(); // consume '{'
             
             // First branch must have a condition
             if self.current_token == Token::Newline { self.advance(); }
             
             self.expect(Token::LParen);
             let condition = self.parse_expression();
             self.expect(Token::RParen);
             
             // Parse statements until we hit ':' or '}'
             let mut then_block = Vec::new();
             loop {
                 while self.current_token == Token::Newline { self.advance(); }
                 if self.current_token == Token::Colon || self.current_token == Token::RBrace || self.current_token == Token::EOF {
                     break;
                 }
                 then_block.push(self.parse_statement());
             }
             
             let mut else_branch = None;
             
             if self.current_token == Token::Colon {
                 self.advance(); // consume ':'
                 while self.current_token == Token::Newline { self.advance(); }
                 
                 // Check if it's an 'else if' or 'else'
                 if self.current_token == Token::LParen {
                     else_branch = Some(Box::new(self.parse_if_inner()));
                 } else {
                     // Final 'else' block
                     let mut else_stmts = Vec::new();
                     loop {
                         while self.current_token == Token::Newline { self.advance(); }
                         if self.current_token == Token::RBrace || self.current_token == Token::EOF {
                             break;
                         }
                         else_stmts.push(self.parse_statement());
                     }
                     else_branch = Some(Box::new(Statement::Block(else_stmts)));
                 }
             }
             
             if self.current_token == Token::RBrace {
                 self.advance();
             }
             
             Statement::If {
                 condition,
                 then_branch: Box::new(Statement::Block(then_block)),
                 else_branch,
             }

        } else {
            // Inline If: if (cond) stmt : ...
            self.expect(Token::LParen);
            let condition = self.parse_expression();
            self.expect(Token::RParen);
            
            let then_stmt = self.parse_statement();
            let mut else_branch = None;
            
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
            
            Statement::If {
                condition,
                then_branch: Box::new(then_stmt),
                else_branch,
            }
        }
    }

    // Helper for Block If recursion
    fn parse_if_inner(&mut self) -> Statement {
         // Expect condition
         self.expect(Token::LParen);
         let condition = self.parse_expression();
         self.expect(Token::RParen);
         
         let mut then_block = Vec::new();
         loop {
             while self.current_token == Token::Newline { self.advance(); }
             if self.current_token == Token::Colon || self.current_token == Token::RBrace || self.current_token == Token::EOF {
                 break;
             }
             then_block.push(self.parse_statement());
         }
         
         let mut else_branch = None;
         if self.current_token == Token::Colon {
             self.advance();
             while self.current_token == Token::Newline { self.advance(); }
             
             if self.current_token == Token::LParen {
                 else_branch = Some(Box::new(self.parse_if_inner()));
             } else {
                 let mut else_stmts = Vec::new();
                 loop {
                     while self.current_token == Token::Newline { self.advance(); }
                     if self.current_token == Token::RBrace || self.current_token == Token::EOF {
                         break;
                     }
                     else_stmts.push(self.parse_statement());
                 }
                 else_branch = Some(Box::new(Statement::Block(else_stmts)));
             }
         }
         
         Statement::If {
             condition,
             then_branch: Box::new(Statement::Block(then_block)),
             else_branch,
         }
    }

    // Helper for Inline If recursion (implicit if)
    fn parse_implicit_if(&mut self) -> Statement {
        self.expect(Token::LParen);
        let condition = self.parse_expression();
        self.expect(Token::RParen);
        
        let then_stmt = self.parse_statement();
        let mut else_branch = None;
        
        if self.current_token == Token::Colon {
            self.advance(); 
            if self.current_token == Token::LParen {
                 else_branch = Some(Box::new(self.parse_implicit_if()));
            } else {
                else_branch = Some(Box::new(self.parse_statement()));
            }
        }
        
        Statement::If {
            condition,
            then_branch: Box::new(then_stmt),
            else_branch
        }
    }

    fn parse_while_stmt(&mut self) -> Statement {
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
             Statement::Block(self.parse_block())
        } else {
            self.parse_statement()
        };

        Statement::While {
            condition,
            body: Box::new(body),
            is_once,
        }
    }

    fn parse_loop_stmt(&mut self) -> Statement {
        self.advance(); // consume 'loop'
        
        let body = if self.current_token == Token::LBrace {
             Statement::Block(self.parse_block())
        } else {
            self.parse_statement()
        };

        Statement::Loop(Box::new(body))
    }

    fn parse_for_stmt(&mut self) -> Statement {
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
             Statement::Block(self.parse_block())
        } else {
            self.parse_statement()
        };
        
        Statement::For {
            binding,
            index_binding,
            iterable,
            body: Box::new(body),
        }
    }

    fn parse_match_stmt(&mut self) -> Statement {
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
                 Statement::Block(self.parse_block())
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
        
        Statement::Match {
            condition,
            arms,
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
        self.advance(); // consume first '|'

        let mut params = Vec::new();

        if self.current_token != Token::Pipe {
            loop {
                if self.current_token == Token::Pipe { break; }

                let param_name = self.expect_identifier();
                
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

                params.push(Parameter {
                    name: param_name,
                    is_owned,
                    ty,
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

        // Parse body - expect block or expression
        let body_stmt = if self.current_token == Token::LBrace {
             Box::new(Statement::Block(self.parse_block()))
        } else {
             // Single expression implicit return
             let expr = self.parse_expression();
             Box::new(Statement::Return(Some(expr)))
        };

        Expr::Closure {
            params,
            body: body_stmt,
            return_type,
        }
    }

    fn parse_enum_stmt(&mut self) -> Statement {
        if self.in_function {
            panic!("Syntax Error: 'enum' can only be declared in global scope");
        }
        self.advance(); // consume 'enum'
        let name = self.expect_identifier();
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
            
            variants.push(EnumVariantDef { name: variant_name, data_type });
            
            if self.current_token == Token::Comma {
                self.advance();
            }
            self.consume_newlines();
        }
        self.expect(Token::RBrace);
        
        Statement::Enum(EnumDef { name, variants })
    }
    fn parse_model_stmt(&mut self) -> Statement {
        if self.in_function {
            panic!("Syntax Error: 'model' can only be declared in global scope");
        }
        self.advance(); // consume 'model'
        let name = self.expect_identifier();
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
        Statement::Model(ModelDef { name, fields })
    }

    fn parse_role_stmt(&mut self) -> Statement {
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
        Statement::Role(RoleDef { name, methods })
    }

    fn parse_extend_stmt(&mut self) -> Statement {
        self.advance(); // consume 'extend'
        
        let target = self.parse_type();
        
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
        
        Statement::Extend { target, role, methods }
    }
}
