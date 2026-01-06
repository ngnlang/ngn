#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    Var,
    Const,
    Static,
    Fn,
    Return,
    If,
    While,
    Loop,
    For,
    In,
    Match,
    Next,
    Break,
    Once,
    Import,
    From,
    As,
    Enum,
    Export,
    Default,
    Model,
    Role,
    Extend,
    With,
    This,

    // Built-ins
    Print,
    Echo,
    Sleep,
    Thread,
    Channel,
    State,
    Map,
    Set,

    // Identifiers and Literals
    Identifier(String),
    Number(i64),
    Float(f64),
    StringStart,
    StringEnd,
    StringPart(String),
    InterpolationStart,
    InterpolationEnd,
    Regex(String),
    Bool(bool),

    // Symbols
    Equal,
    EqualEqual,
    NotEqual,
    Plus,
    Minus,
    Star,
    Slash,
    Power,
    Percent,
    Bang,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Colon,
    Comma,
    DoubleColon,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    PercentEqual,
    StarStarEqual,
    CaretEqual,
    FatArrow,
    LArrow,
    Pipe,
    Question,
    Period,

    // Formatting
    Newline,
    Underscore,
    EOF,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LexMode {
    Normal,
    String,
}

/// Represents a span of source code with byte offsets
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Span {
    pub start: usize, // byte offset (inclusive)
    pub end: usize,   // byte offset (exclusive)
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

pub struct Lexer {
    source: Vec<char>,
    byte_offsets: Vec<usize>, // byte offset for each char index
    cursor: usize,
    mode_stack: Vec<LexMode>,
    brace_stack: Vec<usize>,
    last_token: Option<Token>,
    string_quote: char, // Tracks which quote started the current string (' or ")
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        // Pre-compute byte offsets for each character
        let chars: Vec<char> = source.chars().collect();
        let mut byte_offsets = Vec::with_capacity(chars.len() + 1);
        let mut offset = 0;
        for ch in &chars {
            byte_offsets.push(offset);
            offset += ch.len_utf8();
        }
        byte_offsets.push(offset); // End offset for EOF

        Self {
            source: chars,
            byte_offsets,
            cursor: 0,
            mode_stack: vec![LexMode::Normal],
            brace_stack: vec![0],
            last_token: None,
            string_quote: '\0',
        }
    }

    /// Returns the current byte offset
    pub fn byte_offset(&self) -> usize {
        self.byte_offsets
            .get(self.cursor)
            .copied()
            .unwrap_or(self.byte_offsets.last().copied().unwrap_or(0))
    }

    /// Returns the next token along with its source span
    pub fn next_token_with_span(&mut self) -> (Token, Span) {
        let start = self.byte_offset();
        let token = self.next_token();
        let end = self.byte_offset();
        (token, Span::new(start, end))
    }

    pub fn next_token(&mut self) -> Token {
        let token = self.next_token_inner();
        // Don't track Newline, comments (not tokens here), or EOF as meaningful "last tokens" for division check?
        // Actually, Newline IS meaningful (term ends).
        // But for regex check " / " -> division.
        // If we have "a / b", prev is generic Identifier.
        // If "return /a/" prev is Return.
        // If "match /a/" prev is Match.
        // EOF doesn't matter.
        if token != Token::EOF {
            self.last_token = Some(token.clone());
        }
        token
    }

    fn next_token_inner(&mut self) -> Token {
        let current_mode = *self.mode_stack.last().unwrap_or(&LexMode::Normal);

        if current_mode == LexMode::String {
            return self.read_string_part();
        }

        while self.cursor < self.source.len() {
            let ch = self.source[self.cursor];
            if ch == ' ' || ch == '\t' || ch == '\r' {
                self.cursor += 1;
            } else {
                break;
            }
        }

        if self.cursor >= self.source.len() {
            return Token::EOF;
        }

        let ch = self.source[self.cursor];

        if ch == '\n' {
            self.cursor += 1;
            return Token::Newline;
        }

        if ch.is_ascii_digit() {
            return self.read_number();
        }

        if ch.is_alphabetic() {
            return self.read_identifier();
        }

        if ch == '"' || ch == '\'' {
            self.cursor += 1;
            self.string_quote = ch; // Track which quote started the string
            self.mode_stack.push(LexMode::String);
            return Token::StringStart;
        }

        self.cursor += 1;
        let token = match ch {
            '+' => {
                if self.peek_current() == '=' {
                    self.cursor += 1;
                    Token::PlusEqual
                } else {
                    Token::Plus
                }
            }
            '-' => {
                if self.peek_current() == '=' {
                    self.cursor += 1;
                    Token::MinusEqual
                } else {
                    Token::Minus
                }
            }
            '*' => {
                if self.peek_current() == '*' {
                    self.cursor += 1;
                    if self.peek_current() == '=' {
                        self.cursor += 1;
                        Token::StarStarEqual
                    } else {
                        Token::Power
                    }
                } else if self.peek_current() == '=' {
                    self.cursor += 1;
                    Token::StarEqual
                } else {
                    Token::Star
                }
            }
            '^' => {
                if self.peek_current() == '=' {
                    self.cursor += 1;
                    Token::CaretEqual
                } else {
                    Token::Power
                }
            }
            '%' => {
                if self.peek_current() == '=' {
                    self.cursor += 1;
                    Token::PercentEqual
                } else {
                    Token::Percent
                }
            }
            '/' => {
                let next_char = self.peek_current();
                if next_char == '/' {
                    // Single line comment
                    while self.cursor < self.source.len() && self.source[self.cursor] != '\n' {
                        self.cursor += 1;
                    }
                    // Recursively get next token after comment
                    self.next_token_inner()
                } else if next_char == '*' {
                    // Multi line comment
                    self.cursor += 2; // skip /*
                    while self.cursor < self.source.len() {
                        if self.source[self.cursor] == '*' && self.peek() == '/' {
                            self.cursor += 2; // skip */
                            break;
                        }
                        self.cursor += 1;
                    }
                    // Recursively get next token after comment
                    self.next_token_inner()
                } else if next_char == '=' {
                    self.cursor += 1;
                    Token::SlashEqual
                } else {
                    // Check for Regex vs Division
                    if self.is_division_start() {
                        Token::Slash
                    } else {
                        self.read_regex()
                    }
                }
            }
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => {
                if self.mode_stack.len() > 1 {
                    if let Some(depth) = self.brace_stack.last_mut() {
                        *depth += 1;
                    }
                }
                Token::LBrace
            }
            '}' => {
                if self.mode_stack.len() > 1 {
                    if let Some(depth) = self.brace_stack.last_mut() {
                        if *depth == 0 {
                            self.mode_stack.pop();
                            self.brace_stack.pop();
                            return Token::InterpolationEnd;
                        }
                        *depth -= 1;
                    }
                }
                Token::RBrace
            }
            ':' => {
                if self.peek_current() == ':' {
                    self.cursor += 1;
                    Token::DoubleColon
                } else {
                    Token::Colon
                }
            }
            '=' => {
                if self.peek_current() == '=' {
                    self.cursor += 1;
                    Token::EqualEqual
                } else if self.peek_current() == '>' {
                    self.cursor += 1;
                    Token::FatArrow
                } else {
                    Token::Equal
                }
            }
            '|' => Token::Pipe,
            '!' => {
                if self.peek_current() == '=' {
                    self.cursor += 1;
                    Token::NotEqual
                } else {
                    Token::Bang
                }
            }
            '<' => {
                if self.peek_current() == '=' {
                    self.cursor += 1;
                    Token::LessThanEqual
                } else if self.peek_current() == '-' {
                    self.cursor += 1;
                    Token::LArrow
                } else {
                    Token::LessThan
                }
            }
            '>' => {
                if self.peek_current() == '=' {
                    self.cursor += 1;
                    Token::GreaterThanEqual
                } else {
                    Token::GreaterThan
                }
            }
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            ',' => Token::Comma,
            '?' => Token::Question,
            '.' => Token::Period,
            '_' => Token::Underscore,
            _ => panic!("Unknown character: {}", ch),
        };

        return token;
    }

    // Look at the next character without advancing the cursor
    fn peek(&self) -> char {
        if self.cursor + 1 >= self.source.len() {
            '\0'
        } else {
            self.source[self.cursor + 1]
        }
    }

    fn peek_current(&self) -> char {
        if self.cursor >= self.source.len() {
            '\0'
        } else {
            self.source[self.cursor]
        }
    }

    fn read_identifier(&mut self) -> Token {
        let mut ident = String::new();
        while self.cursor < self.source.len()
            && (self.source[self.cursor].is_alphanumeric() || self.source[self.cursor] == '_')
        {
            ident.push(self.source[self.cursor]);
            self.cursor += 1;
        }
        match ident.as_str() {
            "var" => Token::Var,
            "const" => Token::Const,
            "static" => Token::Static,
            "if" => Token::If,
            "while" => Token::While,
            "once" => Token::Once,
            "loop" => Token::Loop,
            "for" => Token::For,
            "in" => Token::In,
            "match" => Token::Match,
            "next" => Token::Next,
            "break" => Token::Break,
            "fn" => Token::Fn,
            "return" => Token::Return,
            "print" => Token::Print,
            "echo" => Token::Echo,
            "sleep" => Token::Sleep,
            "import" => Token::Import,
            "export" => Token::Export,
            "default" => Token::Default,
            "from" => Token::From,
            "as" => Token::As,
            "thread" => Token::Thread,
            "channel" => Token::Channel,
            "state" => Token::State,
            "enum" => Token::Enum,
            "model" => Token::Model,
            "role" => Token::Role,
            "extend" => Token::Extend,
            "with" => Token::With,
            "this" => Token::This,
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            "map" => Token::Map,
            "set" => Token::Set,
            _ => Token::Identifier(ident),
        }
    }

    fn read_number(&mut self) -> Token {
        let mut num_str = String::new();
        let mut is_float = false;

        while self.cursor < self.source.len() {
            let ch = self.source[self.cursor];

            if ch.is_ascii_digit() {
                num_str.push(ch);
                self.cursor += 1;
            } else if ch == '.' {
                if is_float {
                    break;
                }
                is_float = true;
                num_str.push(ch);
                self.cursor += 1;
            } else {
                break;
            }
        }

        if is_float {
            Token::Float(num_str.parse().unwrap())
        } else {
            Token::Number(num_str.parse().unwrap())
        }
    }

    fn read_string_part(&mut self) -> Token {
        let mut string = String::new();
        let quote = self.string_quote; // Get the quote that started this string
        while self.cursor < self.source.len() {
            let ch = self.source[self.cursor];
            // Check if this is the matching closing quote
            if ch == quote {
                if !string.is_empty() {
                    return Token::StringPart(string);
                }
                self.cursor += 1;
                self.mode_stack.pop();
                return Token::StringEnd;
            }
            match ch {
                '$' => {
                    // Check if this is ${} interpolation
                    if self.cursor + 1 < self.source.len() && self.source[self.cursor + 1] == '{' {
                        if !string.is_empty() {
                            return Token::StringPart(string);
                        }
                        self.cursor += 2; // consume both $ and {
                        self.mode_stack.push(LexMode::Normal);
                        self.brace_stack.push(0);
                        return Token::InterpolationStart;
                    } else {
                        // Just a regular $ character
                        string.push(ch);
                        self.cursor += 1;
                    }
                }
                '\\' => {
                    self.cursor += 1;
                    if self.cursor < self.source.len() {
                        let next = self.source[self.cursor];
                        match next {
                            'n' => string.push('\n'),
                            't' => string.push('\t'),
                            'r' => string.push('\r'),
                            '\\' => string.push('\\'),
                            '"' => string.push('"'),
                            '\'' => string.push('\''),
                            '$' => string.push('$'), // Escape $ to prevent interpolation
                            _ => string.push(next),
                        }
                        self.cursor += 1;
                    }
                }
                _ => {
                    string.push(ch);
                    self.cursor += 1;
                }
            }
        }
        if !string.is_empty() {
            Token::StringPart(string)
        } else {
            Token::EOF
        }
    }

    fn is_division_start(&self) -> bool {
        match &self.last_token {
            Some(t) => matches!(
                t,
                Token::Identifier(_)
                    | Token::Number(_)
                    | Token::Float(_)
                    | Token::StringEnd
                    | Token::InterpolationEnd
                    | Token::Bool(_)
                    | Token::Regex(_)
                    | Token::RParen
                    | Token::RBracket
                    | Token::RBrace
                    | Token::This
            ),
            None => false,
        }
    }

    fn read_regex(&mut self) -> Token {
        let mut pattern = String::new();
        let mut escaped = false;

        while self.cursor < self.source.len() {
            let ch = self.source[self.cursor];

            if escaped {
                pattern.push(ch);
                escaped = false;
                self.cursor += 1;
            } else if ch == '\\' {
                pattern.push(ch);
                escaped = true;
                self.cursor += 1;
            } else if ch == '/' {
                self.cursor += 1; // Consume closing '/'

                // Parse optional flags
                let mut flags = String::new();
                while self.cursor < self.source.len() {
                    let next = self.source[self.cursor];
                    if next.is_alphabetic() {
                        flags.push(next);
                        self.cursor += 1;
                    } else {
                        break;
                    }
                }

                let full_pattern = if flags.is_empty() {
                    pattern
                } else {
                    format!("(?{}){}", flags, pattern)
                };
                return Token::Regex(full_pattern);
            } else {
                pattern.push(ch);
                self.cursor += 1;
            }
        }
        panic!("Unterminated regex literal");
    }
}
