#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    Var, Const, Static, Fn, Return,
    If, While, Until, Loop, For, In, Match, Next, Any, Break, Once,
    Import, From, As, Enum,

	// Built-ins
	Print, Echo, Sleep, Thread, Channel, State,
    
    // Identifiers and Literals
    Identifier(String),
    Number(i64),
    Float(f64),
    StringStart,
    StringEnd,
    StringPart(String),
    InterpolationStart,
    InterpolationEnd,
	Bool(bool),
    
    // Symbols
    Equal, EqualEqual, NotEqual, Plus, Minus, Star, Slash, Power, Percent,
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Colon, Comma, DoubleColon,
	LessThan, GreaterThan, LessThanEqual, GreaterThanEqual,
    PlusEqual, MinusEqual, StarEqual, SlashEqual, PercentEqual, StarStarEqual, CaretEqual,
    FatArrow, LArrow, Pipe, Question, Period,
    
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

pub struct Lexer {
    source: Vec<char>,
    cursor: usize,
    mode_stack: Vec<LexMode>,
    brace_stack: Vec<usize>,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            cursor: 0,
            mode_stack: vec![LexMode::Normal],
            brace_stack: vec![0],
        }
    }

    pub fn next_token(&mut self) -> Token {
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

		if ch == '"' {
            self.cursor += 1;
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
                    self.next_token()
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
                    self.next_token()
                } else if next_char == '=' {
                    self.cursor += 1;
                    Token::SlashEqual
                } else {
                    Token::Slash
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
					panic!("Unknown character '!'");
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
        while self.cursor < self.source.len() && (self.source[self.cursor].is_alphanumeric() || self.source[self.cursor] == '_') {
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
            "any" => Token::Any,
            "next" => Token::Next,
            "break" => Token::Break,
            "until" => Token::Until,
			"fn" => Token::Fn,
			"return" => Token::Return,
			"print" => Token::Print,
            "echo" => Token::Echo,
            "sleep" => Token::Sleep,
			"import" => Token::Import,
			"from" => Token::From,
			"as" => Token::As,
            "thread" => Token::Thread,
            "channel" => Token::Channel,
            "state" => Token::State,
			"enum" => Token::Enum,
			"true" => Token::Bool(true),
    		"false" => Token::Bool(false),
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
        while self.cursor < self.source.len() {
            let ch = self.source[self.cursor];
            match ch {
                '"' => {
                    if !string.is_empty() {
                        return Token::StringPart(string);
                    }
                    self.cursor += 1;
                    self.mode_stack.pop();
                    return Token::StringEnd;
                }
                '{' => {
                    if !string.is_empty() {
                        return Token::StringPart(string);
                    }
                    self.cursor += 1;
                    self.mode_stack.push(LexMode::Normal);
                    self.brace_stack.push(0);
                    return Token::InterpolationStart;
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
                            '{' => string.push('{'),
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
}
