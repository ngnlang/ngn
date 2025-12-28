#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    Var, Const, Static, Fn,
    If, Match, While, Until, Loop, Break, Once,
    Import, From, As,

	// Built-ins
	Print,
    
    // Identifiers and Literals
    Identifier(String),
    Number(i64),
    Float(f64),
    StringLiteral(String),
	Bool(bool),
    
    // Symbols
    Equal, EqualEqual, NotEqual, Plus, Minus, Star, Slash,
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Colon, DoubleColon, Comma, LArrow,
	LessThan, GreaterThan,
    
    // Formatting
    Newline,
    EOF,
}

pub struct Lexer {
    source: Vec<char>,
    cursor: usize,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            cursor: 0,
        }
    }

    pub fn next_token(&mut self) -> Token {
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
            return self.read_string();
        }

        self.cursor += 1;
        let token = match ch {
            '+' => Token::Plus,
            '-' => Token::Minus,
			'*' => Token::Star,
			'/' => {
                let next_char = self.peek_current();
                if next_char == '/' {
                    // Single line comment
                    while self.cursor < self.source.len() && self.source[self.cursor] != '\n' {
                        self.cursor += 1;
                    }
                    // Recursively get next token after comment
                    // Note: This effectively skips the comment
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
                } else {
                    Token::Slash
                }
            }
            '(' => Token::LParen,
            ')' => Token::RParen,
			'{' => Token::LBrace,
            '}' => Token::RBrace,
			':' => Token::Colon,
			'=' => {
				if self.peek_current() == '=' {
					self.cursor += 1;
					Token::EqualEqual
				} else {
					Token::Equal
				}
			}
			'!' => {
				if self.peek_current() == '=' {
					self.cursor += 1;
					Token::NotEqual
				} else {
					panic!("Unknown character '!'");
				}
			}
            '<' => Token::LessThan,
            '>' => Token::GreaterThan,
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            ',' => Token::Comma,
            _ => panic!("Unknown character: {}", ch),
        };

		return token;
    }

	// Look at the next character without advancing the cursor
    fn peek(&self) -> char {
        if self.cursor + 1 >= self.source.len() {
            '\0' // Return a "null" character at the end of the file
        } else {
            self.source[self.cursor + 1]
        }
    }
    
    // Sometimes you might need to peek at the current character 
    // if your cursor has already been moved forward
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
            "if" => Token::If,
            "while" => Token::While,
            "once" => Token::Once,
            "loop" => Token::Loop,
            "break" => Token::Break,
            "until" => Token::Until,
			"fn" => Token::Fn,
			"print" => Token::Print,
			"import" => Token::Import,
			"from" => Token::From,
			"as" => Token::As,
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
                    // Already found a dot, so this second dot terminates the number
                    // e.g. 1.2.3 -> 1.2 and .3 next
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

	fn read_string(&mut self) -> Token {
        self.cursor += 1; // skip opening "
        let mut string = String::new();
        while self.cursor < self.source.len() && self.source[self.cursor] != '"' {
            string.push(self.source[self.cursor]);
            self.cursor += 1;
        }
        self.cursor += 1; // skip closing "
        Token::StringLiteral(string)
    }

	fn skip_whitespace(&mut self) {
        while self.cursor < self.source.len() && self.source[self.cursor].is_whitespace() {
            self.cursor += 1;
        }
    }
}
