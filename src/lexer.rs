#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    Var, Const, Static, Fn,
    If, Else, Match, While, Until,

	// Built-ins
	Print,
    
    // Identifiers and Literals
    Identifier(String),
    Number(i64),
    StringLiteral(String),
    
    // Symbols
    Equal, Plus, Minus, Star, Slash,
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Colon, Comma, LArrow,
	LessThan,
    
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
        match ch {
            '+' => Token::Plus,
            '-' => Token::Minus,
			'*' => Token::Star,
			'/' => Token::Slash,
            '=' => Token::Equal,
            '(' => Token::LParen,
            ')' => Token::RParen,
			'{' => Token::LBrace,
            '}' => Token::RBrace,
            _ => panic!("Unknown character: {}<", ch),
        }
    }

	fn read_identifier(&mut self) -> Token {
        let mut ident = String::new();
        while self.cursor < self.source.len() && self.source[self.cursor].is_alphanumeric() {
            ident.push(self.source[self.cursor]);
            self.cursor += 1;
        }
        match ident.as_str() {
            "var" => Token::Var,
            "const" => Token::Const,
			"fn" => Token::Fn,
			"print" => Token::Print,
            _ => Token::Identifier(ident),
        }
    }

    fn read_number(&mut self) -> Token {
        let mut num_str = String::new();
        while self.cursor < self.source.len() && self.source[self.cursor].is_ascii_digit() {
            num_str.push(self.source[self.cursor]);
            self.cursor += 1;
        }
        Token::Number(num_str.parse().unwrap())
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
