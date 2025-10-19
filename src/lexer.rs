impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Var, Lit, If, While, Until, Match, Echo, Print, Break, Next, True, False, End,
    StarStar, EqEq, NotEq, LessEq, GreaterEq, OrOr,
    Plus, Minus, Star, Slash, Percent, Caret, Eq, Less, Greater, Colon, Comma,
    LParen, RParen, LBracket, RBracket, LBrace, RBrace,
    Ident(String), String(String), Number(f64),
}

pub fn tokenize(input: &str) -> Vec<(usize, Token, usize)> {
    let mut tokens = Vec::new();
    let mut chars = input.char_indices().peekable();
    
    while let Some((pos, ch)) = chars.next() {
        match ch {
            ' ' | '\t' | '\n' | '\r' => continue,
            
            '*' => {
                if chars.peek().map(|(_, c)| *c) == Some('*') {
                    chars.next();
                    tokens.push((pos, Token::StarStar, pos + 2));
                } else {
                    tokens.push((pos, Token::Star, pos + 1));
                }
            }
            
            '=' => {
                if chars.peek().map(|(_, c)| *c) == Some('=') {
                    chars.next();
                    tokens.push((pos, Token::EqEq, pos + 2));
                } else {
                    tokens.push((pos, Token::Eq, pos + 1));
                }
            }
            
            '!' => {
                if chars.peek().map(|(_, c)| *c) == Some('=') {
                    chars.next();
                    tokens.push((pos, Token::NotEq, pos + 2));
                } else {
                    panic!("Unexpected character: !");
                }
            }
            
            '<' => {
                if chars.peek().map(|(_, c)| *c) == Some('=') {
                    chars.next();
                    tokens.push((pos, Token::LessEq, pos + 2));
                } else {
                    tokens.push((pos, Token::Less, pos + 1));
                }
            }
            
            '>' => {
                if chars.peek().map(|(_, c)| *c) == Some('=') {
                    chars.next();
                    tokens.push((pos, Token::GreaterEq, pos + 2));
                } else {
                    tokens.push((pos, Token::Greater, pos + 1));
                }
            }
            
            '|' => {
                if chars.peek().map(|(_, c)| *c) == Some('|') {
                    chars.next();
                    tokens.push((pos, Token::OrOr, pos + 2));
                } else {
                    panic!("Unexpected character: |");
                }
            }
            
            '+' => tokens.push((pos, Token::Plus, pos + 1)),
            '-' => tokens.push((pos, Token::Minus, pos + 1)),
            '/' => tokens.push((pos, Token::Slash, pos + 1)),
            '%' => tokens.push((pos, Token::Percent, pos + 1)),
            '^' => tokens.push((pos, Token::Caret, pos + 1)),
            ':' => tokens.push((pos, Token::Colon, pos + 1)),
            ',' => tokens.push((pos, Token::Comma, pos + 1)),
            '(' => tokens.push((pos, Token::LParen, pos + 1)),
            ')' => tokens.push((pos, Token::RParen, pos + 1)),
            '[' => tokens.push((pos, Token::LBracket, pos + 1)),
            ']' => tokens.push((pos, Token::RBracket, pos + 1)),
            '{' => tokens.push((pos, Token::LBrace, pos + 1)),
            '}' => tokens.push((pos, Token::RBrace, pos + 1)),
            
            '"' => {
                let start = pos;
                let mut string = String::new();
                while let Some((_, c)) = chars.peek() {
                    if *c == '"' {
                        chars.next();
                        break;
                    }
                    string.push(*c);
                    chars.next();
                }
                let end = chars.peek().map(|(i, _)| *i).unwrap_or(input.len());
                tokens.push((start, Token::String(string), end));
            }
            
            c if c.is_alphabetic() || c == '_' => {
                let start = pos;
                let mut ident = String::from(c);
                while let Some((_, c)) = chars.peek() {
                    if c.is_alphanumeric() || *c == '_' {
                        ident.push(*c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                let end = chars.peek().map(|(i, _)| *i).unwrap_or(input.len());
                let token = match ident.as_str() {
                    "var" => Token::Var,
                    "lit" => Token::Lit,
                    "if" => Token::If,
                    "while" => Token::While,
                    "until" => Token::Until,
                    "match" => Token::Match,
                    "echo" => Token::Echo,
                    "print" => Token::Print,
                    "break" => Token::Break,
                    "next" => Token::Next,
                    "true" => Token::True,
                    "false" => Token::False,
                    "end" => Token::End,
                    _ => Token::Ident(ident),
                };
                tokens.push((start, token, end));
            }
            
            c if c.is_numeric() => {
                let start = pos;
                let mut num_str = String::from(c);
                while let Some((_, c)) = chars.peek() {
                    if c.is_numeric() || *c == '.' {
                        num_str.push(*c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                let end = chars.peek().map(|(i, _)| *i).unwrap_or(input.len());
                let num: f64 = num_str.parse().unwrap();
                tokens.push((start, Token::Number(num), end));
            }
            
            c => panic!("Unexpected character: {}", c),
        }
    }
    
    tokens
}
