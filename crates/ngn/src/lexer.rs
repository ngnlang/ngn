impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Any, Var, Const, Static,
    If, Not, While, Until, Match, Echo, Print, Break, Next, True, False,
    StarStar, EqEq, NotEq, LessEq, GreaterEq, Or, And,
    Plus, Minus, One, Once, Star, Slash, Percent, Caret, Eq, Less, Greater,
    Colon, Comma, Period,
    PlusEq, MinusEq, StarEq, SlashEq, PercentEq, StarStarEq, CaretEq, OwnedAssign,
    LParen, RParen, LBracket, RBracket, LBrace, RBrace,
    Newline, Comment(String),
    Fn, Return, ShortReturn,
    Ident(String), String(String), Float(f64), Integer(i64),
    Model, Role, Extend, With,
    Regex(String), Enum, InterpolatedString(Vec<InterpolationToken>),
    LArrow, LArrowMaybe,
    Thread, Channel, Sleep,
    Import, Export, Default, From, As,
    Map, Set,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InterpolationToken {
    Literal(String),
    Variable(String),
}

pub fn tokenize(input: &str) -> Vec<(usize, Token, usize)> {
    let mut tokens = Vec::new();
    let mut chars = input.char_indices().peekable();
    
    while let Some((pos, ch)) = chars.next() {
        match ch {
            ' ' | '\t' => continue,

            '\n' | '\r' => {
                tokens.push((pos, Token::Newline, pos + 1));
                continue;
            }

            '/' => {
                // Check for comment first
                if chars.peek().map(|(_, c)| *c) == Some('/') {
                    let start = pos;  // Start at first /
                    chars.next(); // consume second /
                    let mut comment = String::from("//");  // Include the //
                    
                    while let Some((_, c)) = chars.next() {
                        if c == '\n' || c == '\r' {
                            break;
                        }
                        comment.push(c);
                    }
                    
                    let end = chars.peek().map(|(i, _)| *i).unwrap_or(input.len());
                    tokens.push((start, Token::Comment(comment), end));
                    tokens.push((end, Token::Newline, end + 1));
                    continue;
                }

                // Not a comment, check if it's regex or division
                let is_regex = tokens.is_empty() || matches!(
                    tokens.last().map(|(_, t, _)| t),
                    Some(Token::LParen) | Some(Token::Comma) | Some(Token::Eq) 
                    | Some(Token::EqEq) | Some(Token::NotEq) | Some(Token::Less)
                    | Some(Token::LessEq) | Some(Token::Greater) | Some(Token::GreaterEq)
                    | Some(Token::LBracket) | Some(Token::Return) | Some(Token::Colon)
                );
                
                if is_regex {
                    // Parse regex
                    let start = pos;
                    let mut pattern = String::new();
                    let mut escaped = false;
                    
                    while let Some((_, c)) = chars.peek() {
                        if escaped {
                            pattern.push(*c);
                            escaped = false;
                            chars.next();
                        } else if *c == '\\' {
                            pattern.push(*c);
                            escaped = true;
                            chars.next();
                        } else if *c == '/' {
                            chars.next(); // consume closing /
                            
                            // Parse optional flags
                            let mut flags = String::new();
                            while let Some((_, c)) = chars.peek() {
                                if c.is_alphabetic() {
                                    flags.push(*c);
                                    chars.next();
                                } else {
                                    break;
                                }
                            }
                            
                            let full_pattern = if flags.is_empty() {
                                pattern
                            } else {
                                format!("(?{}){}", flags, pattern)
                            };
                            
                            let end = chars.peek().map(|(i, _)| *i).unwrap_or(input.len());
                            tokens.push((start, Token::Regex(full_pattern), end));
                            break;
                        } else {
                            pattern.push(*c);
                            chars.next();
                        }
                    }
                } else {
                    // Parse division or compound assignment
                    if chars.peek().map(|(_, c)| *c) == Some('=') {
                        chars.next();
                        tokens.push((pos, Token::SlashEq, pos + 2));
                    } else {
                        tokens.push((pos, Token::Slash, pos + 1));
                    }
                }
            }
            
            '*' => {
                if chars.peek().map(|(_, c)| *c) == Some('*') {
                    chars.next(); // consume second *
                    if chars.peek().map(|(_, c)| *c) == Some('=') {
                        chars.next(); // consume =
                        tokens.push((pos, Token::StarStarEq, pos + 3));
                    } else {
                        tokens.push((pos, Token::StarStar, pos + 2));
                    }
                } else if chars.peek().map(|(_, c)| *c) == Some('=') {
                    chars.next(); // consume =
                    tokens.push((pos, Token::StarEq, pos + 2));
                } else {
                    tokens.push((pos, Token::Star, pos + 1));
                }
            }
            
            '=' => {
                if chars.peek().map(|(_, c)| *c) == Some('=') {
                    chars.next();
                    tokens.push((pos, Token::EqEq, pos + 2));
                } else if chars.peek().map(|(_, c)| *c) == Some('<') {
                    chars.next();
                    tokens.push((pos, Token::OwnedAssign, pos + 2));
                } else if chars.peek().map(|(_, c)| *c) == Some('>') {
                    chars.next();
                    tokens.push((pos, Token::ShortReturn, pos + 2));
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
                } else if chars.peek().map(|(_, c)| *c) == Some('-') {
                    chars.next();

                    // Check for '?' after '<-'
                    if chars.peek().map(|(_, c)| *c) == Some('?') {
                        chars.next(); // consume '?'
                        tokens.push((pos, Token::LArrowMaybe, pos + 3));
                    } else {
                        // Just emit LArrow - parser handles count expressions
                        tokens.push((pos, Token::LArrow, pos + 2));
                    }
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
                tokens.push((pos, Token::Or, pos + 1));
            }

            '&' => {
                tokens.push((pos, Token::And, pos + 1));
            }
            
            '+' => {
                if chars.peek().map(|(_, c)| *c) == Some('=') {
                    chars.next();
                    tokens.push((pos, Token::PlusEq, pos + 2));
                } else {
                    tokens.push((pos, Token::Plus, pos + 1));
                }
            },
            '-' => {
                if chars.peek().map(|(_, c)| *c) == Some('=') {
                    chars.next();
                    tokens.push((pos, Token::MinusEq, pos + 2));
                } else {
                    tokens.push((pos, Token::Minus, pos + 1));
                }
            },
            '%' => {
                if chars.peek().map(|(_, c)| *c) == Some('=') {
                    chars.next();
                    tokens.push((pos, Token::PercentEq, pos + 2));
                } else {
                    tokens.push((pos, Token::Percent, pos + 1));
                }
            },
            '^' => {
                if chars.peek().map(|(_, c)| *c) == Some('=') {
                    chars.next();
                    tokens.push((pos, Token::CaretEq, pos + 2));
                } else {
                    tokens.push((pos, Token::Caret, pos + 1));
                }
            },
            ':' => tokens.push((pos, Token::Colon, pos + 1)),
            ',' => tokens.push((pos, Token::Comma, pos + 1)),
            '.' => tokens.push((pos, Token::Period, pos + 1)),
            '(' => tokens.push((pos, Token::LParen, pos + 1)),
            ')' => tokens.push((pos, Token::RParen, pos + 1)),
            '[' => tokens.push((pos, Token::LBracket, pos + 1)),
            ']' => tokens.push((pos, Token::RBracket, pos + 1)),
            '{' => tokens.push((pos, Token::LBrace, pos + 1)),
            '}' => tokens.push((pos, Token::RBrace, pos + 1)),
            
            '"' => {
                let start = pos;
                let mut parts = Vec::new();
                let mut current_literal = String::new();
                
                while let Some((_, c)) = chars.next() {
                    if c == '"' {
                        // Push any remaining literal before the closing quote
                        if !current_literal.is_empty() {
                            parts.push(InterpolationToken::Literal(current_literal.clone()));
                            current_literal.clear();
                        } else {
                            parts.push(InterpolationToken::Literal(current_literal.clone()));
                            current_literal.clear();
                        }
                        break;
                    }
                    if c == '{' {
                        // Save literal part if any
                        if !current_literal.is_empty() {
                            parts.push(InterpolationToken::Literal(current_literal.clone()));
                            current_literal.clear();
                        }
                        
                        // Parse variable name
                        let mut var_name = String::new();
                        while let Some((_, c)) = chars.next() {
                            if c == '}' {
                                break;
                            }
                            var_name.push(c);
                        }
                        
                        if !var_name.is_empty() {
                            parts.push(InterpolationToken::Variable(var_name));
                        }
                        continue;
                    } else if c == '\\' {
                        if let Some((_, next_c)) = chars.next() {
                            match next_c {
                                'n' => current_literal.push('\n'),
                                't' => current_literal.push('\t'),
                                'r' => current_literal.push('\r'),
                                '\\' => current_literal.push('\\'),
                                '\'' => current_literal.push('\''),
                                '"' => current_literal.push('"'),
                                _ => current_literal.push(next_c),
                            }
                        }
                    } else {
                        current_literal.push(c);
                    }
                }
                
                // Push final literal if any
                if !current_literal.is_empty() {
                    parts.push(InterpolationToken::Literal(current_literal));
                }
                
                let end = chars.peek().map(|(i, _)| *i).unwrap_or(input.len());

                // Decide which token to push
                if parts.iter().any(|p| matches!(p, InterpolationToken::Variable(_))) {
                    tokens.push((start, Token::InterpolatedString(parts), end));
                } else {
                    // No variables, treat as regular string
                    let literal = parts.into_iter()
                        .filter_map(|p| match p {
                            InterpolationToken::Literal(s) => Some(s),
                            _ => None,
                        })
                        .collect::<Vec<_>>()
                        .join("");
                    tokens.push((start, Token::String(literal), end));
                }
            }

            '\'' => {
                let start = pos;
                let mut parts = Vec::new();
                let mut current_literal = String::new();
                
                while let Some((_, c)) = chars.next() {
                    if c == '\'' {
                        // Push any remaining literal before the closing quote
                        if !current_literal.is_empty() {
                            parts.push(InterpolationToken::Literal(current_literal.clone()));
                            current_literal.clear();
                        } else {
                            parts.push(InterpolationToken::Literal(current_literal.clone()));
                            current_literal.clear();
                        }
                        break;
                    }
                    if c == '{' {
                        // Save literal part if any
                        if !current_literal.is_empty() {
                            parts.push(InterpolationToken::Literal(current_literal.clone()));
                            current_literal.clear();
                        }
                        
                        // Parse variable name
                        let mut var_name = String::new();
                        while let Some((_, c)) = chars.next() {
                            if c == '}' {
                                break;
                            }
                            var_name.push(c);
                        }
                        
                        if !var_name.is_empty() {
                            parts.push(InterpolationToken::Variable(var_name));
                        }
                        continue;
                    } else if c == '\\' {
                        if let Some((_, next_c)) = chars.next() {
                            match next_c {
                                'n' => current_literal.push('\n'),
                                't' => current_literal.push('\t'),
                                'r' => current_literal.push('\r'),
                                '\\' => current_literal.push('\\'),
                                '\'' => current_literal.push('\''),
                                '"' => current_literal.push('"'),
                                _ => current_literal.push(next_c),
                            }
                        }
                    } else {
                        current_literal.push(c);
                    }
                }
                
                // Push final literal if any
                if !current_literal.is_empty() {
                    parts.push(InterpolationToken::Literal(current_literal));
                }
                
                let end = chars.peek().map(|(i, _)| *i).unwrap_or(input.len());

                // Decide which token to push
                if parts.iter().any(|p| matches!(p, InterpolationToken::Variable(_))) {
                    tokens.push((start, Token::InterpolatedString(parts), end));
                } else {
                    // No variables, treat as regular string
                    let literal = parts.into_iter()
                        .filter_map(|p| match p {
                            InterpolationToken::Literal(s) => Some(s),
                            _ => None,
                        })
                        .collect::<Vec<_>>()
                        .join("");
                    tokens.push((start, Token::String(literal), end));
                }
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
                    "const" => Token::Const,
                    "static" => Token::Static,
                    "if" => Token::If,
                    "not" => Token::Not,
                    "while" => Token::While,
                    "until" => Token::Until,
                    "match" => Token::Match,
                    "echo" => Token::Echo,
                    "print" => Token::Print,
                    "break" => Token::Break,
                    "next" => Token::Next,
                    "true" => Token::True,
                    "false" => Token::False,
                    "one" => Token::One,
                    "once" => Token::Once,
                    "any" => Token::Any,
                    "fn" => Token::Fn,
                    "return" => Token::Return,
                    "model" => Token::Model,
                    "role" => Token::Role,
                    "extend" => Token::Extend,
                    "with" => Token::With,
                    "enum" => Token::Enum,
                    "thread" => Token::Thread,
                    "channel" => Token::Channel,
                    "sleep" => Token::Sleep,
                    "import" => Token::Import,
                    "export" => Token::Export,
                    "default" => Token::Default,
                    "from" => Token::From,
                    "as" => Token::As,
                    "map" => Token::Map,
                    "set" => Token::Set,
                    _ => Token::Ident(ident),
                };
                tokens.push((start, token, end));
            }
            
            c if c.is_numeric() => {
                let start = pos;
                let mut num_str = String::from(c);
                let mut is_float = false;

                while let Some((_, c)) = chars.peek() {
                    if c.is_numeric() {
                        num_str.push(*c);
                        chars.next();
                    } else if *c == '.' && !is_float {
                        is_float = true;
                        num_str.push(*c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                let end = chars.peek().map(|(i, _)| *i).unwrap_or(input.len());
                if is_float {
                    let num: f64 = num_str.parse().unwrap();
                    tokens.push((start, Token::Float(num), end));
                } else {
                    let num: i64 = num_str.parse().unwrap();
                    tokens.push((start, Token::Integer(num), end));
                }
            }
            
            c => panic!("Unexpected character: {}", c),
        }
    }
    
    tokens
}
