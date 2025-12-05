use crate::ast::{EnumDef, Expr, InterpolationPart};
use crate::lexer::{InterpolationToken, Token};
use crate::utils::{infer_enum_name, parse_type};
use std::collections::HashMap;
use std::iter::Peekable;
use std::vec::IntoIter;

pub struct ExprParser {
    tokens: Peekable<IntoIter<(usize, Token, usize)>>,
    enums: HashMap<String, EnumDef>,
}

impl ExprParser {
    pub fn new(tokens: Vec<(usize, Token, usize)>, enums: &HashMap<String, EnumDef>) -> Self {
        ExprParser {
            tokens: tokens.into_iter().peekable(),
            enums: enums.clone(),
        }
    }

    pub fn parse(&mut self) -> Result<Expr, String> {
        self.parse_assignment()
    }

    fn current_token(&mut self) -> Option<Token> {
        self.tokens.peek().map(|(_, token, _)| token.clone())
    }

    fn advance(&mut self) -> Option<(usize, Token, usize)> {
        self.tokens.next()
    }

    fn skip_newlines(&mut self) {
        while matches!(self.current_token(), Some(Token::Newline)) {
            self.advance();
        }
    }

    fn can_start_expression(&mut self) -> bool {
        matches!(
            self.current_token(),
            Some(Token::Ident(_)) | Some(Token::String(_))
        )
    }

    // Precedence levels (lowest to highest)
    fn parse_assignment(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_comparison()?;

        loop {
            match self.current_token() {
                Some(Token::Eq) => {
                    // Simple assignment
                    if let Expr::Var(name) = expr {
                        self.advance();
                        let value = self.parse_assignment()?;
                        expr = Expr::Assign {
                            name,
                            value: Box::new(value),
                        };
                    } else {
                        return Err("Cannot assign to non-variable".to_string());
                    }
                }
                Some(Token::LArrow) => {
                    self.advance();
                    let value = self.parse_assignment()?;
                    // This treats 'chan <- val' as an expression returning Void
                    expr = Expr::Send(Box::new(expr), Box::new(value));
                }
                Some(op_token) if self.is_compound_op(&op_token) => {
                    // Compound assignment (+=, -=, etc.)
                    if let Expr::Var(name) = expr {
                        let op = self.token_to_compound_op(&op_token)?;
                        self.advance();
                        let value = self.parse_assignment()?;
                        let expanded = match op.as_str() {
                            "+=" => Expr::Add(
                                Box::new(Expr::Var(name.clone())),
                                Box::new(value),
                            ),
                            "-=" => Expr::Subtract(
                                Box::new(Expr::Var(name.clone())),
                                Box::new(value),
                            ),
                            "*=" => Expr::Multiply(
                                Box::new(Expr::Var(name.clone())),
                                Box::new(value),
                            ),
                            "/=" => Expr::Divide(
                                Box::new(Expr::Var(name.clone())),
                                Box::new(value),
                            ),
                            "%=" => Expr::Modulo(
                                Box::new(Expr::Var(name.clone())),
                                Box::new(value),
                            ),
                            "**=" | "^=" => Expr::Power(
                                Box::new(Expr::Var(name.clone())),
                                Box::new(value),
                            ),
                            _ => unreachable!(),
                        };
                        expr = Expr::CompoundAssign {
                            name,
                            op,
                            value: Box::new(expanded),
                        };
                    } else {
                        return Err("Cannot compound-assign to non-variable".to_string());
                    }
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_addition()?;

        while let Some(op_token) = self.current_token() {
            let op = match op_token {
                Token::EqEq => "==",
                Token::NotEq => "!=",
                Token::Less => "<",
                Token::LessEq => "<=",
                Token::Greater => ">",
                Token::GreaterEq => ">=",
                _ => break,
            };

            self.advance();
            let right = self.parse_addition()?;
            expr = match op {
                "==" => Expr::Equal(Box::new(expr), Box::new(right)),
                "!=" => Expr::NotEqual(Box::new(expr), Box::new(right)),
                "<" => Expr::LessThan(Box::new(expr), Box::new(right)),
                "<=" => Expr::LessThanOrEqual(Box::new(expr), Box::new(right)),
                ">" => Expr::GreaterThan(Box::new(expr), Box::new(right)),
                ">=" => Expr::GreaterThanOrEqual(Box::new(expr), Box::new(right)),
                _ => unreachable!(),
            };
        }

        Ok(expr)
    }

    fn parse_addition(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_multiplication()?;

        while let Some(op_token) = self.current_token() {
            let op = match op_token {
                Token::Plus => "+",
                Token::Minus => "-",
                _ => break,
            };

            self.advance();
            // Skip newlines after operator
            while matches!(self.current_token(), Some(Token::Newline)) {
                self.advance();
            }
            let right = self.parse_multiplication()?;
            expr = match op {
                "+" => Expr::Add(Box::new(expr), Box::new(right)),
                "-" => Expr::Subtract(Box::new(expr), Box::new(right)),
                _ => unreachable!(),
            };
        }

        Ok(expr)
    }

    fn parse_multiplication(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_unary()?;

        while let Some(op_token) = self.current_token() {
            let op = match op_token {
                Token::Star => "*",
                Token::Slash => "/",
                Token::Percent => "%",
                _ => break,
            };

            self.advance();
            // Skip newlines after operator
            while matches!(self.current_token(), Some(Token::Newline)) {
                self.advance();
            }
            let right = self.parse_unary()?;
            expr = match op {
                "*" => Expr::Multiply(Box::new(expr), Box::new(right)),
                "/" => Expr::Divide(Box::new(expr), Box::new(right)),
                "%" => Expr::Modulo(Box::new(expr), Box::new(right)),
                _ => unreachable!(),
            };
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, String> {
        match self.current_token() {
            Some(Token::Not) => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr::Not(Box::new(expr)))
            }
            Some(Token::Minus) => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr::Negative(Box::new(expr)))
            }
            Some(Token::Plus) => {
                self.advance();
                self.parse_unary()
            }
            Some(Token::LArrow) => {
                self.advance();
                let first_expr = self.parse_unary()?;
                
                // Check if next token can start an expression (meaning there's a channel after the count)
                if self.can_start_expression() {
                    let channel_expr = self.parse_unary()?;
                    // first_expr is count, channel_expr is channel
                    Ok(Expr::CountReceive(Box::new(channel_expr), Box::new(first_expr)))
                } else {
                    // first_expr is the channel (no count)
                    Ok(Expr::Receive(Box::new(first_expr)))
                }
            }
            Some(Token::LArrowMaybe) => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr::MaybeReceive(Box::new(expr)))
            }
            _ => self.parse_power(),
        }
    }

    fn parse_power(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_postfix()?;

        while matches!(
            self.current_token(),
            Some(Token::StarStar) | Some(Token::Caret)
        ) {
            self.advance();
            // Skip newlines after operator
            while matches!(self.current_token(), Some(Token::Newline)) {
                self.advance();
            }
            let right = self.parse_postfix()?;
            expr = Expr::Power(Box::new(expr), Box::new(right));
        }

        Ok(expr)
    }

    fn parse_postfix(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_primary()?;

        loop {
            match self.current_token() {
                Some(Token::Period) => {
                    self.advance();
                    match self.current_token() {
                        Some(Token::Ident(field_name)) => {
                            self.advance();
                            // Check if it's a method call
                            if matches!(self.current_token(), Some(Token::LParen)) {
                                self.advance();
                                let args = self.parse_fn_args()?;
                                self.expect(Token::RParen)?;
                                expr = Expr::MethodCall {
                                    object: Box::new(expr),
                                    method: field_name,
                                    args,
                                };
                            } else {
                                // Field access
                                expr = Expr::FieldAccess {
                                    object: Box::new(expr),
                                    field: field_name,
                                    value: None,
                                };
                            }
                        }
                        _ => return Err("Expected identifier after '.'".to_string()),
                    }
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        match self.current_token() {
            Some(Token::Integer(n)) => {
                self.advance();
                Ok(Expr::I64(n))
            }
            Some(Token::Float(f)) => {
                self.advance();
                Ok(Expr::F64(f))
            }
            Some(Token::String(s)) => {
                self.advance();
                Ok(Expr::String(s))
            }
            Some(Token::InterpolatedString(parts)) => {
                self.advance();
                let expr_parts: Vec<_> = parts.iter().map(|p| {
                    match p {
                        InterpolationToken::Literal(s) => InterpolationPart::Literal(s.clone()),
                        InterpolationToken::Variable(name) => {
                            // Try to parse as expression first (handles "xx + 1")
                            match Self::parse_expression_from_string(name, &self.enums) {
                                Ok(expr) => InterpolationPart::Expression(Box::new(expr)),
                                Err(_) => {
                                    // Fall back to simple variable
                                    InterpolationPart::Expression(Box::new(Expr::Var(name.clone())))
                                }
                            }
                        }
                    }
                }).collect();
                Ok(Expr::InterpolatedString(expr_parts))
            }
            Some(Token::True) => {
                self.advance();
                Ok(Expr::Bool(true))
            }
            Some(Token::False) => {
                self.advance();
                Ok(Expr::Bool(false))
            }
            Some(Token::Channel) => {
                self.advance();
                if matches!(self.current_token(), Some(Token::LParen)) {
                    self.advance(); // (
                    self.expect(Token::RParen)?; // )
                    
                    let type_hint = if matches!(self.current_token(), Some(Token::Colon)) {
                        self.advance();
                        let (ty, _ownership) = parse_type(&mut self.tokens)?;
                        Some(ty)
                    } else {
                        None
                    };
                    return Ok(Expr::MakeChannel(type_hint));
                }
                // Used as identifier (e.g., variable named channel)
                Ok(Expr::Var("channel".to_string()))
            }
            Some(Token::Thread) => {
                self.advance();
                if matches!(self.current_token(), Some(Token::LParen)) {
                    self.advance();
                    let args = self.parse_fn_args()?;
                    self.expect(Token::RParen)?;
                    
                    if args.len() == 1 {
                        return Ok(Expr::Thread(Box::new(args[0].clone())));
                    }
                    return Ok(Expr::Call { name: "thread".to_string(), args });
                }
                Ok(Expr::Var("thread".to_string()))
            }
            Some(Token::Sleep) => {
                self.advance();
                if matches!(self.current_token(), Some(Token::LParen)) {
                    self.advance();
                    let args = self.parse_fn_args()?;
                    self.expect(Token::RParen)?;
                    return Ok(Expr::Call { name: "sleep".to_string(), args });
                }
                Ok(Expr::Var("sleep".to_string()))
            }
            Some(Token::Map) => {
                self.advance();

                if !matches!(self.current_token(), Some(Token::Less)) {
                    return Err(
                        "Map declaration requires explicit types: map<KeyType, ValueType>()\n\
                        Example: var m = map<string, i64>([\"a\": 1])"
                            .to_string()
                    );
                }
                
                // Expect < for generics
                self.expect(Token::Less)?;
                let key_type = parse_type(&mut self.tokens)?;
                self.expect(Token::Comma)?;
                let val_type = parse_type(&mut self.tokens)?;
                self.expect(Token::Greater)?;
                
                // Expect ()
                self.expect(Token::LParen)?;
                
                // Parse optional initial pairs: map<K, V>(["key": val, ...])
                let pairs = if !matches!(self.current_token(), Some(Token::RParen)) {
                    // Expect an array: ["key": val, "key2": val2, ...]
                    self.expect(Token::LBracket)?;
                    
                    let mut pairs = Vec::new();
                    
                    while !matches!(self.current_token(), Some(Token::RBracket)) {
                        let key_expr = self.parse_assignment()?;
                        self.expect(Token::Colon)?;
                        let val_expr = self.parse_assignment()?;
                        
                        pairs.push((Box::new(key_expr), Box::new(val_expr)));
                        
                        if matches!(self.current_token(), Some(Token::Comma)) {
                            self.advance();
                            self.skip_newlines();
                        } else {
                            break;
                        }
                    }
                    
                    self.expect(Token::RBracket)?;
                    pairs
                } else {
                    vec![]
                };
                
                self.expect(Token::RParen)?;
                
                // only pass Type (0) for types, not Ownership
                Ok(Expr::CreateMap(pairs, key_type.0, val_type.0))
            }
            Some(Token::Ident(name)) => {
                if name == "state" {
                    self.advance();
                    if matches!(self.current_token(), Some(Token::LParen)) {
                        self.advance(); // (
                        let initial_value = self.parse_assignment()?;
                        self.expect(Token::RParen)?;
                        return Ok(Expr::MakeState(Box::new(initial_value)));
                    }
                }
                
                self.advance();

                // Check if it's an enum variant (capitalized, or special like Ok, Error, Null, Value)
                if self.is_enum_variant(&name) {
                    if matches!(self.current_token(), Some(Token::LParen)) {
                        self.advance();
                        let data = self.parse_assignment()?;
                        self.expect(Token::RParen)?;
                        return Ok(Expr::EnumVariant {
                            enum_name: infer_enum_name(&name, &self.enums),  // Helper to determine enum
                            variant: name,
                            data: Some(Box::new(data)),
                        });
                    } else {
                        // Unit variant like Null
                        return Ok(Expr::EnumVariant {
                            enum_name: infer_enum_name(&name, &self.enums),
                            variant: name,
                            data: None,
                        });
                    }
                }

                match self.current_token() {
                    // Function call
                    Some(Token::LParen) => {
                        self.advance();
                        let args = self.parse_fn_args()?;
                        self.expect(Token::RParen)?;
                        return Ok(Expr::Call { name, args })
                    }
                    // Model instance
                    Some(Token::LBrace) => {
                        self.advance();
                        // Skip newlines
                        while matches!(self.current_token(), Some(Token::Newline)) {
                            self.advance();
                        }
                        let fields = self.parse_model_fields()?;
                        self.expect(Token::RBrace)?;
                        return Ok(Expr::ModelInstance { name, fields })
                    }
                    // Bare identifier
                    _ => {},
                }

                Ok(Expr::Var(name))
            }
            Some(Token::LBracket) => {
                self.advance();
                self.parse_array()
            }
            Some(Token::LParen) => {
                self.advance();
                // Skip newlines
                while matches!(self.current_token(), Some(Token::Newline)) {
                    self.advance();
                }
                let expr = self.parse_assignment()?;
                // Skip newlines
                while matches!(self.current_token(), Some(Token::Newline)) {
                    self.advance();
                }
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            Some(Token::Regex(pattern)) => {
                self.advance();
                Ok(Expr::Regex(pattern))
            }
            _ => Err(format!(
                "Unexpected token in expression: {:?}",
                self.current_token()
            )),
        }
    }

    fn parse_array(&mut self) -> Result<Expr, String> {
        match self.current_token() {
            Some(Token::RBracket) => {
                self.advance();
                Ok(Expr::Array(vec![]))
            }
            _ => {
                let mut elements = vec![self.parse_assignment()?];
                while matches!(self.current_token(), Some(Token::Comma)) {
                    self.advance();
                    if matches!(self.current_token(), Some(Token::RBracket)) {
                        break;
                    }
                    elements.push(self.parse_assignment()?);
                }
                self.expect(Token::RBracket)?;
                Ok(Expr::Array(elements))
            }
        }
    }

    fn parse_fn_args(&mut self) -> Result<Vec<Expr>, String> {
        let mut args = vec![];
        
        // Skip newlines
        while matches!(self.current_token(), Some(Token::Newline)) {
            self.advance();
        }

        if !matches!(self.current_token(), Some(Token::RParen)) {
            args.push(self.parse_assignment()?);
            while matches!(self.current_token(), Some(Token::Comma)) {
                self.advance();
                // Skip newlines
                while matches!(self.current_token(), Some(Token::Newline)) {
                    self.advance();
                }
                if matches!(self.current_token(), Some(Token::RParen)) {
                    break;
                }
                args.push(self.parse_assignment()?);
                // Skip newlines
                while matches!(self.current_token(), Some(Token::Newline)) {
                    self.advance();
                }
            }
        }

        Ok(args)
    }

    fn parse_model_fields(
        &mut self,
    ) -> Result<Vec<(String, Expr)>, String> {
        let mut fields = vec![];
        
        loop {
            match self.current_token() {
                Some(Token::Ident(name)) => {
                    self.advance();

                    // Check if it's shorthand (no colon) or explicit (with colon)
                    if matches!(self.current_token(), Some(Token::Colon)) {
                        self.advance();
                        let expr = self.parse_assignment()?;
                        fields.push((name, expr));
                    } else {
                        // Shorthand: field name only, becomes Var(name)
                        fields.push((name.clone(), Expr::Var(name)));
                    }
                    
                    // Optional comma
                    if matches!(self.current_token(), Some(Token::Comma)) {
                        self.advance();
                    }
                    
                    // Skip newlines
                    while matches!(self.current_token(), Some(Token::Newline)) {
                        self.advance();
                    }
                    
                    if matches!(self.current_token(), Some(Token::RBrace)) {
                        break;
                    }
                }
                _ => break,
            }
        }

        Ok(fields)
    }

    fn parse_expression_from_string(expr_str: &str, enums: &HashMap<String, EnumDef>) -> Result<Expr, String> {
        let tokens = crate::lexer::tokenize(expr_str);
        
        let mut parser = ExprParser::new(tokens, enums);
        parser.parse()
            .map_err(|e| format!("Failed to parse interpolated expression: {}", e))
    }

    fn is_enum_variant(&self, name: &str) -> bool {
        // Check both built-in and custom enums
        if matches!(name, "Ok" | "Error" | "Value" | "Null") {
            return true;
        }
        // Check custom enums
        for enum_def in self.enums.values() {
            if enum_def.variants.iter().any(|v| v.name == *name) {
                return true;
            }
        }
        false
    }

    fn expect(&mut self, expected: Token) -> Result<(), String> {
        match self.current_token() {
            Some(token) if std::mem::discriminant(&token) == std::mem::discriminant(&expected) => {
                self.advance();
                Ok(())
            }
            other => Err(format!("Expected {:?}, got {:?}", expected, other)),
        }
    }

    fn is_compound_op(&self, token: &Token) -> bool {
        matches!(
            token,
            Token::PlusEq
                | Token::MinusEq
                | Token::StarEq
                | Token::SlashEq
                | Token::PercentEq
                | Token::StarStarEq
                | Token::CaretEq
        )
    }

    fn token_to_compound_op(&self, token: &Token) -> Result<String, String> {
        match token {
            Token::PlusEq => Ok("+=".to_string()),
            Token::MinusEq => Ok("-=".to_string()),
            Token::StarEq => Ok("*=".to_string()),
            Token::SlashEq => Ok("/=".to_string()),
            Token::PercentEq => Ok("%=".to_string()),
            Token::StarStarEq => Ok("**=".to_string()),
            Token::CaretEq => Ok("^=".to_string()),
            _ => Err("Not a compound operator".to_string()),
        }
    }
}
