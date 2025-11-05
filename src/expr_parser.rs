use crate::ast::Expr;
use crate::lexer::Token;
use std::iter::Peekable;
use std::vec::IntoIter;

pub struct ExprParser {
    tokens: Peekable<IntoIter<(usize, Token, usize)>>,
}

impl ExprParser {
    pub fn new(tokens: Vec<(usize, Token, usize)>) -> Self {
        ExprParser {
            tokens: tokens.into_iter().peekable(),
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
            Some(Token::Number(n)) => {
                self.advance();
                Ok(Expr::Number(n))
            }
            Some(Token::String(s)) => {
                self.advance();
                // Handle interpolation
                if s.contains('{') {
                    let parts = crate::parse_interpolated_string(&s)?;
                    Ok(Expr::InterpolatedString(parts))
                } else {
                    Ok(Expr::String(s))
                }
            }
            Some(Token::True) => {
                self.advance();
                Ok(Expr::Bool(true))
            }
            Some(Token::False) => {
                self.advance();
                Ok(Expr::Bool(false))
            }
            Some(Token::Ident(name)) => {
                self.advance();
                match self.current_token() {
                    // Function call
                    Some(Token::LParen) => {
                        self.advance();
                        let args = self.parse_fn_args()?;
                        self.expect(Token::RParen)?;
                        Ok(Expr::Call { name, args })
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
                        Ok(Expr::ModelInstance { name, fields })
                    }
                    // Bare identifier
                    _ => Ok(Expr::Var(name)),
                }
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
                    self.expect(Token::Colon)?;
                    let expr = self.parse_assignment()?;
                    fields.push((name, expr));
                    
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
