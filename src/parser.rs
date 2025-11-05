use crate::ast::*;
use crate::lexer::Token;
use std::iter::Peekable;
use std::vec::IntoIter;

pub struct Parser {
    tokens: Peekable<IntoIter<(usize, Token, usize)>>,
}

impl Parser {
    pub fn new(tokens: Vec<(usize, Token, usize)>) -> Self {
        Parser {
            tokens: tokens.into_iter().peekable(),
        }
    }

    pub fn parse_program(&mut self) -> Result<Vec<Stmt>, String> {
        let mut stmts = Vec::new();
        
        // Skip leading newlines
        self.skip_newlines();
        
        while !self.is_at_end() {
            stmts.push(self.parse_statement()?);
            self.skip_newlines();
        }
        
        Ok(stmts)
    }

    fn current_token(&mut self) -> Option<Token> {
        self.tokens.peek().map(|(_, token, _)| token.clone())
    }

    fn advance(&mut self) -> Option<(usize, Token, usize)> {
        self.tokens.next()
    }

    fn is_at_end(&mut self) -> bool {
        self.tokens.peek().is_none()
    }

    fn skip_newlines(&mut self) {
        while matches!(self.current_token(), Some(Token::Newline)) {
            self.advance();
        }
    }

    fn parse_statement(&mut self) -> Result<Stmt, String> {
        self.skip_newlines();

        eprintln!("parse_statement: current token = {:?}", self.current_token());
        
        match self.current_token() {
            Some(Token::Break) => {
                self.advance();
                Ok(Stmt::Break)
            }
            Some(Token::Next) => {
                self.advance();
                Ok(Stmt::Next)
            }
            Some(Token::Echo) => {
                self.advance();
                self.expect(Token::LParen)?;
                let expr = self.parse_expr()?;
                self.expect(Token::RParen)?;
                Ok(Stmt::Echo(expr))
            }
            Some(Token::Print) => {
                self.advance();
                self.expect(Token::LParen)?;
                let expr = self.parse_expr()?;
                self.expect(Token::RParen)?;
                Ok(Stmt::Print(expr))
            }
            Some(Token::Var) => self.parse_assign_stmt(AssignKind::Var),
            Some(Token::Const) => self.parse_assign_stmt(AssignKind::Const),
            Some(Token::Lit) => self.parse_assign_stmt(AssignKind::Lit),
            Some(Token::Static) => self.parse_assign_stmt(AssignKind::Static),
            Some(Token::Rebind) => self.parse_rebind_stmt(),
            Some(Token::If) => self.parse_if_stmt(),
            Some(Token::While) => self.parse_while_stmt(),
            Some(Token::Until) => self.parse_until_stmt(),
            Some(Token::Match) => self.parse_match_stmt(),
            Some(Token::Fn) => self.parse_fn_def(),
            Some(Token::Model) => self.parse_model_def(),
            Some(Token::Role) => self.parse_role_def(),
            Some(Token::Extend) => self.parse_extend_stmt(),
            Some(Token::Return) => self.parse_return_stmt(),
            Some(Token::Ident(_)) => {
                // Could be: assignment, reassignment, or expression statement
                self.parse_ident_statement()
            }
            other => Err(format!("Unexpected token in statement: {:?}", other)),
        }
    }

    fn parse_assign_stmt(&mut self, kind: AssignKind) -> Result<Stmt, String> {
        self.advance(); // consume var/const/lit/static
        
        let name = self.expect_ident()?;
        
        let declared_type = if matches!(self.current_token(), Some(Token::Colon)) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };
        
        let ownership = match self.current_token() {
            Some(Token::Eq) => {
                self.advance();
                Ownership::Borrowed
            }
            Some(Token::LArrow) => {
                self.advance();
                Ownership::Owned
            }
            _ => return Err("Expected '=' or '<-' in assignment".to_string()),
        };
        
        let value = self.parse_expr()?;
        
        Ok(Stmt::Assign {
            kind,
            declared_type,
            name,
            value,
            ownership,
        })
    }

    fn parse_rebind_stmt(&mut self) -> Result<Stmt, String> {
        self.advance(); // consume 'rebind'
        let name = self.expect_ident()?;
        self.expect(Token::Eq)?;
        let value = self.parse_expr()?;
        Ok(Stmt::Rebind { name, value })
    }

    fn parse_ident_statement(&mut self) -> Result<Stmt, String> {
        let name = self.expect_ident()?;
        
        match self.current_token() {
            Some(Token::Eq) => {
                // Reassignment
                self.advance();
                let value = self.parse_expr()?;
                Ok(Stmt::Reassign { name, value })
            }
            Some(Token::LParen) => {
                // Function call
                self.advance();
                let args = self.parse_fn_args()?;
                self.expect(Token::RParen)?;
                Ok(Stmt::ExprStmt(Expr::Call { name, args }))
            }
            Some(Token::Period) => {
                // Method call: name.method(args)
                let obj_expr = Expr::Var(name);
                self.parse_method_chain(obj_expr).map(Stmt::ExprStmt)
            }
            _ => Err(format!(
                "Expected '=', '(', or '.' after identifier, got {:?}",
                self.current_token()
            )),
        }
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, String> {
        self.advance(); // consume 'if'
        
        // Check if it's multiline (if {...}) or inline (if (...))
        if matches!(self.current_token(), Some(Token::LBrace)) {
            // Multiline if
            self.advance(); // consume {
            self.skip_newlines();
            
            // Parse condition
            let negated = if matches!(self.current_token(), Some(Token::Not)) {
                self.advance();
                true
            } else {
                false
            };
            
            self.expect(Token::LParen)?;
            let mut condition = self.parse_expr()?;
            self.expect(Token::RParen)?;
            
            if negated {
                condition = Expr::Not(Box::new(condition));
            }
            
            self.skip_newlines();
            let then_block = self.parse_block()?;
            
            self.skip_newlines();
            
            // Parse else-ifs and else
            let mut else_ifs = Vec::new();
            let mut is_else_block = false;

            while matches!(self.current_token(), Some(Token::Colon)) {
                self.advance();
                self.skip_newlines();

                let negated = if matches!(self.current_token(), Some(Token::Not)) {
                    self.advance();
                    true
                } else {
                    false
                };
                
                // Check if condition follows or just default
                if !matches!(self.current_token(), Some(Token::LParen)) {
                    is_else_block = true;
                    break;
                }
                
                self.expect(Token::LParen)?;
                let mut cond = self.parse_expr()?;
                self.expect(Token::RParen)?;
                
                if negated {
                    cond = Expr::Not(Box::new(cond));
                }
                
                self.skip_newlines();
                let body = self.parse_block()?;
                else_ifs.push((cond, body));
                
                self.skip_newlines();
            }
            
            let else_block = if is_else_block {
                if matches!(self.current_token(), Some(Token::Colon)) {
                    self.advance(); // consume the : if we haven't already
                }
                self.skip_newlines();
                Some(self.parse_block()?)
            } else {
                None
            };

            self.expect(Token::RBrace)?;
            
            Ok(Stmt::If {
                condition,
                then_block,
                else_ifs,
                else_block,
            })
        } else {
            // Inline if
            let negated = if matches!(self.current_token(), Some(Token::Not)) {
                self.advance();
                true
            } else {
                false
            };
            
            self.expect(Token::LParen)?;
            let mut condition = self.parse_expr()?;
            self.expect(Token::RParen)?;
            
            if negated {
                condition = Expr::Not(Box::new(condition));
            }
            
            let then_block = vec![self.parse_statement()?];
            
            self.skip_newlines();
            
            let mut else_ifs = Vec::new();
            while matches!(self.current_token(), Some(Token::Colon)) {
                self.advance();
                self.skip_newlines();
                
                if !matches!(self.current_token(), Some(Token::LParen)) {
                    break;
                }
                
                let negated = if matches!(self.current_token(), Some(Token::Not)) {
                    self.advance();
                    true
                } else {
                    false
                };
                
                self.expect(Token::LParen)?;
                let mut cond = self.parse_expr()?;
                self.expect(Token::RParen)?;
                
                if negated {
                    cond = Expr::Not(Box::new(cond));
                }
                
                let body = vec![self.parse_statement()?];
                else_ifs.push((cond, body));
                
                self.skip_newlines();
            }
            
            let else_block = if matches!(self.current_token(), Some(Token::Colon)) {
                self.advance();
                self.skip_newlines();
                Some(self.parse_block()?)
            } else {
                None
            };

            self.skip_newlines();
            //self.expect(Token::RBrace)?;
            
            Ok(Stmt::If {
                condition,
                then_block,
                else_ifs,
                else_block,
            })
        }
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, String> {
        self.advance(); // consume 'while'
        
        let once = if matches!(self.current_token(), Some(Token::Once)) {
            self.advance();
            true
        } else {
            false
        };
        
        self.expect(Token::LParen)?;
        let condition = self.parse_expr()?;
        self.expect(Token::RParen)?;
        
        let body = if matches!(self.current_token(), Some(Token::LBrace)) {
            self.advance();
            self.skip_newlines();
            let parsed_body = self.parse_block()?;
            self.expect(Token::RBrace)?;

            parsed_body
        } else {
            vec![self.parse_statement()?]
        };
        
        if once {
            Ok(Stmt::WhileOnce { condition, body })
        } else {
            Ok(Stmt::While { condition, body })
        }
    }

    fn parse_until_stmt(&mut self) -> Result<Stmt, String> {
        self.advance(); // consume 'until'
        
        let once = if matches!(self.current_token(), Some(Token::Once)) {
            self.advance();
            true
        } else {
            false
        };
        
        self.expect(Token::LParen)?;
        let condition = self.parse_expr()?;
        self.expect(Token::RParen)?;
        
        let body = if matches!(self.current_token(), Some(Token::LBrace)) {
            self.advance();
            self.skip_newlines();
            let parsed_body = self.parse_block()?;
            self.expect(Token::RBrace)?;

            parsed_body
        } else {
            vec![self.parse_statement()?]
        };
        
        if once {
            Ok(Stmt::UntilOnce { condition, body })
        } else {
            Ok(Stmt::Until { condition, body })
        }
    }

    fn parse_match_stmt(&mut self) -> Result<Stmt, String> {
        self.advance(); // consume 'match'
        
        let match_type = if matches!(self.current_token(), Some(Token::Any)) {
            self.advance();
            MatchType::Any
        } else {
            MatchType::One
        };
        
        self.expect(Token::LParen)?;
        let expr = self.parse_expr()?;
        self.expect(Token::RParen)?;
        self.expect(Token::LBrace)?;
        self.skip_newlines();
        
        let mut cases = Vec::new();
        let mut default = None;
        
        while !matches!(self.current_token(), Some(Token::RBrace)) {
            // Check for default case (=> without preceding tests)
            if matches!(self.current_token(), Some(Token::ShortReturn)) {
                self.advance(); // consume =>
                self.skip_newlines();
                
                let body = if matches!(self.current_token(), Some(Token::LBrace)) {
                    self.advance();
                    self.skip_newlines();
                    self.parse_block()?
                } else {
                    vec![self.parse_statement()?]
                };
                
                default = Some(body);
                
                self.skip_newlines();
                if matches!(self.current_token(), Some(Token::Comma)) {
                    self.advance();
                }
                self.skip_newlines();
            } else {
                // Parse regular case
                let tests = self.parse_match_tests()?;
                self.expect(Token::ShortReturn)?;
                self.skip_newlines();
                
                let body = if matches!(self.current_token(), Some(Token::LBrace)) {
                    self.advance();
                    self.skip_newlines();
                    let block = self.parse_block()?;
                    self.expect(Token::RBrace)?;
                    block
                } else {
                    vec![self.parse_statement()?]
                };

                cases.push((tests, body));
                
                self.skip_newlines();
                self.expect(Token::Comma)?;
                self.skip_newlines();
            }
        }
        
        self.expect(Token::RBrace)?;
        
        Ok(Stmt::Match {
            expr,
            cases,
            default,
            match_type,
        })
    }

    fn parse_match_tests(&mut self) -> Result<Vec<Expr>, String> {
        let mut tests = vec![self.parse_match_literal()?];
        
        while matches!(self.current_token(), Some(Token::OrOr)) {
            self.advance();
            tests.push(self.parse_match_literal()?);
        }
        
        Ok(tests)
    }

    fn parse_match_literal(&mut self) -> Result<Expr, String> {
        match self.current_token() {
            Some(Token::Number(n)) => {
                self.advance();
                Ok(Expr::Number(n))
            }
            Some(Token::String(s)) => {
                self.advance();
                Ok(Expr::String(s))
            }
            Some(Token::True) => {
                self.advance();
                Ok(Expr::Bool(true))
            }
            Some(Token::False) => {
                self.advance();
                Ok(Expr::Bool(false))
            }
            other => Err(format!("Expected literal in match, got {:?}", other)),
        }
    }

    fn parse_fn_def(&mut self) -> Result<Stmt, String> {
        self.advance(); // consume 'fn'
        
        let name = self.expect_ident()?;
        self.expect(Token::LParen)?;
        let params = self.parse_fn_params()?;
        self.expect(Token::RParen)?;
        
        let return_type = if matches!(self.current_token(), Some(Token::Colon)) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };
        
        let body = if matches!(self.current_token(), Some(Token::LBrace)) {
            eprintln!("parse_fn_def: entering block");
            self.advance();
            self.skip_newlines();
            let block = self.parse_block()?;
            eprintln!("parse_fn_def: after parse_block, current token: {:?}", self.current_token());
            self.expect(Token::RBrace)?;
            eprintln!("parse_fn_def: after expect RBrace, current token: {:?}", self.current_token());
            Some(block)
        } else if matches!(self.current_token(), Some(Token::Newline)) {
            eprintln!("parse_fn_def: signature only");
            None // Just a signature
        } else {
            eprintln!("parse_fn_def: implicit return");
            // Single expression implicit return
            let expr = self.parse_expr()?;
            Some(vec![Stmt::Return(Some(expr))])
        };
        eprintln!("parse_fn_def: body done, current token: {:?}", self.current_token());
        
        Ok(Stmt::FnDef(FnDef {
            name,
            params,
            body,
            return_type,
        }))
    }

    fn parse_fn_params(&mut self) -> Result<Vec<(String, Option<Type>, Ownership)>, String> {
        let mut params = Vec::new();
        
        if !matches!(self.current_token(), Some(Token::RParen)) {
            loop {
                let name = self.expect_ident()?;
                
                let ty = if matches!(self.current_token(), Some(Token::Colon)) {
                    self.advance();
                    Some(self.parse_type()?)
                } else {
                    None
                };
                
                let ownership = if matches!(self.current_token(), Some(Token::Less)) {
                    self.advance();
                    Ownership::Owned
                } else {
                    Ownership::Borrowed
                };
                
                params.push((name, ty, ownership));
                
                if !matches!(self.current_token(), Some(Token::Comma)) {
                    break;
                }
                self.advance();
                self.skip_newlines();
            }
        }
        
        Ok(params)
    }

    fn parse_model_def(&mut self) -> Result<Stmt, String> {
        self.advance(); // consume 'model'
        
        let name = self.expect_ident()?;
        self.expect(Token::LBrace)?;
        self.skip_newlines();
        
        let mut fields = Vec::new();
        while !matches!(self.current_token(), Some(Token::RBrace)) {
            let field_name = self.expect_ident()?;
            self.expect(Token::Colon)?;
            let field_type = self.parse_type()?;
            
            fields.push((field_name, field_type));
            
            if matches!(self.current_token(), Some(Token::Comma)) {
                self.advance();
            }
            self.skip_newlines();
        }
        
        self.expect(Token::RBrace)?;
        
        Ok(Stmt::ModelDef(ModelDef { name, fields }))
    }

    fn parse_role_def(&mut self) -> Result<Stmt, String> {
        self.advance(); // consume 'role'
        
        let name = self.expect_ident()?;
        self.expect(Token::LBrace)?;
        self.skip_newlines();
        
        let mut methods = Vec::new();
        while !matches!(self.current_token(), Some(Token::RBrace)) {
            if let Stmt::FnDef(fn_def) = self.parse_fn_def()? {
                methods.push(fn_def);
            }
            self.skip_newlines();
        }
        
        self.expect(Token::RBrace)?;
        
        Ok(Stmt::RoleDef(RoleDef { name, methods }))
    }

    fn parse_extend_stmt(&mut self) -> Result<Stmt, String> {
        self.advance(); // consume 'extend'
        
        let model_name = self.expect_ident()?;
        self.expect(Token::With)?;
        
        let role_name = if matches!(self.current_token(), Some(Token::Ident(_))) {
            Some(self.expect_ident()?)
        } else {
            None
        };
        
        self.expect(Token::LBrace)?;
        self.skip_newlines();
        
        let mut methods = Vec::new();
        while !matches!(self.current_token(), Some(Token::RBrace)) {
            eprintln!("In extend loop, current token: {:?}", self.current_token());
            if let Stmt::FnDef(fn_def) = self.parse_fn_def()? {
                methods.push(fn_def);
                eprintln!("After parse_fn_def, current token: {:?}", self.current_token());
            }
            self.skip_newlines();
        }
        
        self.expect(Token::RBrace)?;
        
        Ok(Stmt::ExtendModel {
            model_name,
            role_name,
            methods,
        })
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, String> {
        self.advance(); // consume 'return'
        
        if matches!(self.current_token(), Some(Token::Newline)) {
            Ok(Stmt::Return(None))
        } else {
            let expr = self.parse_expr()?;
            Ok(Stmt::Return(Some(expr)))
        }
    }

   fn parse_block(&mut self) -> Result<Vec<Stmt>, String> {
        let mut stmts = Vec::new();
        
        while !matches!(self.current_token(), Some(Token::RBrace | Token::Colon)) && !self.is_at_end() {
            self.skip_newlines();  // Skip newlines before checking again
            if matches!(self.current_token(), Some(Token::RBrace | Token::Colon)) {
                break;
            }
            stmts.push(self.parse_statement()?);
        }
        
        Ok(stmts)
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        let _owned = if matches!(self.current_token(), Some(Token::Less)) {
            self.advance();
            true
        } else {
            false
        };
        
        let type_name = self.expect_ident()?;
        
        let base_type = match type_name.as_str() {
            "number" => Type::Number,
            "string" => Type::Str,
            "bool" => Type::Bool,
            "array" => {
                if matches!(self.current_token(), Some(Token::Less)) {
                    self.advance();
                    let inner = Box::new(self.parse_type()?);
                    self.expect(Token::Greater)?;
                    Type::Array(inner)
                } else {
                    Type::Array(Box::new(Type::Number))
                }
            }
            "void" => Type::Void,
            model_name => Type::Model(model_name.to_string()),
        };
        
        Ok(base_type)
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

    fn expect_ident(&mut self) -> Result<String, String> {
        match self.current_token() {
            Some(Token::Ident(name)) => {
                self.advance();
                Ok(name)
            }
            other => Err(format!("Expected identifier, got {:?}", other)),
        }
    }

    // Expression parsing (delegate to expr_parser)
    fn parse_expr(&mut self) -> Result<Expr, String> {
        let mut expr_tokens = Vec::new();
        let mut paren_depth = 0;
        let mut bracket_depth = 0;
        let mut brace_depth = 0;
        
        while let Some((pos, token, end)) = self.tokens.peek().cloned() {
            match &token {
                Token::LParen => paren_depth += 1,
                Token::RParen => {
                    if paren_depth == 0 {
                        break; // Stop at unmatched )
                    }
                    paren_depth -= 1;
                }
                Token::LBracket => bracket_depth += 1,
                Token::RBracket => {
                    if bracket_depth == 0 {
                        break;
                    }
                    bracket_depth -= 1;
                }
                Token::LBrace => brace_depth += 1,
                Token::RBrace => {
                    if brace_depth == 0 {
                        break; // Stop at unmatched }
                    }
                    brace_depth -= 1;
                }
                Token::Newline | Token::Comma | Token::Colon => {
                    if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 {
                        break;
                    }
                }
                _ => {}
            }
            
            expr_tokens.push((pos, token, end));
            self.advance();
        }
        
        if expr_tokens.is_empty() {
            return Err("Expected expression".to_string());
        }
        
        let mut expr_parser = crate::expr_parser::ExprParser::new(expr_tokens);
        expr_parser.parse()
    }

    fn parse_method_chain(&mut self, mut obj: Expr) -> Result<Expr, String> {
        while matches!(self.current_token(), Some(Token::Period)) {
            self.advance();
            let field_name = self.expect_ident()?;
            
            if matches!(self.current_token(), Some(Token::LParen)) {
                self.advance();
                let args = self.parse_fn_args()?;
                self.expect(Token::RParen)?;
                obj = Expr::MethodCall {
                    object: Box::new(obj),
                    method: field_name,
                    args,
                };
            } else {
                obj = Expr::FieldAccess {
                    object: Box::new(obj),
                    field: field_name,
                };
            }
        }
        
        Ok(obj)
    }

    fn parse_fn_args(&mut self) -> Result<Vec<Expr>, String> {
        let mut args = Vec::new();
        
        self.skip_newlines();
        
        if !matches!(self.current_token(), Some(Token::RParen)) {
            args.push(self.parse_expr()?);
            
            while matches!(self.current_token(), Some(Token::Comma)) {
                self.advance();
                self.skip_newlines();
                
                if matches!(self.current_token(), Some(Token::RParen)) {
                    break;
                }
                
                args.push(self.parse_expr()?);
                self.skip_newlines();
            }
        }
        
        Ok(args)
    }
}
