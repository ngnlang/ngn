use crate::ast::*;
use crate::lexer::Token;
use std::collections::HashMap;
use std::iter::Peekable;
use std::vec::IntoIter;
use crate::utils::infer_enum_name;
use crate::utils::parse_type;

pub struct Parser {
    tokens: Peekable<IntoIter<(usize, Token, usize)>>,
    enums: HashMap<String, EnumDef>,
}

impl Parser {
    pub fn new(tokens: Vec<(usize, Token, usize)>, enums: HashMap<String, EnumDef>) -> Self {
        // remove comment tokens - only used for syntax highlighting
        let filtered_tokens: Vec<_> = tokens
            .into_iter()
            .filter(|(_, token, _)| !matches!(token, Token::Comment(_)))
            .collect();

        Parser {
            tokens: filtered_tokens.into_iter().peekable(),
            enums,
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

    fn is_statement_start(&mut self) -> bool {
        matches!(
            self.current_token(),
            Some(Token::Print | Token::Echo | Token::Var | Token::Const 
                | Token::If | Token::While | Token::Until | Token::Match 
                | Token::Fn | Token::Model | Token::Role | Token::Extend 
                | Token::Return | Token::Break | Token::Next | Token::Rebind)
        )
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

    fn parse_statement(&mut self) -> Result<Stmt, String> {
        self.skip_newlines();
        
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
            Some(Token::LArrow) => {
                let expr = self.parse_expr()?;
                Ok(Stmt::ExprStmt(expr))
            }
            Some(Token::Ident(name)) if name == "thread" => {
                self.advance(); // consume 'thread'
                
                // Check for invocation: ( closure )
                if matches!(self.current_token(), Some(Token::LParen)) {
                    self.advance(); // consume (
                    
                    // We use parse_expr because the argument is a closure expression
                    let closure_expr = self.parse_expr()?;
                    
                    self.expect(Token::RParen)?; // consume )
                    
                    // Return as an ExprStmt wrapping your Expr::Thread
                    return Ok(Stmt::ExprStmt(Expr::Thread(Box::new(closure_expr))));
                }
                
                // If it's used as a variable, fall through (unlikely but safe)
                Err("Expected '(' after thread keyword".to_string())
            }
            Some(Token::Ident(_)) => {
                // Could be: assignment, reassignment, or expression statement
                self.parse_ident_statement()
            },
            Some(Token::Enum) => self.parse_enum_def(),
            other => Err(format!("Unexpected token in statement: {:?}", other)),
        }
    }

    fn parse_assign_stmt(&mut self, kind: AssignKind) -> Result<Stmt, String> {
        self.advance(); // consume var/const/lit/static
        
        let name = self.expect_ident()?;
        
        let declared_type = if matches!(self.current_token(), Some(Token::Colon)) {
            self.advance();
            let (ty, _ownership) = parse_type(&mut self.tokens)?;  // Ignore ownership from type
            Some(ty)
        } else {
            None
        };

        let ownership = match self.current_token() {
            Some(Token::Eq) => {
                self.advance();
                Ownership::Borrowed
            },
            Some(Token::OwnedAssign) => {
                // ownership is only allowed for var
                if kind != AssignKind::Var {
                    return Err("You can only declare ownership with 'var'".to_string());
                }

                self.advance();
                Ownership::Owned
            },
            _ => return Err("Expected '=' or '=<' in assignment".to_string()),
        };

        self.skip_newlines();        
        let value = self.parse_expr()?;

        if let Expr::MakeChannel(_) = value {
            if kind != AssignKind::Const {
                let kind_str = match kind {
                    AssignKind::Var => "var",
                    AssignKind::Lit => "lit",
                    AssignKind::Static => "static",
                    AssignKind::Const => "const", // Unreachable here, but satisfies the match
                };

                return Err(format!(
                    "Channels must be declared with 'const'. Found '{}' for variable '{}'",
                    kind_str, name
                ));
            }
        }
        
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
        
        // Check if it's a field rebind: rebind obj.field = value
        if matches!(self.current_token(), Some(Token::Period)) {
            let mut expr = Expr::Var(name.clone());
            expr = self.parse_method_chain(expr)?;
            
            self.expect(Token::Eq)?;
            let value = self.parse_expr()?;
            
            // expr should be a FieldAccess
            if let Expr::FieldAccess { object, field, value: _ } = expr {
                if let Expr::Var(obj_name) = *object {
                    return Ok(Stmt::RebindField { 
                        object: obj_name, 
                        field, 
                        value 
                    });
                } else {
                    return Err("Can only rebind fields on direct variables".to_string());
                }
            } else {
                return Err("Cannot rebind to non-field".to_string());
            }
        }
        
        // Simple rebind: rebind name = value
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
            Some(Token::Period) => {
                // Could be: method call, field access, or field assignment
                // Parse the postfix chain first
                let mut expr = Expr::Var(name);
                expr = self.parse_method_chain(expr)?;
                
                // Now check if there's an assignment
                if matches!(self.current_token(), Some(Token::Eq)) {
                    self.advance();  // consume =
                    let value = self.parse_expr()?;
                    
                    // expr should be a FieldAccess at this point
                    if let Expr::FieldAccess { object, field, value: _ } = expr {
                        return Ok(Stmt::ExprStmt(Expr::FieldAccess {
                            object,
                            field,
                            value: Some(Box::new(value)),
                        }));
                    } else {
                        return Err("Cannot assign to non-field".to_string());
                    }
                }
                
                Ok(Stmt::ExprStmt(expr))
            }
            Some(op_token) if self.is_compound_op(&op_token) => {
                // Compound assignment (+=, -=, etc.)
                let op = self.token_to_compound_op(&op_token)?;
                self.advance();
                let value = self.parse_expr()?;
                
                // Create the expanded expression
                let expanded = match op.as_str() {
                    "+=" => Expr::Add(Box::new(Expr::Var(name.clone())), Box::new(value)),
                    "-=" => Expr::Subtract(Box::new(Expr::Var(name.clone())), Box::new(value)),
                    "*=" => Expr::Multiply(Box::new(Expr::Var(name.clone())), Box::new(value)),
                    "/=" => Expr::Divide(Box::new(Expr::Var(name.clone())), Box::new(value)),
                    "%=" => Expr::Modulo(Box::new(Expr::Var(name.clone())), Box::new(value)),
                    "**=" | "^=" => Expr::Power(Box::new(Expr::Var(name.clone())), Box::new(value)),
                    _ => unreachable!(),
                };
                
                Ok(Stmt::Reassign { 
                    name, 
                    value: expanded 
                })
            }
            Some(Token::LParen) => {
                // Function call
                self.advance();
                let args = self.parse_fn_args()?;
                self.expect(Token::RParen)?;
                Ok(Stmt::ExprStmt(Expr::Call { name, args }))
            }
            Some(Token::LArrow) => {
                self.advance(); // consume <-
                let value = self.parse_expr()?;
                // Return it as an Expression Statement containing a Send expr
                Ok(Stmt::ExprStmt(Expr::Send(
                    Box::new(Expr::Var(name)),
                    Box::new(value)
                )))
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

            self.skip_newlines();
            
            if negated {
                condition = Expr::Not(Box::new(condition));
            }
            
            let then_block = vec![self.parse_statement()?];
            
            self.skip_newlines();
            
            let mut else_ifs = Vec::new();
            let mut else_block = None;

            while matches!(self.current_token(), Some(Token::Colon)) {
                self.advance();
                self.skip_newlines();

                // Check if this is an else (no condition)
                if !matches!(self.current_token(), Some(Token::LParen)) {
                    // This is an else block, not an else-if
                    else_block = Some(vec![self.parse_statement()?]);
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

            self.skip_newlines();
            
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
                let patterns = self.parse_match_tests()?;
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

                cases.push((patterns, body));
                
                self.skip_newlines();

                // Check if we are at the end (RBrace). If so, no comma needed.
                if matches!(self.current_token(), Some(Token::RBrace)) {
                    break; 
                }

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

    fn parse_match_tests(&mut self) -> Result<Vec<Pattern>, String> {
        let mut patterns = vec![self.parse_match_pattern()?];
        
        while matches!(self.current_token(), Some(Token::Or)) {
            self.advance();
            patterns.push(self.parse_match_pattern()?);
        }
        
        Ok(patterns)
    }

    fn parse_match_pattern(&mut self) -> Result<Pattern, String> {
        match self.current_token() {
            Some(Token::Ident(name)) => {
                self.advance();
                

                // Check if it's a known enum variant
                if matches!(name.as_str(), "Ok" | "Error" | "Value" | "Null") {
                    // Unit variant (no parens) like Null
                    if !matches!(self.current_token(), Some(Token::LParen)) {
                        let enum_name = infer_enum_name(&name, &self.enums);
                        return Ok(Pattern::EnumVariant {
                            enum_name,
                            variant: name,
                            binding: None,
                        });
                    }

                    if matches!(self.current_token(), Some(Token::LParen)) {
                        self.advance();
                        
                        let binding = if matches!(self.current_token(), Some(Token::Ident(_))) {
                            Some(self.expect_ident()?)
                        } else {
                            None
                        };
                        
                        self.expect(Token::RParen)?;
                        
                        let enum_name = infer_enum_name(&name, &self.enums);
                        return Ok(Pattern::EnumVariant {
                            enum_name,
                            variant: name,
                            binding,
                        });
                    }
                } else {
                    // It's not an enum variant, so it must be in the match expression
                    // Put the token back and return wildcard or error
                    return Err("Expected pattern, got function call".to_string());
                }
                
                // Wildcard
                if name == "_" {
                    return Ok(Pattern::Wildcard);
                }
                
                // Literal value (variable reference)
                Ok(Pattern::Literal(Expr::Var(name)))
            }
            Some(Token::Integer(n)) => {
                self.advance();
                Ok(Pattern::Literal(Expr::I64(n)))
            }
            Some(Token::Float(n)) => {
                self.advance();
                Ok(Pattern::Literal(Expr::F64(n)))
            }
            Some(Token::String(s)) => {
                self.advance();
                Ok(Pattern::Literal(Expr::String(s)))
            }
            Some(Token::True) => {
                self.advance();
                Ok(Pattern::Literal(Expr::Bool(true)))
            }
            Some(Token::False) => {
                self.advance();
                Ok(Pattern::Literal(Expr::Bool(false)))
            }
            _ => Err(format!("Expected pattern, got {:?}", self.current_token())),
        }
    }

    fn parse_fn_def(&mut self) -> Result<Stmt, String> {
        self.advance(); // consume 'fn'
        
        let name = self.expect_ident()?;
        self.expect(Token::LParen)?;
        let params = self.parse_fn_params()?;
        self.expect(Token::RParen)?;
        
        let (return_type, _ownership_type) = if matches!(self.current_token(), Some(Token::Colon)) {
            self.advance();
            let (ty, own) = parse_type(&mut self.tokens)?;
            (Some(ty), own)
        } else {
            (None, Ownership::Borrowed)
        };
        
        let body = if matches!(self.current_token(), Some(Token::LBrace)) {
            self.advance();
            self.skip_newlines();
            let block = self.parse_block()?;
            self.expect(Token::RBrace)?;
            Some(block)
        } else if matches!(self.current_token(), Some(Token::Newline)) {
            None // Just a signature
        } else {
            // Single expression implicit return
            let expr = self.parse_expr()?;
            Some(vec![Stmt::Return(Some(expr))])
        };
        
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
                
                let (ty, ownership) = if matches!(self.current_token(), Some(Token::Colon)) {
                    self.advance();
                    let (ty, own) = parse_type(&mut self.tokens)?;
                    (Some(ty), own)
                } else {
                    (None, Ownership:: Borrowed)
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
            let (field_type, _field_ownership) = parse_type(&mut self.tokens)?;
            
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
            if let Stmt::FnDef(fn_def) = self.parse_fn_def()? {
                methods.push(fn_def);
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

    fn parse_enum_def(&mut self) -> Result<Stmt, String> {
        self.advance();  // consume 'enum'
        
        let name = self.expect_ident()?;
        
        // Parse optional type parameters: <T, E>
        let type_params = if matches!(self.current_token(), Some(Token::Less)) {
            self.advance();
            let mut params = vec![self.expect_ident()?];
            while matches!(self.current_token(), Some(Token::Comma)) {
                self.advance();
                params.push(self.expect_ident()?);
            }
            self.expect(Token::Greater)?;
            params
        } else {
            vec![]
        };
        
        self.expect(Token::LBrace)?;
        self.skip_newlines();
        
        let mut variants = Vec::new();
        while !matches!(self.current_token(), Some(Token::RBrace)) {
            let variant_name = self.expect_ident()?;
            
            let data_type = if matches!(self.current_token(), Some(Token::LParen)) {
                self.advance();
                let (ty, _) = parse_type(&mut self.tokens)?;
                self.expect(Token::RParen)?;
                Some(ty)
            } else {
                None
            };
            
            variants.push(EnumVariant {
                name: variant_name,
                data_type,
            });
            
            if matches!(self.current_token(), Some(Token::Comma)) {
                self.advance();
            }
            self.skip_newlines();
        }
        
        self.expect(Token::RBrace)?;
        
        Ok(Stmt::EnumDef(EnumDef {
            name,
            type_params,
            variants,
        }))
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
        // Closures have special syntax (|params|), so handle them directly
        if matches!(self.current_token(), Some(Token::Or)) {
            return self.parse_closure_expr();
        }

        let mut expr_tokens = Vec::new();
        let mut paren_depth = 0;
        let mut bracket_depth = 0;
        let mut brace_depth = 0;
        let mut angle_depth = 0;
        
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
                Token::Less => angle_depth += 1,
                Token::Greater => {
                    if angle_depth > 0 { angle_depth -= 1; }
                }
                Token::Newline | Token::Comma => {
                    if paren_depth == 0 && 
                        bracket_depth == 0 && 
                        brace_depth == 0 && 
                        angle_depth == 0 {
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
        
        let mut expr_parser = crate::expr_parser::ExprParser::new(expr_tokens, &self.enums);
        let expr = expr_parser.parse()?;

        // Check for field assignment: obj.field = value
        if matches!(self.current_token(), Some(Token::Eq)) {
            if let Expr::FieldAccess { object, field, value: _ } = expr {
                self.advance();  // consume =
                let value = self.parse_expr()?;
                return Ok(Expr::FieldAccess {
                    object,
                    field,
                    value: Some(Box::new(value)),
                });
            }
        }

        Ok(expr)
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
                    value: None,
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

    fn parse_closure_expr(&mut self) -> Result<Expr, String> {
        self.expect(Token::Or)?;  // consume opening |
        
        // Parse parameters (can be empty)
        let mut params = Vec::new();
        
        while !matches!(self.current_token(), Some(Token::Or)) {
            let name = self.expect_ident()?;
            
            let param_type = if matches!(self.current_token(), Some(Token::Colon)) {
                self.advance();
                let (ty, _ownership) = parse_type(&mut self.tokens)?;
                Some(ty)
            } else {
                None
            };
            
            params.push((name, param_type));
            
            if matches!(self.current_token(), Some(Token::Comma)) {
                self.advance();
                self.skip_newlines();
            } else {
                break;
            }
        }
        
        self.expect(Token::Or)?;  // consume closing |
        self.skip_newlines();
        
        // Parse optional return type
        let return_type = if matches!(self.current_token(), Some(Token::Colon)) {
            self.advance();
            let (ty, _ownership) = parse_type(&mut self.tokens)?;
            Some(ty)
        } else {
            None
        };
        
        self.skip_newlines();
        
        // Parse body
        let body = if matches!(self.current_token(), Some(Token::LBrace)) {
            // Braces: parse block
            self.advance();
            self.skip_newlines();
            let block = self.parse_block()?;
            self.expect(Token::RBrace)?;
            block
        } else {
            // No braces: parse as expression or statement
            // Check if it starts with a statement keyword
            if self.is_statement_start() {
                vec![self.parse_statement()?]
            } else {
                let expr = self.parse_expr()?;
                vec![Stmt::Return(Some(expr))]
            }
        };
        
        Ok(Expr::Closure(Box::new(ClosureDef {
            params,
            return_type,
            body,
        })))
    }
}
