use crate::lexer::{Lexer, Token};

pub struct Parameter {
    pub name: String,
    pub is_owned: bool,
}

pub enum Statement {
    Declaration {
        name: String,
        is_mutable: bool,
        value: Expr,
    },
    Expression(Expr),
    Function {
        name: String,
        params: Vec<Parameter>,
        body: Vec<Statement>,
    },
    Import { names: Vec<String>, source: String },
    Print(Expr),
    Block(Vec<Statement>),
    If {
        condition: Expr,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    While {
        condition: Expr,
        body: Box<Statement>,
        is_once: bool,
    },
    Loop(Box<Statement>),
    Break,
}

pub enum Expr {
    Assign { name: String, value: Box<Expr> },
    Bool(bool),
    Call { name: String, args: Vec<Expr> },
    Number(i64),
    Float(f64),
    String(String),
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Variable(String),
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
}

pub struct Parser {
    pub lexer: Lexer,
    pub current_token: Token,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let first_token = lexer.next_token();
        Self {
            lexer,
            current_token: first_token,
        }
    }

    // Move to the next token
    pub fn advance(&mut self) {
        self.current_token = self.lexer.next_token();
    }

    fn consume_newlines(&mut self) {
        while self.current_token == Token::Newline {
            self.advance();
        }
    }

    fn expect(&mut self, expected: Token) {
        if self.current_token == expected {
            self.advance();
        } else {
            panic!("Expected {:?}, got {:?}", expected, self.current_token);
        }
    }

    fn expect_identifier(&mut self) -> String {
        if let Token::Identifier(name) = &self.current_token {
            let name_cloned = name.clone();

            self.advance();

            name_cloned
        } else {
            panic!(
                "Syntax Error: Expected an identifier, but found {:?}", 
                self.current_token
            );
        }
    }

    fn parse_function(&mut self) -> Statement {
        self.advance();

        let name = self.expect_identifier();
        self.expect(Token::LParen);

        let mut params = Vec::new();

        while self.current_token != Token::RParen {
            let mut is_owned = false;
            
            // Check for ngn ownership marker '<'
            if self.current_token == Token::LessThan {
                is_owned = true;
                self.advance();
            }

            let param_name = self.expect_identifier();
            
            // Handle optional type: param: i32
            if self.current_token == Token::Colon {
                self.advance();
                self.expect_identifier(); // TODO Skip type name for now
            }

            params.push(Parameter { name: param_name, is_owned });

            if self.current_token == Token::Comma {
                self.advance();
            }
        }
        self.expect(Token::RParen);

        let body = self.parse_block();

        Statement::Function { name, params, body }
    }

    fn parse_import_statement(&mut self) -> Statement {
        self.advance(); // consume 'import'
        self.expect(Token::LBrace);
        
        let mut names = Vec::new();
        while self.current_token != Token::RBrace {
            let name = self.expect_identifier();
            names.push(name);
            if self.current_token == Token::Comma { self.advance(); }
        }
        self.expect(Token::RBrace);
        self.expect(Token::From);
        
        // This will be "tbx::test" or "math"
        let source = match self.current_token.clone() {
            Token::StringLiteral(s) => s,
            _ => panic!("Expected string literal for import source"),
        };
        self.advance();

        Statement::Import { names, source }
    }

    pub fn parse_statement(&mut self) -> Statement {
        self.consume_newlines();

        let stmt = match self.current_token.clone() {
            Token::Const => self.parse_declaration(false),
            Token::Fn => self.parse_function(),
            Token::Identifier(_) => {
                let expr = self.parse_expression();
                Statement::Expression(expr)
            }
            Token::Import => self.parse_import_statement(),
            Token::Print => {
                self.advance();
                self.expect(Token::LParen);
                let expr = self.parse_expression();
                self.expect(Token::RParen);
                Statement::Print(expr)
            }
            Token::Var => self.parse_declaration(true),
            Token::If => self.parse_if_stmt(),
            Token::While => self.parse_while_stmt(),
            Token::Loop => self.parse_loop_stmt(),
            Token::Break => {
                self.advance();
                Statement::Break
            }
             _ => {
               let expr = self.parse_expression();
               Statement::Expression(expr)
            }
        };

        if self.current_token == Token::Newline {
             self.advance();
        }
        
        stmt
    }

    fn parse_declaration(&mut self, is_mutable: bool) -> Statement {
        self.advance(); // consume 'var' or 'const'
        
        let name = self.expect_identifier();
        
        // Handle optional type: var x: i32 = 10
        if self.current_token == Token::Colon {
            self.advance();
            self.parse_type();
        }

        self.expect(Token::Equal);
        let value = self.parse_expression();
        
        Statement::Declaration {
            name,
            is_mutable,
            value,
        }
    }

    fn parse_type(&mut self) -> String {
        let type_name = self.expect_identifier();
        
        if self.current_token == Token::LessThan {
            self.advance(); // consume <
            
            while self.current_token != Token::GreaterThan {
                if let Token::Number(_) = self.current_token {
                    self.advance(); // handle array size N
                } else {
                    self.parse_type();
                }

                if self.current_token == Token::Comma {
                    self.advance();
                }
            }
            self.expect(Token::GreaterThan);
        }
        type_name
    }

    pub fn parse_expression(&mut self) -> Expr {
        // Start with the lowest precedence
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Expr {
        let expr = self.parse_equality();

        // If the next token is '=', this is an assignment
        if self.current_token == Token::Equal {
            self.advance(); // consume '='
            let value = self.parse_assignment(); // Recursive for things like x = y = 10

            // We check if the left side was actually a variable
            if let Expr::Variable(name) = expr {
                return Expr::Assign {
                    name,
                    value: Box::new(value),
                };
            } else {
                panic!("Syntax Error: Invalid assignment target");
            }
        }
        expr
    }

    fn parse_equality(&mut self) -> Expr {
        let mut left = self.parse_comparison();

        while self.current_token == Token::EqualEqual || self.current_token == Token::NotEqual {
            let op = self.current_token.clone();
            self.advance();
            let right = self.parse_comparison();
            left = Expr::Binary { left: Box::new(left), op, right: Box::new(right) };
        }
        left
    }

    fn parse_comparison(&mut self) -> Expr {
        let mut left = self.parse_addition();

        while self.current_token == Token::LessThan || self.current_token == Token::GreaterThan {
            let op = self.current_token.clone();
            self.advance();
            let right = self.parse_addition();
            left = Expr::Binary { left: Box::new(left), op, right: Box::new(right) };
        }
        left
    }

    fn parse_addition(&mut self) -> Expr {
        let mut left = self.parse_multiplication();

        while self.current_token == Token::Plus || self.current_token == Token::Minus {
            let op = self.current_token.clone();
            self.advance();
            let right = self.parse_multiplication();
            
            // Re-wrap the left side into a new binary expression
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        left
    }

    fn parse_multiplication(&mut self) -> Expr {
        let mut left = self.parse_power(); 

        while self.current_token == Token::Star || self.current_token == Token::Slash || self.current_token == Token::Modulo {
            let op = self.current_token.clone();
            self.advance();
            let right = self.parse_power();
            left = Expr::Binary { left: Box::new(left), op, right: Box::new(right) };
        }
        left
    }

    fn parse_power(&mut self) -> Expr {
        let left = self.parse_primary();

        if self.current_token == Token::Power {
            self.advance();
            let right = self.parse_power(); // Right-associative
            return Expr::Binary {
                left: Box::new(left),
                op: Token::Power,
                right: Box::new(right),
            };
        }
        left
    }

    fn parse_primary(&mut self) -> Expr {
        let mut expr = match self.current_token.clone() {
            Token::Bool(b) => {
                self.advance();
                Expr::Bool(b)
            }
            Token::LParen => {
                self.advance();
                
                if self.current_token == Token::RParen {
                    self.advance();
                    Expr::Tuple(Vec::new())
                } else {
                    let expression = self.parse_expression();
                    
                    if self.current_token == Token::Comma {
                        self.advance();
                        let mut elements = vec![expression];
                        
                        while self.current_token != Token::RParen {
                            elements.push(self.parse_expression());
                            if self.current_token == Token::Comma {
                                self.advance();
                            }
                        }
                        self.expect(Token::RParen);
                        Expr::Tuple(elements)
                    } else {
                        self.expect(Token::RParen);
                        expression
                    }
                }
            }
            Token::Number(n) => {
                self.advance();
                Expr::Number(n)
            }
            Token::Float(n) => {
                self.advance();
                Expr::Float(n)
            }
            Token::StringLiteral(s) => {
                self.advance();
                Expr::String(s)
            }
            Token::Identifier(name) => {
                self.advance();
                Expr::Variable(name)
            }
            Token::LBracket => self.parse_array_literal(),
            _ => panic!("Expected expression, found {:?}", self.current_token),
        };

        if self.current_token == Token::LParen {
            self.advance(); // consume '('
            let mut args = Vec::new();
            while self.current_token != Token::RParen {
                args.push(self.parse_expression());
                if self.current_token == Token::Comma { self.advance(); }
            }
            self.expect(Token::RParen);
            
            if let Expr::Variable(name) = expr {
                expr = Expr::Call { name, args };
            }
        }

        expr
    }
    fn parse_array_literal(&mut self) -> Expr {
        self.advance(); // consume '['
        let mut elements = Vec::new();

        if self.current_token == Token::RBracket {
            self.advance(); // empty array
            return Expr::Array(elements);
        }

        while self.current_token != Token::RBracket {
            elements.push(self.parse_expression());
            
            if self.current_token == Token::Comma {
                self.advance();
            } else if self.current_token != Token::RBracket {
                panic!("Expected ',' or ']' in array literal");
            }
        }

        self.expect(Token::RBracket);
        Expr::Array(elements)
    }

    fn parse_block(&mut self) -> Vec<Statement> {
        self.expect(Token::LBrace);
        let mut statements = Vec::new();

        loop {
             // Skip incoming newlines
             while self.current_token == Token::Newline { 
                 self.advance(); 
             }
             
             if self.current_token == Token::RBrace || self.current_token == Token::EOF {
                 break;
             }
             
             statements.push(self.parse_statement());
        }

        self.expect(Token::RBrace);
        statements
    }

    fn parse_if_stmt(&mut self) -> Statement {
        self.advance(); // consume 'if'

        // Check for Block If: if { ... }
        if self.current_token == Token::LBrace {
             self.advance(); // consume '{'
             
             // First branch must have a condition
             if self.current_token == Token::Newline { self.advance(); }
             
             self.expect(Token::LParen);
             let condition = self.parse_expression();
             self.expect(Token::RParen);
             
             // Parse statements until we hit ':' or '}'
             let mut then_block = Vec::new();
             loop {
                 while self.current_token == Token::Newline { self.advance(); }
                 if self.current_token == Token::Colon || self.current_token == Token::RBrace || self.current_token == Token::EOF {
                     break;
                 }
                 then_block.push(self.parse_statement());
             }
             
             let mut else_branch = None;
             
             if self.current_token == Token::Colon {
                 self.advance(); // consume ':'
                 while self.current_token == Token::Newline { self.advance(); }
                 
                 // Check if it's an 'else if' or 'else'
                 if self.current_token == Token::LParen {
                     else_branch = Some(Box::new(self.parse_if_inner()));
                 } else {
                     // Final 'else' block
                     let mut else_stmts = Vec::new();
                     loop {
                         while self.current_token == Token::Newline { self.advance(); }
                         if self.current_token == Token::RBrace || self.current_token == Token::EOF {
                             break;
                         }
                         else_stmts.push(self.parse_statement());
                     }
                     else_branch = Some(Box::new(Statement::Block(else_stmts)));
                 }
             }
             
             if self.current_token == Token::RBrace {
                 self.advance();
             }
             
             Statement::If {
                 condition,
                 then_branch: Box::new(Statement::Block(then_block)),
                 else_branch,
             }

        } else {
            // Inline If: if (cond) stmt : ...
            self.expect(Token::LParen);
            let condition = self.parse_expression();
            self.expect(Token::RParen);
            
            let then_stmt = self.parse_statement();
            let mut else_branch = None;
            
            if self.current_token == Token::Colon {
                self.advance(); // consume ':'
                // Check for 'else if' vs 'else'
                if self.current_token == Token::LParen {
                     // Inline Else If (Implicit If)
                     else_branch = Some(Box::new(self.parse_implicit_if()));
                } else {
                    else_branch = Some(Box::new(self.parse_statement()));
                }
            }
            
            Statement::If {
                condition,
                then_branch: Box::new(then_stmt),
                else_branch,
            }
        }
    }

    // Helper for Block If recursion
    fn parse_if_inner(&mut self) -> Statement {
         // Expect condition
         self.expect(Token::LParen);
         let condition = self.parse_expression();
         self.expect(Token::RParen);
         
         let mut then_block = Vec::new();
         loop {
             while self.current_token == Token::Newline { self.advance(); }
             if self.current_token == Token::Colon || self.current_token == Token::RBrace || self.current_token == Token::EOF {
                 break;
             }
             then_block.push(self.parse_statement());
         }
         
         let mut else_branch = None;
         if self.current_token == Token::Colon {
             self.advance();
             while self.current_token == Token::Newline { self.advance(); }
             
             if self.current_token == Token::LParen {
                 else_branch = Some(Box::new(self.parse_if_inner()));
             } else {
                 let mut else_stmts = Vec::new();
                 loop {
                     while self.current_token == Token::Newline { self.advance(); }
                     if self.current_token == Token::RBrace || self.current_token == Token::EOF {
                         break;
                     }
                     else_stmts.push(self.parse_statement());
                 }
                 else_branch = Some(Box::new(Statement::Block(else_stmts)));
             }
         }
         
         Statement::If {
             condition,
             then_branch: Box::new(Statement::Block(then_block)),
             else_branch,
         }
    }

    // Helper for Inline If recursion (implicit if)
    fn parse_implicit_if(&mut self) -> Statement {
        self.expect(Token::LParen);
        let condition = self.parse_expression();
        self.expect(Token::RParen);
        
        let then_stmt = self.parse_statement();
        let mut else_branch = None;
        
        if self.current_token == Token::Colon {
            self.advance(); 
            if self.current_token == Token::LParen {
                 else_branch = Some(Box::new(self.parse_implicit_if()));
            } else {
                else_branch = Some(Box::new(self.parse_statement()));
            }
        }
        
        Statement::If {
            condition,
            then_branch: Box::new(then_stmt),
            else_branch
        }
    }

    fn parse_while_stmt(&mut self) -> Statement {
        self.advance(); // consume 'while'
        
        let mut is_once = false;
        if self.current_token == Token::Once {
            is_once = true;
            self.advance();
        }

        self.expect(Token::LParen);
        let condition = self.parse_expression();
        self.expect(Token::RParen);

        let body = if self.current_token == Token::LBrace {
             Statement::Block(self.parse_block())
        } else {
            self.parse_statement()
        };

        Statement::While {
            condition,
            body: Box::new(body),
            is_once,
        }
    }

    fn parse_loop_stmt(&mut self) -> Statement {
        self.advance(); // consume 'loop'
        
        let body = if self.current_token == Token::LBrace {
             Statement::Block(self.parse_block())
        } else {
            self.parse_statement()
        };

        Statement::Loop(Box::new(body))
    }
}