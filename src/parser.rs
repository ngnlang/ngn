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
}

pub enum Expr {
    Assign { name: String, value: Box<Expr> },
    Bool(bool),
    Call { name: String, args: Vec<Expr> },
    Number(i64),
    String(String),
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Variable(String),
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

        self.expect(Token::LBrace);
        let mut body = Vec::new();
        
        while self.current_token != Token::RBrace && self.current_token != Token::EOF {
            body.push(self.parse_statement());
        }
        self.expect(Token::RBrace);

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

        let stmt = match self.current_token {
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
            _ => panic!("Expected statement, found {:?}", self.current_token),
        };

        // Expect either a Newline or EOF at the end of every statement
        if self.current_token != Token::EOF {
            self.expect(Token::Newline);
        }
        
        stmt
    }

    fn parse_declaration(&mut self, is_mutable: bool) -> Statement {
        self.advance(); // consume 'var' or 'const'
        
        let name = self.expect_identifier();
        
        // Handle optional type: var x: i32 = 10
        if self.current_token == Token::Colon {
            self.advance();
            let _type_name = self.expect_identifier(); // We'll use this for type checking later
        }

        self.expect(Token::Equal);
        let value = self.parse_expression();
        
        Statement::Declaration {
            name,
            is_mutable,
            value,
        }
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
        let mut left = self.parse_addition();

        while self.current_token == Token::EqualEqual || self.current_token == Token::NotEqual {
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
        let mut left = self.parse_primary(); // Top of the ladder (numbers/parens)

        while self.current_token == Token::Star || self.current_token == Token::Slash {
            let op = self.current_token.clone();
            self.advance();
            let right = self.parse_primary();
            left = Expr::Binary { left: Box::new(left), op, right: Box::new(right) };
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
                let expression = self.parse_expression();
                self.expect(Token::RParen);
                
                expression
            }
            Token::Number(n) => {
                self.advance();
                Expr::Number(n)
            }
            Token::StringLiteral(s) => {
                self.advance();
                Expr::String(s)
            }
            Token::Identifier(name) => {
                self.advance();
                Expr::Variable(name)
            }
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
}