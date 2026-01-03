use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

use ngn::lexer::{Lexer, Token, Span};

#[derive(Debug)]
struct Backend {
    client: Client,
    documents: Arc<RwLock<HashMap<String, String>>>,
}

fn modifier_to_bit(modifier: &str) -> u32 {
    match modifier {
        "declaration" => 1 << 0,
        "readonly" => 1 << 1,
        "static" => 1 << 2,
        "defaultLibrary" => 1 << 3,
        _ => 0,
    }
}

// Helper function to map a token to type and modifiers
fn get_semantic_type(
    token: &Token,
    prev_token: Option<&Token>,
    next_token: Option<&Token>,
    is_constant: bool,
) -> (u32, u32) {
    let prev_is_class_keyword = matches!(prev_token, 
        Some(Token::Enum | Token::Model | Token::Role | Token::Extend | Token::With));

    let is_likely_class = matches!(token, Token::Identifier(name) 
        if name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false));

    let is_method_call = matches!(prev_token, Some(Token::Period)) 
        && matches!(next_token, Some(Token::LParen));

    let mut modifiers = vec![];

    // Declaration check - determine if we are in a declaration
    if let Some(Token::Const | Token::Static) = prev_token {
        if matches!(token, Token::Identifier(_)) {
            modifiers.push("declaration");
            modifiers.push("readonly");
        }
    } else if let Some(Token::Var) = prev_token {
        if matches!(token, Token::Identifier(_)) {
            modifiers.push("declaration");
        }
    }

    // Modifiers for the tokens themselves
    match token {
        Token::Var | Token::Const | Token::Static => {
            modifiers.push("declaration");
        }
        Token::Bool(_) => {
            modifiers.push("readonly");
        },
        Token::This => {
            modifiers.push("readonly");
        }
        Token::Echo | Token::Print | Token::Thread | Token::Channel | Token::Sleep 
            | Token::State | Token::Map | Token::Set => {
            modifiers.push("defaultLibrary");
        }
        Token::Identifier(_) => {
            if is_constant {
                modifiers.push("readonly");
            }
        }
        _ => {}
    }

    let token_type = match token {
        // Keywords
        Token::Var | Token::Const | Token::Static | Token::Break | Token::Enum 
            | Token::Extend | Token::If | Token::Match | Token::Model 
            | Token::Next | Token::Return | Token::Role | Token::While 
            | Token::Loop | Token::For | Token::In | Token::Once
            | Token::With | Token::Fn
            | Token::Import | Token::From | Token::As | Token::Export | Token::Default | Token::This => 0, // keyword

        Token::Float(_) | Token::Number(_) => 1,  // number
        Token::StringStart | Token::StringEnd | Token::StringPart(_) => 2,  // string
        // Punctuation
        Token::LParen | Token::RParen | Token::LBracket | Token::RBracket 
            | Token::LBrace | Token::RBrace | Token::Colon | Token::Comma 
            | Token::Period | Token::DoubleColon => 6,
        Token::Echo | Token::Print | Token::Thread | Token::Channel | Token::Sleep 
            | Token::State | Token::Map | Token::Set => 4, // function
        Token::Identifier(_) => {
            if prev_is_class_keyword || is_likely_class {
                9  // class
            } else if matches!(next_token, Some(Token::LParen)) || is_method_call {
                4  // function
            } else {
                3  // variable
            }
        }
        Token::Bool(_) => 0, // keyword (for fallback)

        // Operators
        Token::Power | Token::EqualEqual | Token::NotEqual | Token::LessThanEqual | Token::GreaterThanEqual
        | Token::Plus | Token::Minus | Token::Star | Token::Slash
        | Token::Percent | Token::Equal | Token::LessThan | Token::GreaterThan
        | Token::PlusEqual | Token::MinusEqual | Token::StarEqual | Token::SlashEqual 
        | Token::PercentEqual | Token::StarStarEqual | Token::CaretEqual
        | Token::FatArrow | Token::LArrow | Token::Pipe | Token::Bang => 5,

        Token::Regex(_) => 10,
        Token::InterpolationStart | Token::InterpolationEnd => 3, // variable-like for braces
        _ => 3,
    };

    let mod_bitset = modifiers
        .iter()
        .map(|m| modifier_to_bit(m))
        .fold(0, |acc, bit| acc | bit);

    (token_type, mod_bitset)
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: SemanticTokensLegend {
                                token_types: vec![
                                    "keyword".into(),    // 0
                                    "number".into(),     // 1
                                    "string".into(),     // 2
                                    "variable".into(),   // 3
                                    "function".into(),   // 4
                                    "operator".into(),   // 5
                                    "punctuation".into(),// 6
                                    "comment".into(),    // 7
                                    "type".into(),       // 8
                                    "class".into(),      // 9
                                    "regexp".into(),     // 10
                                ],
                                token_modifiers: vec![
                                    "declaration".into(),    // 0
                                    "readonly".into(),       // 1
                                    "static".into(),         // 2
                                    "defaultLibrary".into(), // 3
                                ],
                            },
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            ..Default::default()
                        },
                    ),
                ),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "ngn-lsp server initialized!")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        let text = params.text_document.text;
        self.documents.write().await.insert(uri, text);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        if let Some(change) = params.content_changes.last() {
            self.documents.write().await.insert(uri, change.text.clone());
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_string();
    
        let documents = self.documents.read().await;
        let text = match documents.get(&uri) {
            Some(t) => t.clone(),
            None => return Ok(None),
        };
        drop(documents);
        
        // Collect all tokens with their spans
        let mut lexer = Lexer::new(&text);
        let mut tokens_with_spans: Vec<(Token, Span)> = Vec::new();
        
        loop {
            let (token, span) = lexer.next_token_with_span();
            if token == Token::EOF {
                break;
            }
            tokens_with_spans.push((token, span));
        }
        
        // Scope-based constant tracking
        let mut scopes: Vec<std::collections::HashSet<String>> = vec![std::collections::HashSet::new()]; // Global scope

        // Build line/column index from the source (character-based for LSP)
        let mut line_starts = vec![0];
        let mut char_count = 0;
        for (i, c) in text.char_indices() {
            char_count += 1;
            if c == '\n' {
                line_starts.push(char_count);
            }
        }

        // Helper to convert byte offset to (line, character_offset)
        let byte_to_line_col = |byte_offset: usize| -> (u32, u32) {
            let mut current_line = 0;
            let mut current_line_start_byte = 0;
            let mut line_count = 0;
            
            for (i, c) in text.char_indices() {
                if i >= byte_offset {
                    break;
                }
                if c == '\n' {
                    line_count += 1;
                    current_line_start_byte = i + 1;
                }
            }
            
            // Re-calculate character offset in the line
            let line_text = &text[current_line_start_byte..byte_offset];
            let char_offset = line_text.chars().count();
            
            (line_count, char_offset as u32)
        };
        
        let mut semantic_tokens = Vec::new();
        let mut prev_line = 0u32;
        let mut prev_char = 0u32;
        
        for i in 0..tokens_with_spans.len() {
            let (ref token, span) = tokens_with_spans[i];
            let prev_token = if i > 0 { Some(&tokens_with_spans[i - 1].0) } else { None };
            let next_token = tokens_with_spans.get(i + 1).map(|(t, _)| t);

            // Skip newlines in semantic tokens
            if matches!(token, Token::Newline) {
                continue;
            }

            let (line, char) = byte_to_line_col(span.start);
            
            let delta_line = line - prev_line;
            let delta_start = if delta_line == 0 {
                char - prev_char
            } else {
                char
            };

            // Handle Scopes
            match token {
                Token::LBrace => {
                    scopes.push(std::collections::HashSet::new());
                }
                Token::RBrace => {
                    if scopes.len() > 1 {
                        scopes.pop();
                    }
                }
                Token::Identifier(name) => {
                    // Check if declaration of const/static
                    if let Some(Token::Const | Token::Static) = prev_token {
                         if let Some(current_scope) = scopes.last_mut() {
                             current_scope.insert(name.clone());
                         }
                    }
                    // Handle variable shadowing (remove from current scope if var)
                    else if let Some(Token::Var) = prev_token {
                         if let Some(current_scope) = scopes.last_mut() {
                             current_scope.remove(name);
                         }
                    }
                }
                _ => {}
            }

            // Determine if current identifier is a constant lookup
            let mut is_constant = false;
            if let Token::Identifier(name) = token {
                // Determine if declaration (in which case modifiers handle it) or usage
                let is_decl = matches!(prev_token, Some(Token::Const | Token::Static | Token::Var));
                if !is_decl {
                    // Look up in scopes from top to bottom
                    for scope in scopes.iter().rev() {
                        if scope.contains(name) {
                            is_constant = true;
                            break;
                        }
                    }
                }
            }

            let (token_type, token_modifiers_bitset) = get_semantic_type(
                token, 
                prev_token, 
                next_token,
                is_constant
            );
            
            let token_text = &text[span.start..span.end];
            
            // Special handling for Regex with flags (split into regex body + flags)
            if let Token::Regex(content) = token {
                if let Some(last_slash_idx) = content.rfind('/') {
                    if last_slash_idx < content.len() - 1 {
                        // We have flags
                        let body_len = (text[span.start..(span.start + last_slash_idx + 1)].chars().count()) as u32;
                        let flags_len = (text[(span.start + last_slash_idx + 1)..span.end].chars().count()) as u32;
                        
                        // Emit body (regexp)
                        semantic_tokens.push(SemanticToken {
                            delta_line,
                            delta_start,
                            length: body_len,
                            token_type: 10, // regexp
                            token_modifiers_bitset: 0,
                        });
                        
                        // Emit flags (keyword.control or similar)
                        semantic_tokens.push(SemanticToken {
                            delta_line: 0,
                            delta_start: body_len,
                            length: flags_len,
                            token_type: 0,  // keyword
                            token_modifiers_bitset: 0,
                        });
                        
                        // Update tracking for next token in loop
                        prev_line = line;
                        prev_char = char + body_len + flags_len; // Approx advance, though loop resets this
                        continue;
                    }
                }
            }

            let length = token_text.chars().count() as u32;
            
            semantic_tokens.push(SemanticToken {
                delta_line,
                delta_start,
                length,
                token_type,
                token_modifiers_bitset
            });
            
            prev_line = line;
            prev_char = char;
        }

        eprintln!("Generated {} semantic tokens for {}", semantic_tokens.len(), uri);
        
        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: semantic_tokens,
        })))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        documents: Arc::new(RwLock::new(HashMap::new())),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
