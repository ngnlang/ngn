use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

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
        "mutable" => 1 << 3,
        _ => 0,
    }
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
                                    "keyword".into(), // 0
                                    "number".into(), // 1
                                    "string".into(), // 2
                                    "variable".into(), // 3
                                    "function".into(), // 4
                                    "operator".into(), // 5
                                    "punctuation".into(), // 6
                                    "comment".into(), // 7
                                    "type".into(), // 8
                                    "regex".into(), // 9
                                    "builtin".into(), // 10
                                    "boolean".into(), // 11
                                    "class".into(), // 12
                                ],
                                token_modifiers: vec![
                                    "declaration".into(),
                                    "readonly".into(),
                                    "static".into(),
                                    "mutable".into(),
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
            .log_message(MessageType::INFO, "server initialized!")
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
        
        let tokens = ngn::lexer::tokenize(&text);
        
        let mut semantic_tokens = Vec::new();
        let mut prev_line = 0u32;
        let mut prev_char = 0u32;
        let mut prev_token: Option<ngn::Token> = None;
        
        for m in 0..tokens.len() {
            let (start, token, end) = &tokens[m];
            let next_token = tokens.get(m + 1).map(|(_, t, _)| t);

            // Calculate line and char from start position
            let mut line = 0u32;
            let mut char = 0u32;
            for (i, c) in text.chars().enumerate() {
                if i >= *start {
                    break;
                }
                if c == '\n' {
                    line += 1;
                    char = 0;
                } else {
                    char += 1;
                }
            }
            
            let delta_line = line - prev_line;
            let delta_start = if delta_line == 0 {
                char - prev_char
            } else {
                char
            };

            let mut modifiers = vec![];
    
            // If previous token was a declaration keyword, mark this ident as declaration
            if let Some(ngn::Token::Var | ngn::Token::Const | ngn::Token::Lit | ngn::Token::Rebind | ngn::Token::Static) = prev_token {
                if matches!(token, ngn::Token::Ident(_)) {
                    modifiers.push("declaration");
                }
            }

            let prev_is_class_keyword = matches!(prev_token, 
                Some(ngn::Token::Enum | ngn::Token::Model | ngn::Token::Role | ngn::Token::Extend | ngn::Token::With));

            let is_likely_class = matches!(token, ngn::Token::Ident(name) 
                if name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false));

            let is_method_call = matches!(prev_token, Some(ngn::Token::Period)) 
                && matches!(next_token, Some(ngn::Token::LParen));

            // Map tokens to semantic token types
            let token_type = match token {
                // Keywords
                ngn::Token::Any | ngn::Token::Break | ngn::Token::Enum | ngn::Token::Extend | ngn::Token::If 
                    | ngn::Token::Match | ngn::Token::Model 
                    | ngn::Token::Next | ngn::Token::Not | ngn::Token::Return 
                    | ngn::Token::Role | ngn::Token::Until | ngn::Token::While 
                    | ngn::Token::With => 0,

                ngn::Token::Float(_) | ngn::Token::Integer(_) => 1,  // number
                ngn::Token::String(_) => 2,  // string
                // Punctuation
                ngn::Token::LParen | ngn::Token::RParen | ngn::Token::LBracket | ngn::Token::RBracket 
                    | ngn::Token::LBrace | ngn::Token::RBrace | ngn::Token::Colon | ngn::Token::Comma 
                    | ngn::Token::Period => 6,

                ngn::Token::InterpolatedString(parts) => {
                    // 1. Push the opening quote
                    semantic_tokens.push(SemanticToken {
                        delta_line,
                        delta_start,
                        length: 1,
                        token_type: 2, // string
                        token_modifiers_bitset: 0,
                    });

                    // We track the *start* position of the previously emitted sub-token
                    // relative to the start of the whole string.
                    // The opening quote started at index 0.
                    let mut last_sub_token_start = 0;
                    let mut current_pos = 1; // We are now past the opening quote

                    for part in parts {
                        match part {
                            ngn::lexer::InterpolationToken::Literal(s) => {
                                let len = s.len() as u32;
                                if len > 0 {
                                    semantic_tokens.push(SemanticToken {
                                        delta_line: 0,
                                        delta_start: current_pos - last_sub_token_start,
                                        length: len,
                                        token_type: 2, // string
                                        token_modifiers_bitset: 0,
                                    });
                                    last_sub_token_start = current_pos;
                                    current_pos += len;
                                }
                            }
                            ngn::lexer::InterpolationToken::Variable(name) => {
                                // Opening brace '{'
                                semantic_tokens.push(SemanticToken {
                                    delta_line: 0,
                                    delta_start: current_pos - last_sub_token_start,
                                    length: 1,
                                    token_type: 3, // variable
                                    token_modifiers_bitset: 2,
                                });
                                last_sub_token_start = current_pos;
                                current_pos += 1;

                                // Variable Name
                                let name_len = name.len() as u32;
                                
                                if name.starts_with("this.") {
                                    // "this" keyword
                                    semantic_tokens.push(SemanticToken {
                                        delta_line: 0,
                                        delta_start: current_pos - last_sub_token_start,
                                        length: 4,
                                        token_type: 3, // variable
                                        token_modifiers_bitset: 2,
                                    });
                                    last_sub_token_start = current_pos;
                                    current_pos += 4;

                                    // "." punctuation
                                    semantic_tokens.push(SemanticToken {
                                        delta_line: 0,
                                        delta_start: current_pos - last_sub_token_start,
                                        length: 1,
                                        token_type: 6, // punctuation
                                        token_modifiers_bitset: 0,
                                    });
                                    last_sub_token_start = current_pos;
                                    current_pos += 1;

                                    // property name
                                    semantic_tokens.push(SemanticToken {
                                        delta_line: 0,
                                        delta_start: current_pos - last_sub_token_start,
                                        length: name_len - 5,
                                        token_type: 3, // variable
                                        token_modifiers_bitset: 0,
                                    });
                                    last_sub_token_start = current_pos;
                                    current_pos += name_len - 5;
                                } else {
                                    // Standard variable
                                    semantic_tokens.push(SemanticToken {
                                        delta_line: 0,
                                        delta_start: current_pos - last_sub_token_start,
                                        length: name_len,
                                        token_type: 3, // variable
                                        token_modifiers_bitset: 0,
                                    });
                                    last_sub_token_start = current_pos;
                                    current_pos += name_len;
                                }

                                // Closing brace '}'
                                semantic_tokens.push(SemanticToken {
                                    delta_line: 0,
                                    delta_start: current_pos - last_sub_token_start,
                                    length: 1,
                                    token_type: 3, // variable
                                    token_modifiers_bitset: 2,
                                });
                                last_sub_token_start = current_pos;
                                current_pos += 1;
                            }
                        }
                    }

                    // Closing quote
                    semantic_tokens.push(SemanticToken {
                        delta_line: 0,
                        delta_start: current_pos - last_sub_token_start,
                        length: 1,
                        token_type: 2, // string
                        token_modifiers_bitset: 0,
                    });

                    // CRITICAL: Synchronize the outer loop state.
                    // The 'prev_char' for the NEXT token in the file (like a newline or semicolon)
                    // must be the start position of the LAST token we just emitted.
                    // The last token was the Closing Quote.
                    // It started at 'char + current_pos'.
                    prev_line = line;
                    prev_char = char + current_pos; 
                    
                    // Ensure we update prev_token so logic relying on it works for the next item
                    prev_token = Some(token.clone());
                    
                    continue; 
                }
                ngn::Token::Echo | ngn::Token::Print => 8,  // type
                ngn::Token::Ident(_) => {
                    if prev_is_class_keyword | is_likely_class {
                        12  // class
                    } else if is_likely_class {
                        8
                    } else if matches!(next_token, Some(ngn::Token::LParen)) | is_method_call {
                        4  // function (call)
                    } else {
                        3  // variable (reference)
                    }
                }
                ngn::Token::Comment(_) => 7, // Comment

                // Operators
                ngn::Token::StarStar | ngn::Token::EqEq | ngn::Token::NotEq | ngn::Token::LessEq | ngn::Token::GreaterEq
                | ngn::Token::Or | ngn::Token::And | ngn::Token::Plus | ngn::Token::Minus | ngn::Token::Star | ngn::Token::Slash
                | ngn::Token::Percent | ngn::Token::Caret | ngn::Token::Eq | ngn::Token::Less | ngn::Token::Greater
                | ngn::Token::PlusEq | ngn::Token::MinusEq | ngn::Token::StarEq | ngn::Token::SlashEq 
                | ngn::Token::PercentEq | ngn::Token::ShortReturn | ngn::Token::StarStarEq | ngn::Token::CaretEq | ngn::Token::OwnedAssign => 5,

                ngn::Token::Newline => {
                    prev_token = Some(token.clone());
                    continue;
                }, // Skip
                ngn::Token::Regex(_) => 9,
                _ => 3, // Variable
            };

            // Apply modifiers based on token type
            match &token {
                ngn::Token::Const | ngn::Token::Fn | ngn::Token::Static | ngn::Token::Lit 
                    | ngn::Token::Var | ngn::Token::Rebind | ngn::Token::False | ngn::Token::True => {
                    modifiers.push("readonly");
                }
                _ => {}
            }

            let token_modifiers_bitset = modifiers
                .iter()
                .map(|m| modifier_to_bit(m))
                .fold(0, |acc, bit| acc | bit);
            
            semantic_tokens.push(SemanticToken {
                delta_line,
                delta_start,
                length: (end - start) as u32,
                token_type,
                token_modifiers_bitset
            });
            
            prev_line = line;
            prev_char = char;
            prev_token = Some(token.clone())
        }
        
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
