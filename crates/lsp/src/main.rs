use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

use ngn::lexer::{Lexer, Token};

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

// Helper function to map a token to type and modifiers
fn get_semantic_type(
    token: &Token,
    prev_token: Option<&Token>,
    next_token: Option<&Token>,
) -> (u32, u32) {
    let prev_is_class_keyword = matches!(prev_token, 
        Some(Token::Enum | Token::Model | Token::Role | Token::Extend | Token::With));

    let is_likely_class = matches!(token, Token::Identifier(name) 
        if name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false));

    let is_method_call = matches!(prev_token, Some(Token::Period)) 
        && matches!(next_token, Some(Token::LParen));

    // We calculate this first so we can return it in the tuple
    let mut modifiers = vec![];

    // Declaration check
    if let Some(Token::Var | Token::Const | Token::Static) = prev_token {
        if matches!(token, Token::Identifier(_)) {
            modifiers.push("declaration");
        }
    }

    // Readonly check based on the token itself
    match token {
        Token::Const | Token::Fn | Token::Static 
        | Token::Var | Token::Bool(_) => {
            modifiers.push("readonly");
        },
        Token::Identifier(name) if name == "this" => {
            modifiers.push("readonly");
        }
        _ => {}
    }

    let mod_bitset = modifiers
        .iter()
        .map(|m| modifier_to_bit(m))
        .fold(0, |acc, bit| acc | bit);

    let token_type = match token {
        // Keywords
        Token::Break | Token::Enum | Token::Extend | Token::If 
            | Token::Match | Token::Model 
            | Token::Next | Token::Return 
            | Token::Role | Token::While | Token::Loop | Token::For | Token::In | Token::Once
            | Token::With | Token::Var | Token::Const | Token::Static | Token::Fn
            | Token::Import | Token::From | Token::As | Token::Export | Token::Default => 0,

        Token::Float(_) | Token::Number(_) => 1,  // number
        Token::StringStart | Token::StringEnd | Token::StringPart(_) => 2,  // string
        // Punctuation
        Token::LParen | Token::RParen | Token::LBracket | Token::RBracket 
            | Token::LBrace | Token::RBrace | Token::Colon | Token::Comma 
            | Token::Period | Token::DoubleColon => 6,
        Token::Echo | Token::Print | Token::Thread | Token::Channel | Token::Sleep 
            | Token::State | Token::Map | Token::Set => 10,  // builtin
        Token::Identifier(_) => {
            if prev_is_class_keyword || is_likely_class {
                12  // class
            } else if matches!(next_token, Some(Token::LParen)) || is_method_call {
                4  // function (call)
            } else {
                3  // variable (reference)
            }
        }
        Token::Bool(_) => 11, // boolean

        // Operators
        Token::Power | Token::EqualEqual | Token::NotEqual | Token::LessThanEqual | Token::GreaterThanEqual
        | Token::Plus | Token::Minus | Token::Star | Token::Slash
        | Token::Percent | Token::Equal | Token::LessThan | Token::GreaterThan
        | Token::PlusEqual | Token::MinusEqual | Token::StarEqual | Token::SlashEqual 
        | Token::PercentEqual | Token::StarStarEqual | Token::CaretEqual
        | Token::FatArrow | Token::LArrow | Token::Pipe | Token::Bang => 5,

        Token::Regex(_) => 9,
        Token::InterpolationStart | Token::InterpolationEnd => 3, // variable-like for braces
        Token::This => 0, // keyword
        _ => 3,
    };

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
        _params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        // TODO: Re-implement semantic tokens with proper position tracking
        // For now, disable semantic tokens and rely on TextMate grammar
        Ok(None)
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
