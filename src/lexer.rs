use crate::span::FreeSpan;
use logos::{self, skip, Logos};


#[derive(Logos, Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    // Single-character tokens
    #[token("(")] LeftParen,
    #[token(")")] RightParen,
    #[token("{")] LeftBrace,
    #[token("}")] RightBrace,
    #[token(",")] Comma,
    #[token(".")] Dot,
    #[token("-")] Minus,
    #[token("+")] Plus,
    #[token(";")] Semicolon,
    #[token("/")] Div,
    #[token("*")] Mul,

    // One or two character tokens
    #[token("/=")] NotEqual,
    #[token("=")] Equal,
    #[token("==")] EqualEqual,
    #[token(">")] Greater,
    #[token(">=")] GreaterEqual,
    #[token("<")] Less,
    #[token("<=")] LessEqual,

    // Literals
    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*")]
    Identifier,
    #[regex(r#""[^"]*""#)]
    String,
    #[regex(r"[0-9]+(\.[0-9]*)?")]
    Number,

    // Keywords
    #[token("and")] And,
    #[token("assert")] Assert,
    #[token("class")] Class,
    #[token("else")] Else,
    #[token("false")] False,
    #[token("for")] For,
    #[token("fn")] Fn,
    #[token("if")] If,
    #[token("mut")] Mut,
    #[token("not")] Not,
    #[token("or")] Or,
    #[token("print")] Print,
    #[token("rec")] Rec,
    #[token("return")] Return,
    #[token("super")] Super,
    #[token("this")] This,
    #[token("true")] True,
    #[token("let")] Let,
    #[token("while")] While,

    Eof,

    #[error]
    #[regex(r"[ \t\n\r]+", skip)] // whitespace
    #[regex(r"//[^\n]*", skip)] // comments
    Error,
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: FreeSpan,
}

pub struct Lexer<'src> {
    inner: logos::Lexer<'src, TokenKind>,
    current: TokenKind,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Lexer<'src> {
        let mut inner = logos::Lexer::new(source);
        let current = inner.next().unwrap_or(TokenKind::Eof);
        Lexer { current, inner }
    }

    /// Returns the current token
    pub fn peek(&self) -> Token {
        Token {
            kind: self.current,
            span: self.inner.span().into(),
        }
    }

    /// Returns the current token and advance the lexer
    #[allow(clippy::should_implement_trait)] // Iterator returns an Option we always return a Token
    pub fn next(&mut self) -> Token {
        let last = self.peek();
        self.current = self.inner.next().unwrap_or(TokenKind::Eof);
        last
    }

    pub fn source(&self) -> &'src str {
        self.inner.source()
    }
}
