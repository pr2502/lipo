use crate::default;
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
    #[token("/")] Slash,
    #[token("*")] Star,

    // One or two character tokens
    #[token("!")] Bang,
    #[token("!=")] BangEqual,
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
    #[token("class")] Class,
    #[token("else")] Else,
    #[token("false")] False,
    #[token("for")] For,
    #[token("fun")] Fun,
    #[token("if")] If,
    #[token("nil")] Nil,
    #[token("or")] Or,
    #[token("print")] Print,
    #[token("return")] Return,
    #[token("super")] Super,
    #[token("this")] This,
    #[token("true")] True,
    #[token("var")] Var,
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

impl Default for Token {
    fn default() -> Token {
        Token {
            kind: TokenKind::Error,
            span: default(),
        }
    }
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Lexer<'src> {
        let mut inner = logos::Lexer::new(source);
        let current = inner.next().unwrap_or(TokenKind::Eof);
        Lexer { current, inner }
    }

    pub fn peek(&self) -> Token {
        Token {
            kind: self.current,
            span: self.inner.span().into(),
        }
    }

    pub fn next(&mut self) -> Token {
        self.current = self.inner.next().unwrap_or(TokenKind::Eof);
        self.peek()
    }

    pub fn source(&self) -> &str {
        self.inner.source()
    }
}
