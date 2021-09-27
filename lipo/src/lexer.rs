use crate::span::FreeSpan;
use logos::{self, skip, Logos};
use std::fmt::{self, Debug, Display};
use std::iter;


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
    #[regex(r"[0-9][0-9_]*")]
    #[regex(r"[0-9]+\.[0-9]*")]
    #[regex(r"[0-9]+(e|E)(\+|-)?[0-9_]+")]
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

impl Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            TokenKind::LeftParen => "`(`",
            TokenKind::RightParen => "`)`",
            TokenKind::LeftBrace => "`{`",
            TokenKind::RightBrace => "`}`",
            TokenKind::Comma => "`,`",
            TokenKind::Dot => "`.`",
            TokenKind::Minus => "`-`",
            TokenKind::Plus => "`+`",
            TokenKind::Semicolon => "`;`",
            TokenKind::Div => "`/`",
            TokenKind::Mul => "`*`",

            // One or two character tokens
            TokenKind::NotEqual => "`/=`",
            TokenKind::Equal => "`=`",
            TokenKind::EqualEqual => "`==`",
            TokenKind::Greater => "`>`",
            TokenKind::GreaterEqual => "`>=`",
            TokenKind::Less => "`<`",
            TokenKind::LessEqual => "`<=`",

            // Literals
            TokenKind::Identifier => "name",
            TokenKind::String => "string literal",
            TokenKind::Number => "number literal",

            // Keywords
            TokenKind::And => "`and`",
            TokenKind::Assert => "`assert`",
            TokenKind::Class => "`class`",
            TokenKind::Else => "`else`",
            TokenKind::False => "`false`",
            TokenKind::For => "`for`",
            TokenKind::Fn => "`fn`",
            TokenKind::If => "`if`",
            TokenKind::Mut => "`mut`",
            TokenKind::Not => "`not`",
            TokenKind::Or => "`or`",
            TokenKind::Print => "`print`",
            TokenKind::Rec => "`rec`",
            TokenKind::Return => "`return`",
            TokenKind::Super => "`super`",
            TokenKind::This => "`this`",
            TokenKind::True => "`true`",
            TokenKind::Let => "`let`",
            TokenKind::While => "`while`",

            TokenKind::Eof => "EOF",
            TokenKind::Error => "invalid token",
        })
    }
}


#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: FreeSpan,
}


/// Lexer / scanner
///
/// Wraps the [`logos`] lexer and acts as a "peekable" adapter and replaces the iterator
/// `Option::None` with a [`TokenKind::Eof`]. Since the underlying lexer iterator is fused
/// [`next`](Lexer::next) will yield EOF indefinitely after it yields it once.
///
/// # Cloning
/// Cloning the `Lexer` creates a new lexer over the remaining input, it starts at the same token
/// as the old one and they both advance independently.
/// ```rust
/// # #![feature(assert_matches)]
/// # use lipo::lexer::{Lexer, Token, TokenKind};
/// # use std::assert_matches::assert_matches;
/// #
/// let src = "+ - * /";
/// let mut lex1 = Lexer::new(src);
///
/// assert_matches!(lex1.next(), Token { kind: TokenKind::Plus, .. });
/// assert_matches!(lex1.next(), Token { kind: TokenKind::Minus, .. });
///
/// let mut lex2 = lex1.clone();
///
/// assert_matches!(lex1.next(), Token { kind: TokenKind::Mul, .. });
/// assert_matches!(lex1.next(), Token { kind: TokenKind::Div, .. });
/// assert_matches!(lex1.next(), Token { kind: TokenKind::Eof, .. });
///
/// // lex2 will go through the same sequence starting from the point where it was created
/// assert_matches!(lex2.next(), Token { kind: TokenKind::Mul, .. });
/// assert_matches!(lex2.next(), Token { kind: TokenKind::Div, .. });
/// assert_matches!(lex2.next(), Token { kind: TokenKind::Eof, .. });
/// ```
#[derive(Clone)]
pub struct Lexer<'src> {
    inner: logos::Lexer<'src, TokenKind>,
    current: TokenKind,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Lexer<'src> {
        let mut inner = logos::Lexer::new(source);
        let current = inner.next().unwrap_or(TokenKind::Eof);
        Lexer { inner, current }
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
}

impl<'src> Debug for Lexer<'src> {
    /// Shows up to 5 next tokens that would be lexed
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const SHOW_NEXT: usize = 5;

        let mut lex = self.clone();
        let mut tokens = iter::from_fn(|| Some(lex.next().kind))
            .take(SHOW_NEXT)
            .collect::<Vec<_>>();
        if let Some(eof) = tokens.iter().position(|tok| tok == &TokenKind::Eof) {
            // Remove repeated Eof from the end
            tokens.truncate(eof + 1);
        }
        let unfinished = tokens.last().unwrap() != &TokenKind::Eof;

        write!(f, "Lexer")?;
        let mut w = f.debug_list();
        for tok in tokens {
            w.entry(&format_args!("{}", tok));
        }
        if unfinished {
            w.entry(&format_args!("..."));
        }
        w.finish()
    }
}
