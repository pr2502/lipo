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
    #[token(":")] Colon,
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
    #[regex(r#"[a-z]?#*"[^"]*""#, string_lit)]
    StringLit,
    #[regex(r"0b[0-9_]*")]
    BinaryNumber,
    #[regex(r"0o[0-9_]*")]
    OctalNumber,
    #[regex(r"0x[0-9_]*")]
    HexadecimalNumber,
    #[regex(r"[0-9][0-9_]*")]
    DecimalNumber,
    #[regex(r"[0-9]+\.[0-9]*")]
    DecimalPointNumber,
    #[regex(r"[0-9]+(e|E)(\+|-)?[0-9_]+")]
    ExponentialNumber,

    // Keywords
    #[token("and")] And,
    #[token("assert")] Assert,
    #[token("const")] Const,
    #[token("else")] Else,
    #[token("false")] False,
    #[token("fn")] FnKw,
    #[token("for")] For,
    #[token("if")] If,
    #[token("let")] Let,
    #[token("mut")] Mut,
    #[token("not")] Not,
    #[token("or")] Or,
    #[token("print")] Print,
    #[token("return")] Return,
    #[token("self")] SelfKw,
    #[token("true")] True,
    #[token("type")] Type,
    #[token("while")] While,

    Eof,

    #[error]
    #[regex(r"[ \t\n\r]+", skip)] // whitespace
    #[regex(r"//[^\n]*", skip)] // comments
    Error,
}

pub type T = TokenKind;


/// Lex string literals with `#` delimiters
fn string_lit(lex: &mut logos::Lexer<TokenKind>) {
    let matched = lex.slice();

    // Skip the modifier char
    let matched = match matched.as_bytes() {
        [b'a'..=b'z', ..] => &matched[1..],
        _ => matched,
    };

    // The token regex starts with `#*"` so it must contain a `"` to match
    let prefix_hashes = matched.find(|c| c == '"').unwrap();

    if prefix_hashes == 0 {
        return;
    }

    if lex.remainder().starts_with(&matched[..prefix_hashes]) {
        lex.bump(prefix_hashes);
        return;
    }

    let terminator = String::with_capacity(prefix_hashes + 1) + "\"" + &matched[..prefix_hashes];

    if let Some(position) = lex.remainder().find(&terminator) {
        lex.bump(position + prefix_hashes + 1); // +1 for the '"'
    } else {
        // No string terminator found, eat the rest of the input, let the parser report an error
        lex.bump(lex.remainder().len());
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            T::LeftParen => "`(`",
            T::RightParen => "`)`",
            T::LeftBrace => "`{`",
            T::RightBrace => "`}`",
            T::Comma => "`,`",
            T::Dot => "`.`",
            T::Minus => "`-`",
            T::Plus => "`+`",
            T::Semicolon => "`;`",
            T::Colon => "`:`",
            T::Div => "`/`",
            T::Mul => "`*`",

            // One or two character tokens
            T::NotEqual => "`/=`",
            T::Equal => "`=`",
            T::EqualEqual => "`==`",
            T::Greater => "`>`",
            T::GreaterEqual => "`>=`",
            T::Less => "`<`",
            T::LessEqual => "`<=`",

            // Literals
            T::Identifier => "name",
            T::StringLit => "string literal",

            T::BinaryNumber |
            T::OctalNumber |
            T::HexadecimalNumber |
            T::DecimalNumber |
            T::DecimalPointNumber |
            T::ExponentialNumber => "number literal",

            // Keywords
            T::And => "`and`",
            T::Assert => "`assert`",
            T::Const => "`const`",
            T::Else => "`else`",
            T::False => "`false`",
            T::FnKw => "`fn`",
            T::For => "`for`",
            T::If => "`if`",
            T::Let => "`let`",
            T::Mut => "`mut`",
            T::Not => "`not`",
            T::Or => "`or`",
            T::Print => "`print`",
            T::Return => "`return`",
            T::SelfKw => "`self`",
            T::True => "`true`",
            T::Type => "`type`",
            T::While => "`while`",

            T::Eof => "EOF",
            T::Error => "invalid token",
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
/// `Option::None` with a [`T::Eof`]. Since the underlying lexer iterator is fused
/// [`next`](Lexer::next) will yield EOF indefinitely after it yields it once.
///
/// # Cloning
/// Cloning the `Lexer` creates a new lexer over the remaining input, it starts at the same token
/// as the old one and they both advance independently.
/// ```rust
/// # #![feature(assert_matches)]
/// # use lipo::lexer::{Lexer, Token, TokenKind, T};
/// # use std::assert_matches::assert_matches;
/// #
/// let src = "+ - * /";
/// let mut lex1 = Lexer::new(src);
///
/// assert_matches!(lex1.next(), Token { kind: T::Plus, .. });
/// assert_matches!(lex1.next(), Token { kind: T::Minus, .. });
///
/// let mut lex2 = lex1.clone();
///
/// assert_matches!(lex1.next(), Token { kind: T::Mul, .. });
/// assert_matches!(lex1.next(), Token { kind: T::Div, .. });
/// assert_matches!(lex1.next(), Token { kind: T::Eof, .. });
///
/// // lex2 will go through the same sequence starting from the point where it was created
/// assert_matches!(lex2.next(), Token { kind: T::Mul, .. });
/// assert_matches!(lex2.next(), Token { kind: T::Div, .. });
/// assert_matches!(lex2.next(), Token { kind: T::Eof, .. });
/// ```
#[derive(Clone)]
pub struct Lexer<'src> {
    inner: logos::Lexer<'src, TokenKind>,
    current: TokenKind,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Lexer<'src> {
        let mut inner = logos::Lexer::new(source);
        let current = inner.next().unwrap_or(T::Eof);
        Lexer { inner, current }
    }

    pub fn source(&self) -> &'src str {
        self.inner.source()
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
    #[must_use]
    pub fn next(&mut self) -> Token {
        let last = self.peek();
        self.current = self.inner.next().unwrap_or(T::Eof);
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
        if let Some(eof) = tokens.iter().position(|tok| tok == &T::Eof) {
            // Remove repeated Eof from the end
            tokens.truncate(eof + 1);
        }
        let unfinished = tokens.last().unwrap() != &T::Eof;

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
