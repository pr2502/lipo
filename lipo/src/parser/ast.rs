//! AST Types
//!
//! It's more of a Concrete Syntax Tree than an Abstract one because we keep all
//! the tokens in it, but AST is often used more generally and more people
//! understand the initialism.
//!
//! Structure is based on [Appendix I] of CraftingInterpreters.
//!
//! [Appendix I]: https://craftinginterpreters.com/appendix-i.html

use crate::builtins::String;
use crate::lexer::Token;
use crate::span::FreeSpan;
use crate::ObjectRef;


mod fmt;
mod spanned;


pub struct Ast<'alloc> {
    pub source: ObjectRef<'alloc, String>,
    pub items: Vec<Item>,
    pub eof: Eof,
}

// Items

pub enum Item {
    Fn(FnItem),
    Const(ConstItem),
    Let(LetItem),
    Statement(Statement),
    Expr(Expr),
}

pub struct FnItem {
    pub fn_kw: FnKw,
    pub name: Name,
    pub parens: Parens,
    pub parameters: Delimited<Comma, FnParam>,
    pub body: Block,
}

pub struct FnParam {
    pub mut_tok: Option<Mut>,
    pub name: Name,
}

pub struct ConstItem {
    pub const_tok: Const,
    pub name: Name,
    pub equal_tok: Equal,
    pub expr: Expression,
    pub semicolon_tok: Semicolon,
}

pub struct LetItem {
    pub let_tok: Let,
    pub mut_tok: Option<Mut>,
    pub name: Name,
    pub init: Option<LetInit>,
    pub semicolon_tok: Semicolon,
}

pub struct LetInit {
    pub equal_tok: Equal,
    pub expr: Expression,
}

pub struct Expr {
    pub expr: Expression,
    pub semicolon_tok: Option<Semicolon>,
}

// Statements

pub enum Statement {
    For(ForStmt),
    Assert(AssertStmt),
    Print(PrintStmt),
    Return(ReturnStmt),
    While(WhileStmt),
}

pub struct ForStmt {
    pub for_tok: For,
    pub elem: Name,
    pub in_tok: In,
    pub iter: Expression,
    pub body: Block,
}

pub struct AssertStmt {
    pub assert_tok: Assert,
    pub expr: Expression,
    pub semicolon_tok: Semicolon,
}

pub struct PrintStmt {
    pub print_tok: Print,
    pub expr: Expression,
    pub semicolon_tok: Semicolon,
}

pub struct ReturnStmt {
    pub return_tok: Return,
    pub expr: Option<Expression>,
    pub semicolon_tok: Semicolon,
}

pub struct WhileStmt {
    pub while_tok: While,
    pub pred: Expression,
    pub body: Block,
}

// Expressions
//
// Here we deviate from the Lox grammar a bit, not in meaning but in encoding.
// Because encoding the operator precedence in the concrete syntax tree would
// make it very unergonomic to consume. Instead we make `Expression` contain all
// the binary and unary operations directly.

pub enum Expression {
    Unit(UnitExpr),
    Primary(PrimaryExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Group(GroupExpr),
    Tuple(TupleExpr),
    Record(RecordExpr),
    Block(Block),
    If(IfExpr),
    Fn(FnExpr),
    Call(CallExpr),
    String(StringExpr),
}

pub struct UnitExpr {
    pub parens: Parens,
}

pub enum PrimaryExpr {
    Name(Name),
    True(True),
    False(False),
    BinaryNumber(BinaryNumber),
    OctalNumber(OctalNumber),
    HexadecimalNumber(HexadecimalNumber),
    DecimalNumber(DecimalNumber),
    DecimalPointNumber(DecimalPointNumber),
    ExponentialNumber(ExponentialNumber),
}

pub struct UnaryExpr {
    pub operator: Token,
    pub expr: Box<Expression>,
}

// BinaryExpr represents the following rules from the Lox grammar:
// - assignment     : `=`
// - logic_or       : `or`
// - logic_and      : `and`
// - equality       : `/=`, `==`
// - comparison     : `>`, `>=`, `<`, `<=`
// - term           : `-`, `+`
// - factor         : `/`, `*`
// - call           : `.`
pub struct BinaryExpr {
    pub lhs: Box<Expression>,
    pub operator: Token,
    pub rhs: Box<Expression>,
}

pub struct GroupExpr {
    pub parens: Parens,
    pub expr: Box<Expression>,
}

pub struct TupleExpr {
    pub parens: Parens,
    pub exprs: Delimited<Comma, Expression>,
}

pub struct RecordExpr {
    pub braces: Braces,
    pub entries: Delimited<Comma, RecordEntry>,
}

pub struct RecordEntry {
    pub name: Name,
    pub value: Option<EntryInit>,
}

pub struct EntryInit {
    pub colon_tok: Colon,
    pub init: Box<Expression>,
}

pub struct Block {
    pub braces: Braces,
    pub body: Vec<Item>,
}

pub struct IfExpr {
    pub if_tok: If,
    pub pred: Box<Expression>,
    pub body: Block,
    pub else_branch: Option<ElseBranch>,
}

pub struct ElseBranch {
    pub else_tok: Else,
    pub body: Block,
}

pub struct FnExpr {
    pub fn_tok: FnKw,
    pub parens: Parens,
    pub parameters: Delimited<Comma, FnParam>,
    pub body: Box<Expression>,
}

pub struct CallExpr {
    pub callee: Box<Expression>,
    pub parens: Parens,
    pub arguments: Delimited<Comma, Expression>,
}

pub struct StringExpr {
    pub modifier_span: Option<FreeSpan>,
    pub open_delim_span: FreeSpan,
    pub fragments: Vec<StringFragment>,
    pub close_delim_span: FreeSpan,
}

pub enum StringFragment {
    Literal {
        span: FreeSpan,
        unescaped: std::string::String,
    },
    Interpolation {
        name: Name,
        fmt: Option<FreeSpan>,
    },
}

/// List of `Item`s separated by `Delim`eters.
///
/// Parser ensures that `items.len() == delim.len() == 0` for an empty list,
/// `items.len() == delim.len() + 1` or `item.len() == delim.len()` for a non
/// empty list with an optional trailing `Delim`eter.
pub struct Delimited<Delim, Item> {
    pub items: Vec<Item>,
    pub delim: Vec<Delim>,
}

impl<D, I> Default for Delimited<D, I> {
    fn default() -> Self {
        Delimited {
            items: Vec::default(),
            delim: Vec::default(),
        }
    }
}

/// It's unnecessary to store the whole `Token` in the AST when we statically
/// know what `TokenKind` it must be. So we have newtypes for specific
/// `TokenKind`s which only hold their `span`.
// At some point we may want to optimize this further and only store the offset
// instead of the whole span as for most of these their lenght is statically
// known.
macro_rules! specialized_tokens {
    ( $($name:ident),* $(,)? ) => {
        $(
            #[derive(Clone, Copy)]
            pub struct $name {
                pub span: FreeSpan,
            }

            impl From<Token> for $name {
                fn from(tok: Token) -> Self {
                    assert!(tok.kind == crate::lexer::TokenKind::$name);
                    $name { span: tok.span }
                }
            }
        )*
    };
}

specialized_tokens! {
    Assert,
    Colon,
    Comma,
    Const,
    Else,
    Eof,
    Equal,
    False,
    FnKw,
    For,
    If,
    In,
    LeftBrace,
    LeftParen,
    Let,
    Mut,
    Name,
    Print,
    Return,
    RightBrace,
    RightParen,
    Semicolon,
    True,
    While,
    BinaryNumber,
    OctalNumber,
    HexadecimalNumber,
    DecimalNumber,
    DecimalPointNumber,
    ExponentialNumber,
}

#[derive(Clone, Copy)]
pub struct Number {
    pub span: FreeSpan,
}

#[derive(Clone, Copy)]
pub struct Parens {
    pub left: LeftParen,
    pub right: RightParen,
}

#[derive(Clone, Copy)]
pub struct Braces {
    pub left: LeftBrace,
    pub right: RightBrace,
}
