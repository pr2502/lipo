//! AST Types
//!
//! It's more of a Concrete Syntax Tree than an Abstract one because we keep all the tokens in it,
//! but AST is often used more generally and more people understand the initialism.
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


pub struct AST<'alloc> {
    pub source: ObjectRef<'alloc, String>,
    pub items: Vec<Item>,
    pub eof: Token,
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
    pub fn_tok: Token,
    pub name: Identifier,
    pub left_paren_tok: Token,
    pub parameters: Delimited<Comma, FnParam>,
    pub right_paren_tok: Token,
    pub body: Block,
}

pub struct FnParam {
    pub mut_tok: Option<Token>,
    pub name: Identifier,
}

pub struct ConstItem {
    pub const_tok: Token,
    pub name: Identifier,
    pub equal_tok: Token,
    pub expr: Expression,
    pub semicolon_tok: Token,
}

pub struct LetItem {
    pub let_tok: Token,
    pub mut_tok: Option<Token>,
    pub name: Identifier,
    pub init: Option<LetInit>,
    pub semicolon_tok: Token,
}

pub struct LetInit {
    pub equal_tok: Token,
    pub expr: Expression,
}

pub struct Expr {
    pub expr: Expression,
    pub semicolon_tok: Option<Token>,
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
    pub for_tok: Token,
    pub elem: Identifier,
    pub in_tok: Token,
    pub iter: Expression,
    pub body: Block,
}

pub struct AssertStmt {
    pub assert_tok: Token,
    pub expr: Expression,
    pub semicolon_tok: Token,
}

pub struct PrintStmt {
    pub print_tok: Token,
    pub expr: Expression,
    pub semicolon_tok: Token,
}

pub struct ReturnStmt {
    pub return_tok: Token,
    pub expr: Option<Expression>,
    pub semicolon_tok: Token,
}

pub struct WhileStmt {
    pub while_tok: Token,
    pub pred: Expression,
    pub body: Block,
}

pub struct RecordExpr {
    pub left_brace_tok: Token,
    pub entries: Delimited<Comma, RecordEntry>,
    pub right_brace_tok: Token,
}

pub struct RecordEntry {
    pub name: Identifier,
    pub value: Option<EntryInit>,
}

pub struct EntryInit {
    pub colon_tok: Token,
    pub init: Box<Expression>,
}

pub struct Block {
    pub left_brace_tok: Token,
    pub body: Vec<Item>,
    pub right_brace_tok: Token,
}

// Expressions
//
// Here we deviate from the Lox grammar a bit, not in meaning but in encoding. Because encoding the
// operator precedence in the concrete syntax tree would make it very unergonomic to consume.
// Instead we make `Expression` contain all the binary and unary operations directly.

pub enum Expression {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Unit(UnitExpr),
    Group(GroupExpr),
    Tuple(TupleExpr),
    Record(RecordExpr),
    Block(Block),
    If(IfExpr),
    Call(CallExpr),
    String(StringExpr),
    Primary(PrimaryExpr),
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

pub struct UnaryExpr {
    pub operator: Token,
    pub expr: Box<Expression>,
}

pub struct UnitExpr {
    pub left_paren_tok: Token,
    pub right_paren_tok: Token,
}

pub struct GroupExpr {
    pub left_paren_tok: Token,
    pub expr: Box<Expression>,
    pub right_paren_tok: Token,
}

pub struct TupleExpr {
    pub left_paren_tok: Token,
    pub exprs: Delimited<Comma, Expression>,
    pub right_paren_tok: Token,
}

pub struct IfExpr {
    pub if_tok: Token,
    pub pred: Box<Expression>,
    pub body: Block,
    pub else_branch: Option<ElseBranch>,
}

pub struct ElseBranch {
    pub else_tok: Token,
    pub body: Block,
}

pub struct CallExpr {
    pub callee: Box<Expression>,
    pub left_paren_tok: Token,
    pub arguments: Delimited<Comma, Expression>,
    pub right_paren_tok: Token,
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
        ident: Identifier,
        fmt: Option<FreeSpan>,
    },
}

// PrimaryExpr can have the following tokens:
// - True, False
// - This, Super
// - Number, String, Identifier
pub struct PrimaryExpr {
    pub token: Token,
}

/// List of `Item`s separated by `Delim`eters.
///
/// Parser ensures that `items.len() == delim.len() == 0` for an empty list,
/// `items.len() == delim.len() + 1` or `item.len() == delim.len()` for a non empty list with an
/// optional trailing `Delim`eter.
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

/// Specialized Token where kind == Identifier
#[derive(Clone, Copy)]
pub struct Identifier {
    pub span: FreeSpan,
}

/// Specialized Token where kind == Comma
#[derive(Clone, Copy)]
pub struct Comma {
    pub span: FreeSpan,
}
