//! AST Types
//!
//! It's more of a Concrete Syntax Tree than an Abstract one because we keep all the tokens in it,
//! but AST is often used more generally and more people understand the initialism.
//!
//! Structure is based on [Appendix I] of CraftingInterpreters.
//!
//! [Appendix I]: https://craftinginterpreters.com/appendix-i.html

use crate::lexer::Token;
use crate::object::builtins::String;
use crate::object::ObjectRef;


mod fmt;
mod spanned;


pub struct AST<'alloc> {
    pub source: ObjectRef<'alloc, String>,
    pub items: Vec<Item>,
    pub eof: Token,
}

// Items

pub enum Item {
    Class(ClassItem),
    Fn(FnItem),
    Let(LetItem),
    Statement(Statement),
    Expr(Expr),
}

pub struct ClassItem {
    pub class_tok: Token,
    pub name: Identifier,
    pub inherit: Option<ClassInherit>,
    pub open_brace: Token,
    pub methods: Vec<FnItem>,
    pub close_brace: Token,
}

pub struct ClassInherit {
    pub less_tok: Token,
    pub name: Identifier,
}

pub struct FnItem {
    pub fn_tok: Token,
    pub name: Identifier,
    pub left_paren_tok: Token,
    pub parameters: Delimited<Token, FnParam>,
    pub right_paren_tok: Token,
    pub body: Block,
}

pub struct FnParam {
    pub mut_tok: Option<Token>,
    pub name: Identifier,
}

pub struct LetItem {
    pub let_tok: Token,
    pub mut_tok: Option<Token>,
    pub rec_tok: Option<Token>,
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
    Field(FieldExpr),
    Group(GroupExpr),
    Block(Block),
    If(IfExpr),
    Call(CallExpr),
    Primary(PrimaryExpr),
}

// BinaryExpr represents the following rules from the Lox grammar:
// - assignment     : "="
// - logic_or       : "or"
// - logic_and      : "and"
// - equality       : "/=", "=="
// - comparison     : ">", ">=", "<", "<="
// - term           : "-", "+"
// - factor         : "/", "*"
pub struct BinaryExpr {
    pub lhs: Box<Expression>,
    pub operator: Token,
    pub rhs: Box<Expression>,
}

pub struct UnaryExpr {
    pub operator: Token,
    pub expr: Box<Expression>,
}

// maybe also fold this into binary expression, even though the rhs must be an Identifier
pub struct FieldExpr {
    pub expr: Box<Expression>,
    pub dot_tok: Token,
    pub field: Identifier,
}

pub struct GroupExpr {
    pub left_paren_tok: Token,
    pub expr: Option<Box<Expression>>,
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
    pub arguments: Delimited<Token, Expression>,
    pub right_paren_tok: Token,
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

/// Wrapper around Token
///
/// Parser ensures `self.token.kind == TokenKind::Ident`.
#[derive(Clone, Copy)]
pub struct Identifier {
    pub token: Token,
}
