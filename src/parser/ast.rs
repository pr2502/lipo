//! AST Types
//!
//! It's more of a Concrete Syntax Tree than an Abstract one because we keep all the tokens in it,
//! but AST is often used more generally and more people understand the initialism.
//!
//! Structure is based on [Appendix I] of CraftingInterpreters.
//!
//! [Appendix I]: https://craftinginterpreters.com/appendix-i.html

use crate::lexer::Token;


mod fmt;
mod spanned;


pub type Program = Vec<Declaration>;

// Declarations

pub enum Declaration {
    Class(ClassDecl),
    Fun(FunDecl),
    Var(VarDecl),
    Statement(Statement),
}

pub struct ClassDecl {
    pub class_tok: Token,
    pub ident: Identifier,
    pub inherit: Option<ClassInherit>,
    pub open_brace: Token,
    pub methods: Vec<Function>,
    pub close_brace: Token,
}

pub struct ClassInherit {
    pub less_tok: Token,
    pub ident: Identifier,
}

pub struct FunDecl {
    pub fun_tok: Token,
    pub function: Function,
}

pub struct Function {
    pub name: Identifier,
    pub left_paren_tok: Token,
    pub parameters: Delimited<Token, Identifier>,
    pub body: Block,
}

pub struct VarDecl {
    pub var_tok: Token,
    pub ident: Identifier,
    pub init: Option<VarInit>,
    pub semicolon_tok: Token,
}

pub struct VarInit {
    pub equal_tok: Token,
    pub expr: Expression,
}

// Statements

pub enum Statement {
    Expr(ExprStmt),
    For(ForStmt),
    If(IfStmt),
    Assert(AssertStmt),
    Print(PrintStmt),
    Return(ReturnStmt),
    While(WhileStmt),
    Block(Block),
}

pub struct ExprStmt {
    pub expr: Expression,
    pub semicolon_tok: Token,
}

pub struct ForStmt {
    pub for_tok: Token,
    pub left_paren_tok: Token,
    pub init: ForInit,
    pub cond: Option<Expression>,
    pub semicolon_tok: Token,
    pub incr: Option<Expression>,
    pub right_paren_tok: Token,
    pub body: Box<Statement>,
}

pub enum ForInit {
    Var(VarDecl),
    Stmt(ExprStmt),
    Empty {
        semicolon_tok: Token,
    }
}

pub struct IfStmt {
    pub if_tok: Token,
    pub pred: Expression,
    pub body: Block,
    pub else_branch: Option<ElseBranch>,
}

pub struct ElseBranch {
    pub else_tok: Token,
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
    pub expr: Expression,
    pub semicolon_tok: Token,
}

pub struct WhileStmt {
    pub while_tok: Token,
    pub pred: Expression,
    pub body: Block,
}

pub struct Block {
    pub left_brace_tok: Token,
    pub body: Vec<Declaration>,
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
    Call(CallExpr),
    Primary(PrimaryExpr),
}

// BinaryExpr represents the following rules from the Lox grammar:
// - assignment     : "="
// - logic_or       : "or"
// - logic_and      : "and"
// - equality       : "!=", "=="
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
    pub expr: Box<Expression>,
    pub right_paren_tok: Token,
}

pub struct CallExpr {
    pub fun: Box<Expression>,
    pub left_paren_tok: Token,
    pub arguments: Delimited<Token, Expression>,
    pub right_paren_tok: Token,
}

// PrimaryExpr can have the following tokens:
// - Nil
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

/// Wrapper around Token
///
/// Parser ensures `self.ident_tok.kind == TokenKind::Ident`.
#[derive(Clone, Copy)]
pub struct Identifier {
    pub token: Token,
}
