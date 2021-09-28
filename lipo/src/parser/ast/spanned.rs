//! Implement [`Spanned`] for the AST.

use super::*;
use crate::span::{FreeSpan, Spanned};


fn join(a: FreeSpan, b: FreeSpan) -> FreeSpan {
    FreeSpan::join(a, b)
}

impl Spanned for Item {
    fn span(&self) -> FreeSpan {
        match self {
            Item::Class(inner) => inner.span(),
            Item::Fn(inner) => inner.span(),
            Item::Let(inner) => inner.span(),
            Item::Statement(inner) => inner.span(),
            Item::Expr(inner) => inner.span(),
        }
    }
}

impl Spanned for ClassItem {
    fn span(&self) -> FreeSpan {
        join(self.class_tok.span, self.close_brace.span)
    }
}

impl Spanned for FnItem {
    fn span(&self) -> FreeSpan {
        join(self.fn_tok.span, self.body.span())
    }
}

impl Spanned for FnParam {
    fn span(&self) -> FreeSpan {
        if let Some(mut_tok) = &self.mut_tok {
            join(mut_tok.span, self.name.token.span)
        } else {
            self.name.token.span
        }
    }
}

impl Spanned for LetItem {
    fn span(&self) -> FreeSpan {
        join(self.let_tok.span, self.semicolon_tok.span)
    }
}

impl Spanned for LetInit {
    fn span(&self) -> FreeSpan {
        join(self.equal_tok.span, self.expr.span())
    }
}

impl Spanned for Statement {
    fn span(&self) -> FreeSpan {
        match self {
            Statement::For(inner) => inner.span(),
            Statement::If(inner) => inner.span(),
            Statement::Assert(inner) => inner.span(),
            Statement::Print(inner) => inner.span(),
            Statement::Return(inner) => inner.span(),
            Statement::While(inner) => inner.span(),
            Statement::Block(inner) => inner.span(),
        }
    }
}

impl Spanned for Expr {
    fn span(&self) -> FreeSpan {
        if let Some(semicolon_tok) = self.semicolon_tok {
            join(self.expr.span(), semicolon_tok.span)
        } else {
            self.expr.span()
        }
    }
}

impl Spanned for ForStmt {
    fn span(&self) -> FreeSpan {
        join(self.for_tok.span, self.body.span())
    }
}

impl Spanned for IfStmt {
    fn span(&self) -> FreeSpan {
        join(
            self.if_tok.span,
            self.else_branch
                .as_ref().map(Spanned::span)
                .unwrap_or_else(|| self.body.span()),
        )
    }
}

impl Spanned for ElseBranch {
    fn span(&self) -> FreeSpan {
        join(self.else_tok.span, self.body.span())
    }
}

impl Spanned for AssertStmt {
    fn span(&self) -> FreeSpan {
        join(self.assert_tok.span, self.semicolon_tok.span)
    }
}

impl Spanned for PrintStmt {
    fn span(&self) -> FreeSpan {
        join(self.print_tok.span, self.semicolon_tok.span)
    }
}

impl Spanned for ReturnStmt {
    fn span(&self) -> FreeSpan {
        join(self.return_tok.span, self.semicolon_tok.span)
    }
}

impl Spanned for WhileStmt {
    fn span(&self) -> FreeSpan {
        join(self.while_tok.span, self.body.span())
    }
}

impl Spanned for Block {
    fn span(&self) -> FreeSpan {
        join(self.left_brace_tok.span, self.right_brace_tok.span)
    }
}

impl Spanned for Expression {
    fn span(&self) -> FreeSpan {
        match self {
            Expression::Binary(inner) => inner.span(),
            Expression::Unary(inner) => inner.span(),
            Expression::Field(inner) => inner.span(),
            Expression::Group(inner) => inner.span(),
            Expression::Call(inner) => inner.span(),
            Expression::Primary(inner) => inner.span(),
        }
    }
}

impl Spanned for BinaryExpr {
    fn span(&self) -> FreeSpan {
        join(self.lhs.span(), self.rhs.span())
    }
}

impl Spanned for UnaryExpr {
    fn span(&self) -> FreeSpan {
        join(self.operator.span, self.expr.span())
    }
}

impl Spanned for FieldExpr {
    fn span(&self) -> FreeSpan {
        join(self.expr.span(), self.field.span())
    }
}

impl Spanned for GroupExpr {
    fn span(&self) -> FreeSpan {
        join(self.left_paren_tok.span, self.right_paren_tok.span)
    }
}

impl Spanned for CallExpr {
    fn span(&self) -> FreeSpan {
        join(self.callee.span(), self.right_paren_tok.span)
    }
}

impl Spanned for PrimaryExpr {
    fn span(&self) -> FreeSpan {
        self.token.span
    }
}

impl Spanned for Identifier {
    fn span(&self) -> FreeSpan {
        self.token.span
    }
}
