//! Implement [`Spanned`] for the AST.

use super::*;
use crate::span::{FreeSpan, Spanned};


fn join(a: FreeSpan, b: FreeSpan) -> FreeSpan {
    FreeSpan::join(a, b)
}

impl Spanned for Item {
    fn span(&self) -> FreeSpan {
        match self {
            Item::Fn(inner) => inner.span(),
            Item::Const(inner) => inner.span(),
            Item::Let(inner) => inner.span(),
            Item::Statement(inner) => inner.span(),
            Item::Expr(inner) => inner.span(),
        }
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
            join(mut_tok.span, self.name.span)
        } else {
            self.name.span
        }
    }
}

impl Spanned for ConstItem {
    fn span(&self) -> FreeSpan {
        join(self.const_tok.span, self.semicolon_tok.span)
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
            Statement::Assert(inner) => inner.span(),
            Statement::Print(inner) => inner.span(),
            Statement::Return(inner) => inner.span(),
            Statement::While(inner) => inner.span(),
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
            Expression::Unit(inner) => inner.span(),
            Expression::Group(inner) => inner.span(),
            Expression::Tuple(inner) => inner.span(),
            Expression::Record(inner) => inner.span(),
            Expression::Block(inner) => inner.span(),
            Expression::If(inner) => inner.span(),
            Expression::Call(inner) => inner.span(),
            Expression::String(inner) => inner.span(),
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

impl Spanned for UnitExpr {
    fn span(&self) -> FreeSpan {
        join(self.left_paren_tok.span, self.right_paren_tok.span)
    }
}

impl Spanned for GroupExpr {
    fn span(&self) -> FreeSpan {
        join(self.left_paren_tok.span, self.right_paren_tok.span)
    }
}

impl Spanned for TupleExpr {
    fn span(&self) -> FreeSpan {
        join(self.left_paren_tok.span, self.right_paren_tok.span)
    }
}

impl Spanned for RecordExpr {
    fn span(&self) -> FreeSpan {
        join(self.left_brace_tok.span, self.right_brace_tok.span)
    }
}

impl Spanned for RecordEntry {
    fn span(&self) -> FreeSpan {
        if let Some(value) = &self.value {
            join(self.name.span, value.init.span())
        } else {
            self.name.span
        }
    }
}

impl Spanned for IfExpr {
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

impl Spanned for StringExpr {
    fn span(&self) -> FreeSpan {
        join(
            self.modifier_span.unwrap_or(self.open_delim_span),
            self.close_delim_span,
        )
    }
}
