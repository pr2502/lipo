use crate::builtins::String;
use crate::diagnostic::{Diagnostic, Label, Severity};
use crate::lexer::{Lexer, Token, TokenKind, T};
use crate::span::FreeSpan;
use crate::ObjectRef;
use logos::Logos;
use std::mem;


pub mod ast;

use ast::*;


// Parsing implementation
//
// This parsing algorithm is **not** resilient, it will bail on the first error encountered.
// Precedence on infix expressions is handled using the Pratt binding power algorithm.

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken {
        found: Token,
        expected: TokenKind,
    },
    UnexpectedToken2 {
        found: Token,
        expected: &'static [TokenKind],
    },
    ExpectedExpressionStart {
        found: Token,
    },
    ExpectedInfixOrPostfixOperator {
        found: Token,
    },
    UnterminatedStringExpression {
        span: FreeSpan,
    },
    InvalidEscape {
        span: FreeSpan,
    },
}

impl Diagnostic for ParserError {
    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn message(&self) -> std::string::String {
        match self {
            ParserError::UnexpectedToken { .. } |
            ParserError::UnexpectedToken2 { .. } |
            ParserError::ExpectedExpressionStart { .. } |
            ParserError::ExpectedInfixOrPostfixOperator { .. } => "unexpected token".to_string(),
            ParserError::UnterminatedStringExpression { .. } => "unterminated string".to_string(),
            ParserError::InvalidEscape { .. } => "invalid escape or interpolation".to_string(),
        }
    }

    fn labels(&self) -> Vec<Label> {
        match self {
            ParserError::UnexpectedToken { found, .. } |
            ParserError::UnexpectedToken2 { found, .. } |
            ParserError::ExpectedExpressionStart { found } |
            ParserError::ExpectedInfixOrPostfixOperator { found } => vec![
                Label::primary(found.span, format!("unexpected {}", found.kind)),
            ],
            ParserError::UnterminatedStringExpression { span } => vec![
                Label::primary(span.shrink_to_hi(), "unterminated string"),
                Label::secondary(span.shrink_to_lo(), "file ends here"),
            ],
            ParserError::InvalidEscape { span } => vec![
                Label::primary(span, "unrecognized escape or invalid interpolation"),
            ],
        }
    }

    fn notes(&self) -> Vec<std::string::String> {
        match self {
            ParserError::UnexpectedToken { expected, .. } => vec![
                format!("expected {}", expected)
            ],
            ParserError::UnexpectedToken2 { expected, .. } => vec![
                match expected {
                    [] => unreachable!(),
                    [t] => format!("expected {}", t),
                    [t1, t2] => format!("expected {} or {}", t1, t2),
                    [first, mid @ .., last] => {
                        let mut acc = format!("expected one of {}", first);
                        for t in mid {
                            acc.push_str(&format!(", {}", t));
                        }
                        acc.push_str(&format!(" or {}", last));
                        acc
                    },
                }
            ],
            ParserError::ExpectedExpressionStart { .. } => vec![
                "expected expression to start or continue".to_string()
            ],
            ParserError::ExpectedInfixOrPostfixOperator { .. } => vec![
                "expected expression to continue or end".to_string()
            ],
            ParserError::UnterminatedStringExpression { .. } => vec![],
            ParserError::InvalidEscape { .. } => vec![],
        }
    }
}

struct Parser<'src> {
    lexer: Lexer<'src>,
}

type Result<T> = std::result::Result<T, ParserError>;

pub fn parse<'alloc>(source: ObjectRef<'alloc, String>) -> Result<AST> {
    let mut parser = Parser {
        lexer: Lexer::new(&source),
    };
    let items = parser.block_inner()?;
    let eof = parser.expect_next(T::Eof)?;
    Ok(AST {
        source,
        items,
        eof,
    })
}

// Utility functions for parsing
impl<'src> Parser<'src> {
    fn error(&mut self, e: ParserError) -> Result<!> {
        #[cfg(feature = "parser-error-panic")] {
            use crate::diagnostic::Report;
            e.report(self.lexer.source());
            panic!("parser error");
        }
        Err(e)
    }

    fn expect_next(&mut self, kind: TokenKind) -> Result<Token> {
        let token = self.lexer.next();
        if token.kind == kind {
            Ok(token)
        } else {
            self.error(ParserError::UnexpectedToken {
                found: token,
                expected: kind,
            })?;
        }
    }

    fn peek_kind(&self) -> TokenKind {
        self.lexer.peek().kind
    }

    fn match_next(&mut self, kind: TokenKind) -> Option<Token> {
        let token = self.lexer.peek();
        if token.kind == kind {
            let _ = self.lexer.next();
            Some(token)
        } else {
            None
        }
    }
}

impl<'src> Parser<'src> {
    fn item(&mut self) -> Result<Item> {
        // TODO recovery
        Ok(match self.peek_kind() {
            // Items
            T::FnKw => Item::Fn(self.fn_item()?),
            T::Const => Item::Const(self.const_item()?),
            T::Let => Item::Let(self.let_item()?),

            // Statements
            T::For => Item::Statement(Statement::For(self.for_stmt()?)),
            T::Assert => Item::Statement(Statement::Assert(self.assert_stmt()?)),
            T::Print => Item::Statement(Statement::Print(self.print_stmt()?)),
            T::Return => Item::Statement(Statement::Return(self.return_stmt()?)),
            T::While => Item::Statement(Statement::While(self.while_stmt()?)),

            // Expr
            _ => {
                let expr = self.expression()?;
                let semicolon_tok = self.match_next(T::Semicolon);
                Item::Expr(Expr { expr, semicolon_tok })
            }
        })
    }

    fn fn_item(&mut self) -> Result<FnItem> {
        let fn_tok = self.expect_next(T::FnKw)?;
        let name = self.name()?;
        let left_paren_tok = self.expect_next(T::LeftParen)?;
        let mut parameters = Delimited::default();
        while !matches!(self.peek_kind(), T::Eof | T::RightParen) {
            let mut_tok = self.match_next(T::Mut);
            let name = self.name()?;
            parameters.items.push(FnParam { mut_tok, name });

            match self.peek_kind() {
                T::RightParen => break,
                T::Comma => {
                    let comma_tok = self.expect_next(T::Comma).unwrap();
                    let comma = Comma { span: comma_tok.span };
                    parameters.delim.push(comma);
                }
                _ => {
                    self.error(ParserError::UnexpectedToken2 {
                        found: self.lexer.peek(),
                        expected: &[T::Comma, T::RightParen],
                    })?
                }
            }
        }
        let right_paren_tok = self.expect_next(T::RightParen)?;
        let body = self.block()?;
        Ok(FnItem { fn_tok, name, left_paren_tok, parameters, right_paren_tok, body })
    }

    fn const_item(&mut self) -> Result<ConstItem> {
        let const_tok = self.expect_next(T::Const).unwrap();
        let name = self.name()?;
        let equal_tok = self.expect_next(T::Equal)?;
        let expr = self.expression()?;
        let semicolon_tok = self.expect_next(T::Semicolon)?;
        Ok(ConstItem { const_tok, name, equal_tok, expr, semicolon_tok })
    }

    fn let_item(&mut self) -> Result<LetItem> {
        let let_tok = self.expect_next(T::Let)?;
        let mut_tok = self.match_next(T::Mut);
        let name = self.name()?;
        let init = if let Some(equal_tok) = self.match_next(T::Equal) {
            let expr = self.expression()?;
            Some(LetInit { equal_tok, expr })
        } else {
            None
        };
        let semicolon_tok = self.expect_next(T::Semicolon)?;
        Ok(LetItem { let_tok, mut_tok, name, init, semicolon_tok })
    }

    fn for_stmt(&mut self) -> Result<ForStmt> {
        // our for doesn't make sense until we have working methods calls
        todo!()
    }

    fn assert_stmt(&mut self) -> Result<AssertStmt> {
        let assert_tok = self.expect_next(T::Assert)?;
        let expr = self.expression()?;
        let semicolon_tok = self.expect_next(T::Semicolon)?;
        Ok(AssertStmt { assert_tok, expr, semicolon_tok })
    }

    fn print_stmt(&mut self) -> Result<PrintStmt> {
        let print_tok = self.expect_next(T::Print)?;
        let expr = self.expression()?;
        let semicolon_tok = self.expect_next(T::Semicolon)?;
        Ok(PrintStmt { print_tok, expr, semicolon_tok })
    }

    fn return_stmt(&mut self) -> Result<ReturnStmt> {
        let return_tok = self.expect_next(T::Return)?;
        let expr = if self.peek_kind() != T::Semicolon {
            Some(self.expression()?)
        } else {
            None
        };
        let semicolon_tok = self.expect_next(T::Semicolon)?;
        Ok(ReturnStmt { return_tok, expr, semicolon_tok })
    }

    fn while_stmt(&mut self) -> Result<WhileStmt> {
        let while_tok = self.expect_next(T::While)?;
        let pred = self.expression()?;
        let body = self.block()?;
        Ok(WhileStmt { while_tok, pred, body })
    }

    fn block_or_record(&mut self) -> Result<Expression> {
        // LeftBrace can start either a Block or a Record expression
        let mut lex2 = self.lexer.clone();
        let t0 = lex2.next().kind;
        let t1 = lex2.next().kind;
        let t2 = lex2.next().kind;

        match [t0, t1, t2] {
            // Start of a Record literal
            [T::LeftBrace, T::Identifier, T::Colon ] |
            // Start of a simplified Record literal
            [T::LeftBrace, T::Identifier, T::Comma ] |
            // Start and end of a simplified Record literal, this one could also be a Block with a
            // single ident in it but we give Record a priority because it's more useful than a
            // block with a single Primary expression.
            [T::LeftBrace, T::Identifier, T::RightBrace ] |
            // Empty Record or Block, Record has again priority because it's more useful than empty
            // block.
            [T::LeftBrace, T::RightBrace, _ ] => {
                Ok(Expression::Record(self.record()?))
            },
            _ => Ok(Expression::Block(self.block()?)),
        }
    }

    fn record(&mut self) -> Result<RecordExpr> {
        let left_brace_tok = self.expect_next(T::LeftBrace)?;
        let entries = self.record_entry_list()?;
        let right_brace_tok = self.expect_next(T::RightBrace)?;
        Ok(RecordExpr { left_brace_tok, entries, right_brace_tok })
    }

    fn record_entry_list(&mut self) -> Result<Delimited<Comma, RecordEntry>> {
        let mut entries = Delimited::default();

        match self.peek_kind() {
            // expression list end
            T::Semicolon |
            T::RightParen |
            T::RightBrace |
            T::Eof => return Ok(entries),
            _ => {}
        }

        loop {
            let name = self.name()?;

            let value = if let Some(colon_tok) = self.match_next(T::Colon) {
                let init = Box::new(self.expression()?);
                Some(EntryInit { colon_tok, init })
            } else {
                None
            };

            entries.items.push(RecordEntry { name, value });

            if let Some(comma_tok) = self.match_next(T::Comma) {
                let comma = Comma { span: comma_tok.span };
                entries.delim.push(comma);
            }

            match self.peek_kind() {
                // expression list end
                T::Semicolon |
                T::RightParen |
                T::RightBrace |
                T::Eof => return Ok(entries),
                _ => {}
            }
        }
    }

    fn block(&mut self) -> Result<Block> {
        let left_brace_tok = self.expect_next(T::LeftBrace)?;
        let body = self.block_inner()?;
        let right_brace_tok = self.expect_next(T::RightBrace)?;
        Ok(Block { left_brace_tok, body, right_brace_tok })
    }

    fn block_inner(&mut self) -> Result<Vec<Item>> {
        let mut body = Vec::new();
        let mut terminated = true;
        while !matches!(self.peek_kind(), T::RightBrace | T::Eof) {
            let item = self.item()?;

            assert!(terminated, "BUG: previous item was not terminated yet we parsed another one");
            terminated = match &item {
                Item::Expr(Expr { semicolon_tok: None, .. }) => false,
                // All other items are self-terminating
                _ => true,
            };

            body.push(item);
        }
        Ok(body)
    }

    fn unit_or_group_or_tuple(&mut self) -> Result<Expression> {
        let left_paren_tok = self.expect_next(T::LeftParen).unwrap();

        if let Some(right_paren_tok) = self.match_next(T::RightParen) {
            return Ok(Expression::Unit(UnitExpr { left_paren_tok, right_paren_tok }));
        }

        let exprs = self.expression_list()?;
        let right_paren_tok = self.expect_next(T::RightParen)?;
        if exprs.delim.is_empty() {
            assert_eq!(exprs.items.len(), 1);

            let expr = Box::new(exprs.items.into_iter().next().unwrap());
            Ok(Expression::Group(GroupExpr { left_paren_tok, expr, right_paren_tok }))
        } else {
            Ok(Expression::Tuple(TupleExpr { left_paren_tok, exprs, right_paren_tok }))
        }
    }

    fn if_expr(&mut self) -> Result<IfExpr> {
        let if_tok = self.expect_next(T::If)?;
        let pred = Box::new(self.expression()?);
        let body = self.block()?;
        let else_branch = if let Some(else_tok) = self.match_next(T::Else) {
            let body = self.block()?;
            Some(ElseBranch { else_tok, body })
        } else {
            None
        };
        Ok(IfExpr { if_tok, pred, body, else_branch })
    }

    fn expression(&mut self) -> Result<Expression> {
        self.expr_bp(0) // start with binding power none = 0
    }

    fn expr_bp(&mut self, min_bp: u8) -> Result<Expression> {
        let mut lhs = {
            match self.peek_kind() {
                T::LeftParen => self.unit_or_group_or_tuple()?,//Expression::Group(self.group_expr()?),
                T::LeftBrace => self.block_or_record()?,
                T::If => Expression::If(self.if_expr()?),
                T::Minus |
                T::Not => {
                    let operator = self.lexer.next();
                    let ((), r_bp) = prefix_binding_power(operator.kind);
                    let expr = self.expr_bp(r_bp)?;
                    Expression::Unary(UnaryExpr {
                        operator,
                        expr: Box::new(expr),
                    })
                },
                T::True |
                T::False |
                T::BinaryNumber |
                T::OctalNumber |
                T::HexadecimalNumber |
                T::DecimalNumber |
                T::DecimalPointNumber |
                T::ExponentialNumber |
                T::Identifier => {
                    let token = self.lexer.next();
                    Expression::Primary(PrimaryExpr { token })
                },
                T::StringLit => Expression::String(self.string_expr()?),
                _ => {
                    let found = self.lexer.next();
                    self.error(ParserError::ExpectedExpressionStart { found })?;
                },
            }
        };

        loop {
            let operator = self.lexer.peek();
            match operator.kind {
                // infix operators
                T::Equal |
                T::Or |
                T::And |
                T::NotEqual |
                T::EqualEqual |
                T::Greater |
                T::GreaterEqual |
                T::Less |
                T::LessEqual |
                T::Minus |
                T::Plus |
                T::Div |
                T::Mul |
                T::Dot => {}
                // postfix operators
                T::LeftParen => {}
                // expression end
                T::Semicolon |
                T::Comma |
                T::RightParen |
                T::LeftBrace |
                T::RightBrace |
                T::Eof => {
                    break;
                }
                _ => {
                    self.error(ParserError::ExpectedInfixOrPostfixOperator {
                        found: operator,
                    })?;
                }
            }

            if let Some((l_bp, ())) = postfix_binding_power(operator.kind) {
                if l_bp < min_bp {
                    break;
                }

                match operator.kind {
                    T::LeftParen => {
                        lhs = Expression::Call(self.call_expr(Box::new(lhs))?);
                        continue;
                    }
                    _ => unreachable!(),
                }
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(operator.kind) {
                if l_bp < min_bp {
                    break;
                }

                let _ = self.lexer.next(); // throw away the peeked operator token

                let rhs = self.expr_bp(r_bp)?;
                lhs = Expression::Binary(BinaryExpr {
                    lhs: Box::new(lhs),
                    operator,
                    rhs: Box::new(rhs),
                });

                continue;
            }

            break;
        }

        Ok(lhs)
    }

    fn call_expr(&mut self, callee: Box<Expression>) -> Result<CallExpr> {
        let left_paren_tok = self.expect_next(T::LeftParen)?;
        let arguments = self.expression_list()?;
        let right_paren_tok = self.expect_next(T::RightParen)?;
        Ok(CallExpr { callee, left_paren_tok, arguments, right_paren_tok })
    }

    fn expression_list(&mut self) -> Result<Delimited<Comma, Expression>> {
        let mut args = Delimited::default();

        match self.peek_kind() {
            // expression list end
            T::Semicolon |
            T::RightParen |
            T::RightBrace |
            T::Eof => return Ok(args),
            _ => {}
        }

        loop {
            args.items.push(self.expression()?);

            if let Some(comma_tok) = self.match_next(T::Comma) {
                let comma = Comma { span: comma_tok.span };
                args.delim.push(comma);
            }

            match self.peek_kind() {
                // expression list end
                T::Semicolon |
                T::RightParen |
                T::RightBrace |
                T::Eof => return Ok(args),
                _ => {}
            }
        }
    }

    fn name(&mut self) -> Result<Identifier> {
        let token = self.expect_next(T::Identifier)?;
        Ok(Identifier { span: token.span })
    }
}


// # Precedence
// Sorted from lowest binding power (lowest precedence) to highest.
//
// ASSIGNMENT   `=`
// OR           `or`
// AND          `and`
// EQUALITY     `==` `/=`
// COMPARISON   `<` `>` `<=` `>=`
// TERM         `+` `-`
// FACTOR       `*` `/`
// UNARY        `not` `-`
// CALL         `.` `()`
// PRIMARY

fn prefix_binding_power(kind: TokenKind) -> ((), u8) {
    match kind {
        // unary
        T::Not |
        T::Minus => ((), 15),

        _ => unreachable!(),
    }
}

fn postfix_binding_power(kind: TokenKind) -> Option<(u8, ())> {
    Some(match kind {
        // call
        T::LeftParen => (16, ()),

        _ => return None,
    })
}

fn infix_binding_power(kind: TokenKind) -> Option<(u8, u8)> {
    // left associative:  l_bp < r_bp
    // right associative: l_bp > r_bp
    Some(match kind {
        // assignment
        T::Equal        => (2, 1),
        // or
        T::Or           => (3, 4),
        // and
        T::And          => (5, 6),
        // equality
        T::EqualEqual |
        T::NotEqual     => (7, 8),
        // comparison
        T::Less |
        T::LessEqual |
        T::Greater |
        T::GreaterEqual => (9, 10),
        // term
        T::Minus |
        T::Plus         => (11, 12),
        // factor
        T::Div |
        T::Mul          => (13, 14),
        // call
        T::Dot          => (16, 17),

        _ => return None,
    })
}


// String interpolation parsing
impl<'src> Parser<'src> {
    fn string_expr(&mut self) -> Result<StringExpr> {
        let span = self.expect_next(T::StringLit)?.span;
        let slice = span.anchor(self.lexer.source()).as_str();

        let (modifier, slice) = match slice.as_bytes() {
            [modifier @ b'a'..=b'z', ..] => (Some(modifier), &slice[1..]),
            _ => (None, slice),
        };

        // The token regex starts with `#*"` so it must contain a `"` to match
        let hashes = slice.find('"').unwrap();

        let Some(inner_slice) = slice[(hashes + 1)..]
            .strip_suffix(&slice[..hashes])
            .and_then(|s| s.strip_suffix('"'))
        else {
            self.error(ParserError::UnterminatedStringExpression { span })?;
        };

        let modifier_len = modifier.is_some() as u32;
        let hashes = hashes as u32;

        let modifier_span = modifier.map(|_| FreeSpan {
            start: span.start,
            end: span.start + 1,
        });
        let open_delim_span = FreeSpan {
            start: span.start + modifier_len,
            end: span.start + modifier_len + hashes + 1,
        };
        let inner_span = FreeSpan {
            start: open_delim_span.end,
            end: span.end - 1 - hashes,
        };
        let close_delim_span = FreeSpan {
            start: inner_span.end,
            end: span.end,
        };

        let fragments = match modifier {
            Some(b'r') => raw_string_fragments(inner_slice, inner_span),
            Some(m) => todo!("error: unknown string modifier {}", *m as char),
            None => default_string_fragments(inner_slice, inner_span)?,
        };

        Ok(StringExpr {
            modifier_span,
            open_delim_span,
            fragments,
            close_delim_span,
        })
    }
}

fn raw_string_fragments(slice: &str, span: FreeSpan) -> Vec<StringFragment> {
    let unescaped = slice.to_owned();
    vec![StringFragment::Literal { span, unescaped }]
}


#[derive(Logos, Debug, PartialEq, Eq, Clone, Copy)]
enum StringPieceToken {
    #[token(r"{{")] EscBraceOpen,
    #[token(r"}}")] EscBraceClose,

    #[regex(r"\{[^{}]*\}")] Interpolation,

    #[regex(r".")] Plain,

    #[error]
    Error,
}

fn default_string_fragments(slice: &str, span: FreeSpan) -> Result<Vec<StringFragment>> {
    use StringPieceToken::*;

    let mut fragments = Vec::new();

    let mut lex = logos::Lexer::<StringPieceToken>::new(slice);
    let mut unescaped = std::string::String::with_capacity(slice.len());
    let mut span = span.shrink_to_lo();

    while let Some(tok) = lex.next() {
        match tok {
            EscBraceOpen => {
                unescaped.push('{');
                span.end += 2;
            }
            EscBraceClose => {
                unescaped.push('}');
                span.end += 2;
            }
            Interpolation => {
                if !unescaped.is_empty() {
                    let unescaped = mem::take(&mut unescaped);
                    fragments.push(StringFragment::Literal { span, unescaped });
                    span = span.shrink_to_hi();
                }

                let slice = lex.slice();
                span.end += slice.len() as u32;

                let interpolation = string_interpolation(slice, span)?;
                fragments.push(interpolation);

                span = span.shrink_to_hi();
            },
            Plain => {
                unescaped.push_str(lex.slice());
                span.end += 1;
            }
            Error => {
                span.end += lex.slice().len() as u32;
                return Err(ParserError::InvalidEscape { span });
            }
        }
    }

    if !unescaped.is_empty() {
        let unescaped = mem::take(&mut unescaped);
        fragments.push(StringFragment::Literal { span, unescaped });
    }

    Ok(fragments)
}

fn string_interpolation(slice: &str, span: FreeSpan) -> Result<StringFragment> {
    // Caller is responsible for the interpolation being wrapped in `{}`
    let slice = slice
        .strip_prefix('{').unwrap()
        .strip_suffix('}').unwrap();
    let span = FreeSpan {
        start: span.start + 1, // skip '{'
        end: span.end - 1, // skip '}'
    };

    let ident = |len| -> Result<_> {
        let slice = &slice[..len];
        let mut lex = logos::Lexer::<TokenKind>::new(slice);
        if !(matches!(lex.next(), Some(T::Identifier)) && matches!(lex.next(), None)) {
            todo!("error: invalid identifier in string interpolation");
        }
        let span = FreeSpan {
            start: span.start,
            end: span.start + (len as u32),
        };
        Ok(Identifier { span })
    };

    if let Some(colon_idx) = slice.find(':') {
        let ident = ident(colon_idx)?;
        let fmt = Some(FreeSpan {
            start: span.start + (colon_idx as u32),
            end: span.end,
        });
        Ok(StringFragment::Interpolation { ident, fmt })
    } else {
        let ident = ident(slice.len())?;
        Ok(StringFragment::Interpolation { ident, fmt: None })
    }
}
