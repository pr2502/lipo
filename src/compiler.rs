use crate::chunk::{Chunk, ConstKey};
use crate::lexer::TokenKind;
use crate::object::string::String;
use crate::object::{Alloc, ObjectRef};
use crate::opcode::OpCode;
use crate::parser::ast::*;
use crate::span::{FreeSpan, Spanned};
use crate::value::Value;
use std::num::ParseFloatError;


#[derive(Debug)]
pub enum Error {
    TooManyLocals {
        span: FreeSpan,
    },
    Shadowing {
        shadowing_span: FreeSpan,
        shadowed_span: FreeSpan,
    },
    InvalidNumberLiteral {
        cause: ParseFloatError,
        span: FreeSpan,
    },
    InvalidAssignmentTarget {
        span: FreeSpan,
    },
    AssignImmutableBinding {
        let_span: FreeSpan,
        assign_span: FreeSpan,
    },
}

struct Emitter<'alloc> {
    source: ObjectRef<'alloc, String>,
    alloc: &'alloc Alloc,
    chunk: Chunk<'alloc>,

    locals: Vec<Local>,
    scope_depth: i32,
}

struct Local {
    name: Identifier,
    let_span: FreeSpan,
    mutable: bool,
    depth: i32,
}

type Result = std::result::Result<(), Error>;

pub fn compile<'alloc>(ast: AST<'alloc>, alloc: &'alloc Alloc) -> std::result::Result<Chunk<'alloc>, Error> {
    let mut emitter = Emitter {
        source: ast.source,
        alloc,
        chunk: Chunk::new(ast.source),
        locals: Vec::default(),
        scope_depth: 0,
    };

    for d in &ast.items {
        emitter.item(d)?
    }

    emitter.chunk.emit(OpCode::Unit, ast.eof.span);
    emitter.chunk.emit(OpCode::Return, ast.eof.span);

    Ok(emitter.chunk)
}

const DUMMY: u16 = u16::MAX;

impl<'alloc> Emitter<'alloc> {
    fn identifier_constant(&mut self, ident: Identifier) -> ConstKey {
        let span_str = ident.token.span.anchor(&self.source).as_str();
        let value = Value::new_object(String::new(span_str, self.alloc));
        self.chunk.insert_constant(value)
    }

    fn add_local(&mut self, let_item: &LetItem) -> Result {
        if self.locals.len() >= (u16::MAX as usize) {
            return Err(Error::TooManyLocals { span: let_item.name.span() });
        }
        let ident_slice = |ident: Identifier| ident.token.span.anchor(&self.source).as_str();
        let shadowing = self.locals.iter()
            .rev()
            .take_while(|loc| loc.depth == self.scope_depth)
            .find(|loc| ident_slice(loc.name) == ident_slice(let_item.name));
        if let Some(local) = shadowing {
            return Err(Error::Shadowing {
                shadowing_span: let_item.name.span(),
                shadowed_span: local.name.span(),
            });
        }
        self.locals.push(Local {
            name: let_item.name,
            let_span: let_item.span(),
            mutable: let_item.mut_tok.is_some(),
            depth: self.scope_depth,
        });
        Ok(())
    }

    fn resolve_local(&mut self, name: Identifier) -> Option<(u16, &Local)> {
        let ident_slice = |ident: Identifier| ident.token.span.anchor(&self.source).as_str();
        self.locals.iter()
            .rev().enumerate()
            .find(|(_, loc)| ident_slice(loc.name) == ident_slice(name))
            .map(|(slot, loc)| (slot as u16, loc))
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self, span: FreeSpan) {
        assert!(self.scope_depth > 0);
        self.scope_depth -= 1;
        while let Some(local) = self.locals.last() {
            if local.depth <= self.scope_depth {
                break
            }
            self.locals.pop();
            self.chunk.emit(OpCode::Pop, span);
        }
    }
}

impl<'alloc> Emitter<'alloc> {
    fn item(&mut self, item: &Item) -> Result {
        match item {
            Item::Class(class_item) => self.class_item(class_item),
            Item::Fn(fn_item) => self.fn_item(fn_item),
            Item::Let(let_item) => self.let_item(let_item),
            Item::Statement(stmt) => self.statement(stmt),
        }
    }

    fn class_item(&mut self, _class_item: &ClassItem) -> Result {
        todo!("class")
    }

    fn fn_item(&mut self, _fun_item: &FnItem) -> Result {
        todo!("function")
    }

    fn let_item(&mut self, let_item: &LetItem) -> Result {
        let span = let_item.span();

        if let Some(init) = &let_item.init {
            self.expression(&init.expr)?;
        } else {
            // empty initializer, set value to Unit
            self.chunk.emit(OpCode::Unit, span);
        }

        if self.scope_depth == 0 {
            // global variable
            let name_key = self.identifier_constant(let_item.name);
            self.chunk.emit(OpCode::DefGlobal { name_key }, span);
        } else {
            // local variable
            self.add_local(let_item)?;
        }

        Ok(())
    }

    fn statement(&mut self, stmt: &Statement) -> Result {
        match stmt {
            Statement::Expr(expr_stmt) => self.expr_stmt(expr_stmt),
            Statement::For(for_stmt) => self.for_stmt(for_stmt),
            Statement::If(if_stmt) => self.if_stmt(if_stmt),
            Statement::Assert(assert_stmt) => self.assert_stmt(assert_stmt),
            Statement::Print(print_stmt) => self.print_stmt(print_stmt),
            Statement::Return(return_stmt) => self.return_stmt(return_stmt),
            Statement::While(while_stmt) => self.while_stmt(while_stmt),
            Statement::Block(block) => self.block(block),
        }
    }

    fn expr_stmt(&mut self, expr_stmt: &ExprStmt) -> Result {
        self.expression(&expr_stmt.expr)?;
        self.chunk.emit(OpCode::Pop, expr_stmt.semicolon_tok.span);
        Ok(())
    }

    fn for_stmt(&mut self, _for_stmt: &ForStmt) -> Result {
        todo!()
    }

    fn if_stmt(&mut self, if_stmt: &IfStmt) -> Result {
        // if <pred>
        self.expression(&if_stmt.pred)?;
        let then_jump = self.chunk.emit(OpCode::JumpIfFalse { offset: DUMMY }, if_stmt.if_tok.span);

        // then
        self.chunk.emit(OpCode::Pop, if_stmt.if_tok.span);
        self.block(&if_stmt.body)?;
        let else_jump = self.chunk.emit(OpCode::Jump { offset: DUMMY }, if_stmt.if_tok.span);

        // else
        self.chunk.patch_jump(then_jump);
        self.chunk.emit(OpCode::Pop, if_stmt.if_tok.span);
        if let Some(else_branch) = &if_stmt.else_branch {
            self.block(&else_branch.body)?;
        }

        // end
        self.chunk.patch_jump(else_jump);

        Ok(())
    }

    fn assert_stmt(&mut self, assert_stmt: &AssertStmt) -> Result {
        self.expression(&assert_stmt.expr)?;
        self.chunk.emit(OpCode::Assert, assert_stmt.span());
        Ok(())
    }

    fn print_stmt(&mut self, print_stmt: &PrintStmt) -> Result {
        self.expression(&print_stmt.expr)?;
        self.chunk.emit(OpCode::Print, print_stmt.span());
        Ok(())
    }

    fn return_stmt(&mut self, _return_stmt: &ReturnStmt) -> Result {
        todo!()
    }

    fn while_stmt(&mut self, while_stmt: &WhileStmt) -> Result {
        let loop_start = self.chunk.loop_point();

        // while <pred>
        self.expression(&while_stmt.pred)?;
        let span = FreeSpan::join(while_stmt.while_tok.span, while_stmt.pred.span());
        let exit_jump = self.chunk.emit(OpCode::JumpIfFalse { offset: DUMMY }, span);

        // then
        self.chunk.emit(OpCode::Pop, while_stmt.body.left_brace_tok.span);
        self.block(&while_stmt.body)?;
        self.chunk.emit_loop(loop_start, while_stmt.body.right_brace_tok.span);

        // end
        self.chunk.patch_jump(exit_jump);
        self.chunk.emit(OpCode::Pop, while_stmt.body.right_brace_tok.span);

        Ok(())
    }

    fn block(&mut self, block: &Block) -> Result {
        self.begin_scope();
        for i in &block.body {
            self.item(i)?;
        }
        self.end_scope(block.right_brace_tok.span);
        Ok(())
    }

    fn expression(&mut self, expr: &Expression) -> Result {
        match expr {
            Expression::Binary(binary_expr) => self.binary_expr(binary_expr),
            Expression::Unary(unary_expr) => self.unary_expr(unary_expr),
            Expression::Field(field_expr) => self.field_expr(field_expr),
            Expression::Group(group_expr) => self.group_expr(group_expr),
            Expression::Call(call_expr) => self.call_expr(call_expr),
            Expression::Primary(primary_expr) => self.primary_expr(primary_expr),
        }
    }

    fn binary_expr(&mut self, binary_expr: &BinaryExpr) -> Result {
        let op = binary_expr.operator.kind;

        if op == TokenKind::Equal {
            // for now only allow assigning to an identifier
            if let Expression::Primary(primary) = &*binary_expr.lhs {
                if primary.token.kind == TokenKind::Identifier {
                    let ident = Identifier { token: primary.token };
                    self.expression(&binary_expr.rhs)?;
                    if let Some((slot, local)) = self.resolve_local(ident) {
                        if !local.mutable {
                            return Err(Error::AssignImmutableBinding {
                                let_span: local.let_span,
                                assign_span: binary_expr.span(),
                            });
                        }
                        self.chunk.emit(OpCode::SetLocal { slot }, binary_expr.span());
                    } else {
                        let name_key = self.identifier_constant(ident);
                        self.chunk.emit(OpCode::SetGlobal { name_key }, binary_expr.span());
                    }
                    return Ok(())
                }
            }
            // TODO more complex assignment target
            return Err(Error::InvalidAssignmentTarget {
                span: binary_expr.lhs.span(),
            });
        }

        if op == TokenKind::Or {
            return self.or(binary_expr);
        }

        if op == TokenKind::And {
            return self.and(binary_expr);
        }

        // normal binary operations with eagerly evaluated operands

        self.expression(&binary_expr.lhs)?;
        self.expression(&binary_expr.rhs)?;

        let span = binary_expr.span();
        match op {
            TokenKind::NotEqual => {
                self.chunk.emit(OpCode::Equal, span);
                self.chunk.emit(OpCode::Not, span);
            }
            TokenKind::EqualEqual => {
                self.chunk.emit(OpCode::Equal, span);
            }
            TokenKind::Greater => {
                self.chunk.emit(OpCode::Greater, span);
            }
            TokenKind::GreaterEqual => {
                self.chunk.emit(OpCode::Less, span);
                self.chunk.emit(OpCode::Not, span);
            }
            TokenKind::Less => {
                self.chunk.emit(OpCode::Less, span);
            }
            TokenKind::LessEqual => {
                self.chunk.emit(OpCode::Greater, span);
                self.chunk.emit(OpCode::Not, span);
            }
            TokenKind::Plus => {
                self.chunk.emit(OpCode::Add, span);
            }
            TokenKind::Minus => {
                self.chunk.emit(OpCode::Subtract, span);
            }
            TokenKind::Mul => {
                self.chunk.emit(OpCode::Multiply, span);
            }
            TokenKind::Div => {
                self.chunk.emit(OpCode::Divide, span);
            }
            _ => unreachable!()
        }
        Ok(())
    }

    fn and(&mut self, binary_expr: &BinaryExpr) -> Result {
        self.expression(&binary_expr.lhs)?;

        // if lhs is false, short-circuit, jump over rhs
        // span both lhs and the `and` operator
        let span = FreeSpan::join(binary_expr.lhs.span(), binary_expr.operator.span);
        let end_jump = self.chunk.emit(OpCode::JumpIfFalse { offset: DUMMY }, span);

        // pop lhs result, span of the `and` operator
        self.chunk.emit(OpCode::Pop, binary_expr.operator.span);
        self.expression(&binary_expr.rhs)?;

        self.chunk.patch_jump(end_jump);
        Ok(())
    }

    fn or(&mut self, binary_expr: &BinaryExpr) -> Result {
        self.expression(&binary_expr.lhs)?;

        // if lhs is true, short-circuit, jump over rhs
        // span both lhs and the `or` operator
        let span = FreeSpan::join(binary_expr.lhs.span(), binary_expr.operator.span);
        let end_jump = self.chunk.emit(OpCode::JumpIfTrue { offset: DUMMY }, span);

        // pop lhs result, span of the `or` operator
        self.chunk.emit(OpCode::Pop, binary_expr.operator.span);
        self.expression(&binary_expr.rhs)?;

        self.chunk.patch_jump(end_jump);
        Ok(())
    }

    fn unary_expr(&mut self, unary_expr: &UnaryExpr) -> Result {
        let op = unary_expr.operator.kind;

        self.expression(&unary_expr.expr)?;

        let span = unary_expr.span();
        match op {
            TokenKind::Not => {
                self.chunk.emit(OpCode::Not, span);
            }
            TokenKind::Minus => {
                self.chunk.emit(OpCode::Negate, span);
            }
            _ => unreachable!()
        }

        Ok(())
    }

    fn field_expr(&mut self, _field_expr: &FieldExpr) -> Result {
        todo!()
    }

    fn group_expr(&mut self, group_expr: &GroupExpr) -> Result {
        if let Some(expr) = group_expr.expr.as_ref() {
            self.expression(expr)
        } else {
            self.chunk.emit(OpCode::Unit, group_expr.span());
            Ok(())
        }
    }

    fn call_expr(&mut self, _call_expr: &CallExpr) -> Result {
        todo!()
    }

    fn primary_expr(&mut self, primary_expr: &PrimaryExpr) -> Result {
        let op = primary_expr.token.kind;
        let span = primary_expr.span();

        match op {
            TokenKind::True => {
                self.chunk.emit(OpCode::True, span);
            }
            TokenKind::False => {
                self.chunk.emit(OpCode::False, span);
            }
            TokenKind::This => {
                todo!()
            }
            TokenKind::Super => {
                todo!()
            }
            TokenKind::Number => {
                self.float(primary_expr)?;
            }
            TokenKind::String => {
                self.string(primary_expr)?;
            }
            TokenKind::Identifier => {
                self.identifier(primary_expr)?;
            }
            _ => unreachable!()
        }
        Ok(())
    }

    fn float(&mut self, primary: &PrimaryExpr) -> Result {
        let span = primary.token.span;
        let slice = span.anchor(&self.source).as_str();
        match slice.parse() {
            Ok(float) => {
                let value = Value::new_float(float);
                let key = self.chunk.insert_constant(value);
                self.chunk.emit(OpCode::Constant { key }, span);
            }
            Err(cause) => {
                return Err(Error::InvalidNumberLiteral { cause, span });
            }
        }
        Ok(())
    }

    fn string(&mut self, primary: &PrimaryExpr) -> Result {
        let span = primary.token.span;
        let slice = span.anchor(&self.source).as_str()
            .strip_prefix('"').unwrap()
            .strip_suffix('"').unwrap();
        let string = String::new(slice, self.alloc);
        let value = Value::new_object(string);
        let key = self.chunk.insert_constant(value);
        self.chunk.emit(OpCode::Constant { key }, span);
        Ok(())
    }

    fn identifier(&mut self, primary: &PrimaryExpr) -> Result {
        let ident = Identifier { token: primary.token };
        if let Some((slot, _)) = self.resolve_local(ident) {
            self.chunk.emit(OpCode::GetLocal { slot }, ident.span());
        } else {
            let name_key = self.identifier_constant(ident);
            self.chunk.emit(OpCode::GetGlobal { name_key }, ident.span());
        }
        Ok(())
    }
}
