use crate::chunk::{ChunkBuf, ConstKey, LoopPoint, PatchPlace};
use crate::lexer::TokenKind;
use crate::object::builtins::{Closure, Function, String};
use crate::object::{Alloc, ObjectRef};
use crate::opcode::OpCode;
use crate::parser::ast::*;
use crate::span::{FreeSpan, Spanned};
use crate::value::Value;
use std::num::ParseFloatError;


/// Maximum number of function arguments and parameters
const MAX_ARGS: usize = u8::MAX as usize;

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
        bind_span: FreeSpan,
        assign_span: FreeSpan,
    },
    ReturnFromScript {
        return_span: FreeSpan,
    },
    TooManyParameters {
        extra_param_span: FreeSpan,
        limit: usize,
    },
    TooManyArguments {
        extra_arg_span: FreeSpan,
        limit: usize,
    },
    UndefinedName {
        name: FreeSpan,
    },
}

struct Emitter<'alloc> {
    alloc: &'alloc Alloc,
    source: ObjectRef<'alloc, String>,
    fn_stack: Vec<FnScope<'alloc>>,
    errors: Vec<Error>,
}

struct FnScope<'alloc> {
    name: Box<str>,
    chunk: ChunkBuf<'alloc>,
    locals: Vec<Local>,
    upvalues: Vec<Upvalue>,
    scope_depth: u32,
}

#[derive(Clone, Copy)]
struct Local {
    // name of the binding / reference
    name: Identifier,
    // where the binding was introduced
    bind_span: FreeSpan,
    // can be reassigned
    mutable: bool,
    // number of enclosing block scopes
    depth: u32,
}

#[derive(Clone, Copy)]
struct Upvalue {
    // name of the binding / reference
    name: Identifier,
    // reference to the parent scope's bindings
    reference: UpvalueRef,
}

// upvalue can refer to a local binding or another upvalue
#[derive(Clone, Copy)]
enum UpvalueRef {
    Local(u16),
    Upvalue(u8),
}

pub fn compile<'alloc>(ast: AST<'alloc>, alloc: &'alloc Alloc) -> Result<ObjectRef<'alloc, Closure<'alloc>>, Vec<Error>> {
    let mut emitter = Emitter {
        alloc,
        source: ast.source,
        fn_stack: Vec::default(),
        errors: Vec::default(),
    };

    let script = Local {
        name: Identifier { token: ast.eof },
        bind_span: ast.eof.span,
        mutable: false,
        depth: 0,
    };
    emitter.fn_stack.push(FnScope {
        name: "<script>".into(),
        chunk: ChunkBuf::new(emitter.source),
        locals: vec![script],
        upvalues: Vec::default(),
        scope_depth: 0,
    });

    emitter.block_inner(&ast.items);

    emitter.emit(OpCode::Unit, ast.eof.span);
    emitter.emit(OpCode::Return, ast.eof.span);

    let script = emitter.fn_stack.pop().unwrap();
    assert!(emitter.fn_stack.is_empty(), "BUG: unclosed function in compiler");

    if !emitter.errors.is_empty() {
        // Don't even try to check the Chunk for correctness, if there were any errors it likely
        // contains nonsense.
        return Err(emitter.errors);
    }

    let function = Function::new(script.chunk.check(), 0, "<script>".into(), alloc);
    Ok(Closure::new(function, Box::new([]), alloc))
}

const DUMMY: u16 = u16::MAX;

impl<'alloc> Emitter<'alloc> {
    fn fn_scope(&self) -> &FnScope<'alloc> {
        self.fn_stack.last().expect("missing FnScope")
    }

    fn fn_scope_mut(&mut self) -> &mut FnScope<'alloc> {
        self.fn_stack.last_mut().expect("missing FnScope")
    }

    fn emit(&mut self, opcode: OpCode, span: FreeSpan) -> PatchPlace {
        self.fn_scope_mut().chunk.emit(opcode, span)
    }

    fn patch_jump(&mut self, place: PatchPlace) {
        self.fn_scope_mut().chunk.patch_jump(place)
    }

    fn loop_point(&self) -> LoopPoint {
        self.fn_scope().chunk.loop_point()
    }

    fn emit_loop(&mut self, loop_point: LoopPoint, span: FreeSpan) {
        self.fn_scope_mut().chunk.emit_loop(loop_point, span)
    }

    fn insert_constant(&mut self, value: Value<'alloc>) -> ConstKey {
        self.fn_scope_mut().chunk.insert_constant(value)
    }
}

impl<'alloc> FnScope<'alloc> {
    fn add_local(&mut self, name: Identifier, mutable: bool, span: FreeSpan, source: &str) -> Result<(), Error> {
        if self.locals.len() >= usize::from(u16::MAX) {
            return Err(Error::TooManyLocals { span: name.span() });
        }
        let ident_slice = |ident: Identifier| ident.token.span.anchor(source).as_str();
        let shadowing = self.locals.iter()
            .rev()
            .take_while(|loc| loc.depth == self.scope_depth)
            .find(|loc| ident_slice(loc.name) == ident_slice(name));
        if let Some(local) = shadowing {
            return Err(Error::Shadowing {
                shadowing_span: name.span(),
                shadowed_span: local.name.span(),
            });
        }
        let depth = self.scope_depth;
        self.locals.push(Local {
            name,
            bind_span: span,
            mutable,
            depth,
        });
        Ok(())
    }

    fn find_local(&self, name: Identifier, source: &str) -> Option<(u16, Local)> {
        let ident_slice = |ident: Identifier| ident.token.span.anchor(source).as_str();
        self.locals.iter()
            .enumerate()
            .find(|(_, loc)| ident_slice(loc.name) == ident_slice(name))
            .map(|(slot, loc)| (slot.try_into().unwrap(), *loc))
    }

    fn find_upvalue(&self, name: Identifier, source: &str) -> Option<(u8, Upvalue)> {
        let ident_slice = |ident: Identifier| ident.token.span.anchor(source).as_str();
        self.upvalues.iter()
            .enumerate()
            .find(|(_, upval)| ident_slice(upval.name) == ident_slice(name))
            .map(|(idx, upval)| (idx.try_into().unwrap(), *upval))
    }
}

impl<'alloc> Emitter<'alloc> {
    fn add_local(&mut self, name: Identifier, mutable: bool, span: FreeSpan) {
        let source = self.source;
        let res = self.fn_scope_mut()
            .add_local(name, mutable, span, &source);
        if let Err(e) = res {
            self.errors.push(e);
        }
    }

    fn resolve_local(&self, name: Identifier) -> Option<(u16, Local)> {
        let source = self.source;
        self.fn_scope()
            .find_local(name, &source)
    }

    fn resolve_upvalue(&mut self, name: Identifier) -> Option<(u8, Upvalue)> {
        // Find an existing upvalue
        if let Some(existing) = self.fn_scope().find_upvalue(name, &self.source) {
            return Some(existing);
        }

        // Find which enclosing scope, if any, has the binding we want
        //
        // If no binding is found we return `None`
        let (fn_scope_idx, (slot, local)) = self.fn_stack.iter()
            .enumerate()
            .rev() // scan upwards
            .skip(1) // skip the current fn_scope, we already looked there in `resolve_local`
            .find_map(|(fn_scope_idx, fn_scope)| {
                let found = fn_scope.find_local(name, &self.source)?;
                Some((fn_scope_idx, found))
            })?;

        if local.mutable {
            todo!("error: cannot capture a mutable variable")
        }

        // For each enclosing fn_scope register an upvalue, if it doesn't already exist
        //
        // `upvalue_ref` keeps the reference to the previous upvalue/local, we initialize it
        // with a reference to the original Local.
        let mut upvalue_ref = UpvalueRef::Local(slot);
        for fn_scope in &mut self.fn_stack[fn_scope_idx + 1 ..] {
            if let Some((slot, _)) = fn_scope.find_upvalue(name, &self.source) {
                // If the upvalue is found we just update the reference for the next iteration to
                // point to the existing upvalue.
                upvalue_ref = UpvalueRef::Upvalue(slot);
            } else {
                let slot: u8 = fn_scope.upvalues.len()
                    .try_into().expect("too many upvalues"); // TODO error handling
                fn_scope.upvalues.push(Upvalue {
                    name,
                    reference: upvalue_ref,
                });
                upvalue_ref = UpvalueRef::Upvalue(slot);
            }
        }

        // The last iteration inserted the new upvalue into the current fn_scope
        if let UpvalueRef::Upvalue(slot) = upvalue_ref {
            Some((slot, self.fn_scope().upvalues[usize::from(slot)]))
        } else {
            unreachable!("BUG: invalid upvalue slot");
        }
    }

    fn begin_scope(&mut self) {
        self.fn_scope_mut().scope_depth += 1;
    }

    fn end_scope(&mut self, span: FreeSpan) {
        assert!(self.fn_scope().scope_depth > 0);
        self.fn_scope_mut().scope_depth -= 1;
        while let Some(local) = self.fn_scope().locals.last() {
            if local.depth <= self.fn_scope().scope_depth {
                break
            }
            self.fn_scope_mut().locals.pop();
            self.emit(OpCode::Pop, span);
        }
    }
}

impl<'alloc> Emitter<'alloc> {
    fn item(&mut self, item: &Item) {
        match item {
            Item::Class(class_item) => self.class_item(class_item),
            Item::Fn(fn_item) => self.fn_item(fn_item),
            Item::Let(let_item) => self.let_item(let_item),
            Item::Statement(stmt) => self.statement(stmt),
        }
    }

    fn class_item(&mut self, _class_item: &ClassItem) {
        todo!("class")
    }

    fn fn_item(&mut self, fn_item: &FnItem) {
        // Add an immutable local into the outer fn
        self.add_local(fn_item.name, false, fn_item.span());

        let name = fn_item.name.span().anchor(&self.source).as_str().into();
        // Reference to callee in the 0th stack slot
        let recur = Local {
            name: fn_item.name,
            bind_span: fn_item.span(),
            mutable: false,
            depth: 0,
        };
        self.fn_stack.push(FnScope {
            name,
            chunk: ChunkBuf::new(self.source),
            locals: vec![recur],
            upvalues: Vec::default(),
            scope_depth: 0,
        });

        for (i, param) in fn_item.parameters.items.iter().enumerate() {
            if i >= MAX_ARGS {
                self.errors.push(Error::TooManyParameters {
                    extra_param_span: param.span(),
                    limit: MAX_ARGS,
                });
            }
            self.add_local(param.name, param.mut_tok.is_some(), param.span());
        }

        self.block_inner(&fn_item.body.body);

        self.emit(OpCode::Unit, fn_item.body.right_brace_tok.span);
        self.emit(OpCode::Return, fn_item.body.right_brace_tok.span);

        let function = self.fn_stack.pop().unwrap();
        let upvals = function.upvalues.len().try_into().unwrap();

        for upval in &function.upvalues {
            match upval.reference {
                UpvalueRef::Local(slot) => self.emit(OpCode::GetLocal { slot }, upval.name.span()),
                UpvalueRef::Upvalue(slot) => self.emit(OpCode::GetUpvalue { slot }, upval.name.span()),
            };
        }

        let function = Value::new_object(Function::new(
            function.chunk.check(),
            fn_item.parameters.items.len().try_into().unwrap(),
            function.name,
            self.alloc,
        ));
        let fn_key = self.insert_constant(function);
        self.emit(OpCode::Closure { fn_key, upvals }, fn_item.span());
    }

    fn let_item(&mut self, let_item: &LetItem) {
        let span = let_item.span();

        if let Some(init) = &let_item.init {
            self.expression(&init.expr);
        } else {
            // empty initializer, set value to Unit
            self.emit(OpCode::Unit, span);
        }

        self.add_local(let_item.name, let_item.mut_tok.is_some(), let_item.span());
    }

    fn statement(&mut self, stmt: &Statement) {
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

    fn expr_stmt(&mut self, expr_stmt: &ExprStmt) {
        self.expression(&expr_stmt.expr);
        self.emit(OpCode::Pop, expr_stmt.semicolon_tok.span);
    }

    fn for_stmt(&mut self, _for_stmt: &ForStmt) {
        todo!()
    }

    fn if_stmt(&mut self, if_stmt: &IfStmt) {
        // if <pred>
        self.expression(&if_stmt.pred);
        let then_jump = self.emit(OpCode::JumpIfFalse { offset: DUMMY }, if_stmt.if_tok.span);

        // then
        self.emit(OpCode::Pop, if_stmt.if_tok.span);
        self.block(&if_stmt.body);
        let else_jump = self.emit(OpCode::Jump { offset: DUMMY }, if_stmt.if_tok.span);

        // else
        self.patch_jump(then_jump);
        self.emit(OpCode::Pop, if_stmt.if_tok.span);
        if let Some(else_branch) = &if_stmt.else_branch {
            self.block(&else_branch.body);
        }

        // end
        self.patch_jump(else_jump);
    }

    fn assert_stmt(&mut self, assert_stmt: &AssertStmt) {
        self.expression(&assert_stmt.expr);
        self.emit(OpCode::Assert, assert_stmt.span());
    }

    fn print_stmt(&mut self, print_stmt: &PrintStmt) {
        self.expression(&print_stmt.expr);
        self.emit(OpCode::Print, print_stmt.span());
    }

    fn return_stmt(&mut self, return_stmt: &ReturnStmt) {
        if self.fn_stack.len() == 1 {
            self.errors.push(Error::ReturnFromScript {
                return_span: return_stmt.span(),
            });
        }
        if let Some(expr) = &return_stmt.expr {
            self.expression(expr);
        } else {
            self.emit(OpCode::Unit, return_stmt.semicolon_tok.span);
        }
        self.emit(OpCode::Return, return_stmt.return_tok.span);
    }

    fn while_stmt(&mut self, while_stmt: &WhileStmt) {
        let loop_start = self.loop_point();

        // while <pred>
        self.expression(&while_stmt.pred);
        let span = FreeSpan::join(while_stmt.while_tok.span, while_stmt.pred.span());
        let exit_jump = self.emit(OpCode::JumpIfFalse { offset: DUMMY }, span);

        // then
        self.emit(OpCode::Pop, while_stmt.body.left_brace_tok.span);
        self.block(&while_stmt.body);
        self.emit_loop(loop_start, while_stmt.body.right_brace_tok.span);

        // end
        self.patch_jump(exit_jump);
        self.emit(OpCode::Pop, while_stmt.body.right_brace_tok.span);
    }

    fn block(&mut self, block: &Block) {
        self.begin_scope();
        self.block_inner(&block.body);
        self.end_scope(block.right_brace_tok.span);
    }

    fn block_inner(&mut self, items: &[Item]) {
        for i in items {
            self.item(i);
        }
    }

    fn expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Binary(binary_expr) => self.binary_expr(binary_expr),
            Expression::Unary(unary_expr) => self.unary_expr(unary_expr),
            Expression::Field(field_expr) => self.field_expr(field_expr),
            Expression::Group(group_expr) => self.group_expr(group_expr),
            Expression::Call(call_expr) => self.call_expr(call_expr),
            Expression::Primary(primary_expr) => self.primary_expr(primary_expr),
        }
    }

    fn binary_expr(&mut self, binary_expr: &BinaryExpr) {
        let op = binary_expr.operator.kind;

        if op == TokenKind::Equal {
            // for now only allow assigning to an identifier
            if let Expression::Primary(primary) = &*binary_expr.lhs {
                if primary.token.kind == TokenKind::Identifier {
                    let ident = Identifier { token: primary.token };
                    self.expression(&binary_expr.rhs);
                    if let Some((slot, local)) = self.resolve_local(ident) {
                        if !local.mutable {
                            self.errors.push(Error::AssignImmutableBinding {
                                bind_span: local.bind_span,
                                assign_span: binary_expr.span(),
                            });
                        }
                        self.emit(OpCode::SetLocal { slot }, binary_expr.span());
                    } else if let Some((_slot, _upvalue)) = self.resolve_upvalue(ident) {
                        todo!() // error: we can't assign upvalues
                    } else {
                        self.errors.push(Error::UndefinedName { name: ident.span() })
                    }
                    return
                }
            }
            // TODO more complex assignment target
            self.errors.push(Error::InvalidAssignmentTarget {
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

        self.expression(&binary_expr.lhs);
        self.expression(&binary_expr.rhs);

        let span = binary_expr.span();
        match op {
            TokenKind::NotEqual => {
                self.emit(OpCode::Equal, span);
                self.emit(OpCode::Not, span);
            }
            TokenKind::EqualEqual => {
                self.emit(OpCode::Equal, span);
            }
            TokenKind::Greater => {
                self.emit(OpCode::Greater, span);
            }
            TokenKind::GreaterEqual => {
                self.emit(OpCode::Less, span);
                self.emit(OpCode::Not, span);
            }
            TokenKind::Less => {
                self.emit(OpCode::Less, span);
            }
            TokenKind::LessEqual => {
                self.emit(OpCode::Greater, span);
                self.emit(OpCode::Not, span);
            }
            TokenKind::Plus => {
                self.emit(OpCode::Add, span);
            }
            TokenKind::Minus => {
                self.emit(OpCode::Subtract, span);
            }
            TokenKind::Mul => {
                self.emit(OpCode::Multiply, span);
            }
            TokenKind::Div => {
                self.emit(OpCode::Divide, span);
            }
            _ => unreachable!()
        }
    }

    fn and(&mut self, binary_expr: &BinaryExpr) {
        self.expression(&binary_expr.lhs);

        // if lhs is false, short-circuit, jump over rhs
        // span both lhs and the `and` operator
        let span = FreeSpan::join(binary_expr.lhs.span(), binary_expr.operator.span);
        let end_jump = self.emit(OpCode::JumpIfFalse { offset: DUMMY }, span);

        // pop lhs result, span of the `and` operator
        self.emit(OpCode::Pop, binary_expr.operator.span);
        self.expression(&binary_expr.rhs);

        self.patch_jump(end_jump);
    }

    fn or(&mut self, binary_expr: &BinaryExpr) {
        self.expression(&binary_expr.lhs);

        // if lhs is true, short-circuit, jump over rhs
        // span both lhs and the `or` operator
        let span = FreeSpan::join(binary_expr.lhs.span(), binary_expr.operator.span);
        let end_jump = self.emit(OpCode::JumpIfTrue { offset: DUMMY }, span);

        // pop lhs result, span of the `or` operator
        self.emit(OpCode::Pop, binary_expr.operator.span);
        self.expression(&binary_expr.rhs);

        self.patch_jump(end_jump);
    }

    fn unary_expr(&mut self, unary_expr: &UnaryExpr) {
        let op = unary_expr.operator.kind;

        self.expression(&unary_expr.expr);

        let span = unary_expr.span();
        match op {
            TokenKind::Not => {
                self.emit(OpCode::Not, span);
            }
            TokenKind::Minus => {
                self.emit(OpCode::Negate, span);
            }
            _ => unreachable!()
        }
    }

    fn field_expr(&mut self, _field_expr: &FieldExpr) {
        todo!()
    }

    fn group_expr(&mut self, group_expr: &GroupExpr) {
        if let Some(expr) = group_expr.expr.as_ref() {
            self.expression(expr)
        } else {
            self.emit(OpCode::Unit, group_expr.span());
        }
    }

    fn call_expr(&mut self, call_expr: &CallExpr) {
        self.expression(&call_expr.callee);
        for (i, arg) in call_expr.arguments.items.iter().enumerate() {
            if i >= MAX_ARGS {
                self.errors.push(Error::TooManyArguments {
                    extra_arg_span: arg.span(),
                    limit: MAX_ARGS,
                });
            }
            self.expression(arg);
        }
        let args = call_expr.arguments.items.len().try_into().unwrap();
        self.emit(OpCode::Call { args }, call_expr.span());
    }

    fn primary_expr(&mut self, primary_expr: &PrimaryExpr) {
        let op = primary_expr.token.kind;
        let span = primary_expr.span();

        match op {
            TokenKind::True => {
                self.emit(OpCode::True, span);
            }
            TokenKind::False => {
                self.emit(OpCode::False, span);
            }
            TokenKind::This => {
                todo!()
            }
            TokenKind::Super => {
                todo!()
            }
            TokenKind::Number => {
                self.float(primary_expr);
            }
            TokenKind::String => {
                self.string(primary_expr);
            }
            TokenKind::Identifier => {
                self.identifier(primary_expr);
            }
            _ => unreachable!()
        }
    }

    fn float(&mut self, primary: &PrimaryExpr) {
        let span = primary.token.span;
        let slice = span.anchor(&self.source).as_str();
        match slice.parse() {
            Ok(float) => {
                let value = Value::new_float(float);
                let key = self.insert_constant(value);
                self.emit(OpCode::Constant { key }, span);
            }
            Err(cause) => {
                self.errors.push(Error::InvalidNumberLiteral { cause, span });
            }
        }
    }

    fn string(&mut self, primary: &PrimaryExpr) {
        let span = primary.token.span;
        let slice = span.anchor(&self.source).as_str()
            .strip_prefix('"').unwrap()
            .strip_suffix('"').unwrap();
        let string = String::new(slice, self.alloc);
        let value = Value::new_object(string);
        let key = self.insert_constant(value);
        self.emit(OpCode::Constant { key }, span);
    }

    fn identifier(&mut self, primary: &PrimaryExpr) {
        let ident = Identifier { token: primary.token };
        if let Some((slot, _)) = self.resolve_local(ident) {
            self.emit(OpCode::GetLocal { slot }, ident.span());
        } else if let Some((slot, _)) = self.resolve_upvalue(ident) {
            self.emit(OpCode::GetUpvalue { slot }, ident.span());
        } else {
            self.errors.push(Error::UndefinedName { name: ident.span() })
        }
    }
}
