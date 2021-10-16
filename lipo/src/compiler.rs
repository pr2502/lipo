use crate::builtins::{Float, Function, String};
use crate::chunk::{ChunkBuf, LoopPoint, PatchPlace};
use crate::lexer::T;
use crate::name::Name;
use crate::opcode::OpCode;
use crate::parser::ast::*;
use crate::span::{FreeSpan, Spanned};
use crate::value::Value;
use crate::{Alloc, ObjectRef};


pub mod error;

use error::*;
use error::{CompilerError, Error};


// Limits
/// Maximum number of function arguments and parameters
const MAX_ARGS: usize = u8::MAX as usize;
const MAX_LOCALS: usize = u16::MAX as usize;
const MAX_TUPLE_ITEMS: usize = u8::MAX as usize;
const MAX_RECORD_ENTRIES: usize = u8::MAX as usize;


struct Emitter<'alloc> {
    alloc: &'alloc Alloc,
    source: ObjectRef<'alloc, String>,
    fn_stack: Vec<FnScope<'alloc>>,
    errors: Vec<CompilerError>,
}

struct FnScope<'alloc> {
    name: Name<'alloc>,
    fndef_span: FreeSpan,
    chunk: ChunkBuf<'alloc>,
    // Outer Vec represents nested blocks
    locals: Vec<Vec<Local<'alloc>>>,
    upvalues: Vec<Upvalue<'alloc>>,
    // Can capture upvalues
    closure: bool,
}

#[derive(Clone, Copy)]
struct Local<'alloc> {
    // Name of the binding / reference
    name: Name<'alloc>,
    // Where the binding was introduced
    bind_span: FreeSpan,
    // Binding can be reassigned
    mutable: bool,
}

#[derive(Clone, Copy)]
struct Upvalue<'alloc> {
    // Name of the binding / reference
    name: Name<'alloc>,
    // Where the upvalue was captured (only the first time if used referenced multiple times)
    capture_span: FreeSpan,
    // Reference to the parent scope's bindings
    reference: UpvalueRef,
}

// upvalue can refer to a local binding or another upvalue
#[derive(Clone, Copy)]
enum UpvalueRef {
    Local(u16),
    Upvalue(u8),
}

pub fn compile<'alloc>(ast: AST<'alloc>, alloc: &'alloc Alloc) -> Result<ObjectRef<'alloc, Function<'alloc>>, Vec<CompilerError>> {
    let mut emitter = Emitter {
        alloc,
        source: ast.source,
        fn_stack: Vec::default(),
        errors: Vec::default(),
    };
    let script_name = emitter.intern_string("<script>");

    emitter.fn_stack.push(FnScope {
        name: script_name,
        fndef_span: FreeSpan::zero(),
        chunk: ChunkBuf::new(emitter.source, 0),
        locals: vec![Vec::default()],
        upvalues: Vec::default(),
        closure: false,
    });

    // Empty script outputs Unit
    emitter.emit(OpCode::Unit, FreeSpan::zero());

    emitter.block_inner(&ast.items);

    // Implicit return at the end of script
    emitter.emit(OpCode::Return, ast.eof.span);

    let script = emitter.fn_stack.pop().expect("BUG: missing function");
    assert!(emitter.fn_stack.is_empty(), "BUG: unfinished function");

    if !emitter.errors.is_empty() {
        // Don't even try to check the Chunk for correctness, if there were any errors it likely
        // contains nonsense.
        return Err(emitter.errors);
    }

    Ok(Function::new(script.chunk.check(0), 0, script_name, alloc))
}

const DUMMY: u16 = u16::MAX;

impl<'alloc> Emitter<'alloc> {
    fn error(&mut self, err: impl Error) {
        self.errors.push(CompilerError::new(err));
    }

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
        self.fn_scope_mut().chunk.patch_jump(place);
    }

    fn loop_point(&self) -> LoopPoint {
        self.fn_scope().chunk.loop_point()
    }

    fn emit_loop(&mut self, loop_point: LoopPoint, span: FreeSpan) {
        self.fn_scope_mut().chunk.emit_loop(loop_point, span);
    }

    fn insert_constant(&mut self, value: Value<'alloc>) -> u16 {
        self.fn_scope_mut().chunk.insert_constant(value)
    }

    fn intern_ident(&self, ident: Identifier) -> Name<'alloc> {
        self.intern_string(ident.span.anchor(&self.source).as_str())
    }

    fn intern_string(&self, string: &str) -> Name<'alloc> {
        self.alloc.intern_name(string)
    }
}

impl<'alloc> FnScope<'alloc> {
    fn find_local(&self, name: Name<'alloc>) -> Option<(u16, Local<'alloc>)> {
        self.locals.iter()
            .flatten()
            .enumerate()
            .find(|(_, loc)| loc.name == name)
            .map(|(slot, loc)| (slot.try_into().unwrap(), *loc))
    }

    fn find_upvalue(&self, name: Name<'alloc>) -> Option<(u8, Upvalue<'alloc>)> {
        self.upvalues.iter()
            .enumerate()
            .find(|(_, upval)| upval.name == name)
            .map(|(idx, upval)| (idx.try_into().unwrap(), *upval))
    }
}

impl<'alloc> Emitter<'alloc> {
    fn add_local(&mut self, name: Name<'alloc>, mutable: bool, span: FreeSpan) {
        if self.fn_scope().locals.len() >= MAX_LOCALS {
            self.error(TooManyLocals {
                span,
                limit: MAX_LOCALS,
            });
            return;
        }
        let shadowing = self.fn_scope()
            .locals.last().unwrap().iter()
            .rev()
            .find(|loc| loc.name == name);
        if let Some(local) = shadowing.copied() {
            self.error(Shadowing {
                shadowing_span: span,
                shadowed_span: local.bind_span,
            });
            return;
        }
        self.fn_scope_mut()
            .locals.last_mut().unwrap()
            .push(Local {
                name,
                bind_span: span,
                mutable,
            });
    }

    fn resolve_local(&self, name: Name<'alloc>) -> Option<(u16, Local<'alloc>)> {
        self.fn_scope()
            .find_local(name)
    }

    fn resolve_upvalue(&mut self, name: Name<'alloc>, span: FreeSpan) -> Option<(u8, Upvalue<'alloc>)> {
        // Find an existing upvalue
        if let Some(existing) = self.fn_scope().find_upvalue(name) {
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
                let found = fn_scope.find_local(name)?;
                Some((fn_scope_idx, found))
            })?;

        if local.mutable {
            self.error(CaptureMutable {
                bind_span: local.bind_span,
                closure_def_span: self.fn_scope().fndef_span,
                capture_span: span,
            });
        }

        // For each enclosing fn_scope register an upvalue, if it doesn't already exist
        //
        // `upvalue_ref` keeps the reference to the previous upvalue/local, we initialize it
        // with a reference to the original Local.
        let mut upvalue_ref = UpvalueRef::Local(slot);
        for fn_scope in &mut self.fn_stack[fn_scope_idx + 1 ..] {
            if let Some((slot, _)) = fn_scope.find_upvalue(name) {
                // If the upvalue is found we just update the reference for the next iteration to
                // point to the existing upvalue.
                upvalue_ref = UpvalueRef::Upvalue(slot);
            } else {
                let slot: u8 = fn_scope.upvalues.len()
                    .try_into().expect("too many upvalues"); // TODO error handling
                if !fn_scope.closure {
                    self.errors.push(CompilerError::new(CaptureNotClosure {
                        bind_span: local.bind_span,
                        capture_span: span,
                        fndef_span: fn_scope.fndef_span,
                    }));
                }
                fn_scope.upvalues.push(Upvalue {
                    name,
                    capture_span: span,
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
        self.fn_scope_mut().locals.push(Vec::new());
    }

    fn end_scope(&mut self, span: FreeSpan) {
        let scope_locals = self.fn_scope_mut()
            .locals.pop().unwrap();

        let mut left = scope_locals.len();
        while left > 0 {
            let pop1 = Ord::min((u8::MAX as usize) + 1, left);
            left -= pop1;

            let n = (pop1 - 1).try_into().unwrap();
            // PopBlock pops `n+1` because we can encode `0` pops as not emitting it
            self.emit(OpCode::PopBlock { n }, span);
        }
    }
}

impl<'alloc> Emitter<'alloc> {
    fn item(&mut self, item: &Item) {
        // Pop the previous item output
        self.emit(OpCode::Pop, item.span().shrink_to_lo());

        match item {
            Item::Fn(fn_item) => self.fn_item(fn_item),
            Item::Const(_const_item) => todo!("const item"),
            Item::Let(let_item) => self.let_item(let_item),
            Item::Statement(stmt) => self.statement(stmt),
            Item::Expr(expr) => self.expr_item(expr),
        }
    }

    fn fn_item(&mut self, fn_item: &FnItem) {
        let name = self.intern_ident(fn_item.name);

        // Add an immutable local into the outer fn
        self.add_local(name, false, fn_item.span());

        // Reference to callee in the 0th stack slot
        let recur = Local {
            name,
            bind_span: fn_item.span(),
            mutable: false,
        };
        self.fn_stack.push(FnScope {
            name,
            fndef_span: fn_item.span(),
            chunk: ChunkBuf::new(self.source, fn_item.parameters.items.len() + 1),
            locals: vec![vec![recur]],
            upvalues: Vec::default(),
            // No upvalues in fn_item
            closure: false,
        });

        for (i, param) in fn_item.parameters.items.iter().enumerate() {
            if i >= MAX_ARGS {
                self.error(TooManyParameters {
                    extra_param_span: param.span(),
                    fn_params_span: FreeSpan::join(fn_item.left_paren_tok.span, fn_item.right_paren_tok.span),
                    limit: MAX_ARGS,
                });
            }
            let param_name = self.intern_ident(param.name);
            self.add_local(param_name, param.mut_tok.is_some(), param.span());
        }

        // Empty function body implicitly returns Unit
        self.emit(OpCode::Unit, fn_item.body.left_brace_tok.span);

        self.block_inner(&fn_item.body.body);

        // Implicit Return at the end of function body
        self.emit(OpCode::Return, fn_item.body.right_brace_tok.span);

        let fn_scope = self.fn_stack.pop().unwrap();
        let function = Value::from(Function::new(
            fn_scope.chunk.check(0),
            fn_item.parameters.items.len().try_into().unwrap(),
            fn_scope.name,
            self.alloc,
        ));
        let key = self.insert_constant(function);
        // Populate the local variable declared above
        self.emit(OpCode::Constant { key }, fn_item.span());

        // Item output is Unit
        self.emit(OpCode::Unit, fn_item.span().shrink_to_hi());
    }

    fn let_item(&mut self, let_item: &LetItem) {
        let span = let_item.span();

        if let Some(init) = &let_item.init {
            self.expression(&init.expr);
        } else {
            // empty initializer, set value to Unit
            self.emit(OpCode::Unit, span);
        }

        let name = self.intern_ident(let_item.name);
        self.add_local(name, let_item.mut_tok.is_some(), let_item.span());

        // Item output is Unit
        self.emit(OpCode::Unit, let_item.span().shrink_to_hi());
    }

    fn statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::For(for_stmt) => self.for_stmt(for_stmt),
            Statement::Assert(assert_stmt) => self.assert_stmt(assert_stmt),
            Statement::Print(print_stmt) => self.print_stmt(print_stmt),
            Statement::Return(return_stmt) => self.return_stmt(return_stmt),
            Statement::While(while_stmt) => self.while_stmt(while_stmt),
        }

        // Item output is Unit
        self.emit(OpCode::Unit, stmt.span().shrink_to_hi());
    }

    fn for_stmt(&mut self, _for_stmt: &ForStmt) {
        todo!()
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
            self.error(ReturnFromScript {
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
        // Loop block doesn't evaluate to anything
        self.emit(OpCode::Pop, while_stmt.body.right_brace_tok.span);
        self.emit_loop(loop_start, while_stmt.body.right_brace_tok.span);

        // end
        self.patch_jump(exit_jump);
        self.emit(OpCode::Pop, while_stmt.body.right_brace_tok.span);
    }

    fn block(&mut self, block: &Block) {
        self.begin_scope();

        // Empty block evaluates to Unit
        self.emit(OpCode::Unit, block.span().shrink_to_lo());

        self.block_inner(&block.body);

        self.end_scope(block.right_brace_tok.span);
    }

    fn block_inner(&mut self, items: &[Item]) {
        for i in items {
            self.item(i);
        }
    }

    fn expr_item(&mut self, expr: &Expr) {
        self.expression(&expr.expr);
        if let Some(semicolon_tok) = &expr.semicolon_tok {
            self.emit(OpCode::Pop, semicolon_tok.span);
            self.emit(OpCode::Unit, semicolon_tok.span);
        }
    }

    fn expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Unit(unit_expr) => self.unit_expr(unit_expr),
            Expression::Primary(primary_expr) => self.primary_expr(primary_expr),
            Expression::Unary(unary_expr) => self.unary_expr(unary_expr),
            Expression::Binary(binary_expr) => self.binary_expr(binary_expr),
            Expression::Group(group_expr) => self.group_expr(group_expr),
            Expression::Tuple(tuple_expr) => self.tuple_expr(tuple_expr),
            Expression::Record(record_expr) => self.record_expr(record_expr),
            Expression::Block(block) => self.block(block),
            Expression::If(if_expr) => self.if_expr(if_expr),
            Expression::Fn(fn_expr) => self.fn_expr(fn_expr),
            Expression::Call(call_expr) => self.call_expr(call_expr),
            Expression::String(string_expr) => self.string_expr(string_expr),
        }
    }

    fn binary_expr(&mut self, binary_expr: &BinaryExpr) {
        let op = binary_expr.operator.kind;

        if op == T::Equal {
            // For now only allow assigning to an identifier
            if let Expression::Primary(primary) = &*binary_expr.lhs {
                if primary.token.kind == T::Identifier {
                    let name_span = primary.token.span;
                    let name = self.intern_ident(Identifier { span: name_span });
                    self.expression(&binary_expr.rhs);
                    if let Some((slot, local)) = self.resolve_local(name) {
                        if !local.mutable {
                            self.error(AssignImmutableBinding {
                                bind_span: local.bind_span,
                                assign_span: binary_expr.span(),
                            });
                        }
                        self.emit(OpCode::SetLocal { slot }, binary_expr.span());
                        // Expression has to evaluate into something, assignment evaluates to Unit
                        self.emit(OpCode::Unit, binary_expr.span());
                    } else if let Some((_slot, _upvalue)) = self.resolve_upvalue(name, name_span) {
                        todo!() // error: we can't assign upvalues
                    } else {
                        self.error(UndefinedName { name_span });
                    }
                    return;
                }
            }
            // TODO more complex assignment target?
            self.error(InvalidAssignmentTarget {
                span: binary_expr.lhs.span(),
            });
            return;
        }

        if op == T::Dot {
            self.expression(&binary_expr.lhs);

            if let Expression::Primary(primary) = &*binary_expr.rhs {
                match primary.token.kind {
                    T::Identifier => {
                        let name = self.intern_ident(Identifier { span: primary.token.span });
                        let value = Value::from(name);
                        let name_key = self.insert_constant(value);

                        let span = FreeSpan::join(binary_expr.operator.span, binary_expr.rhs.span());
                        self.emit(OpCode::GetRecord { name_key }, span);
                        return;
                    }
                    T::DecimalNumber => {
                        let span = primary.token.span.anchor(&self.source).as_str();
                        let slot = span.parse::<u8>().expect("invalid tuple slot");

                        let span = FreeSpan::join(binary_expr.operator.span, binary_expr.rhs.span());
                        self.emit(OpCode::GetTuple { slot }, span);
                        return;
                    }
                    _ => {}
                }
            }
            self.error(InvalidFieldExpr {
                span: binary_expr.rhs.span(),
            });
            return;
        }

        if op == T::Or {
            return self.or(binary_expr);
        }

        if op == T::And {
            return self.and(binary_expr);
        }

        // Normal binary operations with eagerly evaluated operands

        self.expression(&binary_expr.lhs);
        self.expression(&binary_expr.rhs);

        let span = binary_expr.span();
        match op {
            T::NotEqual => {
                self.emit(OpCode::Equal, span);
                self.emit(OpCode::Not, span);
            }
            T::EqualEqual => {
                self.emit(OpCode::Equal, span);
            }
            T::Greater => {
                self.emit(OpCode::Greater, span);
            }
            T::GreaterEqual => {
                self.emit(OpCode::Less, span);
                self.emit(OpCode::Not, span);
            }
            T::Less => {
                self.emit(OpCode::Less, span);
            }
            T::LessEqual => {
                self.emit(OpCode::Greater, span);
                self.emit(OpCode::Not, span);
            }
            T::Plus => {
                self.emit(OpCode::Add, span);
            }
            T::Minus => {
                self.emit(OpCode::Subtract, span);
            }
            T::Mul => {
                self.emit(OpCode::Multiply, span);
            }
            T::Div => {
                self.emit(OpCode::Divide, span);
            }
            _ => unreachable!()
        }
    }

    fn and(&mut self, binary_expr: &BinaryExpr) {
        self.expression(&binary_expr.lhs);

        // If lhs is false, short-circuit, jump over rhs
        // Span both lhs and the `and` operator
        let span = FreeSpan::join(binary_expr.lhs.span(), binary_expr.operator.span);
        let end_jump = self.emit(OpCode::JumpIfFalse { offset: DUMMY }, span);

        // Pop lhs result, span of the `and` operator
        self.emit(OpCode::Pop, binary_expr.operator.span);
        self.expression(&binary_expr.rhs);

        self.patch_jump(end_jump);
    }

    fn or(&mut self, binary_expr: &BinaryExpr) {
        self.expression(&binary_expr.lhs);

        // If lhs is true, short-circuit, jump over rhs
        // Span of both lhs and the `or` operator
        let span = FreeSpan::join(binary_expr.lhs.span(), binary_expr.operator.span);
        let end_jump = self.emit(OpCode::JumpIfTrue { offset: DUMMY }, span);

        // Pop lhs result, span of the `or` operator
        self.emit(OpCode::Pop, binary_expr.operator.span);
        self.expression(&binary_expr.rhs);

        self.patch_jump(end_jump);
    }

    fn unary_expr(&mut self, unary_expr: &UnaryExpr) {
        let op = unary_expr.operator.kind;

        self.expression(&unary_expr.expr);

        let span = unary_expr.span();
        match op {
            T::Not => {
                self.emit(OpCode::Not, span);
            }
            T::Minus => {
                self.emit(OpCode::Negate, span);
            }
            _ => unreachable!()
        }
    }

    fn unit_expr(&mut self, unit_expr: &UnitExpr) {
        self.emit(OpCode::Unit, unit_expr.span());
    }

    fn group_expr(&mut self, group_expr: &GroupExpr) {
        self.expression(&group_expr.expr);
    }

    fn tuple_expr(&mut self, tuple_expr: &TupleExpr) {
        for (i, item) in tuple_expr.exprs.items.iter().enumerate() {
            if i >= MAX_TUPLE_ITEMS {
                self.error(TooManyTupleItems {
                    extra_item_span: item.span(),
                    tuple_expr_span: tuple_expr.span(),
                    limit: MAX_ARGS,
                });
                return;
            }
            self.expression(item);
        }
        let len = tuple_expr.exprs.items.len().try_into().unwrap();
        self.emit(OpCode::MakeTuple { len }, tuple_expr.span());
    }

    fn record_expr(&mut self, record_expr: &RecordExpr) {
        let mut entries = Vec::new();

        for (i, entry) in record_expr.entries.items.iter().enumerate() {
            if i > MAX_RECORD_ENTRIES {
                self.error(TooManyRecordEntries {
                    extra_entry_span: entry.span(),
                    record_expr_span: record_expr.span(),
                    limit: MAX_RECORD_ENTRIES,
                });
                return;
            }
            entries.push((self.intern_ident(entry.name), entry));
        }

        // Stable sort to make the duplicate diagnostics make sense (keep source order)
        entries.sort_by_key(|(name, _)| *name);

        // Ensure there are no duplicate entries
        for [(first_name, first), (second_name, second)] in entries.array_windows() {
            if first_name == second_name {
                self.error(DuplicateRecordEntry {
                    duplicate_span: second.name.span,
                    previous_span: first.name.span,
                    record_expr_span: record_expr.span(),
                });
            }
        }

        // Emit record keys
        for (name, entry) in entries.iter() {
            let key = self.insert_constant(Value::from(*name));
            self.emit(OpCode::Constant { key }, entry.name.span);
        }

        // Emit entry values
        for (name, entry) in entries.iter() {
            if let Some(value) = &entry.value {
                self.expression(&value.init);
            } else {
                // Name punning - if there is no initializer expression, look up the entry name in
                // the local scope
                self.name(*name, entry.name.span);
            }
        }

        let len = entries.len().try_into().unwrap();
        self.emit(OpCode::MakeRecord { len }, record_expr.span());
    }

    fn if_expr(&mut self, if_expr: &IfExpr) {
        // if <pred>
        self.expression(&if_expr.pred);
        let then_jump = self.emit(OpCode::JumpIfFalse { offset: DUMMY }, if_expr.if_tok.span);

        // then
        self.emit(OpCode::Pop, if_expr.if_tok.span);
        self.block(&if_expr.body);
        let else_jump = self.emit(OpCode::Jump { offset: DUMMY }, if_expr.if_tok.span);

        // else
        self.patch_jump(then_jump);
        self.emit(OpCode::Pop, if_expr.if_tok.span);
        if let Some(else_branch) = &if_expr.else_branch {
            self.block(&else_branch.body);
        } else {
            self.emit(OpCode::Unit, if_expr.span().shrink_to_hi());
        }

        // end
        self.patch_jump(else_jump);
    }

    fn fn_expr(&mut self, fn_expr: &FnExpr) {
        let name = self.intern_string("<closure>");
        // FIXME? Because the callee is lower on the stack than its arguments the VM has to leave
        // it on the stack while it runs, and because we use one field to mean both where the
        // callee stack starts and where it should be truncated when it returns we have to leave
        // the callee in stack slot 0. However unlike named functions (fn_item) anonymous functions
        // can't call themselves, so the first variable name should be unresolvable.
        let dummy = Local {
            name,
            bind_span: FreeSpan::zero(),
            mutable: false,
        };
        self.fn_stack.push(FnScope {
            name,
            fndef_span: fn_expr.span(),
            chunk: ChunkBuf::new(self.source, fn_expr.parameters.items.len() + 1),
            locals: Vec::from([Vec::from([dummy])]),
            upvalues: Vec::default(),
            closure: true,
        });

        for (i, param) in fn_expr.parameters.items.iter().enumerate() {
            if i >= MAX_ARGS {
                self.error(TooManyParameters {
                    extra_param_span: param.span(),
                    fn_params_span: FreeSpan::join(fn_expr.left_paren_tok.span, fn_expr.right_paren_tok.span),
                    limit: MAX_ARGS,
                });
            }
            let param_name = self.intern_ident(param.name);
            self.add_local(param_name, param.mut_tok.is_some(), param.span());
        }

        self.expression(&fn_expr.body);
        self.emit(OpCode::Return, fn_expr.body.span().shrink_to_hi());

        let function = self.fn_stack.pop().unwrap();
        let upvals = function.upvalues.len().try_into().unwrap();

        for upval in &function.upvalues {
            match upval.reference {
                UpvalueRef::Local(slot) => self.emit(OpCode::GetLocal { slot }, upval.capture_span),
                UpvalueRef::Upvalue(slot) => self.emit(OpCode::GetUpvalue { slot }, upval.capture_span),
            };
        }

        let function = Value::from(Function::new(
            function.chunk.check(upvals),
            fn_expr.parameters.items.len().try_into().unwrap(),
            function.name,
            self.alloc,
        ));
        let fn_key = self.insert_constant(function);
        self.emit(OpCode::Closure { fn_key, upvals }, fn_expr.span());
    }

    fn call_expr(&mut self, call_expr: &CallExpr) {
        // TODO PERF special-case Expr::Field as a direct method lookup on the lhs type
        self.expression(&call_expr.callee);
        for (i, arg) in call_expr.arguments.items.iter().enumerate() {
            if i >= MAX_ARGS {
                self.error(TooManyArguments {
                    extra_arg_span: arg.span(),
                    call_span: call_expr.span(),
                    limit: MAX_ARGS,
                });
                return;
            }
            self.expression(arg);
        }
        let args = call_expr.arguments.items.len().try_into().unwrap();
        self.emit(OpCode::Call { args }, call_expr.span());
    }

    fn string_expr(&mut self, string_expr: &StringExpr) {
        let fragments = &string_expr.fragments;

        if fragments.is_empty() {
            self.string_frag_literal("", string_expr.span());
            return;
        }

        if let [StringFragment::Literal { unescaped, .. }] = fragments.as_slice() {
            self.string_frag_literal(unescaped, string_expr.span());
            return;
        }
        for frag in fragments {
            match frag {
                StringFragment::Literal { span, unescaped } => self.string_frag_literal(unescaped, *span),
                StringFragment::Interpolation { ident, fmt: None } => {
                    let name = self.intern_ident(*ident);
                    self.name(name, ident.span);
                    // TODO format with empty fmt
                }
                StringFragment::Interpolation { ident: _, fmt: Some(_fmt) } => {
                    todo!("string interpolation fmt");
                }
            }
        }

        if fragments.len() == 1 {
            // No need to concat 1 String
            return;
        }

        let n = (fragments.len() - 2).try_into()
            .expect("error: too many string fragments");

        self.emit(OpCode::Concat { n }, string_expr.span());
    }

    fn string_frag_literal(&mut self, unescaped: &str, span: FreeSpan) {
        let string = String::new(unescaped, self.alloc);
        let value = Value::from(string);
        let key = self.insert_constant(value);
        self.emit(OpCode::Constant { key }, span);
    }

    fn primary_expr(&mut self, primary_expr: &PrimaryExpr) {
        let op = primary_expr.token.kind;
        let span = primary_expr.span();

        match op {
            T::True => {
                self.emit(OpCode::True, span);
            },
            T::False => {
                self.emit(OpCode::False, span);
            },
            T::SelfKw => {
                todo!()
            },
            T::BinaryNumber |
            T::OctalNumber |
            T::HexadecimalNumber => {
                todo!()
            },
            T::DecimalNumber => {
                self.int32(primary_expr);
            },
            T::DecimalPointNumber |
            T::ExponentialNumber => {
                self.float(primary_expr);
            },
            T::Identifier => {
                let ident = Identifier { span };
                let name = self.intern_ident(ident);
                self.name(name, span);
            },
            _ => unreachable!()
        }
    }

    fn int32(&mut self, primary: &PrimaryExpr) {
        let span = primary.token.span;
        let slice = span.anchor(&self.source).as_str();
        match slice.parse::<i32>() {
            Ok(int) => {
                let value = Value::from(int);
                let key = self.insert_constant(value);
                self.emit(OpCode::Constant { key }, span);
            }
            Err(cause) => {
                self.error(InvalidInt32Literal { cause, span });
            }
        }
    }

    fn float(&mut self, primary: &PrimaryExpr) {
        let span = primary.token.span;
        let slice = span.anchor(&self.source).as_str();
        match slice.parse::<f64>() {
            Ok(float) => {
                let float = Float::new(float, self.alloc)
                    .expect("impossible number literal");
                let value = Value::from(float);
                let key = self.insert_constant(value);
                self.emit(OpCode::Constant { key }, span);
            }
            Err(cause) => {
                self.error(InvalidFloatLiteral { cause, span });
            }
        }
    }

    fn name(&mut self, name: Name<'alloc>, span: FreeSpan) {
        if let Some((slot, _)) = self.resolve_local(name) {
            self.emit(OpCode::GetLocal { slot }, span);
        } else if let Some((slot, _)) = self.resolve_upvalue(name, span) {
            self.emit(OpCode::GetUpvalue { slot }, span);
        } else {
            self.error(UndefinedName { name_span: span });
        }
    }
}
