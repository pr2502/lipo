use crate::builtins::{Float, Function, String};
use crate::chunk::{ChunkBuf, LoopPoint, PatchPlace};
use crate::fmt::SourceDebug;
use crate::lexer::T;
use crate::name::Name;
use crate::opcode::OpCode;
use crate::parser::ast::{self, *};
use crate::span::{FreeSpan, Spanned};
use crate::value::Value;
use crate::{Alloc, ObjectRef};

pub mod const_eval;
use const_eval::{ConstEval, ConstId};

pub mod error;
use error::kind::*;
use error::{CompilerError, Error};


// Limits
/// Maximum number of function arguments and parameters
const MAX_ARGS: usize = u8::MAX as usize;
const MAX_LOCALS: usize = u16::MAX as usize;
const MAX_CONSTS: usize = u16::MAX as usize;
const MAX_TUPLE_ITEMS: usize = u8::MAX as usize;
const MAX_RECORD_ENTRIES: usize = u8::MAX as usize;

struct Emitter<'a, 'alloc> {
    alloc: &'a Alloc<'a, 'alloc>,
    const_eval: ConstEval<'alloc>,
    source: ObjectRef<'alloc, String>,
    fn_stack: Vec<FnScope<'alloc>>,
    errors: Vec<CompilerError>,
}

struct FnScope<'alloc> {
    id: ConstId,
    name: Name<'alloc>,
    fndef_span: FreeSpan,
    chunk: ChunkBuf<'alloc>,
    blocks: Vec<BlockScope<'alloc>>,
    upvalues: Vec<Upvalue<'alloc>>,
    // Can capture upvalues other than consts
    closure: bool,
}

struct BlockScope<'alloc> {
    locals: Vec<Local<'alloc>>,
    consts: Vec<Const<'alloc>>,
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
struct Const<'alloc> {
    name: Name<'alloc>,
    bind_span: FreeSpan,
    id: ConstId,
    const_key: u16,
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

pub fn compile<'alloc>(
    ast: Ast<'alloc>,
    alloc: &Alloc<'_, 'alloc>,
) -> Result<ObjectRef<'alloc, Function<'alloc>>, Vec<CompilerError>> {
    let mut emitter = Emitter {
        alloc,
        const_eval: ConstEval::new(),
        source: ast.source,
        fn_stack: Vec::new(),
        errors: Vec::new(),
    };
    let script_name = emitter.intern_string("<script>");
    let id = emitter.const_eval.add_const(script_name);

    emitter.fn_stack.push(FnScope {
        id,
        name: script_name,
        fndef_span: FreeSpan::zero(),
        chunk: ChunkBuf::new(emitter.source, 0),
        blocks: Vec::new(),
        upvalues: Vec::new(),
        closure: false,
    });

    emitter.begin_scope();
    emitter.block_inner(&ast.items);
    emitter.emit(OpCode::Return, ast.eof.span);

    let script = emitter.fn_stack.pop().expect("BUG: missing function");
    assert!(emitter.fn_stack.is_empty(), "BUG: unfinished function");

    if !emitter.errors.is_empty() {
        // Don't even try to check the Chunk for correctness, if there were any errors
        // it likely contains nonsense.
        return Err(emitter.errors);
    }

    emitter.const_eval.print();
    // TODO error handling
    emitter.const_eval.resolve_all();

    let chunk = script.chunk.check();
    chunk.resolve_constants(&emitter.const_eval);

    Ok(Function::new(chunk, script_name, alloc))
}

impl<'alloc> Emitter<'_, 'alloc> {
    fn error(&mut self, err: impl Error) {
        self.errors.push(CompilerError::new(err));
    }

    fn fn_scope(&self) -> &FnScope<'alloc> {
        self.fn_stack.last().expect("BUG: missing FnScope")
    }

    fn fn_scope_mut(&mut self) -> &mut FnScope<'alloc> {
        self.fn_stack.last_mut().expect("BUG: missing FnScope")
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

    fn insert_constant(&mut self, promise: ConstId) -> u16 {
        self.fn_scope_mut().chunk.insert_constant(promise)
    }

    fn intern_token(&self, name_tok: ast::Name) -> Name<'alloc> {
        self.intern_string(name_tok.span.anchor(&self.source).as_str())
    }

    fn intern_string(&self, string: &str) -> Name<'alloc> {
        self.alloc.intern_name(string)
    }
}

impl<'alloc> FnScope<'alloc> {
    fn block(&self) -> &BlockScope<'alloc> {
        self.blocks.last().expect("BUG: missing block scope")
    }

    fn block_mut(&mut self) -> &mut BlockScope<'alloc> {
        self.blocks.last_mut().expect("BUG: missing block scope")
    }

    fn find_local(&self, name: Name<'alloc>) -> Option<(u16, Local<'alloc>)> {
        self.blocks
            .iter()
            .flat_map(|BlockScope { locals, .. }| locals)
            .enumerate()
            .find(|(_, loc)| loc.name == name)
            .map(|(slot, loc)| (slot.try_into().unwrap(), *loc))
    }

    fn find_const(&self, name: Name<'alloc>) -> Option<Const<'alloc>> {
        self.blocks
            .iter()
            .flat_map(|BlockScope { consts, .. }| consts)
            .find(|cnst| cnst.name == name)
            .copied()
    }

    fn find_upvalue(&self, name: Name<'alloc>) -> Option<(u8, Upvalue<'alloc>)> {
        self.upvalues
            .iter()
            .enumerate()
            .find(|(_, upval)| upval.name == name)
            .map(|(idx, upval)| (idx.try_into().unwrap(), *upval))
    }
}

impl<'alloc> Emitter<'_, 'alloc> {
    fn add_const(&mut self, name: Name<'alloc>, span: FreeSpan) {
        let n_consts = self
            .fn_scope()
            .blocks
            .iter()
            .map(|BlockScope { consts, .. }| consts.len())
            .sum::<usize>();
        if n_consts >= MAX_CONSTS {
            todo!() // error: too many consts
        }
        let conflict = self
            .fn_scope()
            .block()
            .consts
            .iter()
            .rev()
            .find(|cnst| cnst.name == name);
        if let Some(cnst) = conflict.copied() {
            self.error(Shadowing {
                // TODO specialize shadowing into const-const, local-const
                shadowing_span: span,
                shadowed_span: cnst.bind_span,
            });
        }
        // Insert placeholder constant to obtain a stable key
        let id = self.const_eval.add_const(name);
        let const_key = self.fn_scope_mut().chunk.insert_constant(id);
        self.fn_scope_mut().block_mut().consts.push(Const {
            name,
            bind_span: span,
            id,
            // const_cell,
            const_key,
        });
    }

    fn add_local(&mut self, name: Name<'alloc>, mutable: bool, span: FreeSpan) {
        let n_locals = self
            .fn_scope()
            .blocks
            .iter()
            .map(|BlockScope { locals, .. }| locals.len())
            .sum::<usize>();
        if n_locals >= MAX_LOCALS {
            self.error(TooManyLocals { span, limit: MAX_LOCALS });
            return;
        }
        let shadowing = self
            .fn_scope()
            .block()
            .locals
            .iter()
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
            .block_mut()
            .locals
            .push(Local { name, bind_span: span, mutable });
    }

    fn resolve_local(&self, name: Name<'alloc>) -> Option<(u16, Local<'alloc>)> {
        self.fn_scope().find_local(name)
    }

    fn resolve_const(&mut self, name: Name<'alloc>) -> Option<Const<'alloc>> {
        if let Some(local_const) = self.fn_scope().find_const(name) {
            return Some(local_const);
        }

        let outer_const = self
            .fn_stack
            .iter()
            .rev()
            .skip(1)
            .find_map(|fn_scope| fn_scope.find_const(name))?;
        let const_key = self.fn_scope_mut().chunk.insert_constant(outer_const.id);
        let local_const = Const { const_key, ..outer_const };
        self.fn_scope_mut().blocks[0].consts.push(local_const);

        // we've looked up a const outside a fn_item function body
        self.const_eval
            .dependency(self.fn_scope().id, outer_const.id);

        Some(local_const)
    }

    fn resolve_upvalue(
        &mut self,
        name: Name<'alloc>,
        span: FreeSpan,
    ) -> Option<(u8, Upvalue<'alloc>)> {
        // Find an existing upvalue
        if let Some(existing) = self.fn_scope().find_upvalue(name) {
            return Some(existing);
        }

        // Find which enclosing scope, if any, has the binding we want
        //
        // If no binding is found we return `None`
        let (fn_scope_idx, (slot, local)) = self
            .fn_stack
            .iter()
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
        // `upvalue_ref` keeps the reference to the previous upvalue/local, we
        // initialize it with a reference to the original Local.
        let mut upvalue_ref = UpvalueRef::Local(slot);
        for fn_scope in &mut self.fn_stack[fn_scope_idx + 1..] {
            if let Some((slot, _)) = fn_scope.find_upvalue(name) {
                // If the upvalue is found we just update the reference for the next iteration
                // to point to the existing upvalue.
                upvalue_ref = UpvalueRef::Upvalue(slot);
            } else {
                let slot: u8 = fn_scope
                    .upvalues
                    .len()
                    .try_into()
                    .expect("too many upvalues"); // TODO error handling
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
        let fn_scope = self.fn_scope_mut();
        fn_scope
            .blocks
            .push(BlockScope { locals: Vec::new(), consts: Vec::new() });
    }

    fn end_scope(&mut self, span: FreeSpan) {
        let scope = self.fn_scope_mut().blocks.pop().unwrap();

        let mut left = scope.locals.len();
        while left > 0 {
            let pop1 = Ord::min((u8::MAX as usize) + 1, left);
            left -= pop1;

            let n = (pop1 - 1).try_into().unwrap();
            // PopBlock pops `n+1` because we can encode `0` pops as not emitting it
            self.emit(OpCode::PopBlock { n }, span);
        }
    }
}

impl<'alloc> Emitter<'_, 'alloc> {
    fn fn_item(&mut self, fn_item: &FnItem) {
        dbg!(fn_item.wrap(&self.source));
        let name = self.intern_token(fn_item.name);
        // If the function has too many parameters we'll emit a specific error, the `0`
        // is a dummy value which will get discarded with the chunk.
        let params = fn_item.parameters.items.len().try_into().unwrap_or(0);

        let Const { id, .. } = self.resolve_const(name).unwrap();

        // Reference to callee in the 0th stack slot
        let recur = Local {
            name,
            bind_span: fn_item.span(),
            mutable: false,
        };
        self.fn_stack.push(FnScope {
            id,
            name,
            fndef_span: fn_item.span(),
            chunk: ChunkBuf::new(self.source, params),
            blocks: Vec::from([BlockScope {
                locals: Vec::from([recur]),
                consts: Vec::new(),
            }]),
            upvalues: Vec::new(),
            // No upvalues in fn_item
            closure: false,
        });

        for (i, param) in fn_item.parameters.items.iter().enumerate() {
            if i >= MAX_ARGS {
                self.error(TooManyParameters {
                    extra_param_span: param.span(),
                    fn_params_span: fn_item.parens.span(),
                    limit: MAX_ARGS,
                });
            }
            let param_name = self.intern_token(param.name);
            self.add_local(param_name, param.mut_tok.is_some(), param.span());
        }

        self.begin_scope();
        self.block_inner(&fn_item.body.body);
        self.emit(OpCode::Return, fn_item.body.braces.right.span);

        let fn_scope = self.fn_stack.pop().unwrap();
        let function = Value::from(Function::new(
            fn_scope.chunk.check(),
            fn_scope.name,
            self.alloc,
        ));
        self.const_eval.set_const_value(id, function);
    }

    fn const_item(&mut self, const_item: &ConstItem) {
        let name = self.intern_token(const_item.name);
        let Const { id: _, .. } = self.resolve_const(name).unwrap();
        // TODO we need to provide the value (code in this case?) to ConstEval
        // if let Some(value) = self.const_expr(&const_item.expr) {
        //     const_cell.set(value).unwrap();
        // }
    }

    fn type_item(&mut self, type_item: &TypeItem) {
        if let Some(_parameters) = &type_item.parameters {
            todo!("type function"); // impl similar to fn_item
        } else {
            let name = self.intern_token(type_item.name);
            let Const { id: _, .. } = self.resolve_const(name).unwrap();
            // TODO we need to provide the value (code in this case?) to
            // ConstEval
            //
            // if let Some(value) = self.const_expr(&type_item.expr) {
            //     const_cell.set(value).unwrap();
            // }
        }
    }

    fn const_expr(&mut self, expr: &Expression) -> Option<Value<'alloc>> {
        match expr {
            Expression::Primary(primary_expr) => match primary_expr {
                PrimaryExpr::True(_) => Some(Value::from(true)),
                PrimaryExpr::False(_) => Some(Value::from(false)),
                PrimaryExpr::BinaryNumber(_)
                | PrimaryExpr::OctalNumber(_)
                | PrimaryExpr::HexadecimalNumber(_) => {
                    todo!()
                },
                PrimaryExpr::DecimalNumber(DecimalNumber { span }) => {
                    let slice = span.anchor(&self.source).as_str();
                    match slice.parse::<i32>() {
                        Ok(int) => Some(Value::from(int)),
                        Err(cause) => {
                            self.error(InvalidInt32Literal { cause, span: *span });
                            None
                        },
                    }
                },
                PrimaryExpr::DecimalPointNumber(DecimalPointNumber { span })
                | PrimaryExpr::ExponentialNumber(ExponentialNumber { span }) => {
                    let slice = span.anchor(&self.source).as_str();
                    match slice.parse::<f64>() {
                        Ok(float) => {
                            let float =
                                Float::new(float, self.alloc).expect("impossible number literal");
                            Some(Value::from(float))
                        },
                        Err(cause) => {
                            self.error(InvalidFloatLiteral { cause, span: *span });
                            None
                        },
                    }
                },
                PrimaryExpr::Name(_) => {
                    todo!()
                },
            },
            _ => todo!(),
        }
    }

    fn let_item(&mut self, let_item: &LetItem) {
        let span = let_item.span();

        if let Some(init) = &let_item.init {
            self.expression(&init.expr);
        } else {
            // empty initializer, set value to Unit
            self.emit(OpCode::Unit, span);
        }

        let name = self.intern_token(let_item.name);
        self.add_local(name, let_item.mut_tok.is_some(), let_item.span());
    }

    fn statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::For(for_stmt) => self.for_stmt(for_stmt),
            Statement::Assert(assert_stmt) => self.assert_stmt(assert_stmt),
            Statement::Print(print_stmt) => self.print_stmt(print_stmt),
            Statement::Return(return_stmt) => self.return_stmt(return_stmt),
            Statement::While(while_stmt) => self.while_stmt(while_stmt),
        }
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
            self.error(ReturnFromScript { return_span: return_stmt.span() });
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
        let exit_jump = self.emit(OpCode::JumpIfFalse_UNPATCHED, span);

        // then
        self.emit(OpCode::Pop, while_stmt.body.braces.left.span);
        self.block(&while_stmt.body);
        // Loop block doesn't evaluate to anything
        self.emit(OpCode::Pop, while_stmt.body.braces.right.span);
        self.emit_loop(loop_start, while_stmt.body.braces.right.span);

        // end
        self.patch_jump(exit_jump);
        self.emit(OpCode::Pop, while_stmt.body.braces.right.span);
    }

    fn block(&mut self, block: &Block) {
        self.begin_scope();
        self.block_inner(&block.body);
        self.end_scope(block.braces.right.span);
    }

    fn block_inner(&mut self, items: &[Item]) {
        // Register constants
        for item in items {
            match item {
                Item::Fn(fn_item) => {
                    let name = self.intern_token(fn_item.name);
                    self.add_const(name, FreeSpan::join(fn_item.fn_kw.span, fn_item.name.span));
                },
                Item::Const(const_item) => {
                    let name = self.intern_token(const_item.name);
                    self.add_const(
                        name,
                        FreeSpan::join(const_item.const_tok.span, const_item.name.span),
                    );
                },
                Item::Type(type_item) => {
                    let name = self.intern_token(type_item.name);
                    self.add_const(
                        name,
                        FreeSpan::join(type_item.type_tok.span, type_item.name.span),
                    );
                },
                Item::Let(_) | Item::Statement(_) | Item::Expr(_) => {},
            }
        }

        // Compile code
        let mut ret = false;
        for item in items {
            match item {
                Item::Fn(fn_item) => self.fn_item(fn_item),
                Item::Const(const_item) => self.const_item(const_item),
                Item::Type(type_item) => self.type_item(type_item),

                Item::Let(let_item) => self.let_item(let_item),
                Item::Statement(stmt) => self.statement(stmt),
                Item::Expr(expr) => {
                    ret |= self.expr_item(expr);
                },
            }
        }

        if !ret {
            // Empty block evaluates to Unit
            self.emit(OpCode::Unit, FreeSpan::zero());
        }
    }

    fn expr_item(&mut self, expr: &Expr) -> bool {
        self.expression(&expr.expr);
        if let Some(semicolon_tok) = &expr.semicolon_tok {
            self.emit(OpCode::Pop, semicolon_tok.span);
            false
        } else {
            true
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
            // For now only allow assigning to a name
            if let Expression::Primary(PrimaryExpr::Name(name_tok)) = &*binary_expr.lhs {
                let name = self.intern_token(*name_tok);
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
                } else if let Some(_cnst) = self.resolve_const(name) {
                    todo!() // error: we can't assign constants
                } else if let Some((_slot, _upvalue)) = self.resolve_upvalue(name, name_tok.span) {
                    todo!() // error: we can't assign upvalues
                } else {
                    self.error(UndefinedName { name_span: name_tok.span });
                }
            } else {
                // TODO more complex assignment target?
                self.error(InvalidAssignmentTarget { span: binary_expr.lhs.span() });
            }
            return;
        }

        if op == T::Dot {
            self.expression(&binary_expr.lhs);

            match &*binary_expr.rhs {
                Expression::Primary(PrimaryExpr::Name(name_tok)) => {
                    let name = self.intern_token(*name_tok);
                    let value = Value::from(name);

                    let id = self.const_eval.add_const_value(name, value);
                    let name_key = self.insert_constant(id);

                    let span = FreeSpan::join(binary_expr.operator.span, binary_expr.rhs.span());
                    self.emit(OpCode::GetRecord { name_key }, span);
                    return;
                },
                Expression::Primary(PrimaryExpr::DecimalNumber(number)) => {
                    let span = number.span.anchor(&self.source).as_str();
                    let slot = span.parse::<u8>().expect("invalid tuple slot");

                    let span = FreeSpan::join(binary_expr.operator.span, binary_expr.rhs.span());
                    self.emit(OpCode::GetTuple { slot }, span);
                    return;
                },
                _ => {
                    let span = binary_expr.rhs.span();
                    self.error(InvalidFieldExpr { span });
                },
            }
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
            T::TypeEqual => {
                self.emit(OpCode::Subtype, span);
            },
            T::NotEqual => {
                self.emit(OpCode::Equal, span);
                self.emit(OpCode::Not, span);
            },
            T::EqualEqual => {
                self.emit(OpCode::Equal, span);
            },
            T::Greater => {
                self.emit(OpCode::Greater, span);
            },
            T::GreaterEqual => {
                self.emit(OpCode::Less, span);
                self.emit(OpCode::Not, span);
            },
            T::Less => {
                self.emit(OpCode::Less, span);
            },
            T::LessEqual => {
                self.emit(OpCode::Greater, span);
                self.emit(OpCode::Not, span);
            },
            T::TypeOr => {
                self.emit(OpCode::TypeOr, span);
            },
            T::Plus => {
                self.emit(OpCode::Add, span);
            },
            T::Minus => {
                self.emit(OpCode::Subtract, span);
            },
            T::Mul => {
                self.emit(OpCode::Multiply, span);
            },
            T::Div => {
                self.emit(OpCode::Divide, span);
            },
            _ => unreachable!(),
        }
    }

    fn and(&mut self, binary_expr: &BinaryExpr) {
        self.expression(&binary_expr.lhs);

        // If lhs is false, short-circuit, jump over rhs
        // Span both lhs and the `and` operator
        let span = FreeSpan::join(binary_expr.lhs.span(), binary_expr.operator.span);
        let end_jump = self.emit(OpCode::JumpIfFalse_UNPATCHED, span);

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
        let end_jump = self.emit(OpCode::JumpIfTrue_UNPATCHED, span);

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
            },
            T::Minus => {
                self.emit(OpCode::Negate, span);
            },
            _ => unreachable!(),
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
            entries.push((self.intern_token(entry.name), entry));
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
            let id = self
                .const_eval
                .add_const_value(self.intern_string(""), Value::from(*name));
            // NOTE maybe worth splitting consts into "trivial consts" for which values are
            // known immediately and are only used by the code that's being
            // emitted currently and "user consts" which are generated with
            // const_item, type_item, fn_item, fn_expr, ...
            let key = self.insert_constant(id);
            self.emit(OpCode::Constant { key }, entry.name.span);
        }

        // Emit entry values
        for (name, entry) in entries.iter() {
            if let Some(value) = &entry.value {
                self.expression(&value.init);
            } else {
                // Name punning - if there is no initializer expression, look up the entry name
                // in the local scope
                self.name(*name, entry.name.span);
            }
        }

        let len = entries.len().try_into().unwrap();
        self.emit(OpCode::MakeRecord { len }, record_expr.span());
    }

    fn if_expr(&mut self, if_expr: &IfExpr) {
        // if <pred>
        self.expression(&if_expr.pred);
        let then_jump = self.emit(OpCode::JumpIfFalse_UNPATCHED, if_expr.if_tok.span);

        // then
        self.emit(OpCode::Pop, if_expr.if_tok.span);
        self.block(&if_expr.body);
        let else_jump = self.emit(OpCode::Jump_UNPATCHED, if_expr.if_tok.span);

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
        // If the function has too many parameters we'll emit a specific error, the `0`
        // is a dummy value which will get discarded with the chunk.
        let params = fn_expr.parameters.items.len().try_into().unwrap_or(0);
        // FIXME? Because the callee is lower on the stack than its arguments the VM has
        // to leave it on the stack while it runs, and because we use one field
        // to mean both where the callee stack starts and where it should be
        // truncated when it returns we have to leave the callee in stack slot
        // 0. However unlike named functions (fn_item) anonymous functions can't
        // call themselves, so the first variable name should be unresolvable and
        // doesn't theoretically need to be there.
        let dummy = Local {
            name,
            bind_span: FreeSpan::zero(),
            mutable: false,
        };
        let id = self.const_eval.add_const(name);
        self.const_eval.dependency(self.fn_scope().id, id);
        self.fn_stack.push(FnScope {
            id,
            name,
            fndef_span: fn_expr.span(),
            chunk: ChunkBuf::new(self.source, params),
            blocks: Vec::from([BlockScope {
                locals: Vec::from([dummy]),
                consts: Vec::new(),
            }]),
            upvalues: Vec::new(),
            closure: true,
        });

        for (i, param) in fn_expr.parameters.items.iter().enumerate() {
            if i >= MAX_ARGS {
                self.error(TooManyParameters {
                    extra_param_span: param.span(),
                    fn_params_span: fn_expr.parens.span(),
                    limit: MAX_ARGS,
                });
            }
            let param_name = self.intern_token(param.name);
            self.add_local(param_name, param.mut_tok.is_some(), param.span());
        }

        self.begin_scope();
        self.expression(&fn_expr.body);
        self.emit(OpCode::Return, fn_expr.body.span().shrink_to_hi());

        let function = self.fn_stack.pop().unwrap();

        for upval in &function.upvalues {
            match upval.reference {
                UpvalueRef::Local(slot) => self.emit(OpCode::GetLocal { slot }, upval.capture_span),
                UpvalueRef::Upvalue(slot) => {
                    self.emit(OpCode::GetUpvalue { slot }, upval.capture_span)
                },
            };
        }

        let upvals = function.upvalues.len().try_into().unwrap();
        let mut chunk = function.chunk;
        chunk.upvalues = upvals;
        let function = Value::from(Function::new(chunk.check(), function.name, self.alloc));
        let id = self.const_eval.add_const_value(name, function);
        let fn_key = self.insert_constant(id);
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
                StringFragment::Literal { span, unescaped } => {
                    self.string_frag_literal(unescaped, *span)
                },
                StringFragment::Interpolation { name: name_tok, fmt: None } => {
                    let name = self.intern_token(*name_tok);
                    self.name(name, name_tok.span);
                    // TODO format with empty fmt
                },
                StringFragment::Interpolation { name: _, fmt: Some(_fmt) } => {
                    todo!("string interpolation fmt");
                },
            }
        }

        if fragments.len() == 1 {
            // No need to concat 1 String
            return;
        }

        let n = (fragments.len() - 2)
            .try_into()
            .expect("error: too many string fragments");

        self.emit(OpCode::Concat { n }, string_expr.span());
    }

    fn string_frag_literal(&mut self, unescaped: &str, span: FreeSpan) {
        let string = String::new(unescaped, self.alloc);
        let value = Value::from(string);
        let id = self
            .const_eval
            .add_const_value(self.intern_string(""), value);
        let key = self.insert_constant(id);
        self.emit(OpCode::Constant { key }, span);
    }

    fn primary_expr(&mut self, primary_expr: &PrimaryExpr) {
        match primary_expr {
            PrimaryExpr::True(tok) => {
                self.emit(OpCode::True, tok.span);
            },
            PrimaryExpr::False(tok) => {
                self.emit(OpCode::False, tok.span);
            },
            PrimaryExpr::BinaryNumber(_)
            | PrimaryExpr::OctalNumber(_)
            | PrimaryExpr::HexadecimalNumber(_) => {
                todo!()
            },
            PrimaryExpr::DecimalNumber(tok) => {
                self.int32(tok.span);
            },
            PrimaryExpr::DecimalPointNumber(DecimalPointNumber { span })
            | PrimaryExpr::ExponentialNumber(ExponentialNumber { span }) => {
                self.float(*span);
            },
            PrimaryExpr::Name(name_tok) => {
                let name = self.intern_token(*name_tok);
                self.name(name, name_tok.span);
            },
        }
    }

    fn int32(&mut self, span: FreeSpan) {
        let slice = span.anchor(&self.source).as_str();
        match slice.parse::<i32>() {
            Ok(int) => {
                let value = Value::from(int);
                let id = self
                    .const_eval
                    .add_const_value(self.intern_string(""), value);
                let key = self.insert_constant(id);
                self.emit(OpCode::Constant { key }, span);
            },
            Err(cause) => {
                self.error(InvalidInt32Literal { cause, span });
            },
        }
    }

    fn float(&mut self, span: FreeSpan) {
        let slice = span.anchor(&self.source).as_str();
        match slice.parse::<f64>() {
            Ok(float) => {
                let float = Float::new(float, self.alloc).expect("impossible number literal");
                let _value = Value::from(float);
                let id = self.const_eval.add_const(self.intern_string(""));
                let key = self.insert_constant(id);
                self.emit(OpCode::Constant { key }, span);
            },
            Err(cause) => {
                self.error(InvalidFloatLiteral { cause, span });
            },
        }
    }

    fn name(&mut self, name: Name<'alloc>, span: FreeSpan) {
        if let Some((slot, _)) = self.resolve_local(name) {
            self.emit(OpCode::GetLocal { slot }, span);
        } else if let Some(cnst) = self.resolve_const(name) {
            self.emit(OpCode::Constant { key: cnst.const_key }, span);
        } else if let Some((slot, _)) = self.resolve_upvalue(name, span) {
            self.emit(OpCode::GetUpvalue { slot }, span);
        } else {
            self.error(UndefinedName { name_span: span });
        }
    }
}
