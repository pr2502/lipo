use crate::builtins::{Closure, Float, Function, Name, NativeFunction, Record, String, Tuple};
use crate::chunk::Chunk;
use crate::opcode::OpCode;
use crate::{Alloc, ObjectRef, Trace, Value};


pub mod error;

use error::*;
pub use error::VmError;


pub struct VM<'alloc> {
    alloc: &'alloc Alloc,
    call_stack: Vec<Frame<'alloc>>,
    stack: Vec<Value<'alloc>>,

    // PERF Cache the top frame values so we save a pointer dereference and eliminate bounds checks
    // on call_stack.
    ip: *const u8,
    stack_offset: usize,
}

#[derive(Debug)]
struct Frame<'alloc> {
    closure: ObjectRef<'alloc, Closure<'alloc>>,
    ip: *const u8,
    stack_offset: usize,
}

impl<'alloc> VM<'alloc> {
    pub fn new(closure: ObjectRef<'alloc, Closure<'alloc>>, alloc: &'alloc Alloc) -> VM<'alloc> {
        let frame = Frame {
            closure,
            ip: closure.function.chunk.code().as_ptr(),
            stack_offset: 0,
        };
        VM {
            // Init from the first frame
            ip: frame.ip,
            stack_offset: frame.stack_offset,

            alloc,
            call_stack: vec![frame],
            stack: vec![Value::from(closure)],
        }
    }

    fn pop(&mut self) -> Value<'alloc> {
        self.stack.pop()
            .expect("pop empty stack")
    }

    fn peek(&mut self) -> Value<'alloc> {
        self.stack.last()
            .copied()
            .expect("peek empty stack")
    }

    fn push(&mut self, value: Value<'alloc>) {
        self.stack.push(value);
    }

    fn frame(&self) -> &Frame<'alloc> {
        match self.call_stack.last() {
            Some(frame) => frame,
            None => {
                #[cfg(debug_assertions)]
                { unreachable!("BUG: VM tried to continue with empty call stack") }

                // SAFETY VM breaks the interpreter loop when `op_return` pops the last frame from
                // the call_stack.
                #[cfg(not(debug_assertions))]
                unsafe { std::hint::unreachable_unchecked() }
            }
        }
    }

    fn chunk(&self) -> &Chunk<'alloc> {
        &self.frame().closure.function.chunk
    }

    fn get_constant(&self, key: u16) -> Value<'alloc> {
        match self.chunk().get_constant(key) {
            Some(constant) => constant,
            None => {
                #[cfg(debug_assertions)]
                { unreachable!("BUG: VM encountered invalid constant key {:?}, Chunk::check is incorrect", key) }

                // SAFETY Chunk is checked when the VM is constructed, all constant references must
                // be valid.
                #[cfg(not(debug_assertions))]
                unsafe { std::hint::unreachable_unchecked() }
            }
        }
    }

    fn gc(&mut self) {
        // TODO unless feature=gc-stress is enabled don't run the GC on every call.
        // check memory usage first.

        self.stack.iter().for_each(Trace::mark);
        self.call_stack.iter().for_each(|frame| frame.closure.mark());

        // SAFETY we've marked all the roots above
        unsafe { self.alloc.sweep(); }
    }

    fn offset(&self) -> usize {
        (self.ip as usize) - (self.chunk().code().as_ptr() as usize)
    }

    fn read_u8(&mut self) -> u8 {
        debug_assert!(
            self.chunk().code().as_ptr_range().contains(&self.ip),
            "BUG: ip is out of code range, Chunk::check is incorrect",
        );

        // SAFETY Chunk is checked when the VM is constructed.
        // - every jump must be at the start of a valid instruction
        // - the Chunk ends with a return instruction that prevents reading past the end
        let byte = unsafe { *self.ip };
        self.ip = unsafe { self.ip.add(1) };

        byte
    }

    fn read_u16(&mut self) -> u16 {
        let a = self.read_u8();
        let b = self.read_u8();
        u16::from_le_bytes([a, b])
    }

    pub fn run(mut self) -> Result<(), VmError> {
        log::debug!("script = {:?}", &self.chunk());

        loop {
            #[cfg(feature = "gc-stress")]
            self.gc();

            let opcode = self.read_u8();

            match opcode {
                OpCode::CONSTANT        => self.op_constant(),
                OpCode::UNIT            => self.op_unit(),
                OpCode::TRUE            => self.op_true(),
                OpCode::FALSE           => self.op_false(),
                OpCode::POP             => self.op_pop(),
                OpCode::POP_BLOCK       => self.op_pop_block(),
                OpCode::GET_LOCAL       => self.op_get_local(),
                OpCode::SET_LOCAL       => self.op_set_local(),
                OpCode::GET_UPVALUE     => self.op_get_upval(),
                OpCode::GET_TUPLE       => self.op_get_tuple()?,
                OpCode::GET_RECORD      => self.op_get_record()?,
                OpCode::EQUAL           => self.op_equal()?,
                OpCode::GREATER         => self.op_greater()?,
                OpCode::LESS            => self.op_less()?,
                OpCode::ADD             => self.op_add()?,
                OpCode::CONCAT          => self.op_concat()?,
                OpCode::SUBTRACT        => self.op_subtract()?,
                OpCode::MULTIPLY        => self.op_multiply()?,
                OpCode::DIVIDE          => self.op_divide()?,
                OpCode::NOT             => self.op_not()?,
                OpCode::NEGATE          => self.op_negate()?,
                OpCode::MAKE_TUPLE      => self.op_make_tuple(),
                OpCode::MAKE_RECORD     => self.op_make_record(),
                OpCode::ASSERT          => self.op_assert()?,
                OpCode::PRINT           => self.op_print(),
                OpCode::JUMP            => self.op_jump(),
                OpCode::JUMP_IF_TRUE    => self.op_jump_if_true()?,
                OpCode::JUMP_IF_FALSE   => self.op_jump_if_false()?,
                OpCode::LOOP            => self.op_loop(),
                OpCode::CALL            => self.op_call()?,
                OpCode::CLOSURE         => self.op_closure(),
                OpCode::RETURN          => if self.op_return() { break Ok(()) },
                _ => {
                    #[cfg(debug_assertions)]
                    unreachable!("BUG: VM encountered invalid opcode {:#02X}, Chunk::check is incorrect", opcode);

                    // SAFETY Chunk is checked when the VM is constructed, all OpCodes must be valid.
                    // IP can only be at the start of an OpCode if every instruction consumes all of
                    // it's argument bytes.
                    #[cfg(not(debug_assertions))]
                    unsafe { std::hint::unreachable_unchecked(); }
                }
            }
        }
    }

    ////////////////////////////////////////
    // Instruction implementation

    fn op_constant(&mut self) {
        let key = self.read_u16();

        let constant = self.get_constant(key);
        self.push(constant);
    }

    fn op_unit(&mut self) {
        self.push(Value::unit());
    }

    fn op_true(&mut self) {
        self.push(Value::from(true));
    }

    fn op_false(&mut self) {
        self.push(Value::from(false));
    }

    fn op_pop(&mut self) {
        self.pop();
    }

    fn op_pop_block(&mut self) {
        let n = usize::from(self.read_u8());

        let top = self.pop();
        self.stack.truncate(self.stack.len() - n - 1);
        self.push(top);
    }

    fn op_get_local(&mut self) {
        let slot = usize::from(self.read_u16());

        let offset = self.stack_offset;
        let Some(&value) = self.stack.get(offset + slot) else {
            unreachable!("invalid stack access, slot={}", slot);
        };
        self.push(value);
    }

    fn op_set_local(&mut self) {
        let slot = usize::from(self.read_u16());

        let value = self.peek();
        let offset = self.stack_offset;
        let Some(slot) = self.stack.get_mut(offset + slot) else {
            unreachable!("invalid stack access, slot={}", slot);
        };
        *slot = value;
    }

    fn op_get_upval(&mut self) {
        let slot = usize::from(self.read_u8());

        let Some(&value) = self.frame().closure.upvalues.get(slot) else {
            unreachable!("invalid upvalue access, slot={}", slot);
        };
        self.push(value);
    }

    fn op_get_tuple(&mut self) -> Result<(), VmError> {
        let slot = usize::from(self.read_u8());

        let Some(tuple) = self.pop().downcast::<Tuple>() else {
            return Err(VmError::new(TypeError {
                span: self.chunk().span(self.offset() - OpCode::GetTuple { slot: 0 }.len()),
                msg: "can only access slots on Tuples",
            }));
        };
        let Some(&value) = tuple.get(slot) else {
            return Err(VmError::new(TypeError {
                span: self.chunk().span(self.offset() - OpCode::GetTuple { slot: 0 }.len()),
                // TODO tuple of type (X, Y, Z) accessed field `N`
                msg: "Tuple access out of bounds",
            }));
        };
        self.push(value);
        Ok(())
    }

    fn op_get_record(&mut self) -> Result<(), VmError> {
        let field_key = self.read_u16();

        let Some(record) = self.pop().downcast::<Record>() else {
            return Err(VmError::new(TypeError {
                span: self.chunk().span(self.offset() - OpCode::GetRecord { name_key: 0 }.len()),
                msg: "field access only supported on Records",
            }));
        };
        let field_name = self.get_constant(field_key)
            .downcast::<Name>().unwrap();
        let Some(value) = record.get(field_name) else {
            dbg!(field_name);
            return Err(VmError::new(TypeError {
                span: self.chunk().span(self.offset() - OpCode::GetRecord { name_key: 0 }.len()),
                msg: "record doesn't contain the requested field",
            }));
        };
        self.push(value);
        Ok(())
    }

    fn op_equal(&mut self) -> Result<(), VmError> {
        let rhs = self.pop();
        let lhs = self.pop();
        let Some(result) = lhs.partial_eq(&rhs) else {
            todo!("type error");
        };
        self.push(Value::from(result));
        Ok(())
    }

    fn op_greater(&mut self) -> Result<(), VmError> {
        let rhs = self.pop();
        let lhs = self.pop();
        let result = match (lhs.downcast::<Float>(), rhs.downcast::<Float>()) {
            (Some(lhs), Some(rhs)) => Value::from(*lhs > *rhs),
            _ => {
                return Err(VmError::new(TypeError {
                    span: self.chunk().span(self.offset() - OpCode::Greater.len()),
                    msg: "comparison only supported on Numbers",
                }));
            },
        };
        self.push(result);
        Ok(())
    }

    fn op_less(&mut self) -> Result<(), VmError> {
        let rhs = self.pop();
        let lhs = self.pop();
        let result = match (lhs.downcast::<Float>(), rhs.downcast::<Float>()) {
            (Some(lhs), Some(rhs)) => Value::from(*lhs < *rhs),
            _ => {
                return Err(VmError::new(TypeError {
                    span: self.chunk().span(self.offset() - OpCode::Less.len()),
                    msg: "comparison only supported on Numbers",
                }));
            },
        };
        self.push(result);
        Ok(())
    }

    fn op_add(&mut self) -> Result<(), VmError> {
        let rhs = self.pop();
        let lhs = self.pop();
        let (Some(lhs), Some(rhs)) = (lhs.downcast::<Float>(), rhs.downcast::<Float>()) else {
            return Err(VmError::new(TypeError {
                span: self.chunk().span(self.offset() - OpCode::Add.len()),
                msg: "addition only supported on Numbers",
            }));
        };
        let Some(result) = Float::new(lhs.inner() + rhs.inner(), self.alloc) else {
            return Err(VmError::new(MathError {
                span: self.chunk().span(self.offset() - OpCode::Add.len()),
                msg: "operation resulted in a NaN",
            }));
        };
        self.push(Value::from(result));
        Ok(())
    }

    fn op_concat(&mut self) -> Result<(), VmError> {
        let strings = usize::from(self.read_u8()) + 2;

        let mut buffer = std::string::String::new();

        let from = self.stack.len().checked_sub(strings)
            .expect("peek past the start of stack");
        for value in self.stack.drain(from..) {
            if let Some(string) = value.downcast::<String>() {
                buffer.push_str(&string);
            } else {
                // Should be unreachable since only StringExpr compiles to OpCode::Concat and it
                // converts all interpolations to String. But it's not yet in ChunkBuf::check.
                unreachable!("tried to concat non-String Values");
            }
        }

        let value = Value::from(String::new_owned(buffer.into_boxed_str(), self.alloc));
        self.push(value);
        Ok(())
    }

    fn op_subtract(&mut self) -> Result<(), VmError> {
        let rhs = self.pop();
        let lhs = self.pop();
        let (Some(lhs), Some(rhs)) = (lhs.downcast::<Float>(), rhs.downcast::<Float>()) else {
            return Err(VmError::new(TypeError {
                span: self.chunk().span(self.offset() - OpCode::Subtract.len()),
                msg: "subtraction only supported on Numbers",
            }));
        };
        let Some(result) = Float::new(lhs.inner() - rhs.inner(), self.alloc) else {
            return Err(VmError::new(MathError {
                span: self.chunk().span(self.offset() - OpCode::Subtract.len()),
                msg: "operation resulted in a NaN",
            }));
        };
        self.push(Value::from(result));
        Ok(())
    }

    fn op_multiply(&mut self) -> Result<(), VmError> {
        let rhs = self.pop();
        let lhs = self.pop();
        let (Some(lhs), Some(rhs)) = (lhs.downcast::<Float>(), rhs.downcast::<Float>()) else {
            return Err(VmError::new(TypeError {
                span: self.chunk().span(self.offset() - OpCode::Multiply.len()),
                msg: "multiplication only supported on Numbers",
            }));
        };
        let Some(result) = Float::new(lhs.inner() * rhs.inner(), self.alloc) else {
            return Err(VmError::new(MathError {
                span: self.chunk().span(self.offset() - OpCode::Multiply.len()),
                msg: "operation resulted in a NaN",
            }));
        };
        self.push(Value::from(result));
        Ok(())
    }

    fn op_divide(&mut self) -> Result<(), VmError> {
        let rhs = self.pop();
        let lhs = self.pop();
        let (Some(lhs), Some(rhs)) = (lhs.downcast::<Float>(), rhs.downcast::<Float>()) else {
            return Err(VmError::new(TypeError {
                span: self.chunk().span(self.offset() - OpCode::Divide.len()),
                msg: "division only supported on Numbers",
            }));
        };
        let Some(result) = Float::new(lhs.inner() / rhs.inner(), self.alloc) else {
            return Err(VmError::new(MathError {
                span: self.chunk().span(self.offset() - OpCode::Divide.len()),
                msg: "operation resulted in a NaN",
            }));
        };
        self.push(Value::from(result));
        Ok(())
    }

    fn op_not(&mut self) -> Result<(), VmError> {
        let value = self.pop();
        let Some(b) = value.downcast::<bool>() else {
            return Err(VmError::new(TypeError {
                span: self.chunk().span(self.offset() - OpCode::Not.len()),
                msg: "not only supported on Bool",
            }));
        };
        let value = Value::from(!b);
        self.push(value);
        Ok(())
    }

    fn op_negate(&mut self) -> Result<(), VmError> {
        let value = self.pop();
        let Some(f) = value.downcast::<Float>() else {
            return Err(VmError::new(TypeError {
                span: self.chunk().span(self.offset() - OpCode::Negate.len()),
                msg: "negation only supported on Numbers",
            }));
        };
        let value = Value::from(Float::new(-f.inner(), self.alloc).unwrap());
        self.push(value);
        Ok(())
    }

    fn op_make_tuple(&mut self) {
        let len = usize::from(self.read_u8());

        let from = self.stack.len().checked_sub(len)
            .expect("peek past the start of stack");
        let items = self.stack.drain(from..).collect();
        let value = Value::from(Tuple::new(items, self.alloc));
        self.push(value);
    }

    fn op_make_record(&mut self) {
        let len = usize::from(self.read_u8());

        let from = self.stack.len().checked_sub(len * 2)
            .expect("peek past the start of stack");
        // SAFETY Chunk is checked when VM is constructed.
        // - TODO this check is not yet implemented, but we panic in debug mode
        let record = unsafe { Record::new_from_sorted(&self.stack[from..], self.alloc) };
        let value = Value::from(record);
        self.stack.truncate(from);
        self.push(value);
    }

    fn op_assert(&mut self) -> Result<(), VmError> {
        let value = self.pop();
        let Some(pred) = value.downcast::<bool>() else {
            return Err(VmError::new(TypeError {
                span: self.chunk().span(self.offset() - OpCode::Assert.len()),
                msg: "asserted expression must return a Bool",
            }));
        };
        if pred {
            Ok(())
        } else {
            Err(VmError::new(AssertionError {
                span: self.chunk().span(self.offset() - OpCode::Assert.len()),
            }))
        }
    }

    fn op_print(&mut self) {
        let value = self.pop();
        println!("{:?}", value);
    }

    fn op_jump(&mut self) {
        let offset = usize::from(self.read_u16());

        // SAFETY Chunk is checked when VM is constructed.
        // - every jump must be at the start of a valid instruction
        self.ip = unsafe { self.ip.add(offset) };
    }

    fn op_jump_if_true(&mut self) -> Result<(), VmError> {
        let offset = usize::from(self.read_u16());

        let value = self.peek();
        let Some(pred) = value.downcast::<bool>() else {
            return Err(VmError::new(TypeError {
                span: self.chunk().span(self.offset() - OpCode::JumpIfTrue { offset: 0 }.len()),
                msg: "if predicate must be a Bool",
            }));
        };
        if pred {
            // SAFETY Chunk is checked when VM is constructed.
            // - every jump must be at the start of a valid instruction
            self.ip = unsafe { self.ip.add(offset) };
        }
        Ok(())
    }

    fn op_jump_if_false(&mut self) -> Result<(), VmError> {
        let offset = usize::from(self.read_u16());

        let value = self.peek();
        match value.downcast::<bool>() {
            Some(true) => {}
            Some(false) => {
                // SAFETY Chunk is checked when VM is constructed.
                // - every jump must be at the start of a valid instruction
                self.ip = unsafe { self.ip.add(offset) };
            }
            None => {
                return Err(VmError::new(TypeError {
                    span: self.chunk().span(self.offset() - OpCode::JumpIfFalse { offset: 0 }.len()),
                    msg: "if predicate must be a Bool",
                }));
            }
        }
        Ok(())
    }

    fn op_loop(&mut self) {
        let offset = usize::from(self.read_u16());

        // SAFETY Chunk is checked when VM is constructed.
        // - every jump must be at the start of a valid instruction
        self.ip = unsafe { self.ip.sub(offset) };
    }

    fn op_call(&mut self) -> Result<(), VmError> {
        let args = usize::from(self.read_u8());

        // The stack layou before call:
        //
        //         caller stack_offset (or stack top)                stack.len() offset puts us
        //         |                                                 |   one past the last arg
        //         v                                                 v
        // stack: [caller, tmp1, tmp2, tmp3, callee, arg1, arg2, arg3]
        //                                   ^       ^     ^     ^
        //                                   |       |_____|_____|__ args = 3
        //                                   |       |
        //                                   |       for native functions we start the args here
        //                                   |         arg_start = stack.len() - args
        //                                   |
        //                                   callee stack_start = stack.len() - args - 1
        //                                                      = stack.len() - (args + 1)

        let callee_idx = self.stack.len().checked_sub(args + 1)
            .expect("peek past the start of stack");
        // SAFETY because we checked for underflow callee_idx is always < stack.len() and in bounds
        let callee = unsafe { *self.stack.get_unchecked(callee_idx) };

        if let Some(native_function) = callee.downcast::<NativeFunction>() {
            let arg_start = self.stack.len() - args;
            let res = native_function.call(&self.stack[arg_start..]);
            match res {
                Ok(val) => {
                    // Native function arguments don't include the callee, but we need to remove it
                    // from the stack too.
                    self.stack.truncate(arg_start - 1);
                    self.push(val);
                    Ok(())
                }
                Err(err) => {
                    Err(VmError::new(NativeError { msg: err.msg }))
                }
            }
        } else if let Some(closure) = callee.downcast::<Closure>() {
            let arity = usize::try_from(closure.function.arity).unwrap();
            if arity != args {
                return Err(VmError::new(WrongArity {
                    span: self.chunk().span(self.offset() - OpCode::Call { args: 0 }.len()),
                    arity,
                    args,
                }));
            }

            // Include callee in the new stack frame at slot 0.
            let stack_start = self.stack.len() - args - 1;

            log::debug!("stack {:#?}", &self.stack[stack_start..]);
            log::debug!("function {} = {:?}", closure.function.name, &closure.function.chunk);

            // Save cached values back in the frame.
            let caller_frame = self.call_stack.last_mut().unwrap();
            caller_frame.ip = self.ip;
            caller_frame.stack_offset = self.stack_offset;

            let callee_frame = Frame {
                closure,
                ip: closure.function.chunk.code().as_ptr(),
                stack_offset: stack_start,
            };

            // Initialize cache for the new frame.
            self.ip = callee_frame.ip;
            self.stack_offset = callee_frame.stack_offset;

            self.call_stack.push(callee_frame);


            Ok(())
        } else {
            Err(VmError::new(ValueNotCallable {
                span: self.chunk().span(self.offset() - OpCode::Call { args: 0 }.len()),
                dbg: format!("{:?}", callee),
            }))
        }
    }

    fn op_closure(&mut self) {
        let key = self.read_u16();
        let upvals = usize::from(self.read_u8());

        let constant = self.get_constant(key);
        let Some(function) = constant.downcast::<Function>() else {
            #[cfg(debug_assertions)]
            unreachable!("BUG: OpCode::Closure referenced a constant that was not a Function");

            // SAFETY Chunk is checked when the VM is constructed, all constant references
            // from OpCode::Closure must be valid and reference a Function object.
            #[cfg(not(debug_assertions))]
            unsafe { std::hint::unreachable_unchecked(); }
        };
        let upvalues = self.stack.drain((self.stack.len() - upvals)..).collect();
        let closure = Value::from(Closure::new(function, upvalues, self.alloc));
        self.push(closure);
    }

    /// Returns true when returning from the top level call frame
    fn op_return(&mut self) -> bool {
        let result = self.pop();

        let stack_top = self.stack_offset;
        self.call_stack.pop();

        if let Some(frame) = self.call_stack.last() {
            // Function stack has the callee in slot 0 so truncating here removes callee too.
            self.stack.truncate(stack_top);
            self.stack.push(result);

            // Restore previous frame cached values.
            self.ip = frame.ip;
            self.stack_offset = frame.stack_offset;

            self.gc();

            // Continue in the caller
            false
        } else {
            // Empty call stack, break the interpreter loop
            true
        }
    }
}
