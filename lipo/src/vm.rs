use crate::chunk::{Chunk, ConstKey};
use crate::object::builtins::{Function, NativeError, NativeFunction, String};
use crate::object::{Alloc, ObjectRef, Trace};
use crate::opcode::OpCode;
use crate::span::FreeSpan;
use crate::value::Value;
use fxhash::FxHashMap as HashMap;


pub struct VM<'alloc> {
    alloc: &'alloc Alloc,
    call_stack: Vec<Frame<'alloc>>,
    stack: Vec<Value<'alloc>>,
    globals: HashMap<ObjectRef<'alloc, String>, Value<'alloc>>,
}

#[derive(Debug)]
struct Frame<'alloc> {
    function: ObjectRef<'alloc, Function<'alloc>>,
    ip: *const u8,
    stack_offset: usize,
}

#[derive(Debug)]
pub enum VmError<'alloc> {
    RuntimeError {
        source: ObjectRef<'alloc, String>,
        span: FreeSpan,
        kind: RuntimeErrorKind<'alloc>,
    },
    NativeError(NativeError<'alloc>),
}

#[derive(Debug)]
pub enum RuntimeErrorKind<'alloc> {
    AssertionError,
    TypeError(&'static str),
    UndefinedGlobalVariable(std::string::String),
    ValueNotCallable(Value<'alloc>),
    WrongArity {
        arity: usize,
        args: usize,
    },
}

impl<'alloc> VM<'alloc> {
    pub fn new(function: ObjectRef<'alloc, Function<'alloc>>, alloc: &'alloc Alloc) -> VM<'alloc> {
        let frame = Frame {
            function,
            ip: function.chunk.code().as_ptr(),
            stack_offset: 0,
        };
        VM {
            alloc,
            call_stack: vec![frame],
            stack: vec![Value::new_object(function)],
            globals: HashMap::default(),
        }
    }

    pub fn add_global(&mut self, name: &'static str, value: Value<'alloc>) {
        let name = String::new(name, self.alloc);
        self.globals.insert(name, value);
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

    fn peek_n(&mut self, from_end: usize) -> Value<'alloc> {
        let idx = self.stack.len()
            .checked_sub(from_end + 1) // +1 because idx=vec.len() is one past the end
            .expect("peek past the start of stack");
        // SAFETY checked for underflow, idx is always < len
        unsafe { *self.stack.get_unchecked(idx) }
    }

    fn push(&mut self, value: Value<'alloc>) {
        self.stack.push(value);
    }

    fn frame(&self) -> &Frame<'alloc> {
        self.call_stack.last().unwrap()
    }

    fn frame_mut(&mut self) -> &mut Frame<'alloc> {
        self.call_stack.last_mut().unwrap()
    }

    fn chunk(&self) -> &Chunk<'alloc> {
        &self.call_stack.last().unwrap().function.chunk
    }

    fn get_constant(&self, key: ConstKey) -> Value<'alloc> {
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

    fn get_global_name(&self, key: ConstKey) -> ObjectRef<'alloc, String> {
        match self.get_constant(key).downcast::<String>() {
            Some(name) => name,
            None => {
                #[cfg(debug_assertions)] // TODO get type name
                { unreachable!("BUG: VM encountered invalid constant type for {:?}, expected String found ?, Chunk::check is incorrect", key) }

                // SAFETY Chunk is checked when the VM is constructed, all constant references used
                // in DefGlobal, GetGlobal or SetGlobal OpCodes must reference a String.
                #[cfg(not(debug_assertions))]
                unsafe { std::hint::unreachable_unchecked() }
            }
        }
    }

    fn gc(&mut self) {
        // TODO unless feature=gc-stress is enabled don't run the GC on every call.
        // check memory usage first.

        self.stack.iter().for_each(Trace::mark);
        self.globals.iter().for_each(|(key, val)| { key.mark(); val.mark() });
        self.call_stack.iter().for_each(|frame| frame.function.mark());

        // SAFETY we've marked all the roots above
        unsafe { self.alloc.sweep(); }
    }

    fn offset(&self) -> usize {
        (self.frame().ip as usize) - (self.chunk().code().as_ptr() as usize)
    }

    fn read_u8(&mut self) -> u8 {
        debug_assert!(
            self.chunk().code().as_ptr_range().contains(&self.frame().ip),
            "BUG: ip is out of code range, Chunk::check is incorrect",
        );

        // SAFETY Chunk is checked when the VM is constructed.
        // - every jump must be at the start of a valid instruction
        // - the Chunk ends with a return instruction that prevents reading past the end
        let byte = unsafe { *self.frame().ip };
        self.frame_mut().ip = unsafe { self.frame().ip.add(1) };

        byte
    }

    fn read_u16(&mut self) -> u16 {
        let a = self.read_u8();
        let b = self.read_u8();
        u16::from_le_bytes([a, b])
    }

    fn read_const_key(&mut self) -> ConstKey {
        let a = self.read_u8();
        let b = self.read_u8();
        ConstKey::from_le_bytes([a, b])
    }

    pub fn run(mut self) -> Result<(), VmError<'alloc>> {
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
                OpCode::GET_LOCAL       => self.op_get_local(),
                OpCode::SET_LOCAL       => self.op_set_local(),
                OpCode::GET_GLOBAL      => self.op_get_global()?,
                OpCode::DEF_GLOBAL      => self.op_def_global(),
                OpCode::SET_GLOBAL      => self.op_set_global()?,
                OpCode::EQUAL           => self.op_equal()?,
                OpCode::GREATER         => self.op_greater()?,
                OpCode::LESS            => self.op_less()?,
                OpCode::ADD             => self.op_add()?,
                OpCode::SUBTRACT        => self.op_subtract()?,
                OpCode::MULTIPLY        => self.op_multiply()?,
                OpCode::DIVIDE          => self.op_divide()?,
                OpCode::NOT             => self.op_not(),
                OpCode::NEGATE          => self.op_negate()?,
                OpCode::ASSERT          => self.op_assert()?,
                OpCode::PRINT           => self.op_print(),
                OpCode::JUMP            => self.op_jump(),
                OpCode::JUMP_IF_TRUE    => self.op_jump_if_true(),
                OpCode::JUMP_IF_FALSE   => self.op_jump_if_false(),
                OpCode::LOOP            => self.op_loop(),
                OpCode::CALL            => self.op_call()?,
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
        let key = self.read_const_key();

        let constant = self.get_constant(key);
        self.push(constant);
    }

    fn op_unit(&mut self) {
        self.push(Value::new_unit());
    }

    fn op_true(&mut self) {
        self.push(Value::new_bool(true));
    }

    fn op_false(&mut self) {
        self.push(Value::new_bool(false));
    }

    fn op_pop(&mut self) {
        self.pop();
    }

    fn op_get_local(&mut self) {
        let slot = self.read_u16() as usize;

        let offset = self.frame().stack_offset;
        let value = *self.stack.get(offset + slot)
            .unwrap_or_else(|| unreachable!("invalid stack access, slot={}", slot));
        self.push(value);
    }

    fn op_set_local(&mut self) {
        let slot = self.read_u16() as usize;

        let value = self.peek();
        let offset = self.frame().stack_offset;
        let slot = self.stack.get_mut(offset + slot)
            .unwrap_or_else(|| unreachable!("invalid stack access, slot={}", slot));
        *slot = value;
    }

    fn op_get_global(&mut self) -> Result<(), VmError<'alloc>> {
        let key = self.read_const_key();

        let name = self.get_global_name(key);
        let value = *self.globals.get(&name)
            .ok_or_else(|| VmError::RuntimeError {
                source: self.chunk().source(),
                span: self.chunk().span(self.offset() - 3),
                kind: RuntimeErrorKind::UndefinedGlobalVariable(name.as_str().to_string()),
            })?;
        self.push(value);
        Ok(())
    }

    fn op_def_global(&mut self) {
        let key = self.read_const_key();

        let name = self.get_global_name(key);
        let value = self.pop();
        self.globals.insert(name, value);
    }

    fn op_set_global(&mut self) -> Result<(), VmError<'alloc>> {
        let key = self.read_const_key();

        let name = self.get_global_name(key);
        let value = self.peek();
        if self.globals.insert(name, value).is_none() {
            return Err(VmError::RuntimeError {
                source: self.chunk().source(),
                span: self.chunk().span(self.offset() - 3),
                kind: RuntimeErrorKind::UndefinedGlobalVariable(name.as_str().to_string()),
            });
        }
        Ok(())
    }

    fn op_equal(&mut self) -> Result<(), VmError<'alloc>> {
        let rhs = self.pop();
        let lhs = self.pop();
        if let Some(result) = lhs.partial_eq(&rhs) {
            self.push(Value::new_bool(result));
            Ok(())
        } else {
            todo!("type error");
        }
    }

    fn op_greater(&mut self) -> Result<(), VmError<'alloc>> {
        let rhs = self.pop();
        let lhs = self.pop();
        let result = match (lhs.to_float(), rhs.to_float()) {
            (Some(lhs), Some(rhs)) => Value::new_bool(lhs > rhs),
            _ => {
                return Err(VmError::RuntimeError {
                    source: self.chunk().source(),
                    span: self.chunk().span(self.offset() - 1),
                    kind: RuntimeErrorKind::TypeError("comparison only supported on Numbers"),
                })
            },
        };
        self.push(result);
        Ok(())
    }

    fn op_less(&mut self) -> Result<(), VmError<'alloc>> {
        let rhs = self.pop();
        let lhs = self.pop();
        let result = match (lhs.to_float(), rhs.to_float()) {
            (Some(lhs), Some(rhs)) => Value::new_bool(lhs < rhs),
            _ => {
                dbg!(lhs, rhs);
                return Err(VmError::RuntimeError {
                    source: self.chunk().source(),
                    span: self.chunk().span(self.offset() - 1),
                    kind: RuntimeErrorKind::TypeError("comparison only supported on Numbers"),
                })
            },
        };
        self.push(result);
        Ok(())
    }

    fn op_add(&mut self) -> Result<(), VmError<'alloc>> {
        let rhs = self.pop();
        let lhs = self.pop();
        let result = if let (Some(lhs), Some(rhs)) = (lhs.to_float(), rhs.to_float()) {
            Value::new_float(lhs + rhs)
        } else if let (Some(lhs), Some(rhs)) = (lhs.downcast::<String>(), rhs.downcast::<String>()) {
            let sum = lhs.as_str().to_string() + rhs.as_str();
            Value::new_object(String::new_owned(sum.into_boxed_str(), self.alloc))
        } else {
            return Err(VmError::RuntimeError {
                source: self.chunk().source(),
                span: self.chunk().span(self.offset() - 1),
                kind: RuntimeErrorKind::TypeError("addition only supported on Numbers and Strings"),
            });
        };
        self.push(result);
        Ok(())
    }

    fn op_subtract(&mut self) -> Result<(), VmError<'alloc>> {
        let rhs = self.pop();
        let lhs = self.pop();
        let result = match (lhs.to_float(), rhs.to_float()) {
            (Some(lhs), Some(rhs)) => Value::new_float(lhs - rhs),
            _ => return Err(VmError::RuntimeError {
                source: self.chunk().source(),
                span: self.chunk().span(self.offset() - 1),
                kind: RuntimeErrorKind::TypeError("subtraction only supported on Numbers"),
            }),
        };
        self.push(result);
        Ok(())
    }

    fn op_multiply(&mut self) -> Result<(), VmError<'alloc>> {
        let rhs = self.pop();
        let lhs = self.pop();
        let result = match (lhs.to_float(), rhs.to_float()) {
            (Some(lhs), Some(rhs)) => Value::new_float(lhs * rhs),
            _ => return Err(VmError::RuntimeError {
                source: self.chunk().source(),
                span: self.chunk().span(self.offset() - 1),
                kind: RuntimeErrorKind::TypeError("multiplication only supported on Numbers"),
            }),
        };
        self.push(result);
        Ok(())
    }

    fn op_divide(&mut self) -> Result<(), VmError<'alloc>> {
        let rhs = self.pop();
        let lhs = self.pop();
        let result = match (lhs.to_float(), rhs.to_float()) {
            (Some(lhs), Some(rhs)) => Value::new_float(lhs / rhs),
            _ => return Err(VmError::RuntimeError {
                source: self.chunk().source(),
                span: self.chunk().span(self.offset() - 1),
                kind: RuntimeErrorKind::TypeError("division only supported on Numbers"),
            }),
        };
        self.push(result);
        Ok(())
    }

    fn op_not(&mut self) {
        let value = self.pop();
        let value = Value::new_bool(value.is_falsy());
        self.push(value);
    }

    fn op_negate(&mut self) -> Result<(), VmError<'alloc>> {
        let value = self.pop();
        let value = value.to_float()
            .map(|n| Value::new_float(-n))
            .ok_or_else(|| VmError::RuntimeError {
                source: self.chunk().source(),
                span: self.chunk().span(self.offset() - 1),
                kind: RuntimeErrorKind::TypeError("negation only supported on Numbers"),
            })?;
        self.push(value);
        Ok(())
    }

    fn op_assert(&mut self) -> Result<(), VmError<'alloc>> {
        let value = self.pop();
        match value.to_bool() {
            Some(true) => {}
            Some(false) => return Err(VmError::RuntimeError {
                source: self.chunk().source(),
                span: self.chunk().span(self.offset() - 1),
                kind: RuntimeErrorKind::AssertionError
            }),
            _ => return Err(VmError::RuntimeError {
                source: self.chunk().source(),
                span: self.chunk().span(self.offset() - 1),
                kind: RuntimeErrorKind::TypeError("asserted expression must return a Bool"),
            }),
        }
        Ok(())
    }

    fn op_print(&mut self) {
        let value = self.pop();
        println!("{:?}", value);
    }

    fn op_jump(&mut self) {
        let offset = self.read_u16() as usize;

        // SAFETY Chunk is checked when VM is constructed.
        // - every jump must be at the start of a valid instruction
        self.frame_mut().ip = unsafe { self.frame().ip.add(offset as usize) };
    }

    fn op_jump_if_true(&mut self) {
        let offset = self.read_u16();

        let value = self.peek();
        if !value.is_falsy() {
            // SAFETY Chunk is checked when VM is constructed.
            // - every jump must be at the start of a valid instruction
            self.frame_mut().ip = unsafe { self.frame().ip.add(offset as usize) };
        }
    }

    fn op_jump_if_false(&mut self) {
        let offset = self.read_u16();

        let value = self.peek();
        if value.is_falsy() {
            // SAFETY Chunk is checked when VM is constructed.
            // - every jump must be at the start of a valid instruction
            self.frame_mut().ip = unsafe { self.frame().ip.add(offset as usize) };
        }
    }

    fn op_loop(&mut self) {
        let offset = self.read_u16();

        // SAFETY Chunk is checked when VM is constructed.
        // - every jump must be at the start of a valid instruction
        self.frame_mut().ip = unsafe { self.frame().ip.sub(offset as usize) };
    }

    fn op_call(&mut self) -> Result<(), VmError<'alloc>> {
        let args = self.read_u8() as usize;

        let callee = self.peek_n(args);

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
                    Err(VmError::NativeError(err))
                }
            }
        } else if let Some(function) = callee.downcast::<Function>() {
            let arity = function.arity as usize;
            if arity != args {
                return Err(VmError::RuntimeError {
                    source: self.chunk().source(),
                    span: self.chunk().span(self.offset() - 2),
                    kind: RuntimeErrorKind::WrongArity { arity, args },
                });
            }

            // Put callee in stack slot 0.
            let stack_start = self.stack.len() - args - 1;

            log::debug!("stack {:#?}", &self.stack[stack_start..]);
            log::debug!("function {} = {:?}", &function.name, &function.chunk);

            self.call_stack.push(Frame {
                function,
                ip: function.chunk.code().as_ptr(),
                stack_offset: stack_start,
            });

            Ok(())
        } else {
            Err(VmError::RuntimeError {
                source: self.chunk().source(),
                span: self.chunk().span(self.offset() - 2),
                kind: RuntimeErrorKind::ValueNotCallable(callee),
            })
        }
    }

    /// Returns true when returning from the top level call frame
    fn op_return(&mut self) -> bool {
        let result = self.pop();

        let stack_top = self.frame().stack_offset;
        self.call_stack.pop();

        // Function stack has the callee in slot 0 so truncating here removes callee too.
        self.stack.truncate(stack_top);
        self.push(result);

        self.gc();

        // If there are no call frames left break the interpreter loop.
        self.call_stack.is_empty()
    }
}
