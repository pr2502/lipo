use crate::chunk::{Chunk, ConstKey};
use crate::object::string::String;
use crate::object::{Alloc, ObjectRef, Trace};
use crate::opcode::OpCode;
use crate::span::FreeSpan;
use crate::value::Value;
use fxhash::FxHashMap as HashMap;
use std::hint;


pub struct VM<'code, 'alloc> {
    chunk: &'code Chunk<'alloc>,
    alloc: &'alloc Alloc,
    ip: *const u8,
    stack: Vec<Value<'alloc>>,
    globals: HashMap<ObjectRef<'alloc, String>, Value<'alloc>>,
}

#[derive(Debug)]
pub enum VmError<'alloc> {
    CompileError(CodeError),
    RuntimeError {
        source: ObjectRef<'alloc, String>,
        span: FreeSpan,
        kind: RuntimeErrorKind,
    },
}

#[derive(Debug)]
pub enum CodeError {
    UnexpectedEndOfCode,
    InvalidConstantKey(ConstKey),
    InvalidStackSlot(u16),
    PopEmptyStack,
}

#[derive(Debug)]
pub enum RuntimeErrorKind {
    AssertionError,
    TypeError(&'static str),
    UndefinedGlobalVariable(std::string::String),
}

impl<'code, 'alloc> VM<'code, 'alloc> {
    pub fn new(chunk: &'code Chunk<'alloc>, alloc: &'alloc Alloc) -> VM<'code, 'alloc> {
        chunk.check();
        VM {
            chunk,
            alloc,
            ip: chunk.code().as_ptr(),
            stack: Vec::default(),
            globals: HashMap::default(),
        }
    }

    fn pop(&mut self) -> Result<Value<'alloc>, VmError<'alloc>> {
        self.stack.pop()
            .ok_or(VmError::CompileError(CodeError::PopEmptyStack))
    }

    fn peek(&mut self) -> Result<Value<'alloc>, VmError<'alloc>> {
        self.stack.last()
            .copied()
            .ok_or(VmError::CompileError(CodeError::PopEmptyStack))
    }

    fn push(&mut self, value: Value<'alloc>) {
        self.stack.push(value);
    }

    fn get_span(&self, byte_offset: usize) -> FreeSpan {
        let code = self.chunk.code();
        let mut scan = code;
        let mut span_idx = 0;
        loop {
            // NOTE Not sure this is actually technically ok, it works in practice but...
            let scan_offset = (scan.as_ptr() as usize) - (code.as_ptr() as usize);
            if scan_offset == byte_offset {
                break span_idx;
            }
            let (_, next) = OpCode::decode(scan)
                .unwrap(); // Chunk is checked when the VM is constructed.
            scan = next;
            span_idx += 1;
        };

        self.chunk.spans()
            .get(span_idx)
            .copied()
            .expect("missing span information")
    }

    fn gc(&mut self) {
        self.stack.iter().for_each(Trace::mark);
        self.globals.iter().for_each(|(key, val)| { key.mark(); val.mark() });
        self.chunk.mark();

        // SAFETY we've marked all the roots above
        unsafe { self.alloc.sweep(); }
    }

    fn offset(&self) -> usize {
        (self.ip as usize) - (self.chunk.code().as_ptr() as usize)
    }

    fn read_u8(&mut self) -> u8 {
        debug_assert!(
            self.chunk.code().as_ptr_range().contains(&self.ip),
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

    fn read_const_key(&mut self) -> ConstKey {
        let a = self.read_u8();
        let b = self.read_u8();
        ConstKey::from_le_bytes([a, b])
    }

    pub fn run(mut self) -> Result<Value<'alloc>, VmError<'alloc>> {
        loop {
            #[cfg(feature = "gc-stress")]
            self.gc();

            let opcode = self.read_u8();

            match opcode {
                OpCode::CONSTANT        => self.op_constant()?,
                OpCode::UNIT            => self.op_unit(),
                OpCode::TRUE            => self.op_true(),
                OpCode::FALSE           => self.op_false(),
                OpCode::POP             => self.op_pop()?,
                OpCode::GET_LOCAL       => self.op_get_local()?,
                OpCode::SET_LOCAL       => self.op_set_local()?,
                OpCode::GET_GLOBAL      => self.op_get_global()?,
                OpCode::DEF_GLOBAL      => self.op_def_global()?,
                OpCode::SET_GLOBAL      => self.op_set_global()?,
                OpCode::EQUAL           => self.op_equal()?,
                OpCode::GREATER         => self.op_greater()?,
                OpCode::LESS            => self.op_less()?,
                OpCode::ADD             => self.op_add()?,
                OpCode::SUBTRACT        => self.op_subtract()?,
                OpCode::MULTIPLY        => self.op_multiply()?,
                OpCode::DIVIDE          => self.op_divide()?,
                OpCode::NOT             => self.op_not()?,
                OpCode::NEGATE          => self.op_negate()?,
                OpCode::ASSERT          => self.op_assert()?,
                OpCode::PRINT           => self.op_print()?,
                OpCode::JUMP            => self.op_jump(),
                OpCode::JUMP_IF_TRUE    => self.op_jump_if_true()?,
                OpCode::JUMP_IF_FALSE   => self.op_jump_if_false()?,
                OpCode::LOOP            => self.op_loop(),
                OpCode::RETURN          => {
                    // return is inlined because we need to break the dispatch loop (for now)
                    let value = self.pop()?;
                    log::debug!("return value {:?}", &value);
                    break Ok(value);
                }
                // SAFETY Chunk is checked when the VM is constructed, all OpCodes must be valid.
                // IP can only be at the start of an OpCode if every instruction consumes all of
                // it's argument bytes.
                _ => unsafe { hint::unreachable_unchecked() },
            }
        }
    }

    ////////////////////////////////////////
    // Instruction implementation

    fn op_constant(&mut self) -> Result<(), VmError<'alloc>> {
        let key = self.read_const_key();
        let constant = self.chunk.get_constant(key)
            .ok_or(VmError::CompileError(CodeError::InvalidConstantKey(key)))?;
        log::trace!("constant {:?}", &constant);
        self.push(constant);
        Ok(())
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

    fn op_pop(&mut self) -> Result<(), VmError<'alloc>> {
        self.pop()?;
        Ok(())
    }

    fn op_get_local(&mut self) -> Result<(), VmError<'alloc>> {
        let slot = self.read_u16();
        let value = *self.stack.get(slot as usize)
            .ok_or(VmError::CompileError(CodeError::InvalidStackSlot(slot)))?;
        self.push(value);
        Ok(())
    }

    fn op_set_local(&mut self) -> Result<(), VmError<'alloc>> {
        let slot = self.read_u16();
        let value = self.peek()?;
        let slot = self.stack.get_mut(slot as usize)
            .ok_or(VmError::CompileError(CodeError::InvalidStackSlot(slot)))?;
        *slot = value;
        Ok(())
    }

    fn op_get_global(&mut self) -> Result<(), VmError<'alloc>> {
        let key = self.read_const_key();
        let name = self.chunk.get_constant(key)
            .and_then(Value::downcast::<String>)
            .ok_or(VmError::CompileError(CodeError::InvalidConstantKey(key)))?;
        let value = *self.globals.get(&name)
            .ok_or_else(|| VmError::RuntimeError {
                source: self.chunk.source(),
                span: self.get_span(self.offset() - 3),
                kind: RuntimeErrorKind::UndefinedGlobalVariable(name.as_str().to_string()),
            })?;
        self.push(value);
        Ok(())
    }

    fn op_def_global(&mut self) -> Result<(), VmError<'alloc>> {
        let key = self.read_const_key();
        let name = self.chunk.get_constant(key)
            .and_then(Value::downcast::<String>)
            .ok_or(VmError::CompileError(CodeError::InvalidConstantKey(key)))?;
        let value = self.pop()?;
        log::trace!("define global variable name {:?}", name);
        self.globals.insert(name, value);
        Ok(())
    }

    fn op_set_global(&mut self) -> Result<(), VmError<'alloc>> {
        let key = self.read_const_key();
        let name = self.chunk.get_constant(key)
            .and_then(Value::downcast::<String>)
            .ok_or(VmError::CompileError(CodeError::InvalidConstantKey(key)))?;
        let value = self.peek()?;
        log::trace!("define global variable name {:?}", name);
        if self.globals.insert(name, value).is_none() {
            return Err(VmError::RuntimeError {
                source: self.chunk.source(),
                span: self.get_span(self.offset() - 3),
                kind: RuntimeErrorKind::UndefinedGlobalVariable(name.as_str().to_string()),
            });
        }
        Ok(())
    }

    fn op_equal(&mut self) -> Result<(), VmError<'alloc>> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        let result = lhs == rhs;
        self.push(Value::new_bool(result));
        Ok(())
    }

    fn op_greater(&mut self) -> Result<(), VmError<'alloc>> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        let result = match (lhs.to_float(), rhs.to_float()) {
            (Some(lhs), Some(rhs)) => Value::new_bool(lhs > rhs),
            _ => {
                dbg!(lhs, rhs);
                return Err(VmError::RuntimeError {
                    source: self.chunk.source(),
                    span: self.get_span(self.offset() - 1),
                    kind: RuntimeErrorKind::TypeError("comparison only supported on Numbers"),
                })
            },
        };
        self.push(result);
        Ok(())
    }

    fn op_less(&mut self) -> Result<(), VmError<'alloc>> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        let result = match (lhs.to_float(), rhs.to_float()) {
            (Some(lhs), Some(rhs)) => Value::new_bool(lhs < rhs),
            _ => {
                dbg!(lhs, rhs);
                return Err(VmError::RuntimeError {
                    source: self.chunk.source(),
                    span: self.get_span(self.offset() - 1),
                    kind: RuntimeErrorKind::TypeError("comparison only supported on Numbers"),
                })
            },
        };
        self.push(result);
        Ok(())
    }

    fn op_add(&mut self) -> Result<(), VmError<'alloc>> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        let result = if let (Some(lhs), Some(rhs)) = (lhs.to_float(), rhs.to_float()) {
            Value::new_float(lhs + rhs)
        } else if let (Some(lhs), Some(rhs)) = (lhs.downcast::<String>(), rhs.downcast::<String>()) {
            let sum = lhs.as_str().to_string() + rhs.as_str();
            Value::new_object(String::new_owned(sum.into_boxed_str(), self.alloc))
        } else {
            return Err(VmError::RuntimeError {
                source: self.chunk.source(),
                span: self.get_span(self.offset() - 1),
                kind: RuntimeErrorKind::TypeError("addition only supported on Numbers and Strings"),
            });
        };
        self.push(result);
        Ok(())
    }

    fn op_subtract(&mut self) -> Result<(), VmError<'alloc>> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        let result = match (lhs.to_float(), rhs.to_float()) {
            (Some(lhs), Some(rhs)) => Value::new_float(lhs - rhs),
            _ => return Err(VmError::RuntimeError {
                source: self.chunk.source(),
                span: self.get_span(self.offset() - 1),
                kind: RuntimeErrorKind::TypeError("subtraction only supported on Numbers"),
            }),
        };
        self.push(result);
        Ok(())
    }

    fn op_multiply(&mut self) -> Result<(), VmError<'alloc>> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        let result = match (lhs.to_float(), rhs.to_float()) {
            (Some(lhs), Some(rhs)) => Value::new_float(lhs * rhs),
            _ => return Err(VmError::RuntimeError {
                source: self.chunk.source(),
                span: self.get_span(self.offset() - 1),
                kind: RuntimeErrorKind::TypeError("multiplication only supported on Numbers"),
            }),
        };
        self.push(result);
        Ok(())
    }

    fn op_divide(&mut self) -> Result<(), VmError<'alloc>> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        let result = match (lhs.to_float(), rhs.to_float()) {
            (Some(lhs), Some(rhs)) => Value::new_float(lhs / rhs),
            _ => return Err(VmError::RuntimeError {
                source: self.chunk.source(),
                span: self.get_span(self.offset() - 1),
                kind: RuntimeErrorKind::TypeError("division only supported on Numbers"),
            }),
        };
        self.push(result);
        Ok(())
    }

    fn op_not(&mut self) -> Result<(), VmError<'alloc>> {
        let value = self.pop()?;
        let value = Value::new_bool(value.is_falsy());
        self.push(value);
        Ok(())
    }

    fn op_negate(&mut self) -> Result<(), VmError<'alloc>> {
        let value = self.pop()?;
        let value = value.to_float()
            .map(|n| Value::new_float(-n))
            .ok_or_else(|| VmError::RuntimeError {
                source: self.chunk.source(),
                span: self.get_span(self.offset() - 1),
                kind: RuntimeErrorKind::TypeError("negation only supported on Numbers"),
            })?;
        self.push(value);
        Ok(())
    }

    fn op_assert(&mut self) -> Result<(), VmError<'alloc>> {
        let value = self.pop()?;
        match value.to_bool() {
            Some(true) => {}
            Some(false) => return Err(VmError::RuntimeError {
                source: self.chunk.source(),
                span: self.get_span(self.offset() - 1),
                kind: RuntimeErrorKind::AssertionError
            }),
            _ => return Err(VmError::RuntimeError {
                source: self.chunk.source(),
                span: self.get_span(self.offset() - 1),
                kind: RuntimeErrorKind::TypeError("asserted expression must return a Bool"),
            }),
        }
        Ok(())
    }

    fn op_print(&mut self) -> Result<(), VmError<'alloc>> {
        let value = self.pop()?;
        println!("{:?}", value);
        Ok(())
    }

    fn op_jump(&mut self) {
        let offset = self.read_u16() as usize;

        // SAFETY Chunk is checked when VM is constructed.
        // - every jump must be at the start of a valid instruction
        self.ip = unsafe { self.ip.add(offset as usize) };
    }

    fn op_jump_if_true(&mut self) -> Result<(), VmError<'alloc>> {
        let offset = self.read_u16();

        let value = self.peek()?;
        if !value.is_falsy() {
            // SAFETY Chunk is checked when VM is constructed.
            // - every jump must be at the start of a valid instruction
            self.ip = unsafe { self.ip.add(offset as usize) };
        }
        Ok(())
    }

    fn op_jump_if_false(&mut self) -> Result<(), VmError<'alloc>> {
        let offset = self.read_u16();

        let value = self.peek()?;
        if value.is_falsy() {
            // SAFETY Chunk is checked when VM is constructed.
            // - every jump must be at the start of a valid instruction
            self.ip = unsafe { self.ip.add(offset as usize) };
        }
        Ok(())
    }

    fn op_loop(&mut self) {
        let offset = self.read_u16();

        // SAFETY Chunk is checked when VM is constructed.
        // - every jump must be at the start of a valid instruction
        self.ip = unsafe { self.ip.sub(offset as usize) };
    }
}
