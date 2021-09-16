use crate::chunk::{Chunk, ConstKey};
use crate::object::string::String;
use crate::object::{Alloc, ObjectRef, Trace};
use crate::opcode::OpCode;
use crate::span::FreeSpan;
use crate::value::Value;
use fxhash::FxHashMap as HashMap;
use log::{debug, trace};


pub struct VM<'code, 'alloc> {
    chunk: &'code Chunk<'alloc>,
    alloc: &'alloc Alloc,
    ip: &'code [u8],
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
        VM {
            chunk,
            alloc,
            ip: chunk.code(),
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
            let (_, next) = OpCode::decode(scan).expect("invalid code");
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

        // SAFETY we've marked all the roots
        unsafe { self.alloc.sweep(); }
    }

    pub fn run(mut self) -> Result<Value<'alloc>, VmError<'alloc>> {
        loop {
            #[cfg(feature = "gc-stress")]
            self.gc();

            let offset = (self.ip.as_ptr() as usize) - (self.chunk.code().as_ptr() as usize);
            let (opcode, next) = match OpCode::decode(self.ip) {
                Some(res) => res,
                None => return Ok(Value::new_unit()),
            };
            trace!("stack {:?}", &self.stack);
            trace!("decode {:04}: {:?}", offset, opcode);
            self.ip = next;
            match opcode {
                OpCode::Constant { key } => self.op_constant(key)?,
                OpCode::Unit => self.op_unit()?,
                OpCode::True => self.op_true()?,
                OpCode::False => self.op_false()?,
                OpCode::Pop => self.op_pop()?,
                OpCode::GetLocal { slot } => self.op_get_local(slot)?,
                OpCode::SetLocal { slot } => self.op_set_local(slot)?,
                OpCode::GetGlobal { name_key } => self.op_get_global(name_key, offset)?,
                OpCode::DefGlobal { name_key } => self.op_def_global(name_key)?,
                OpCode::SetGlobal { name_key } => self.op_set_global(name_key, offset)?,
                OpCode::Equal => self.op_equal()?,
                OpCode::Greater => self.op_greater(offset)?,
                OpCode::Less => self.op_less(offset)?,
                OpCode::Add => self.op_add(offset)?,
                OpCode::Subtract => self.op_subtract(offset)?,
                OpCode::Multiply => self.op_multiply(offset)?,
                OpCode::Divide => self.op_divide(offset)?,
                OpCode::Not => self.op_not()?,
                OpCode::Negate => self.op_negate(offset)?,
                OpCode::Assert => self.op_assert(offset)?,
                OpCode::Print => self.op_print()?,
                OpCode::Jump { offset } => self.op_jump(offset)?,
                OpCode::JumpIfTrue { offset } => self.op_jump_if_true(offset)?,
                OpCode::JumpIfFalse { offset } => self.op_jump_if_false(offset)?,
                OpCode::Loop { offset } => self.op_loop(offset)?,
                OpCode::Return => {
                    // return is inlined because we need to break the dispatch loop (for now)
                    let value = self.pop()?;
                    debug!("return value {:?}", &value);
                    break Ok(value);
                }
            }
        }
    }

    ////////////////////////////////////////
    // instruction implementation

    fn op_constant(&mut self, key: ConstKey) -> Result<(), VmError<'alloc>> {
        let constant = self.chunk.get_constant(key)
            .ok_or(VmError::CompileError(CodeError::InvalidConstantKey(key)))?;
        trace!("constant {:?}", &constant);
        self.push(constant);
        Ok(())
    }

    fn op_unit(&mut self) -> Result<(), VmError<'alloc>> {
        self.push(Value::new_unit());
        Ok(())
    }

    fn op_true(&mut self) -> Result<(), VmError<'alloc>> {
        self.push(Value::new_bool(true));
        Ok(())
    }

    fn op_false(&mut self) -> Result<(), VmError<'alloc>> {
        self.push(Value::new_bool(false));
        Ok(())
    }

    fn op_pop(&mut self) -> Result<(), VmError<'alloc>> {
        self.pop()?;
        Ok(())
    }

    fn op_get_local(&mut self, slot: u16) -> Result<(), VmError<'alloc>> {
        let value = *self.stack.get(slot as usize)
            .ok_or(VmError::CompileError(CodeError::InvalidStackSlot(slot)))?;
        self.push(value);
        Ok(())
    }

    fn op_set_local(&mut self, slot: u16) -> Result<(), VmError<'alloc>> {
        let value = self.peek()?;
        let slot = self.stack.get_mut(slot as usize)
            .ok_or(VmError::CompileError(CodeError::InvalidStackSlot(slot)))?;
        *slot = value;
        Ok(())
    }

    fn op_get_global(&mut self, key: ConstKey, offset: usize) -> Result<(), VmError<'alloc>> {
        let name = self.chunk.get_constant(key)
            .and_then(Value::downcast::<String>)
            .ok_or(VmError::CompileError(CodeError::InvalidConstantKey(key)))?;
        let value = *self.globals.get(&name)
            .ok_or_else(|| VmError::RuntimeError {
                source: self.chunk.source(),
                span: self.get_span(offset),
                kind: RuntimeErrorKind::UndefinedGlobalVariable(name.as_str().to_string()),
            })?;
        self.push(value);
        Ok(())
    }

    fn op_def_global(&mut self, key: ConstKey) -> Result<(), VmError<'alloc>> {
        let name = self.chunk.get_constant(key)
            .and_then(Value::downcast::<String>)
            .ok_or(VmError::CompileError(CodeError::InvalidConstantKey(key)))?;
        let value = self.pop()?;
        trace!("define global variable name {:?}", name);
        self.globals.insert(name, value);
        Ok(())
    }

    fn op_set_global(&mut self, key: ConstKey, offset: usize) -> Result<(), VmError<'alloc>> {
        let name = self.chunk.get_constant(key)
            .and_then(Value::downcast::<String>)
            .ok_or(VmError::CompileError(CodeError::InvalidConstantKey(key)))?;
        let value = self.peek()?;
        trace!("define global variable name {:?}", name);
        if self.globals.insert(name, value).is_none() {
            return Err(VmError::RuntimeError {
                source: self.chunk.source(),
                span: self.get_span(offset),
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

    fn op_greater(&mut self, offset: usize) -> Result<(), VmError<'alloc>> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        let result = match (lhs.to_float(), rhs.to_float()) {
            (Some(lhs), Some(rhs)) => Value::new_bool(lhs > rhs),
            _ => return Err(VmError::RuntimeError {
                source: self.chunk.source(),
                span: self.get_span(offset),
                kind: RuntimeErrorKind::TypeError("comparison only supported on Numbers"),
            }),
        };
        self.push(result);
        Ok(())
    }

    fn op_less(&mut self, offset: usize) -> Result<(), VmError<'alloc>> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        let result = match (lhs.to_float(), rhs.to_float()) {
            (Some(lhs), Some(rhs)) => Value::new_bool(lhs < rhs),
            _ => return Err(VmError::RuntimeError {
                source: self.chunk.source(),
                span: self.get_span(offset),
                kind: RuntimeErrorKind::TypeError("comparison only supported on Numbers"),
            }),
        };
        self.push(result);
        Ok(())
    }

    fn op_add(&mut self, offset: usize) -> Result<(), VmError<'alloc>> {
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
                span: self.get_span(offset),
                kind: RuntimeErrorKind::TypeError("addition only supported on Numbers and Strings"),
            });
        };
        self.push(result);
        Ok(())
    }

    fn op_subtract(&mut self, offset: usize) -> Result<(), VmError<'alloc>> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        let result = match (lhs.to_float(), rhs.to_float()) {
            (Some(lhs), Some(rhs)) => Value::new_float(lhs - rhs),
            _ => return Err(VmError::RuntimeError {
                source: self.chunk.source(),
                span: self.get_span(offset),
                kind: RuntimeErrorKind::TypeError("subtraction only supported on Numbers"),
            }),
        };
        self.push(result);
        Ok(())
    }

    fn op_multiply(&mut self, offset: usize) -> Result<(), VmError<'alloc>> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        let result = match (lhs.to_float(), rhs.to_float()) {
            (Some(lhs), Some(rhs)) => Value::new_float(lhs * rhs),
            _ => return Err(VmError::RuntimeError {
                source: self.chunk.source(),
                span: self.get_span(offset),
                kind: RuntimeErrorKind::TypeError("multiplication only supported on Numbers"),
            }),
        };
        self.push(result);
        Ok(())
    }

    fn op_divide(&mut self, offset: usize) -> Result<(), VmError<'alloc>> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        let result = match (lhs.to_float(), rhs.to_float()) {
            (Some(lhs), Some(rhs)) => Value::new_float(lhs / rhs),
            _ => return Err(VmError::RuntimeError {
                source: self.chunk.source(),
                span: self.get_span(offset),
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

    fn op_negate(&mut self, offset: usize) -> Result<(), VmError<'alloc>> {
        let value = self.pop()?;
        let value = value.to_float()
            .map(|n| Value::new_float(-n))
            .ok_or_else(|| VmError::RuntimeError {
                source: self.chunk.source(),
                span: self.get_span(offset),
                kind: RuntimeErrorKind::TypeError("negation only supported on Numbers"),
            })?;
        self.push(value);
        Ok(())
    }

    fn op_assert(&mut self, offset: usize) -> Result<(), VmError<'alloc>> {
        let value = self.pop()?;
        match value.to_bool() {
            Some(true) => {}
            Some(false) => return Err(VmError::RuntimeError {
                source: self.chunk.source(),
                span: self.get_span(offset),
                kind: RuntimeErrorKind::AssertionError
            }),
            _ => return Err(VmError::RuntimeError {
                source: self.chunk.source(),
                span: self.get_span(offset),
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

    fn op_jump(&mut self, offset: u16) -> Result<(), VmError<'alloc>> {
        let offset = offset as usize;
        self.ip = &self.ip[offset..];
        Ok(())
    }

    fn op_jump_if_true(&mut self, offset: u16) -> Result<(), VmError<'alloc>> {
        let value = self.peek()?;
        if !value.is_falsy() {
            let offset = offset as usize;
            self.ip = &self.ip[offset..];
        }
        Ok(())
    }

    fn op_jump_if_false(&mut self, offset: u16) -> Result<(), VmError<'alloc>> {
        let value = self.peek()?;
        if value.is_falsy() {
            let offset = offset as usize;
            self.ip = &self.ip[offset..];
        }
        Ok(())
    }

    fn op_loop(&mut self, offset: u16) -> Result<(), VmError<'alloc>> {
        let offset = offset as usize;
        let ip_offset = self.chunk.code().len() - self.ip.len();
        let jump_to = ip_offset - offset;
        self.ip = &self.chunk.code()[jump_to..];
        Ok(())
    }
}
