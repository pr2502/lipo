use crate::chunk::Chunk;
use crate::default;
use crate::opcode::OpCode;
use crate::span::Span;
use crate::string::String;
use crate::value::Value;
use log::{debug, trace};

pub struct VM<'code> {
    chunk: &'code Chunk,
    ip: &'code [u8],
    stack: Vec<Value>,
}

#[derive(Debug)]
pub enum VmError<'code> {
    CompileError(CodeError),
    RuntimeError {
        span: Span<'code>,
        kind: RuntimeErrorKind,
    },
}

#[derive(Debug)]
pub enum CodeError {
    UnexpectedEndOfCode,
    InvalidConstantIndex(u16),
    PopEmptyStack,
}

#[derive(Debug)]
pub enum RuntimeErrorKind {
    TypeError,
}

impl<'code> VM<'code> {
    pub fn new(chunk: &'code Chunk) -> VM<'code> {
        VM {
            chunk,
            ip: chunk.code(),
            stack: default(),
        }
    }

    fn pop(&mut self) -> Result<Value, VmError<'code>> {
        self.stack.pop()
            .ok_or(VmError::CompileError(CodeError::PopEmptyStack))
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    pub fn run(mut self) -> Result<Value, VmError<'code>> {
        loop {
            let offset = (self.ip.as_ptr() as usize) - (self.chunk.code().as_ptr() as usize);
            let (opcode, next) = OpCode::decode(self.ip).ok_or(VmError::CompileError(CodeError::UnexpectedEndOfCode))?;
            trace!("stack {:?}", &self.stack);
            trace!("decode {:04}: {:?}", offset, opcode);
            self.ip = next;
            match opcode {
                OpCode::Constant { index } => {
                    let constant = self.chunk.get_constant(index)
                        .ok_or(VmError::CompileError(CodeError::InvalidConstantIndex(index)))?;
                    trace!("constant {:?}", &constant);
                    self.push(constant);
                }
                OpCode::Nil => {
                    self.push(Value::Nil);
                }
                OpCode::True => {
                    self.push(Value::Bool(true));
                }
                OpCode::False => {
                    self.push(Value::Bool(false));
                }
                OpCode::Equal => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    let result = match (lhs, rhs) {
                        (Value::Nil, Value::Nil) => true,
                        (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
                        (Value::Number(lhs), Value::Number(rhs)) => (lhs - rhs).abs() <= f64::EPSILON,
                        (Value::Object(lhs), Value::Object(rhs)) => lhs == rhs,
                        // different types are never equal
                        _ => false,
                    };
                    self.push(Value::Bool(result));
                }
                OpCode::Greater => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    let result = match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => Value::Bool(lhs > rhs),
                        _ => return Err(VmError::RuntimeError {
                            span: self.chunk.spans().nth(offset).expect("missing span information"),
                            kind: RuntimeErrorKind::TypeError
                        }),
                    };
                    self.push(result);
                }
                OpCode::Less => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    let result = match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => Value::Bool(lhs < rhs),
                        _ => return Err(VmError::RuntimeError {
                            span: self.chunk.spans().nth(offset).expect("missing span information"),
                            kind: RuntimeErrorKind::TypeError
                        }),
                    };
                    self.push(result);
                }
                OpCode::Add => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    let result = match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs + rhs),
                        (Value::Object(lhs), Value::Object(rhs)) if lhs.is::<String>() && rhs.is::<String>() => {
                            let lhs = lhs.downcast::<String>().unwrap();
                            let rhs = rhs.downcast::<String>().unwrap();
                            let sum = lhs.as_str().to_string() + rhs.as_str();
                            Value::Object(String::new_owned(sum.into_boxed_str()).upcast())
                        }
                        _ => return Err(VmError::RuntimeError {
                            span: self.chunk.spans().nth(offset).expect("missing span information"),
                            kind: RuntimeErrorKind::TypeError
                        }),
                    };
                    self.push(result);
                }
                OpCode::Subtract => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    let result = match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs - rhs),
                        _ => return Err(VmError::RuntimeError {
                            span: self.chunk.spans().nth(offset).expect("missing span information"),
                            kind: RuntimeErrorKind::TypeError
                        }),
                    };
                    self.push(result);
                }
                OpCode::Multiply => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    let result = match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs * rhs),
                        _ => return Err(VmError::RuntimeError {
                            span: self.chunk.spans().nth(offset).expect("missing span information"),
                            kind: RuntimeErrorKind::TypeError
                        }),
                    };
                    self.push(result);
                }
                OpCode::Divide => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    let result = match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs / rhs),
                        _ => return Err(VmError::RuntimeError {
                            span: self.chunk.spans().nth(offset).expect("missing span information"),
                            kind: RuntimeErrorKind::TypeError
                        }),
                    };
                    self.push(result);
                }
                OpCode::Not => {
                    let value = self.pop()?;
                    let value = match value {
                        // only `nil` and `false` are "falsey", so their negation is `true`
                        Value::Nil | Value::Bool(false) => Value::Bool(true),
                        // everything else is "truthy" so the negation is always false
                        _ => Value::Bool(false),
                    };
                    self.push(value);
                }
                OpCode::Negate => {
                    let value = self.pop()?;
                    let value = match value {
                        Value::Number(n) => Value::Number(-n),
                        _ => return Err(VmError::RuntimeError {
                            span: self.chunk.spans().nth(offset).expect("missing span information"),
                            kind: RuntimeErrorKind::TypeError
                        }),
                    };
                    self.push(value);
                }
                OpCode::Return => {
                    let value = self.pop()?;
                    debug!("return value {:?}", &value);
                    break Ok(value);
                }
            }
        }
    }
}
