use crate::chunk::Chunk;
use crate::default;
use crate::object::{ObjectRef, ObjectRefAny};
use crate::opcode::OpCode;
use crate::span::Span;
use crate::string::String as RoxString;
use crate::value::Value;
use fxhash::FxHashMap as HashMap;
use log::{debug, trace};

pub struct VM<'code> {
    chunk: &'code Chunk,
    ip: &'code [u8],
    stack: Vec<Value>,
    globals: HashMap<ObjectRef<RoxString>, Value>,
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
    AssertionError,
    TypeError(&'static str),
    UndefinedGlobalVariable(String),
}

impl<'code> VM<'code> {
    pub fn new(chunk: &'code Chunk) -> VM<'code> {
        VM {
            chunk,
            ip: chunk.code(),
            stack: default(),
            globals: default(),
        }
    }

    fn pop(&mut self) -> Result<Value, VmError<'code>> {
        self.stack.pop()
            .ok_or(VmError::CompileError(CodeError::PopEmptyStack))
    }

    fn peek(&mut self) -> Result<Value, VmError<'code>> {
        self.stack.last()
            .copied()
            .ok_or(VmError::CompileError(CodeError::PopEmptyStack))
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    pub fn run(mut self) -> Result<Value, VmError<'code>> {
        loop {
            let offset = (self.ip.as_ptr() as usize) - (self.chunk.code().as_ptr() as usize);
            let (opcode, next) = if let Some(res) = OpCode::decode(self.ip) { res } else {
                return Ok(Value::Nil);
            };
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
                OpCode::Pop => {
                    self.pop()?;
                }
                OpCode::GetGlobal { index } => {
                    let name = self.chunk.get_constant(index)
                        .and_then(|value| if let Value::Object(objref) = value { Some(objref) } else { None })
                        .and_then(ObjectRefAny::downcast::<RoxString>) 
                        .ok_or(VmError::CompileError(CodeError::InvalidConstantIndex(index)))?;
                    let value = *self.globals.get(&name)
                        .ok_or_else(|| VmError::RuntimeError {
                            span: self.chunk.spans().nth(offset).expect("missing span information"),
                            kind: RuntimeErrorKind::UndefinedGlobalVariable(name.as_str().to_string()),
                        })?;
                    self.push(value);
                }
                OpCode::DefGlobal { index } => {
                    let name = self.chunk.get_constant(index)
                        .and_then(|value| if let Value::Object(objref) = value { Some(objref) } else { None })
                        .and_then(ObjectRefAny::downcast::<RoxString>) 
                        .ok_or(VmError::CompileError(CodeError::InvalidConstantIndex(index)))?;
                    let value = self.pop()?;
                    trace!("define global variable name {:?}", name);
                    self.globals.insert(name, value);
                }
                OpCode::SetGlobal { index } => {
                    let name = self.chunk.get_constant(index)
                        .and_then(|value| if let Value::Object(objref) = value { Some(objref) } else { None })
                        .and_then(ObjectRefAny::downcast::<RoxString>) 
                        .ok_or(VmError::CompileError(CodeError::InvalidConstantIndex(index)))?;
                    let value = self.peek()?;
                    trace!("define global variable name {:?}", name);
                    if self.globals.insert(name, value).is_none() {
                        return Err(VmError::RuntimeError {
                            span: self.chunk.spans().nth(offset).expect("missing span information"),
                            kind: RuntimeErrorKind::UndefinedGlobalVariable(name.as_str().to_string()),
                        });
                    }
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
                            kind: RuntimeErrorKind::TypeError("comparison only supported on Numbers"),
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
                            kind: RuntimeErrorKind::TypeError("comparison only supported on Numbers"),
                        }),
                    };
                    self.push(result);
                }
                OpCode::Add => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    let result = match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs + rhs),
                        (Value::Object(lhs), Value::Object(rhs)) if lhs.is::<RoxString>() && rhs.is::<RoxString>() => {
                            let lhs = lhs.downcast::<RoxString>().unwrap();
                            let rhs = rhs.downcast::<RoxString>().unwrap();
                            let sum = lhs.as_str().to_string() + rhs.as_str();
                            Value::Object(RoxString::new_owned(sum.into_boxed_str()).upcast())
                        }
                        _ => return Err(VmError::RuntimeError {
                            span: self.chunk.spans().nth(offset).expect("missing span information"),
                            kind: RuntimeErrorKind::TypeError("addition only supported on Numbers and Strings"),
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
                            kind: RuntimeErrorKind::TypeError("subtraction only supported on Numbers"),
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
                            kind: RuntimeErrorKind::TypeError("multiplication only supported on Numbers"),
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
                            kind: RuntimeErrorKind::TypeError("division only supported on Numbers"),
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
                            kind: RuntimeErrorKind::TypeError("negation only supported on Numbers"),
                        }),
                    };
                    self.push(value);
                }
                OpCode::Assert => {
                    let value = self.pop()?;
                    match value {
                        Value::Bool(true) => {}
                        Value::Bool(false) => return Err(VmError::RuntimeError {
                            span: self.chunk.spans().nth(offset).expect("missing span information"),
                            kind: RuntimeErrorKind::AssertionError
                        }),
                        _ => return Err(VmError::RuntimeError {
                            span: self.chunk.spans().nth(offset).expect("missing span information"),
                            kind: RuntimeErrorKind::TypeError("asserted expression must return a Bool"),
                        }),
                    }
                    println!("{:?}", value);
                }
                OpCode::Print => {
                    let value = self.pop()?;
                    println!("{:?}", value);
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
