use crate::chunk::Chunk;
use crate::default;
use crate::opcode::OpCode;
use crate::value::Value;
use log::{debug, trace};

pub struct VM<'code> {
    chunk: &'code Chunk,
    ip: &'code [u8],
    stack: Vec<Value>,
}

#[derive(Debug)]
pub enum VmError {
    CompileError(CodeError),
    RuntimeError,
}

#[derive(Debug)]
pub enum CodeError {
    UnexpectedEndOfCode,
    InvalidConstantIndex(u16),
    PopEmptyStack,
}

impl<'code> VM<'code> {
    pub fn new(chunk: &'code Chunk) -> VM<'code> {
        VM {
            chunk,
            ip: chunk.code(),
            stack: default(),
        }
    }

    pub fn run(mut self) -> Result<Value, VmError> {
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
                    self.stack.push(constant);
                }
                OpCode::Add => {
                    let rhs = self.stack.pop()
                        .ok_or(VmError::CompileError(CodeError::PopEmptyStack))?;
                    let lhs = self.stack.pop()
                        .ok_or(VmError::CompileError(CodeError::PopEmptyStack))?;
                    let result = Value { float: lhs.float + rhs.float };
                    self.stack.push(result);
                }
                OpCode::Subtract => {
                    let rhs = self.stack.pop()
                        .ok_or(VmError::CompileError(CodeError::PopEmptyStack))?;
                    let lhs = self.stack.pop()
                        .ok_or(VmError::CompileError(CodeError::PopEmptyStack))?;
                    let result = Value { float: lhs.float - rhs.float };
                    self.stack.push(result);
                }
                OpCode::Multiply => {
                    let rhs = self.stack.pop()
                        .ok_or(VmError::CompileError(CodeError::PopEmptyStack))?;
                    let lhs = self.stack.pop()
                        .ok_or(VmError::CompileError(CodeError::PopEmptyStack))?;
                    let result = Value { float: lhs.float * rhs.float };
                    self.stack.push(result);
                }
                OpCode::Divide => {
                    let rhs = self.stack.pop()
                        .ok_or(VmError::CompileError(CodeError::PopEmptyStack))?;
                    let lhs = self.stack.pop()
                        .ok_or(VmError::CompileError(CodeError::PopEmptyStack))?;
                    let result = Value { float: lhs.float / rhs.float };
                    self.stack.push(result);
                }
                OpCode::Negate => {
                    let value = self.stack.pop()
                        .ok_or(VmError::CompileError(CodeError::PopEmptyStack))?;
                    let value = Value { float: -value.float };
                    self.stack.push(value);
                }
                OpCode::Return => {
                    let value = self.stack.pop()
                        .ok_or(VmError::CompileError(CodeError::PopEmptyStack))?;
                    debug!("return value {:?}", &value);
                    break Ok(value);
                }
            }
        }
    }
}
