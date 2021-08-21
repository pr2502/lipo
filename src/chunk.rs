use crate::default;
use crate::opcode::OpCode;
use crate::span::{FreeSpan, Span};
use crate::value::Value;
use indexmap::IndexSet;
use std::assert_matches::assert_matches;
use std::convert::TryInto;
use std::fmt::{self, Debug};
use std::iter;


/// Emmited bytecode Chunk
pub struct Chunk<'src> {
    /// Packed bytecode opcodes
    code: Vec<u8>,

    /// Constant pool
    ///
    /// Chunk may contain up to `u16::MAX` unique constants.
    constants: IndexSet<Value>,

    /// Opcode origin spans
    ///
    /// Indexed by bytes in `code`. Free spans are anchored against `source`.
    spans: Vec<FreeSpan>,

    /// Original source code
    source: &'src str,
}

impl<'src> Chunk<'src> {
    pub fn new(source: &'src str) -> Chunk<'src> {
        Chunk {
            code: default(),
            constants: default(),
            spans: default(),
            source,
        }
    }

    pub fn write(&mut self, opcode: OpCode, span: FreeSpan) -> usize {
        let before = self.code.len();
        opcode.encode(&mut self.code);
        let bytes = self.code.len() - before;
        for _ in 0..bytes {
            // FIXME this is an extremely wasteful way of storing the debug information, make
            // something a bit better
            self.spans.push(span);
        }
        before
    }

    pub fn patch_jump(&mut self, position: usize) {
        // Check that the position really points to a placeholder JUMP* instruction
        assert_matches!(
            self.code[position..][..3],
            [OpCode::JUMP | OpCode::JUMP_IF_TRUE | OpCode::JUMP_IF_FALSE, 0xFF, 0xFF],
        );

        // JUMP* is patched to jump forward right after the code that's meant to be
        // executed.

        // Without any jumping the IP would be after the JUMP* instruction.
        let old_ip = position + 3;
        // We want to set it at the current end of the chunk.
        let new_ip = self.code.len();

        // We want `new_ip = old_ip + offset`.
        let offset = (new_ip - old_ip).try_into()
            .expect("max jump length exceeded");

        let [x, y] = u16::to_le_bytes(offset);
        self.code[position+1] = x;
        self.code[position+2] = y;
    }

    pub fn insert_constant(&mut self, value: Value) -> u16 {
        let (index, _) = self.constants.insert_full(value);
        index.try_into().expect("constant pool size limit reached")
    }

    pub fn get_constant(&self, index: u16) -> Option<Value> {
        self.constants.get_index(index as usize).copied()
    }

    pub fn opcodes(&self) -> impl Iterator<Item = OpCode> + '_ {
        let mut code = self.code.as_slice();
        iter::from_fn(move || {
            let (opcode, rest) = OpCode::decode(code)?;
            code = rest;
            Some(opcode)
        })
    }

    pub fn spans(&self) -> impl Iterator<Item = Span<'_>> + '_ {
        self.spans.iter()
            .map(|fs| fs.anchor(self.source))
    }

    pub fn code(&self) -> &[u8] {
        &self.code
    }

    pub fn source(&self) -> &'src str {
        self.source
    }
}

impl<'src> Debug for Chunk<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut prev_line = 0;
        writeln!(f, "Chunk {{")?;
        for (opcode, span) in self.opcodes().zip(self.spans()) {
            let (line, _) = span.lines();
            if line != prev_line {
                prev_line = line;
                write!(f, "{:>4}  ", line)?;
            } else {
                write!(f, "   |  ")?;
            };
            write!(f, "  {:?}", opcode)?;
            match opcode {
                OpCode::Constant { index } => {
                    if let Some(value) = self.get_constant(index) {
                        write!(f, "\t; {:?}", value)?;
                    } else {
                        write!(f, "\t; missing constant index={}", index)?;
                    }
                }
                OpCode::DefGlobal { index } |
                OpCode::GetGlobal { index } |
                OpCode::SetGlobal { index } => {
                    if let Some(value) = self.get_constant(index) {
                        write!(f, "\t; var {:?}", value)?;
                    } else {
                        write!(f, "\t; missing constant index={}", index)?;
                    }
                }
                _ => {}
            }
            writeln!(f)?;
        }
        writeln!(f, "}}")
    }
}
