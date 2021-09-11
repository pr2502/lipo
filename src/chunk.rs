use crate::fmt::SourceDebug;
use crate::opcode::OpCode;
use crate::span::FreeSpan;
use crate::value::Value;
use indexmap::IndexSet;
use std::assert_matches::assert_matches;
use std::convert::TryInto;
use std::fmt::{self, Debug};
use std::iter;


/// Emmited bytecode Chunk
#[derive(Default)]
pub struct Chunk<'alloc> {
    /// Packed bytecode opcodes
    code: Vec<u8>,

    /// Constant pool
    ///
    /// Chunk may contain up to `u16::MAX` unique constants.
    constants: IndexSet<Value<'alloc>>,

    /// Opcode origin spans
    ///
    /// Indexed by bytes in `code`. Free spans are anchored against `source`.
    spans: Vec<FreeSpan>,
}

impl<'alloc> SourceDebug for Chunk<'alloc> {
    fn fmt(&self, source: &str, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const RED: &str = "\x1B[31m";
        const RESET: &str = "\x1B[m";

        let opcodes = self.opcodes();
        let spans = self.spans.iter()
            .map(|fs| fs.anchor(source));

        let mut prev_line = 0;
        writeln!(f, "Chunk {{")?;
        for (opcode, span) in opcodes.zip(spans) {
            let (line, _) = span.lines();
            if line != prev_line {
                prev_line = line;
                write!(f, "{:>4}  ", line)?;
            } else {
                write!(f, "   |  ")?;
            };
            let opcodefmt = format!("{:?}", opcode);
            if let Some((before, span, after)) = span.line_parts() {
                writeln!(f, "  {:<32} |  {}{}{}{}{}", opcodefmt, before, RED, span, RESET, after)?;
            } else {
                writeln!(f, "  {    } |", opcodefmt)?;
            }
        }
        writeln!(f, "}}")
    }
}

impl<'alloc> Chunk<'alloc> {
    pub fn opcodes(&self) -> impl Iterator<Item = OpCode> + '_ {
        let mut code = self.code.as_slice();
        iter::from_fn(move || {
            let (opcode, rest) = OpCode::decode(code)?;
            code = rest;
            Some(opcode)
        })
    }

    pub fn code(&self) -> &[u8] {
        &self.code
    }

    pub fn spans(&self) -> &[FreeSpan] {
        &self.spans
    }

    pub fn constants(&self) -> impl Iterator<Item = &Value<'alloc>> {
        self.constants.iter()
    }
}


#[derive(Clone, Copy, Debug)]
pub struct PatchPlace {
    position: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct LoopPoint {
    position: usize,
}

impl<'alloc> Chunk<'alloc> {
    pub fn emit(&mut self, opcode: OpCode, span: FreeSpan) -> Option<PatchPlace> {
        let position = self.code.len();
        opcode.encode(&mut self.code);
        self.spans.push(span);
        matches!(opcode, OpCode::Jump { .. } | OpCode::JumpIfTrue { .. } | OpCode::JumpIfFalse { .. })
            .then(|| PatchPlace { position })
    }

    pub fn patch_jump(&mut self, place: Option<PatchPlace>) {
        let PatchPlace { position } = place.expect("tried to patch an unpatchable instruction");

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

    pub fn loop_point(&self) -> LoopPoint {
        LoopPoint {
            position: self.code.len(),
        }
    }

    pub fn emit_loop(&mut self, loop_point: LoopPoint, span: FreeSpan) {
        let LoopPoint { position } = loop_point;

        // LOOP offset is subtracted after the IP has been advanced past it.
        // Add 3 for the encoded size.
        let old_ip = self.code.len() + 3;

        // We want to set the IP back to where `loop_point` has been created.
        let new_ip = position;

        // We want `new_ip = old_ip - offset`.
        let offset = (old_ip - new_ip).try_into()
            .expect("loop body too large");

        self.emit(OpCode::Loop { offset }, span);
    }
}


#[derive(Clone, Copy, PartialEq)]
pub struct ConstKey {
    index: u16,
}

impl ConstKey {
    pub fn to_le_bytes(self) -> [u8; 2] {
        self.index.to_le_bytes()
    }

    pub fn from_le_bytes(bytes: [u8; 2]) -> ConstKey {
        ConstKey {
            index: u16::from_le_bytes(bytes),
        }
    }
}

impl Debug for ConstKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}", self.index)
    }
}

impl<'alloc> Chunk<'alloc> {
    pub fn insert_constant(&mut self, value: Value<'alloc>) -> ConstKey {
        let (index, _) = self.constants.insert_full(value);
        let index = index.try_into().expect("constant pool size limit reached");
        ConstKey { index }
    }

    pub fn get_constant(&self, key: ConstKey) -> Option<Value<'alloc>> {
        let ConstKey { index } = key;
        self.constants.get_index(index as usize).copied()
    }
}
