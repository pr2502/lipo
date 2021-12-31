use std::assert_matches::assert_matches;
use std::collections::hash_map::Entry;
use std::convert::TryInto;
use std::fmt::{self, Debug};
use std::hash::Hash;
use std::iter;

use fxhash::FxHashMap as HashMap;

use crate::builtins::String;
use crate::compiler::constant::ConstCell;
use crate::opcode::OpCode;
use crate::span::FreeSpan;
use crate::{ObjectRef, Trace, Value};


/// Bytecode Chunk
#[derive(Trace)]
pub struct Chunk<'alloc> {
    /// Packed bytecode
    code: Box<[u8]>,

    /// Constant pool
    ///
    /// Chunk may contain up to `u16::MAX` unique constants.
    constants: Box<[Value<'alloc>]>,

    /// Maximum required stack size for temporaries
    ///
    /// Includes the parameters
    max_stack: usize,

    /// Params
    ///
    /// Number of input parameters (initial stack values without the callee at
    /// 0)
    params: u8,

    /// Upvalues
    ///
    /// Number of upvalues the Chunk works with, it
    upvalues: u8,

    /// Source code
    source: ObjectRef<'alloc, String>,

    /// Origin span for each opcode
    spans: Box<[FreeSpan]>,
}

impl<'alloc> Chunk<'alloc> {
    pub fn code(&self) -> &[u8] {
        &self.code
    }

    pub fn max_stack(&self) -> usize {
        self.max_stack
    }

    pub fn params(&self) -> u8 {
        self.params
    }

    /// Get span from byte offset of the OpCode
    ///
    /// Will panic if the byte offset code incorrect.
    pub fn span(&self, byte_offset: usize) -> FreeSpan {
        let idx = OffsetIter::new(&self.code)
            .position(|(_, offset)| offset == byte_offset)
            .expect("invalid OpCode byte offset");

        // TODO don't panic if debuginfo is missing
        self.spans[idx]
    }

    pub fn get_constant(&self, key: u16) -> Option<Value<'alloc>> {
        self.constants.get(usize::from(key)).copied()
    }

    pub fn constants(&self) -> &[Value<'alloc>] {
        &self.constants
    }
}

impl<'alloc> Debug for Chunk<'alloc> {
    // TODO doesn't play nice with std pretty printer indentaion
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const RED: &str = "\x1B[31m";
        const RESET: &str = "\x1B[m";

        writeln!(f, "Chunk {{")?;

        writeln!(f, "    max_stack: {}", self.max_stack)?;
        writeln!(f, "    params: {}", self.params)?;
        writeln!(f, "    upvalues: {}", self.upvalues)?;

        if !self.constants.is_empty() {
            writeln!(f, "    constants:")?;
            for (index, val) in self.constants.iter().enumerate() {
                writeln!(f, "    {:>4}  {:?}", index, val)?;
            }
            writeln!(f)?;
        }

        writeln!(f, "    code:")?;
        let opcodes = Iter::new(&self.code);
        let spans = self.spans.iter().map(|fs| fs.anchor(&self.source));
        let mut prev_line = 0;
        for (opcode, span) in opcodes.zip(spans) {
            let (line, _) = span.lines();
            if line != prev_line {
                prev_line = line;
                write!(f, "    {:>4}", line)?;
            } else {
                write!(f, "       |")?;
            };
            let opcodefmt = format!("{:?}", opcode);
            write!(f, "  {:<36} |", opcodefmt)?;
            if let Some((before, span, after)) = span.line_parts() {
                writeln!(f, "  {}{}{}{}{}", before, RED, span, RESET, after)?;
            } else {
                writeln!(f)?;
            }
        }

        writeln!(f, "}}")
    }
}


/// Wrapper around `Value` which allows to deduplicate `Value`s implementing
/// `Hash + Eq` using a `HashMap`.
struct DedupValue<'alloc>(Value<'alloc>);

impl<'alloc> PartialEq for DedupValue<'alloc> {
    fn eq(&self, other: &Self) -> bool {
        self.0.partial_eq(&other.0).unwrap_or(false)
    }
}

impl<'alloc> Eq for DedupValue<'alloc> {}

impl<'alloc> Hash for DedupValue<'alloc> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if let Some(hash_code) = self.0.hash_code() {
            state.write_usize(hash_code);
        }
    }
}


/// Incomplete and/or unchecked [`Chunk`].
pub struct ChunkBuf<'alloc> {
    /// Packed bytecode buffer
    code: Vec<u8>,

    /// Constant pool
    constants: Vec<Value<'alloc>>,

    /// Deduplicating map for constant pool
    constant_hash: HashMap<DedupValue<'alloc>, u16>,

    /// Function parameters (initial stack size without callee)
    params: u8,

    /// Function upvalues
    pub upvalues: u8,

    /// Source code
    source: ObjectRef<'alloc, String>,

    /// Origin span for each opcode
    spans: Vec<FreeSpan>,
}

impl<'alloc> ChunkBuf<'alloc> {
    pub fn new(source: ObjectRef<'alloc, String>, params: u8) -> ChunkBuf<'alloc> {
        ChunkBuf {
            code: Vec::default(),
            constants: Vec::default(),
            constant_hash: HashMap::default(),
            params,
            upvalues: 0,
            source,
            spans: Vec::default(),
        }
    }
}

mod check;


#[derive(Debug)]
pub struct PatchPlace {
    /// Byte offset of the start of the instruction into the code vector
    ///
    /// None if the emitted instruction isn't patchable.
    position: Option<usize>,
}

impl Drop for PatchPlace {
    fn drop(&mut self) {
        // Turn `PatchPlace` into a Drop Bomb if the compiler forgets to patch a jump
        if self.position.is_some() {
            panic!("BUG: unpatched jump");
        }
    }
}

#[derive(Debug)]
pub struct LoopPoint {
    /// Byte offset of the start of the instruction into the code vector
    position: usize,
}

impl<'alloc> ChunkBuf<'alloc> {
    pub fn emit(&mut self, opcode: OpCode, span: FreeSpan) -> PatchPlace {
        let position = self.code.len();
        opcode.encode(&mut self.code);
        self.spans.push(span);

        PatchPlace {
            position: match opcode {
                OpCode::Jump { offset }
                | OpCode::JumpIfTrue { offset }
                | OpCode::JumpIfFalse { offset } => {
                    assert!(offset == u16::MAX, "invalid placeholder instruction");
                    Some(position)
                },
                _ => None,
            },
        }
    }

    pub fn patch_jump(&mut self, place: PatchPlace) {
        let position = {
            let mut place = place;
            // Defuse PatchPlace Drop Bomb
            place.position.take()
        };
        let position = position.expect("tried to patch an unpatchable instruction");

        // Check that the position really points to a placeholder JUMP* instruction
        assert_matches!(
            self.code[position..][..3],
            [
                OpCode::JUMP | OpCode::JUMP_IF_TRUE | OpCode::JUMP_IF_FALSE,
                0xff,
                0xff
            ],
            "invalid PatchPlace",
        );

        // JUMP* is patched to jump forward right after the code that's meant to be
        // executed.

        // Without any jumping the IP would be after the JUMP* instruction.
        let old_ip = position + 3;
        // We want to set it at the current end of the chunk.
        let new_ip = self.code.len();

        // We want `new_ip = old_ip + offset`.
        let offset = (new_ip - old_ip)
            .try_into()
            .expect("max jump length exceeded");

        let [x, y] = u16::to_le_bytes(offset);
        self.code[position + 1] = x;
        self.code[position + 2] = y;
    }

    pub fn loop_point(&self) -> LoopPoint {
        LoopPoint { position: self.code.len() }
    }

    pub fn emit_loop(&mut self, loop_point: LoopPoint, span: FreeSpan) {
        let LoopPoint { position } = loop_point;

        // LOOP offset is subtracted after the IP has been advanced past it.
        // Add 3 for the encoded size.
        let old_ip = self.code.len() + 3;

        // We want to set the IP back to where `loop_point` has been created.
        let new_ip = position;

        // We want `new_ip = old_ip - offset`.
        let offset = (old_ip - new_ip).try_into().expect("loop body too large");

        self.emit(OpCode::Loop { offset }, span);
    }

    pub fn insert_constant(&mut self, value: Value<'alloc>) -> u16 {
        // Don't try to deduplicate ConstCell, we don't have the Value yet
        if value.is::<ConstCell>() {
            let key = self.constants.len();
            let key = key.try_into().expect("constant pool size limit reached");
            self.constants.push(value);
            return key;
        }

        match self.constant_hash.entry(DedupValue(value)) {
            Entry::Vacant(e) => {
                let key = self.constants.len();
                let key = key.try_into().expect("constant pool size limit reached");
                self.constants.push(value);
                e.insert(key);
                key
            },
            Entry::Occupied(e) => *e.get(),
        }
    }
}


pub struct Iter<'code> {
    code: &'code [u8],
}

impl<'code> Iter<'code> {
    fn new(code: &'code [u8]) -> Iter<'code> {
        Iter { code }
    }
}

impl<'code> Iterator for Iter<'code> {
    type Item = OpCode;
    fn next(&mut self) -> Option<Self::Item> {
        if self.code.is_empty() {
            return None;
        }
        match OpCode::decode(self.code) {
            Some((opcode, rest)) => {
                self.code = rest;
                Some(opcode)
            },
            None => {
                let sample = if self.code.len() > 8 {
                    &self.code[..8]
                } else {
                    self.code
                };
                panic!(
                    "BUG: atempted to iterate invalid code. invalid code starts with {sample:?}"
                );
            },
        }
    }
}

impl<'code> iter::FusedIterator for Iter<'code> {}


pub struct OffsetIter<'code> {
    decode: Iter<'code>,
    offset: usize,
}

impl<'code> OffsetIter<'code> {
    fn new(code: &'code [u8]) -> OffsetIter<'code> {
        OffsetIter { decode: Iter::new(code), offset: 0 }
    }
}

impl<'code> Iterator for OffsetIter<'code> {
    type Item = (OpCode, usize);
    fn next(&mut self) -> Option<Self::Item> {
        match self.decode.next() {
            Some(opcode) => {
                let offset = self.offset;
                self.offset += opcode.len();
                Some((opcode, offset))
            },
            None => None,
        }
    }
}

impl<'code> iter::FusedIterator for OffsetIter<'code> {}
