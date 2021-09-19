use crate::object::builtins::String;
use crate::object::{ObjectRef, Trace};
use crate::opcode::OpCode;
use crate::span::FreeSpan;
use crate::value::Value;
use fxhash::FxHashMap as HashMap;
use std::assert_matches::assert_matches;
use std::cmp::Ordering;
use std::collections::hash_map::Entry;
use std::convert::TryInto;
use std::fmt::{self, Debug};
use std::iter;


derive_Object!(Chunk<'alloc>);
/// Bytecode Chunk
#[derive(Hash, PartialEq, Eq)]
pub struct Chunk<'alloc> {
    /// Packed bytecode
    code: Box<[u8]>,

    /// Constant pool
    ///
    /// Chunk may contain up to `u16::MAX` unique constants.
    constants: Box<[Value<'alloc>]>,

    /// Source code
    source: ObjectRef<'alloc, String>,

    /// Origin span for each opcode
    spans: Box<[FreeSpan]>,
}

impl<'alloc> Chunk<'alloc> {
    pub fn code(&self) -> &[u8] {
        &self.code
    }

    pub fn source(&self) -> ObjectRef<'alloc, String> {
        self.source
    }

    /// Get span from byte offset of the OpCode
    ///
    /// Will panic if the byte offset code incorrect.
    pub fn span(&self, byte_offset: usize) -> FreeSpan {
        let mut ip = &self.code[..];
        let mut span_idx = 0;
        assert!(byte_offset < self.code.len(), "invalid OpCode byte offset");
        loop {
            let scan_offset = (ip.as_ptr() as usize) - (self.code.as_ptr() as usize);
            match Ord::cmp(&scan_offset, &byte_offset) {
                Ordering::Less => {
                    // Continue scanning
                }
                Ordering::Equal => {
                    // Found
                    break span_idx;
                }
                Ordering::Greater => {
                    // Misaligned byte offset
                    panic!("invalid OpCode byte offset");
                }
            }
            let (_, next_ip) = OpCode::decode(ip).unwrap();
            ip = next_ip;
            span_idx += 1;
        };

        // TODO don't panic if debuginfo is missing
        self.spans[span_idx]
    }

    pub fn get_constant(&self, key: ConstKey) -> Option<Value<'alloc>> {
        let ConstKey { index } = key;
        self.constants.get(index as usize).copied()
    }
}

unsafe impl<'alloc> Trace for Chunk<'alloc> {
    fn mark(&self) {
        self.source.mark();
        self.constants.iter().for_each(Trace::mark);
    }
}

impl<'alloc> Debug for Chunk<'alloc> {
    // TODO doesn't play nice with std pretty printer indentaion
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const RED: &str = "\x1B[31m";
        const RESET: &str = "\x1B[m";

        writeln!(f, "Chunk {{")?;

        if !self.constants.is_empty() {
            writeln!(f, "constants:")?;
            for (index, val) in self.constants.iter().enumerate() {
                writeln!(f, "#{:<3} {:?}", index, val)?;
            }
            writeln!(f)?;
        }


        writeln!(f, "code:")?;
        let opcodes = {
            let mut code = &self.code[..];
            iter::from_fn(move || {
                let (opcode, rest) = OpCode::decode(code)?;
                code = rest;
                Some(opcode)
            })
        };
        let spans = self.spans.iter()
            .map(|fs| fs.anchor(&self.source));
        let mut prev_line = 0;
        for (opcode, span) in opcodes.zip(spans) {
            let (line, _) = span.lines();
            if line != prev_line {
                prev_line = line;
                write!(f, "{:>4}", line)?;
            } else {
                write!(f, "   |")?;
            };
            let opcodefmt = format!("{:?}", opcode);
            write!(f, "    {:<32} |", opcodefmt)?;
            if let Some((before, span, after)) = span.line_parts() {
                writeln!(f, "  {}{}{}{}{}", before, RED, span, RESET, after)?;
            } else {
                writeln!(f)?;
            }
        }

        writeln!(f, "}}")
    }
}


/// Incomplete and/or unchecked [`Chunk`].
pub struct ChunkBuf<'alloc> {
    /// Packed bytecode buffer
    code: Vec<u8>,

    /// Constant pool
    constants: Vec<Value<'alloc>>,

    /// Deduplicating map for constant pool
    constant_hash: HashMap<Value<'alloc>, ConstKey>,

    /// Source code
    source: ObjectRef<'alloc, String>,

    /// Origin span for each opcode
    spans: Vec<FreeSpan>,
}

impl<'alloc> ChunkBuf<'alloc> {
    pub fn new(source: ObjectRef<'alloc, String>) -> ChunkBuf<'alloc> {
        ChunkBuf {
            code: Vec::default(),
            constants: Vec::default(),
            constant_hash: HashMap::default(),
            source,
            spans: Vec::default(),
        }
    }

    /// Checks the [`ChunkBuf`] contains valid bytecode and if yes returns a [`Chunk`].
    ///
    /// This ensures every instance of [`Chunk`] only contains only valid bytecode that is safe to
    /// execute in an unsafe VM in [`VM::run`](crate::vm::VM::run).
    ///
    /// Asserted invariants are:
    ///
    /// 1. All opcodes decode successfully
    /// 2. All jumps are in bounds
    /// 3. All jumps land on a valid opcode boundary
    /// 4. All constants exist
    /// 5. Constants used to reference a global are of type [`String`].
    /// 6. Chunk ends with the return opcode
    ///
    /// For now stack slot accesses must still be checked by the VM.
    ///
    /// # TODO
    /// - track stack effects and check stack access
    /// - track max temp-stack size so we can resize stack before calling a function and not check
    ///   for overflow on every push
    /// - return a Result instead of panicking
    pub fn check(self) -> Chunk<'alloc> {
        let code = &self.code[..];
        let len = code.len();

        let mut valid_boundaries = Vec::new();
        let mut jump_targets = Vec::new();
        let mut ip = code;

        // Dummy instruction, the only important thing is that it's NOT an `OpCode::Return` so that
        // an empty code slice doesn't accidentally pass (6).
        let mut prev_opcode = OpCode::Unit;

        loop {
            let before_offset = (ip.as_ptr() as usize) - (code.as_ptr() as usize);

            match Ord::cmp(&before_offset, &len) {
                Ordering::Less => {
                    // Continue decoding
                }
                Ordering::Equal => {
                    // Assert 6.
                    // Last opcode in the chunk must be a return to prevent reading past the end of
                    // the `code` slice.
                    assert!(prev_opcode == OpCode::Return, "last opcode must be a return");

                    assert!(ip.is_empty()); // Sanity check that the offset is calculated correctly.

                    // Finished decoding
                    break;
                }
                Ordering::Greater => unreachable!(),
            }

            // Assert 1.
            // All opcodes must decode successfully.
            let (opcode, next_ip) = OpCode::decode(ip).expect("invalid opcode");
            prev_opcode = opcode;
            ip = next_ip;

            // The instruction was decoded successfully, it's ok to jump to it.
            valid_boundaries.push(before_offset);

            // Jump offsets are calculated relative to the IP after the instruction was decoded.
            let after_offset = (ip.as_ptr() as usize) - (code.as_ptr() as usize);

            match opcode {
                OpCode::Constant { key } |
                OpCode::GetGlobal { name_key: key } |
                OpCode::DefGlobal { name_key: key } |
                OpCode::SetGlobal { name_key: key } => {
                    // Assert 4.
                    // All constants referenced by the instructions must be present in the chunk
                    // constant pool. Because constants keys are consecutive indexes we can just
                    // compare the length.
                    assert!(
                        (key.index as usize) < self.constants.len(),
                        "constant index out of range",
                    );

                    match opcode {
                        OpCode::GetGlobal { name_key } |
                        OpCode::DefGlobal { name_key } |
                        OpCode::SetGlobal { name_key } => {
                            // Assert 5.
                            // Constants used as a global keys are of type `String`.
                            assert!(
                                self.constants[name_key.index as usize].downcast::<String>().is_some(),
                                "name_key is not of type String",
                            );
                        }
                        _ => {}
                    }
                }

                OpCode::Unit => {},
                OpCode::True => {},
                OpCode::False => {},
                OpCode::Pop => {},
                OpCode::GetLocal { slot: _ } => {},
                OpCode::SetLocal { slot: _ } => {},

                OpCode::Equal => {},
                OpCode::Greater => {},
                OpCode::Less => {},
                OpCode::Add => {},
                OpCode::Subtract => {},
                OpCode::Multiply => {},
                OpCode::Divide => {},
                OpCode::Not => {},
                OpCode::Negate => {},
                OpCode::Assert => {},
                OpCode::Print => {},

                OpCode::Jump { offset: jump } |
                OpCode::JumpIfTrue { offset: jump } |
                OpCode::JumpIfFalse { offset: jump } => {
                    let jump_to = after_offset.checked_add(jump as usize).expect("overflow");
                    jump_targets.push(jump_to);
                },

                OpCode::Loop { offset: jump } => {
                    let jump_to = after_offset.checked_sub(jump as usize).expect("overflow");
                    jump_targets.push(jump_to);
                },

                OpCode::Call { args: _ } => {},
                OpCode::Return => {},
            }
        }

        for offset in jump_targets {
            // Assert 2. and 3.
            // We stored the starting offset of every decoded instruction in `valid_boundaries`,
            // every jump must land on one of them. If not it's either out of bounds (2) or
            // misaligned (3).
            assert!(
                valid_boundaries.binary_search(&offset).is_ok(),
                "jump to an invalid boundary",
            );
        }

        // Now that the ChunkBuf has been checked we can construct a Chunk
        Chunk {
            code: self.code.into_boxed_slice(),
            constants: self.constants.into_boxed_slice(),
            source: self.source,
            spans: self.spans.into_boxed_slice(),
        }
    }
}


#[derive(Debug)]
pub struct PatchPlace {
    /// Byte offset of the start of the instruction into the code vector
    ///
    /// None if the emitted instruction isn't patchable.
    position: Option<usize>,
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
            position: matches!(
                opcode.tag(),
                OpCode::JUMP | OpCode::JUMP_IF_TRUE | OpCode::JUMP_IF_FALSE
            )
            .then(|| position),
        }
    }

    pub fn patch_jump(&mut self, place: PatchPlace) {
        let PatchPlace { position } = place;
        let position = position.expect("tried to patch an unpatchable instruction");

        // Check that the position really points to a placeholder JUMP* instruction
        assert_matches!(
            self.code[position..][..3],
            [OpCode::JUMP | OpCode::JUMP_IF_TRUE | OpCode::JUMP_IF_FALSE, 0xFF, 0xFF],
            "invalid PatchPlace",
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
    /// Index into the constant pool
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

impl<'alloc> ChunkBuf<'alloc> {
    pub fn insert_constant(&mut self, value: Value<'alloc>) -> ConstKey {
        match self.constant_hash.entry(value) {
            Entry::Vacant(e) => {
                let index = self.constants.len();
                let index = index.try_into().expect("constant pool size limit reached");
                let key = ConstKey { index };
                self.constants.push(value);
                e.insert(key);
                key
            }
            Entry::Occupied(e) => {
                *e.get()
            }
        }
    }
}
