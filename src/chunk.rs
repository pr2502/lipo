use crate::object::string::String;
use crate::object::{Trace, ObjectRef};
use crate::opcode::OpCode;
use crate::span::FreeSpan;
use crate::value::Value;
use indexmap::IndexSet;
use std::assert_matches::assert_matches;
use std::convert::TryInto;
use std::fmt::{self, Debug};
use std::iter;


derive_Object!(['alloc] Chunk<'alloc>);
/// Emmited bytecode Chunk
pub struct Chunk<'alloc> {
    /// Packed bytecode opcodes
    code: Vec<u8>,

    /// Constant pool
    ///
    /// Chunk may contain up to `u16::MAX` unique constants.
    constants: IndexSet<Value<'alloc>>,

    /// Source code
    source: ObjectRef<'alloc, String>,

    /// Opcode origin spans
    ///
    /// One span per instruction in `code`. Free spans are anchored against `source`.
    spans: Vec<FreeSpan>,
}

unsafe impl<'alloc> Trace for Chunk<'alloc> {
    fn mark(&self) {
        self.source.mark();
        self.constants().for_each(Trace::mark);
    }
}

impl<'alloc> Debug for Chunk<'alloc> {
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
        let opcodes = self.opcodes();
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

impl<'alloc> Chunk<'alloc> {
    pub fn new(source: ObjectRef<'alloc, String>) -> Chunk<'alloc> {
        Chunk {
            code: Vec::new(),
            constants: IndexSet::new(),
            source,
            spans: Vec::new(),
        }
    }

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

    pub fn source(&self) -> ObjectRef<'alloc, String> {
        self.source
    }

    pub fn spans(&self) -> &[FreeSpan] {
        &self.spans
    }

    pub fn constants(&self) -> impl Iterator<Item = &Value<'alloc>> {
        self.constants.iter()
    }

    /// Decodes the chunk opcodes one by one and asserts that:
    ///
    /// 1. all opcodes decode successfully
    /// 2. all jumps are in bounds
    /// 3. all jumps land on a valid opcode boundary
    /// 4. all constants exis
    /// 5. the last instruction in the chunk is a return
    ///
    /// If this function passes it should be safe to work with raw `ip` pointer in [`VM::run`].
    ///
    /// Stack slot accesses must however still be checked for now.
    ///
    /// # TODO
    /// - track stack effects and check stack access
    /// - create a separate `ChunkBuf` type that is used for emitting and may be invalid
    ///   and run a check before creating a `Chunk` and prevent invalid `Chunk`s from existing
    pub fn check(&self) {
        let code = &self.code[..];
        let len = code.len();

        let mut valid_boundaries = Vec::new();
        let mut jump_targets = Vec::new();
        let mut ip = code;

        // dummy instruction, the only important thing is that it's NOT a OpCode::Return so that an
        // empty code doesn't accidentally pass 5.
        let mut prev_opcode = OpCode::Unit;

        loop {
            let offset = (ip.as_ptr() as usize) - (code.as_ptr() as usize);

            if offset == len {
                // assert 5.
                assert!(prev_opcode == OpCode::Return, "last opcode must be a return");
                break;
            } else if offset > len {
                unreachable!()
            }

            valid_boundaries.push(offset);

            // assert 1.
            let (opcode, next) = OpCode::decode(ip).expect("invalid opcode");
            prev_opcode = opcode;
            ip = next;

            let offset = (ip.as_ptr() as usize) - (code.as_ptr() as usize);

            match opcode {
                OpCode::Constant { key } |
                OpCode::GetGlobal { name_key: key } |
                OpCode::DefGlobal { name_key: key } |
                OpCode::SetGlobal { name_key: key } => {
                    // assert 4.
                    assert!(
                        (key.index as usize) < self.constants.len(),
                        "constant index out of range",
                    );
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
                    let jump_to = offset.checked_add(jump as usize).expect("overflow");
                    jump_targets.push(jump_to);
                },

                OpCode::Loop { offset: jump } => {
                    let jump_to = offset.checked_sub(jump as usize).expect("overflow");
                    jump_targets.push(jump_to);
                },

                OpCode::Return => {},
            }
        }

        for offset in jump_targets {
            // assert 2.
            assert!(offset < len, "jump out of bounds");

            // assert 3.
            assert!(
                valid_boundaries.binary_search(&offset).is_ok(),
                "jump to an invalid boundary",
            );
        }
    }
}


#[derive(Debug)]
pub struct PatchPlace {
    /// Byte offset of the start of the instruction into the code vector
    position: usize,
}

#[derive(Debug)]
pub struct LoopPoint {
    /// Byte offset of the start of the instruction into the code vector
    position: usize,
}

impl<'alloc> Chunk<'alloc> {
    pub fn emit(&mut self, opcode: OpCode, span: FreeSpan) -> Option<PatchPlace> {
        let position = self.code.len();
        opcode.encode(&mut self.code);
        self.spans.push(span);

        matches!(opcode.tag(), OpCode::JUMP | OpCode::JUMP_IF_TRUE | OpCode::JUMP_IF_FALSE)
            .then(|| PatchPlace { position })
    }

    pub fn patch_jump(&mut self, place: Option<PatchPlace>) {
        let PatchPlace { position } = place.expect("tried to patch an unpatchable instruction");

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
