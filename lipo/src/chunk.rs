use crate::builtins::{Function, String};
use crate::opcode::OpCode;
use crate::span::FreeSpan;
use crate::{Object, ObjectRef, Trace, Value};
use fxhash::FxHashMap as HashMap;
use std::assert_matches::assert_matches;
use std::collections::hash_map::Entry;
use std::convert::TryInto;
use std::fmt::{self, Debug};
use std::{iter, mem};


/// Bytecode Chunk
#[derive(Object, Trace, Hash, PartialEq, Eq)]
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

    pub fn max_stack(&self) -> usize {
        self.max_stack
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
        let opcodes = Iter::new(&self.code);
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


/// Incomplete and/or unchecked [`Chunk`].
pub struct ChunkBuf<'alloc> {
    /// Packed bytecode buffer
    code: Vec<u8>,

    /// Constant pool
    constants: Vec<Value<'alloc>>,

    /// Deduplicating map for constant pool
    constant_hash: HashMap<Value<'alloc>, u16>,

    /// Function parameters / initial stack size
    params: usize,

    /// Source code
    source: ObjectRef<'alloc, String>,

    /// Origin span for each opcode
    spans: Vec<FreeSpan>,
}

impl<'alloc> ChunkBuf<'alloc> {
    pub fn new(source: ObjectRef<'alloc, String>, params: usize) -> ChunkBuf<'alloc> {
        ChunkBuf {
            code: Vec::default(),
            constants: Vec::default(),
            constant_hash: HashMap::default(),
            params,
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
    /// 5. All constants referenced by Closure are type Function
    /// 6. Chunk ends with the return opcode
    /// 7. All branches and loops agree on the stack size at each point in execution.
    /// 8. All stack accesses are within the initialized slots at that point in execution.
    ///
    /// For now stack slot accesses must still be checked by the VM.
    ///
    /// # TODO
    /// - track stack effects and check stack access
    /// - track max temp-stack size so we can resize stack before calling a function and not check
    ///   for overflow on every push
    /// - return a Result instead of panicking
    pub fn check(self) -> Chunk<'alloc> {

        /// Assert [1]
        ///
        /// All opcodes must decode successfully.
        ///
        /// Returns dedoced opcodes.
        fn check_decode(code: &[u8]) -> Vec<(OpCode, usize)> {
            OffsetIter::new(code).collect::<Vec<_>>()
        }

        /// Assert [2] and [3]
        ///
        /// Every jump must land on a starting offset of an instruction, if not it's either out of
        /// bounds [2] or misaligned [3].
        fn check_jumps(decoded: &[(OpCode, usize)]) {
            let jump_targets = decoded.iter()
                .filter_map(|(opcode, offset)| {
                    let jump_from = isize::try_from(offset + opcode.len()).unwrap();
                    Some(match opcode {
                        // forward jumps
                        OpCode::Jump { offset } |
                        OpCode::JumpIfTrue { offset } |
                        OpCode::JumpIfFalse { offset } => {
                            let offset: isize = (*offset).try_into().unwrap();
                            jump_from + offset
                        }

                        // backward jump
                        OpCode::Loop { offset } => {
                            let offset: isize = (*offset).try_into().unwrap();
                            jump_from - offset
                        }

                        // not jumps
                        _ => return None,
                    })
                });
            let invalid_jumps = jump_targets
                    .filter(|jump_target| {
                        decoded
                            .binary_search_by_key(jump_target, |(_, offset)| isize::try_from(*offset).unwrap())
                            .is_err()
                    })
                    .collect::<Vec<_>>();
            assert!(
                invalid_jumps.is_empty(),
                "invalid jump target",
            );
        }

        /// Assert [4]
        ///
        /// All constants referenced by the instructions must be present in the chunk constant pool.
        /// Because constants keys are consecutive indexes we can just compare the length.
        fn check_constants(decoded: &[(OpCode, usize)], constants: &[Value]) {
            let missing_constants = decoded.iter()
                .filter_map(|(opcode, _)| {
                    match opcode {
                        // constants
                        OpCode::Constant { key } => Some(*key),
                        OpCode::Closure { fn_key, upvals: _ } => Some(*fn_key),

                        // not constants
                        _ => None,
                    }
                })
                .any(|key| usize::from(key) >= constants.len());
            assert!(
                !missing_constants,
                "constant index out of range",
            );
        }

        /// Assert [5]
        ///
        /// All constants referenced by OpCode::Closure must be of type Function.
        fn check_constant_types(decoded: &[(OpCode, usize)], constants: &[Value]) {
            let wrong_constant_types = decoded.iter()
                .filter_map(|(opcode, _)| {
                    match opcode {
                        // typed constants
                        OpCode::Closure { fn_key, upvals: _ } => Some(*fn_key),

                        // not constants or any-typed constants
                        _ => None,
                    }
                })
                .any(|fn_key| constants[usize::from(fn_key)].downcast::<Function>().is_none());
            assert!(
                !wrong_constant_types,
                "constant has an incorrect type",
            );
        }

        /// Assert [6]
        ///
        /// Last opcode in the chunk must be a return to prevent reading past the end of the `code`
        /// slice.
        fn check_ends_return(decoded: &[(OpCode, usize)]) {
            assert!(
                decoded.last().map(|(opcode, _)| matches!(opcode, OpCode::Return)).unwrap_or(false),
                "last opcode must be a return",
            );
        }

        /// Assert [7] and [8]
        ///
        /// All branches and loops agree on the stack size at each point in execution [7], and all
        /// stack accessing OpCodes are accessing stack slots that are initialized at that point in
        /// execution [8].
        ///
        /// Returns the maximum stack required for all the temporaries in this Chunk.
        fn check_stack_access(decoded: &[(OpCode, usize)], params: usize) -> usize {
            use std::collections::{BTreeMap, BTreeSet};
            use std::collections::btree_map::Entry;

            let mut stack_size = BTreeMap::from([(0, params)]);
            let mut threads = BTreeSet::from([0]);

            for &(opcode, ip) in decoded {
                for thread_ip in mem::take(&mut threads) {
                    if thread_ip != ip {
                        // Skip threads which are ahead
                        threads.insert(thread_ip);
                        continue;
                    }

                    if matches!(opcode, OpCode::Return) {
                        // Diverging thread
                        continue;
                    }

                    let (pops, pushes) = opcode.stack_effect();
                    let prev_stack_size = stack_size.get(&ip).unwrap();
                    let next_stack_size = prev_stack_size
                        // Assert part [8] stack access is never below 0
                        .checked_sub(pops).expect("stack access below 0")
                        .checked_add(pushes).unwrap();

                    match opcode {
                        OpCode::GetLocal { slot } |
                        OpCode::SetLocal { slot } => {
                            // Assert part [8] direct stack access is below the current stack height
                            let slot = usize::from(slot);
                            assert!(slot < *prev_stack_size, "access uninitialized stack");
                        },
                        _ => {},
                    }

                    let mut next_ip = |next_ip: usize| {
                        match stack_size.entry(next_ip) {
                            Entry::Vacant(e) => {
                                e.insert(next_stack_size);
                            },
                            Entry::Occupied(e) => {
                                // Assert [7]
                                assert_eq!(
                                    *e.get(), next_stack_size,
                                    "threads disagree on stack size",
                                );
                            },
                        }

                        threads.insert(next_ip);
                    };

                    match opcode {
                        OpCode::Jump { offset } => {
                            // Jump forward
                            next_ip(ip + opcode.len() + usize::from(offset));
                        },
                        OpCode::Loop { offset } => {
                            // Jump backward
                            next_ip(ip + opcode.len() - usize::from(offset));
                        },
                        OpCode::JumpIfTrue { offset } |
                        OpCode::JumpIfFalse { offset } => {
                            // Take branch
                            next_ip(ip + opcode.len() + usize::from(offset));
                            // Don't take branch
                            next_ip(ip + opcode.len());
                        },
                        _ => {
                            // Ordinary instruction advance
                            next_ip(ip + opcode.len());
                        },
                    }
                }
            }

            stack_size.into_iter()
                .map(|(_, stack_size)| stack_size)
                .max()
                .unwrap()
        }

        // Run the checks
        let decoded = check_decode(&self.code);
        check_jumps(&decoded);
        check_constants(&decoded, &self.constants);
        check_constant_types(&decoded, &self.constants);
        check_ends_return(&decoded);
        let max_stack = check_stack_access(&decoded, self.params);

        // Now that the ChunkBuf has been checked we can construct a Chunk
        Chunk {
            code: self.code.into_boxed_slice(),
            constants: self.constants.into_boxed_slice(),
            max_stack,
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
            position: matches!(
                opcode.tag(),
                OpCode::JUMP | OpCode::JUMP_IF_TRUE | OpCode::JUMP_IF_FALSE
            )
            .then(|| position),
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

    pub fn insert_constant(&mut self, value: Value<'alloc>) -> u16 {
        match self.constant_hash.entry(value) {
            Entry::Vacant(e) => {
                let key = self.constants.len();
                let key = key.try_into().expect("constant pool size limit reached");
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
            }
            None => {
                let sample = if self.code.len() > 8 { &self.code[..8] } else { self.code };
                panic!("BUG: atempted to iterate invalid code. invalid code starts with {:?}", sample);
            }
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
        OffsetIter {
            decode: Iter::new(code),
            offset: 0,
        }
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
            }
            None => None,
        }
    }
}

impl<'code> iter::FusedIterator for OffsetIter<'code> {}
