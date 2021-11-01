use super::{Chunk, ChunkBuf, OffsetIter};
use crate::builtins::Function;
use crate::opcode::OpCode;
use crate::Value;
use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, BTreeSet};
use std::mem;


impl<'alloc> ChunkBuf<'alloc> {
    /// Checks the [`ChunkBuf`] contains valid bytecode and if yes returns a [`Chunk`].
    ///
    /// This ensures every instance of [`Chunk`] only contains only valid bytecode that is safe to
    /// execute in an unsafe VM in [`VM::run`](crate::vm::VM::run).
    ///
    /// Asserted invariants are:
    ///
    ///  1. All opcodes decode successfully
    ///  2. All jumps are in bounds
    ///  3. All jumps land on a valid opcode boundary
    ///  4. All constants exist
    ///  5. All constants referenced by Closure are type Function
    ///  6. Chunk ends with the return opcode
    ///  7. All branches and loops agree on the stack size at each point in execution.
    ///  8. All stack accesses are within the initialized slots at that point in execution.
    ///  9. All upvalue references are within the given bound.
    /// 10. All opcode::Closure number of upvalues matches the referenced Function.
    ///
    /// For now stack slot accesses must still be checked by the VM.
    ///
    /// # TODO
    /// - return a Result instead of panicking
    pub fn check(self) -> Chunk<'alloc> {
        let ChunkBuf {
            code,
            constants,
            params,
            upvalues,
            source,
            spans,
            ..
        } = self;

        let decoded = check_decode(&code);
        check_jumps(&decoded);
        check_constants(&decoded, &constants);
        let max_stack = check_stack(&decoded, params);
        check_upvalues(&decoded, &constants, upvalues);

        // Now that we know the ChunkBuf is correct we can construct a Chunk
        Chunk {
            code: code.into_boxed_slice(),
            constants: constants.into_boxed_slice(),
            max_stack,
            upvalues,
            params,
            source,
            spans: spans.into_boxed_slice(),
        }
    }
}


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
    for (opcode, offset) in decoded {
        let jump_from = isize::try_from(offset + opcode.len()).unwrap();
        let jump_to = match opcode {
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
            _ => continue,
        };
        if decoded.binary_search_by_key(&jump_to, |(_, offset)| isize::try_from(*offset).unwrap()).is_err() {
            panic!("ChunkError: invalid jump target. offset={}", jump_to);
        }
    }
}

/// Assert [4] and [5]
///
/// All constants referenced by the instructions must be present in the chunk constant pool [4].
/// Because constants keys are consecutive indexes we can just compare the length.
///
/// All constants referenced by OpCode::Closure must be of type Function [5].
fn check_constants(decoded: &[(OpCode, usize)], constants: &[Value]) {
    for (opcode, _) in decoded {
        match opcode {
            OpCode::Constant { key } |
            OpCode::Closure { fn_key: key, upvals: _ } => {
                let key = usize::from(*key);
                assert!(
                    key < constants.len(),
                    "ChunkError: constant key out of range. key={}, constants={}",
                    key,
                    constants.len(),
                );
            },
            _ => {},
        }
        match opcode {
            OpCode::Closure { fn_key, upvals: _ } => {
                let constant = constants[usize::from(*fn_key)];
                assert!(
                    constant.is::<Function>(),
                    "ChunkError: constant referenced by OpCode::Closure is not of type Function",
                );
            },
            _ => {},
        }
    }
}

/// Assert [6], [7] and [8]
///
/// All branches and loops agree on the stack size at each point in execution [7], and all
/// stack accessing OpCodes are accessing stack slots that are initialized at that point in
/// execution [8].
///
/// All threads must diverge before they reach the end of of the Chunk, this generalizes [6].
///
/// Returns the maximum stack required for all the temporaries in this Chunk.
fn check_stack(decoded: &[(OpCode, usize)], params: u8) -> usize {
    struct StackSize {
        size: usize,
        /// What was the previous ip of the thread that first set the stack size
        prev_ip: usize,
    }

    let init_size = usize::from(params) + 1;
    let mut stack_size = BTreeMap::from([(0, StackSize { size: init_size, prev_ip: 0 })]);
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
            let prev_stack_size = stack_size.get(&ip).unwrap().size;
            let next_stack_size = prev_stack_size
                // Assert part [8] stack access is never below 0
                .checked_sub(pops)
                .unwrap_or_else(|| panic!(
                    "ChunkError: stack access below 0. slot={}",
                    (prev_stack_size as isize) - (pops as isize),
                ))
                .checked_add(pushes)
                .unwrap();

            match opcode {
                OpCode::GetLocal { slot } |
                OpCode::SetLocal { slot } => {
                    // Assert part [8] direct stack access is below the current stack height
                    let slot = usize::from(slot);
                    assert!(
                        slot < prev_stack_size,
                        "ChunkError: accessed uninitialized stack. initialized={}, slot={}",
                        prev_stack_size,
                        slot,
                    );
                }
                _ => {}
            }

            let mut next_ip = |next_ip: usize| {
                match stack_size.entry(next_ip) {
                    Entry::Vacant(e) => {
                        e.insert(StackSize {
                            size: next_stack_size,
                            prev_ip: thread_ip,
                        });
                    }
                    Entry::Occupied(e) => {
                        // Assert [7]
                        let entry = e.get();
                        if entry.size != next_stack_size {
                            // Either a Jump* instruction was here before us and disagreed, or we
                            // just jumped back via a Loop instruction. We want to highlight where
                            // the branch was were the threads separated.
                            let branch = if next_ip < thread_ip {
                                // If the next_ip is lower than the previous ip, it's us Looping.
                                thread_ip
                            } else {
                                // Otherwise the other thread jumped forward.
                                entry.prev_ip
                            };
                            let end = usize::max(next_ip, thread_ip);
                            let debuginfo = decoded.iter()
                                .take_while(|(_, ip)| *ip <= end)
                                .map(|(opcode, ip)| {
                                    let entry = stack_size.get(ip).unwrap();
                                    let opcode_dbg = format!("{:?}", opcode);
                                    let mut line = format!("{:>4}  {:<30}  {}", ip, opcode_dbg, entry.size);

                                    if *ip == branch {
                                        line.push_str("  <= branched here");
                                    }
                                    if *ip == next_ip {
                                        line.push_str(&format!("  {}  <= conflict here", next_stack_size));
                                    }

                                    line
                                })
                                .collect::<Vec<_>>()
                                .join("\n");
                            panic!("ChunkError: threads disagree on stack size\n\n{}\n\n", debuginfo);
                        }
                    }
                }

                // Don't insert jumps backward
                if next_ip > thread_ip {
                    threads.insert(next_ip);
                } else {
                    // Sanity check that the jump backwards would've really landed on a previously
                    // visited instruction.
                    assert!(stack_size.get(&next_ip).is_some());
                }
            };

            match opcode {
                OpCode::Jump { offset } => {
                    // Jump forward
                    next_ip(ip + opcode.len() + usize::from(offset));
                }
                OpCode::Loop { offset } => {
                    // Jump backward
                    next_ip(ip + opcode.len() - usize::from(offset));
                }
                OpCode::JumpIfTrue { offset } |
                OpCode::JumpIfFalse { offset } => {
                    // Take branch (jump forward)
                    next_ip(ip + opcode.len() + usize::from(offset));
                    // Don't take branch
                    next_ip(ip + opcode.len());
                }
                _ => {
                    // Ordinary instruction advance
                    next_ip(ip + opcode.len());
                }
            }
        }
    }
    assert!(
        threads.is_empty(),
        "ChunkError: thread run past the end",
    );

    stack_size
        .into_iter()
        .map(|(_, stack_size)| stack_size.size)
        .max()
        .unwrap()
}


/// Assert [9] and [10]
///
/// All upvalue accesses are within the provided bound. And all closure constructions have
/// matching number of upvalues to the referenced Function.
fn check_upvalues(decoded: &[(OpCode, usize)], constants: &[Value], upvalues: u8) {
    for (opcode, _) in decoded {
        match opcode {
            OpCode::GetUpvalue { slot } => {
                assert!(
                    *slot <= upvalues,
                    "ChunkError: upvalue access out of bounds. slot={}, upvalues={}",
                    slot,
                    upvalues,
                );
            },
            OpCode::Closure { fn_key, upvals } => {
                let fun = constants[usize::from(*fn_key)].downcast::<Function>().unwrap();
                assert!(
                    fun.chunk.upvalues == *upvals,
                    "ChunkError: mismatched number of upvalues. function {:?} declares {}, OpCode::Closure uses {}",
                    fun,
                    *upvals,
                    fun.chunk.upvalues,
                );
            }
            _ => {},
        }
    }
}
