use fxhash::FxHashSet as HashSet;

use crate::builtins::Function;
use crate::chunk::check::{check_constants, check_decode, check_upvalues};
use crate::chunk::Chunk;
use crate::compiler::const_eval::ConstEval;

impl<'alloc> Chunk<'alloc> {
    pub fn resolve_constants(&self, const_eval: &ConstEval<'alloc>) {
        let mut visited = HashSet::default();
        resolve_recursive(self, &mut visited, const_eval);

        fn resolve_recursive<'alloc>(
            chunk: &Chunk<'alloc>,
            visited: &mut HashSet<usize>,
            const_eval: &ConstEval<'alloc>,
        ) {
            if !visited.insert(chunk as *const _ as usize) {
                return;
            }

            let constants = chunk
                .constant_promises
                .iter()
                .map(|id| const_eval.resolve(*id))
                .collect::<Box<_>>();
            let decoded = check_decode(&chunk.code);
            check_constants(&decoded, &constants);
            check_upvalues(&decoded, &constants, chunk.upvalues);
            chunk
                .constants
                .set(constants)
                .expect("BUG: resolve_constants called multiple times");

            for constant in chunk.constants().unwrap().iter() {
                if let Some(fun) = constant.downcast::<Function>() {
                    resolve_recursive(&fun.chunk, &mut *visited, const_eval)
                }
            }
        }
    }
}
