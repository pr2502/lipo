use fxhash::FxHashSet as HashSet;

use crate::builtins::Function;
use crate::chunk::check::{check_constants, check_decode, check_upvalues};
use crate::chunk::Chunk;
use crate::compiler::const_eval::ConstEval;

impl<'alloc> Chunk<'alloc> {
    pub fn resolve_constants(&self, const_eval: &ConstEval<'alloc>) {
        let mut visited = HashSet::default();
        self.resolve_recursive(&mut visited, const_eval);
    }

    fn to_ptr(&self) -> usize {
        self as *const _ as usize
    }

    fn resolve_recursive(&self, visited: &mut HashSet<usize>, const_eval: &ConstEval<'alloc>) {
        if !visited.insert(self.to_ptr()) {
            return;
        }

        let constants = self
            .constant_promises
            .iter()
            .map(|id| const_eval.resolve(*id))
            .collect::<Box<_>>();
        let decoded = check_decode(&self.code);
        check_constants(&decoded, &constants);
        check_upvalues(&decoded, &constants, self.upvalues);
        self.constants
            .set(constants)
            .expect("BUG: resolve_constants called multiple times");

        for constant in self.constants().unwrap().iter() {
            if let Some(fun) = constant.downcast::<Function>() {
                fun.chunk.resolve_recursive(&mut *visited, const_eval)
            }
        }
    }
}
