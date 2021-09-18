use crate::chunk::Chunk;
use crate::object::{Alloc, Object, ObjectRef, Trace};
use std::fmt::{self, Debug};


derive_Object!(Function<'alloc>);
#[derive(Hash)]
pub struct Function<'alloc> {
    pub(crate) chunk: Chunk<'alloc>,
    pub(crate) name: Box<str>,
    pub(crate) arity: u32,
}

unsafe impl<'alloc> Trace for Function<'alloc> {
    fn mark(&self) {
        self.chunk.mark();
    }
}

impl<'alloc> Function<'alloc> {
    pub fn new(chunk: Chunk<'alloc>, arity: u32, name: Box<str>, alloc: &'alloc Alloc) -> ObjectRef<'alloc, Function<'alloc>> {
        Object::init(Function { chunk, name, arity }, alloc)
    }
}

impl<'alloc> Debug for Function<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Function")
            .field("name", &self.name)
            .field("arity", &self.arity)
            .field("code", &self.chunk)
            .finish()
    }
}
