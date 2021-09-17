use crate::chunk::Chunk;
use crate::object::{Alloc, Object, ObjectRef, Trace};
use std::fmt::{self, Debug};


derive_Object!(['alloc] Function<'alloc>);
pub struct Function<'alloc> {
    pub(crate) chunk: Chunk<'alloc>,
    pub(crate) name: Box<str>,
    pub(crate) _arity: u32,
}

unsafe impl<'alloc> Trace for Function<'alloc> {
    fn mark(&self) {
        self.chunk.mark();
    }
}

impl<'alloc> Function<'alloc> {
    pub fn new(chunk: Chunk<'alloc>, _arity: u32, name: &str, alloc: &'alloc Alloc) -> ObjectRef<'alloc, Function<'alloc>> {
        let name = name.into();
        Object::init(Function { chunk, name, _arity }, alloc)
    }
}

impl<'alloc> Debug for Function<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Function({})", self.name)
    }
}
