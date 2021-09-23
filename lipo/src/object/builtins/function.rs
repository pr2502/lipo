use crate::chunk::Chunk;
use crate::object::{Alloc, Object, ObjectRef, Trace};
use crate::value::Value;
use std::fmt::{self, Debug};


#[derive(Object, Hash, PartialEq, Eq)]
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
        alloc.alloc(Function { chunk, name, arity })
    }
}

impl<'alloc> Debug for Function<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Function({})", &self.name)
    }
}


#[derive(Object)]
pub struct Closure<'alloc> {
    pub(crate) function: ObjectRef<'alloc, Function<'alloc>>,
    pub(crate) upvalues: Box<[Value<'alloc>]>,
}

unsafe impl<'alloc> Trace for Closure<'alloc> {
    fn mark(&self) {
        self.function.mark();
        self.upvalues.iter().for_each(Trace::mark);
    }
}

impl<'alloc> Closure<'alloc> {
    pub fn new(
        function: ObjectRef<'alloc, Function<'alloc>>,
        upvalues: Box<[Value<'alloc>]>,
        alloc: &'alloc Alloc,
    ) -> ObjectRef<'alloc, Closure<'alloc>> {
        alloc.alloc(Closure { function, upvalues })
    }
}

impl<'alloc> Debug for Closure<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Closure({})", &self.function.name)
    }
}
