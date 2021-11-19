use crate::chunk::Chunk;
use crate::name::Name;
use crate::{Alloc, Object, ObjectRef, Trace, Value};
use std::fmt::{self, Debug};


#[derive(Object, Trace)]
pub struct Function<'alloc> {
    pub chunk: Chunk<'alloc>,
    pub name: Name<'alloc>,
}

impl<'alloc> Function<'alloc> {
    pub fn new(chunk: Chunk<'alloc>, name: Name<'alloc>, alloc: &'alloc Alloc) -> ObjectRef<'alloc, Function<'alloc>> {
        alloc.alloc(Function { chunk, name })
    }
}

impl<'alloc> Debug for Function<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Function({})", self.name)
    }
}


#[derive(Object, Trace)]
pub struct Closure<'alloc> {
    pub function: ObjectRef<'alloc, Function<'alloc>>,
    pub upvalues: Box<[Value<'alloc>]>,
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
        write!(f, "Closure({})", self.function.name)
    }
}
