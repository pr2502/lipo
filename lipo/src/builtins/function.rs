use crate::chunk::Chunk;
use crate::name::Name;
use crate::{Alloc, Object, ObjectRef, Trace, Value};
use std::fmt::{self, Debug};


#[derive(Object, Trace, Hash, PartialEq, Eq)]
pub struct Function<'alloc> {
    pub(crate) chunk: Chunk<'alloc>,
    pub(crate) name: Name<'alloc>,
    pub(crate) arity: u32,
}

impl<'alloc> Function<'alloc> {
    pub fn new(chunk: Chunk<'alloc>, arity: u32, name: Name<'alloc>, alloc: &'alloc Alloc) -> ObjectRef<'alloc, Function<'alloc>> {
        alloc.alloc(Function { chunk, name, arity })
    }

    pub fn name(&self) -> Name<'alloc> {
        self.name
    }

    pub fn chunk(&self) -> &Chunk<'alloc> {
        &self.chunk
    }
}

impl<'alloc> Debug for Function<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Function({})", self.name)
    }
}


#[derive(Object, Trace)]
pub struct Closure<'alloc> {
    pub(crate) function: ObjectRef<'alloc, Function<'alloc>>,
    pub(crate) upvalues: Box<[Value<'alloc>]>,
}

impl<'alloc> Closure<'alloc> {
    pub fn new(
        function: ObjectRef<'alloc, Function<'alloc>>,
        upvalues: Box<[Value<'alloc>]>,
        alloc: &'alloc Alloc,
    ) -> ObjectRef<'alloc, Closure<'alloc>> {
        alloc.alloc(Closure { function, upvalues })
    }

    pub fn function(&self) -> ObjectRef<'alloc, Function<'alloc>> {
        self.function
    }
}

impl<'alloc> Debug for Closure<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Closure({})", self.function.name)
    }
}
