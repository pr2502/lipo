use crate::chunk::Chunk;
use crate::object::{ObjectHeader, ObjectRef};
use std::fmt::{self, Debug};


#[repr(C)]
pub struct Function {
    /// ObjectHeader
    header: ObjectHeader,

    pub(crate) chunk: Chunk,
    pub(crate) name: Box<str>,
    pub(crate) arity: u32,
}

impl_Object! { for Function {
    fn debug_fmt(this, f) {
        Debug::fmt(&*this, f)
    }

    fn eq(_this, _other) {
        false // nop impl
    }

    fn hash(_this, _hasher) -> () {
        // nop impl
    }
}}

impl Function {
    pub fn new(chunk: Chunk, arity: u32, name: &str) -> ObjectRef<Function> {
        let name = name.into();
        init_object!(Function { chunk, name, arity })
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Function({})", self.name)
    }
}
