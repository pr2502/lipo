#![feature(array_windows)]
#![feature(assert_matches)]
#![feature(is_sorted)]
#![feature(let_else)]
#![feature(never_type)]
#![feature(once_cell)]

// spooky scary specialization
#![allow(incomplete_features)]
#![feature(specialization)]

// use RFC 2585 explicitly, don't wait for it to get enabled by default
#![deny(unsafe_op_in_unsafe_fn)]

// we like to give lifetimes meaningful names
#![allow(clippy::needless_lifetimes)]


macro_rules! debug_unreachable {
    ($($args:tt)*) => {{
        #[cfg(debug_assertions)]
        ::std::unreachable!($($args)*);

        // SAFETY Chunk is checked when the VM is constructed, all constant references
        // from OpCode::Closure must be valid and reference a Function object.
        #[cfg(not(debug_assertions))]
        unsafe { ::std::hint::unreachable_unchecked(); }
    }};
}


mod chunk;
mod compiler;
mod diagnostic;
mod fmt;
pub mod lexer;
mod name;
mod opcode;
mod parser;
mod span;
mod value;
mod vm;

pub mod builtins {
    mod float;
    mod function;
    mod native_function;
    mod record;
    mod string;
    mod tuple;

    pub use float::Float;
    pub use function::{Closure, Function};
    pub use native_function::{NativeError, NativeFunction};
    pub use record::Record;
    pub use string::String;
    pub use tuple::Tuple;

    pub use crate::chunk::Chunk;
    pub use crate::name::Name;
}

pub mod error {
    pub use crate::compiler::error::CompilerError;
    pub use crate::diagnostic::{Diagnostic, Label, Report, Severity};
    pub use crate::parser::ParserError;
    pub use crate::vm::error::VmError;
}

// derive macros
#[doc(hidden)] pub use value::object::__derive_object;
#[doc(hidden)] pub use value::object::__derive_trace;
pub use lipo_macro::{Object, Trace};

pub use compiler::compile;
pub use parser::parse;
pub use value::object::gc::{Alloc, Trace};
pub use value::object::{Object, ObjectRef};
pub use value::primitive::Primitive;
pub use value::Value;
pub use vm::error::VmError;
pub use vm::VM;


#[cfg(test)]
mod test;
