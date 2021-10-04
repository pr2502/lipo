#![feature(assert_matches)]
#![feature(const_panic)]
#![feature(never_type)]
#![feature(once_cell)]

// spooky scary specialization
#![allow(incomplete_features)]
#![feature(specialization)]

// use RFC 2585 explicitly, don't wait for it to get enabled by default
#![deny(unsafe_op_in_unsafe_fn)]

// we like to give lifetimes meaningful names
#![allow(clippy::needless_lifetimes)]


pub mod chunk;
pub mod compiler;
pub mod diagnostic;
pub mod fmt;
pub mod lexer;
mod name;
pub mod opcode;
pub mod parser;
pub mod span;
mod value;
pub mod vm;

pub mod builtins {
    mod float;
    mod function;
    mod native_function;
    mod string;
    mod tuple;

    pub use float::Float;
    pub use function::{Closure, Function};
    pub use native_function::{NativeError, NativeFunction};
    pub use string::String;
    pub use tuple::Tuple;

    pub use crate::chunk::Chunk;
    pub use crate::name::Name;
}


// derive macros
#[doc(hidden)] pub use value::object::__derive_object;
#[doc(hidden)] pub use value::object::__derive_trace;
pub use lipo_macro::{Object, Trace};

// reexport types
pub use value::object::gc::{Alloc, Trace};
pub use value::object::{Object, ObjectRef};
pub use value::primitive::Primitive;
pub use value::Value;


#[cfg(test)]
mod test;
