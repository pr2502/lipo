#![feature(assert_matches)]
#![feature(const_type_id)]
#![feature(drain_filter)]
#![feature(new_uninit)]
#![feature(option_result_unwrap_unchecked)]

// spooky scary specialization
#![allow(incomplete_features)]
#![feature(specialization)]

// use RFC 2585 explicitly, don't wait for it to get enabled by default
#![deny(unsafe_op_in_unsafe_fn)]

// we like to give lifetimes meaningful names
#![allow(clippy::needless_lifetimes)]


pub mod chunk;
pub mod compiler;
pub mod fmt;
pub mod lexer;
pub mod object;
pub mod opcode;
pub mod parser;
pub mod span;
pub mod value;
pub mod vm;

pub use object::__derive_object;
pub use object::__derive_trace;

#[cfg(test)]
mod test;
