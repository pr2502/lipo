#![feature(assert_matches)]
#![feature(const_type_id)]
#![feature(drain_filter)]
#![feature(new_uninit)]
#![feature(option_result_unwrap_unchecked)]

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

#[cfg(test)]
mod test;
