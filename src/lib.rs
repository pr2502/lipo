#![feature(assert_matches)]
#![feature(const_type_id)]
#![feature(drain_filter)]
#![feature(new_uninit)]
#![feature(option_result_unwrap_unchecked)]

pub mod chunk;
pub mod compiler;
pub mod lexer;
pub mod object;
pub mod opcode;
pub mod span;
pub mod string;
pub mod value;
pub mod vm;

fn default<T: Default>() -> T {
    <T as Default>::default()
}

#[cfg(test)]
mod test;
