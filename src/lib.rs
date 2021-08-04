pub mod chunk;
pub mod opcode;
pub mod span;
pub mod value;

#[cfg(test)]
mod test {
    use super::chunk::Chunk;
    use super::opcode::OpCode;
    use super::span::FreeSpan;
    use super::value::Value;

    #[test]
    fn debug_output() {
        let mut chunk = Chunk::new("".into());
        chunk.insert_constant(Value { float: 42.0 });
        chunk.write(
            OpCode::Constant { index: 0 },
            FreeSpan { offset: 0, len: 0 },
        );
        chunk.write(OpCode::Return, FreeSpan { offset: 0, len: 0 });

        assert_eq!(
            format!("{:?}", chunk),
            "\
Chunk {
   1    Constant { index: 0 }\t; 42.0
   |    Return
}
"
        );
    }
}
