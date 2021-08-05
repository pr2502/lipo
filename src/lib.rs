pub mod chunk;
pub mod opcode;
pub mod span;
pub mod value;
pub mod vm;

fn default<T: Default>() -> T {
    <T as Default>::default()
}

#[cfg(test)]
mod test {
    use crate::chunk::Chunk;
    use crate::default;
    use crate::opcode::OpCode;
    use crate::span::FreeSpan;
    use crate::value::Value;
    use crate::vm::VM;

    fn init() {
        use std::sync::Once;
        static INIT: Once = Once::new();
        INIT.call_once(|| {
            pretty_env_logger::init();
        });
    }

    #[test]
    fn debug_output() {
        init();

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

    #[test]
    fn vm() {
        init();

        let mut chunk = Chunk::new("".into());
        chunk.insert_constant(Value { float: 42.0 });
        chunk.write(OpCode::Constant { index: 0 }, default());
        chunk.write(OpCode::Negate, default());
        chunk.write(OpCode::Return, default());

        let vm = VM::new(&chunk);
        assert_eq!(
            vm.run().unwrap(),
            Value { float: -42.0 },
        );
    }

    #[test]
    fn vm2() {
        init();

        let mut chunk = Chunk::new("".into());

        let index = chunk.insert_constant(Value { float: 1.2 });
        chunk.write(OpCode::Constant { index }, default());

        let index = chunk.insert_constant(Value { float: 3.4 });
        chunk.write(OpCode::Constant { index }, default());

        chunk.write(OpCode::Add, default());

        let index = chunk.insert_constant(Value { float: 4.6 });
        chunk.write(OpCode::Constant { index }, default());

        chunk.write(OpCode::Divide, default());
        chunk.write(OpCode::Negate, default());

        chunk.write(OpCode::Return, default());

        let vm = VM::new(&chunk);
        assert_eq!(
            vm.run().unwrap(),
            Value { float: -1.0 },
        );
    }
}
