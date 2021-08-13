#![feature(const_type_id)]
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
mod test {
    use crate::chunk::Chunk;
    use crate::compiler::compile;
    use crate::default;
    use crate::opcode::OpCode;
    use crate::span::FreeSpan;
    use crate::string::String as RoxString;
    use crate::value::Value;
    use crate::vm::{RuntimeErrorKind, VmError, VM};

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
        chunk.insert_constant(Value::Number(42.0));
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
        chunk.insert_constant(Value::Number(42.0));
        chunk.write(OpCode::Constant { index: 0 }, default());
        chunk.write(OpCode::Negate, default());
        chunk.write(OpCode::Return, default());

        let vm = VM::new(&chunk);
        assert_eq!(
            vm.run().unwrap(),
            Value::Number(-42.0),
        );
    }

    #[test]
    fn vm2() {
        init();

        let mut chunk = Chunk::new("".into());

        let index = chunk.insert_constant(Value::Number(1.2));
        chunk.write(OpCode::Constant { index }, default());

        let index = chunk.insert_constant(Value::Number(3.4));
        chunk.write(OpCode::Constant { index }, default());

        chunk.write(OpCode::Add, default());

        let index = chunk.insert_constant(Value::Number(4.6));
        chunk.write(OpCode::Constant { index }, default());

        chunk.write(OpCode::Divide, default());
        chunk.write(OpCode::Negate, default());

        chunk.write(OpCode::Return, default());

        let vm = VM::new(&chunk);
        assert_eq!(
            vm.run().unwrap(),
            Value::Number(-1.0),
        );
    }

    #[test]
    fn parser() {
        init();

        let chunk = compile("(-1 + 2) * 3 - -4".to_string()).unwrap();
        let opcodes = chunk.opcodes().collect::<Vec<_>>();

        assert_eq!(chunk.get_constant(0), Some(Value::Number(1.0)));
        assert_eq!(chunk.get_constant(1), Some(Value::Number(2.0)));
        assert_eq!(chunk.get_constant(2), Some(Value::Number(3.0)));
        assert_eq!(chunk.get_constant(3), Some(Value::Number(4.0)));

        assert_eq!(
            opcodes,
            vec![
                OpCode::Constant { index: 0 },
                OpCode::Negate,
                OpCode::Constant { index: 1 },
                OpCode::Add,
                OpCode::Constant { index: 2 },
                OpCode::Multiply,
                OpCode::Constant { index: 3 },
                OpCode::Negate,
                OpCode::Subtract,
                OpCode::Return,
            ],
        )
    }

    #[test]
    fn type_error() {
        init();

        let chunk = compile("1 / true".to_string()).unwrap();
        dbg!(&chunk);
        let vm = VM::new(&chunk);
        assert!(matches!(
            vm.run(),
            Err(VmError::RuntimeError { kind: RuntimeErrorKind::TypeError, .. }),
        ));
    }

    #[test]
    fn weird_expr() {
        init();

        let chunk = compile("!(5 - 4 > 3 * 2 == !nil)".to_string()).unwrap();
        dbg!(&chunk);
        let vm = VM::new(&chunk);
        assert_eq!(vm.run().unwrap(), Value::Bool(true));
    }

    #[test]
    fn strings_ops() {
        let chunk = compile(r#""string" == "string""#.to_string()).unwrap();
        dbg!(&chunk);
        let vm = VM::new(&chunk);
        assert_eq!(vm.run().unwrap(), Value::Bool(true));

        let chunk = compile(r#""foo" + "bar""#.to_string()).unwrap();
        dbg!(&chunk);
        let vm = VM::new(&chunk);
        assert_eq!(vm.run().unwrap(), Value::Object(RoxString::new(Box::from("foobar")).upcast()));
    }
}
