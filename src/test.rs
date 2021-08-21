use crate::chunk::Chunk;
use crate::compiler::compile; use crate::default; use crate::opcode::OpCode;
use crate::span::FreeSpan;
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
    chunk.write(OpCode::Constant { index: 0 }, FreeSpan::from(0..0));
    chunk.write(OpCode::Return, FreeSpan::from(0..0));

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

    let mut chunk = Chunk::new("");
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

    let mut chunk = Chunk::new("");

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

    let chunk = compile("(-1 + 2) * 3 - -4;").unwrap();
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
            OpCode::Pop,
        ],
    )
}

macro_rules! run {
    ( $code:literal, $($tt:tt)* ) => {{
        init();

        let chunk = compile($code).unwrap();
        dbg!(&chunk);
        let vm = VM::new(&chunk);
        let res = vm.run();
        dbg!(&res);
        assert!(matches!(res, $($tt)*));
    }};
    ( $code:literal ) => { run!( $code, Ok(Value::Nil) ) };
}

#[test]
fn type_error() {
    run!(
        "1 / true;",
        Err(VmError::RuntimeError { kind: RuntimeErrorKind::TypeError(_), .. }),
    );
}

#[test]
fn weird_expr() {
    run!("assert !(5 - 4 > 3 * 2 == !nil);");
}

#[test]
fn strings_ops() {
    run!(r#"assert "string" == "string";"#);
    run!(r#"assert "foo" + "bar" == "foobar";"#);
}

#[test]
fn print() {
    run!(r#"
        print 1 + 2;
        print 3 + 4;
    "#);
}

#[test]
fn global() {
    run!("
        var a;
        var b = 2;
        print b;
        b = 3;
        print b;
    ");
}

#[test]
fn locals() {
    run!("{
        var a = 1;
        {
            var a = a;
            print a;
        }
    }");
}

#[test]
fn ifs() {
    run!("
        if nil {
            assert false;
        }
        if true {
            assert true;
        }
    ");
}

#[test]
fn ifelse() {
    run!("
        var a;
        if nil {
            assert false;
        } else {
            a = true;
        }
        assert a;
    ");
    run!("
        var b;
        if true {
            b = true;
        } else {
            assert false;
        }
        assert b;
    ");
}

#[test]
fn andor() {
    run!("assert true and true;");
    run!("assert false or true;");
}

#[test]
fn whileloop() {
    run!("{
        var a = 10;
        while a > 0 {
            print a;
            a = a - 1;
        }
    }");
}
