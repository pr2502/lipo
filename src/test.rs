use crate::compiler::compile;
use crate::parser::parse;
use crate::vm::{RuntimeErrorKind, VmError, VM};


fn init() {
    use std::sync::Once;
    static INIT: Once = Once::new();
    INIT.call_once(|| {
        pretty_env_logger::init();
    });
}

macro_rules! run {
    ( $code:literal, $($tt:tt)* ) => {{
        init();

        let src = $code;

        let ast = parse(src).unwrap();
        let chunk = compile(src, ast).unwrap();
        {
            let chunk = chunk.debug(src);
            println!("{}", src);
            println!("{:?}", chunk);
        }

        let vm = VM::new(&chunk, src);
        let res = vm.run();
        dbg!(&res);
        std::assert_matches::assert_matches!(res, $($tt)*);
    }};
    ( $code:literal ) => { run!( $code, Ok(v) if v.is_nil() ) };
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
