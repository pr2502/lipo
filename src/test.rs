use crate::compiler::compile;
use crate::object::string::String;
use crate::object::Alloc;
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

        let alloc = Alloc::new();

        let src = String::new($code, &alloc);
        println!("src:\n{}\n", src.as_str());

        let ast = parse(src).unwrap();
        println!("ast:\n{:#?}\n", &ast);

        let chunk = compile(ast, &alloc).unwrap();
        println!("{:?}", &chunk);

        let vm = VM::new(&chunk, &alloc);
        let res = vm.run();
        dbg!(&res);
        std::assert_matches::assert_matches!(res, $($tt)*);
    }};
    ( $code:literal ) => { run!( $code, Ok(v) if v.is_unit() ) };
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
    run!("assert not (5 - 4 > 3 * 2 == not ());");
}

#[test]
fn strings_ops() {
    run!(r#"assert "string" == "string";"#);
    run!(r#"assert "string1" /= "string2";"#);
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
        let a;
        let b = 2;
        print b;
        b = 3;
        print b;
    ");
}

#[test]
fn locals() {
    run!("{
        let a = 1;
        {
            let a = a;
            print a;
        }
    }");
}

#[test]
fn ifs() {
    run!("
        if () {
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
        let mut a;
        if () {
            assert false;
        } else {
            a = true;
        }
        assert a;
    ");
    run!("
        let b;
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
        let mut a = 10;
        while a > 0 {
            print a;
            a = a - 1;
        }
    }");
}

#[ignore = "not yet implemented"]
#[test]
fn function() {
    run!("
        fn foo(a, mut b) {
        }
    ")
}
