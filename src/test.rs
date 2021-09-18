use crate::compiler::{self, compile};
use crate::object::builtins::{NativeFunction, String};
use crate::object::Alloc;
use crate::parser::parse;
use crate::value::Value;
use crate::vm::{RuntimeErrorKind, VmError, VM};


fn init() {
    use std::sync::Once;
    static INIT: Once = Once::new();
    INIT.call_once(|| {
        pretty_env_logger::init();
    });
}

macro_rules! parse {
    ( @$alloc:ident, $code:literal, $($tt:tt)* ) => {{
        init();

        let src = String::new($code, &$alloc);
        println!("src:\n{}\n", src.as_str());

        let ast = parse(src);
        println!("ast:\n{:#?}\n", &ast);
        std::assert_matches::assert_matches!(ast, $($tt)*);

        ast
    }};

    ( $code:literal, $($tt:tt)* ) => {{
        let alloc = Alloc::new();
        let _ = parse!(@alloc, $code, $($tt)*);
    }};
    ( $code:literal ) => {
        parse!($code, Ok(_))
    };
}

macro_rules! compile {
    ( @$alloc:ident, $code:literal, $($tt:tt)* ) => {{
        let ast = parse!(@$alloc, $code, Ok(_)).unwrap();

        let script = compile(ast, &$alloc);
        println!("script:\n{:#?}\n", &script);
        std::assert_matches::assert_matches!(script, $($tt)*);

        script
    }};

    ( $code:literal, $($tt:tt)* ) => {{
        let alloc = Alloc::new();
        let _ = compile!(@alloc, $code, $($tt)*);
    }};
    ( $code:literal ) => {
        compile!($code, Ok(_))
    };
}

macro_rules! run {
    ( @$alloc:ident, $code:literal, $($tt:tt)* ) => {{
        let script = compile!(@$alloc, $code, Ok(_)).unwrap();

        let mut vm = VM::new(script, &$alloc);
        // TODO move builtin functions elsewhere
        vm.add_global("dbg", Value::new_object(
            NativeFunction::new("dbg", |args| {
                args.iter().enumerate()
                    .for_each(|(i, arg)| println!("dbg#{}  {:?}", i, arg));
                Ok(Value::new_unit())
            }, &$alloc),
        ));
        let res = vm.run();
        println!("result\n{:#?}\n", &res);
        std::assert_matches::assert_matches!(res, $($tt)*);
    }};

    ( $code:literal, $($tt:tt)* ) => {{
        let alloc = Alloc::new();
        run!(@alloc, $code, $($tt)*)
    }};
    ( $code:literal ) => {
        run!($code, Ok(_))
    };
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

#[test]
fn return_from_script() {
    compile!(
        "return;",
        Err(compiler::Error::ReturnFromScript { .. }),
    );
}

#[test]
fn function() {
    run!(r#"
        dbg((), 1, true, "foo");
    "#);
    run!(r#"
        fn foo() {
            assert true;
            return true;
        }
        assert foo();
    "#);
}
