use crate::compiler::{self, compile};
use crate::object::builtins::String;
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

macro_rules! res {
    ( $res:pat ) => { $res };
    ( ) => { Ok(_) };
}

macro_rules! parse {
    ( @$alloc:ident, $code:literal, $res:pat ) => {{
        init();

        let src = unindent::unindent($code);
        let src = String::new_owned(src.into(), &$alloc);
        let ast = parse(src);
        std::assert_matches::assert_matches!(ast, $res);

        ast
    }};

    (
        $(#[$m:meta])*
        $name:ident,
        $code:literal
        $(, $res:pat)? $(,)?
    ) => {
        $(#[$m])*
        #[test]
        fn $name() {
            let alloc = Alloc::new();
            let _ = parse!(@alloc, $code, res!($($res)*));
        }
    };
}

macro_rules! compile {
    ( @$alloc:ident, $code:literal, $res:pat ) => {{
        let ast = parse!(@$alloc, $code, Ok(_)).unwrap();

        let script = compile(ast, &$alloc);
        std::assert_matches::assert_matches!(script, $res);

        script
    }};

    (
        $(#[$m:meta])*
        $name:ident,
        $code:literal
        $(, $res:pat)? $(,)?
    ) => {
        $(#[$m])*
        #[test]
        fn $name() {
            let alloc = Alloc::new();
            let _ = compile!(@alloc, $code, res!($($res)*));
        }
    };
}

macro_rules! run {
    ( @$alloc:ident, $code:literal, $res:pat ) => {{
        let script = compile!(@$alloc, $code, Ok(_)).unwrap();

        let vm = VM::new(script, &$alloc);
        let res = vm.run();
        std::assert_matches::assert_matches!(res, $res);
    }};

    (
        $(#[$m:meta])*
        $name:ident,
        $code:literal
        $(, $res:pat)? $(,)?
    ) => {
        $(#[$m])*
        #[test]
        fn $name() {
            let alloc = Alloc::new();
            run!(@alloc, $code, res!($($res)*))
        }
    };
}


run! {
    type_error,
    "1 / true;",
    Err(VmError::RuntimeError { kind: RuntimeErrorKind::TypeError(_), .. }),
}

run! {
    weird_expr,
    "assert not (5 - 4 > 3 * 2 == not ());",
}

run! {
    string_ops,
    r#"
        assert "string" == "string";
        assert "string1" /= "string2";
        assert "foo" + "bar" == "foobar";
    "#,
}

run! {
    print,
    r#"
        print 1 + 2;
        print 3 + 4;
    "#,
}

run! {
    // Globals are no longer globals but that doesn't matter,
    // we still have let bindings at the top scope.
    global,
    "
        let a;
        let mut b = 2;
        print b;
        b = 3;
        print b;
    ",
}

run! {
    locals,
    "{
        let a = 1;
        {
            let a = a;
            print a;
        }
    }",
}

run! {
    locals_resolving_order,
    "
        let a = 1;
        let b = 2;
        let c = 3;
        assert a == 1;
        assert b == 2;
        assert c == 3;
    ",
}

run! {
    ifs,
    "
        if () {
            assert false;
        }
        if true {
            assert true;
        }
    ",
}

run! {
    ifelse,
    "
        let mut a;
        if () {
            assert false;
        } else {
            a = true;
        }
        assert a;
    ",
}

run! {
    ifelse2,
    "
        let mut b;
        if true {
            b = true;
        } else {
            assert false;
        }
        assert b;
    ",
}

run! {
    and_or,
    "
        assert true and true;
        assert false or true;
    ",
}

run! {
    whileloop,
    "
        let mut a = 10;
        while a > 0 {
            print a;
            a = a - 1;
        }
    ",
}

compile! {
    // `return` keyword is not allowed in the top level scope
    return_from_script,
    "
        return;
    ",
    Err(compiler::Error::ReturnFromScript { .. }),
}

run! {
    function,
    r#"
        fn foo() {
            assert true;
            return true;
        }
        assert foo();
    "#,
}

run! {
    functions_with_params,
    "
        fn add(a, b) {
            return a + b;
        }
        assert add(1, 2) == 3;

        fn first(a, b, c) {
            return a;
        }
        assert first(1,2,3) == 1;
    ",
}

run! {
    fibonacci,
    "
        fn fib(n) {
            if n <= 1 {
                return 1;
            } else {
                return fib(n-1) + fib(n-2);
            }
        }
        assert fib(5) == 8;
    ",
}

run! {
    closure,
    r#"
        fn make_closure(param) {
            fn closure() {
                print param;
                return param;
            }
            return closure;
        }
        let closure = make_closure("a");
        let closure2 = make_closure("b");
        assert closure() == "a";
        assert closure2() == "b";
    "#,
}

compile! {
    // TODO proper error handling
    #[should_panic = "cannot capture a mutable variable"]
    capture_mutable_binding,
    r#"
        let mut a = "a";
        fn closure() {
            return a;
        }
        a = "b";
        assert closure() == "???";
    "#,
}
