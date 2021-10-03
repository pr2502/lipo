use crate::builtins::String;
use crate::compiler::{compile, error::*};
use crate::diagnostic::Report;
use crate::Alloc;
use crate::parser::parse;
use crate::vm::{error::*, VM};


fn init() {
    use std::sync::Once;
    static INIT: Once = Once::new();
    INIT.call_once(|| {
        pretty_env_logger::init();
    });
}

macro_rules! if_ok {
    ( (Ok $($_:tt)*) $($tt:tt)* ) => { $($tt)* };
    ( (Err $($_:tt)*) $($__:tt)* ) => {};
}

macro_rules! parse {
    ( @$alloc:ident, $code:expr, $($res:tt)* ) => {{
        init();

        let src = unindent::unindent(&$code);
        let src = String::new_owned(src.into(), &$alloc);
        let ast = parse(src);

        if_ok! { ($($res)*)
            if let Err(err) = &ast {
                err.report(&src);
            }
        }

        std::assert_matches::assert_matches!(ast, $($res)*);

        ast
    }};

    (
        $(#[$m:meta])*
        $name:ident,
        $code:literal,
        $($res:tt)*
    ) => {
        $(#[$m])*
        #[test]
        fn $name() {
            let alloc = Alloc::new();
            let _ = parse!(@alloc, $code, $($res)*);
        }
    };
}

macro_rules! compile {
    ( @$alloc:ident, $code:expr, $($res:tt)* ) => {{
        let ast = parse!(@$alloc, $code, Ok(_)).unwrap();

        #[allow(unused_variables)]
        let source = ast.source;
        let script = compile(ast, &$alloc);

        // Change the type from `Result<T, Vec<E>>` to `Result<T, &[E]>` so we
        // can reasonably match on it.
        let res = match &script {
            Ok(script) => Ok(*script),
            Err(e) => {
                if_ok! { ($($res)*)
                    for err in e.iter() {
                        err.report(&source);
                    }
                }
                Err(e.as_slice())
            },
        };
        std::assert_matches::assert_matches!(res, $($res)*);

        script
    }};

    (
        $(#[$m:meta])*
        $name:ident,
        $code:expr,
        $($res:tt)*
    ) => {
        $(#[$m])*
        #[test]
        fn $name() {
            let alloc = Alloc::new();
            let _ = compile!(@alloc, $code, $($res)*);
        }
    };
}

macro_rules! run {
    ( @$alloc:ident, $code:literal, $($res:tt)* ) => {{
        let script = compile!(@$alloc, $code, Ok(_)).unwrap();

        let vm = VM::new(script, &$alloc);
        let res = vm.run();

        if_ok! { ($($res)*)
            if let Err(err) = &res {
                let src = unindent::unindent($code);
                err.report(&src);
            }
        }

        std::assert_matches::assert_matches!(res, $($res)*);
    }};

    (
        $(#[$m:meta])*
        $name:ident,
        $code:literal,
        $($res:tt)*
    ) => {
        $(#[$m])*
        #[test]
        fn $name() {
            let alloc = Alloc::new();
            run!(@alloc, $code, $($res)*)
        }
    };
}


run! {
    rt_err_type_error,
    "1 / true;",
    Err(e) if e.is::<TypeError>(),
}

run! {
    weird_expr,
    "assert not (5 - 4 > 3 * 2 == not false);",
    Ok(_),
}

run! {
    string_ops,
    r#"
        assert "string" == "string";
        assert "string1" /= "string2";
        let f = "foo"; let b = "bar";
        assert "{f}{b}" == "foobar";
    "#,
    Ok(_),
}

run! {
    print,
    r#"
        print 1 + 2;
        print 3 + 4;
    "#,
    Ok(_),
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
    Ok(_),
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
    Ok(_),
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
    Ok(_),
}

run! {
    ifs,
    "
        if false {
            assert false;
        };
        if true {
            assert true;
        };
    ",
    Ok(_),
}

run! {
    ifelse,
    "
        let mut a;
        if false {
            assert false;
        } else {
            a = true;
        };
        assert a;
    ",
    Ok(_),
}

run! {
    ifelse2,
    "
        let mut b;
        if true {
            b = true;
        } else {
            assert false;
        };
        assert b;
    ",
    Ok(_),
}

run! {
    and_or,
    "
        assert true and true;
        assert false or true;
    ",
    Ok(_),
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
    Ok(_),
}

compile! {
    // `return` keyword is not allowed in the top level scope
    c_err_return_from_script,
    "
        return;
    ",
    Err([e]) if e.is::<ReturnFromScript>(),
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
    Ok(_),
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
    Ok(_),
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
    Ok(_),
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
    Ok(_),
}

compile! {
    c_err_capture_mutable_binding,
    r#"
        let mut a = "a";
        fn closure() {
            return a;
        }
        a = "b";
        assert closure() == "???";
    "#,
    Err([e]) if e.is::<CaptureMutable>(),
}

compile! {
    c_err_assign_immutable,
    r#"
        let imm = "a";
        imm = "b";
    "#,
    Err([e]) if e.is::<AssignImmutableBinding>(),
}

compile! {
    c_err_shadowing,
    r#"
        let a = "a";
        let a = "b";
    "#,
    Err([e]) if e.is::<Shadowing>(),
}

compile! {
    c_err_undefined_name,
    r#"
        let a = 1;
        print b;
    "#,
    Err([e]) if e.is::<UndefinedName>(),
}

compile! {
    c_err_invalid_number,
    "1e-_;",
    Err([e]) if e.is::<InvalidNumberLiteral>(),
}

compile! {
    c_err_invalid_assign,
    "1 = 2;",
    Err([e]) if e.is::<InvalidAssignmentTarget>(),
}

compile! {
    #[cfg_attr(miri, ignore = "slow")]
    c_err_too_many_params,
    {
        let params = (0..=(u8::MAX as usize))
            .map(|i| format!("  p{},\n", i))
            .collect::<std::string::String>();
        format!("fn f(\n{}) {{}}", params)
    },
    Err([e]) if e.is::<TooManyParameters>(),
}

compile! {
    #[cfg_attr(miri, ignore = "slow")]
    c_err_too_many_args,
    {
        let params = (0..=(u8::MAX as usize))
            .map(|_| "  a,\n")
            .collect::<std::string::String>();
        format!(
            "fn f() {{}}\nlet a;\nf(\n{}\n);",
            params.strip_suffix(",\n").unwrap(),
        )
    },
    Err([e]) if e.is::<TooManyArguments>(),
}

compile! {
    #[ignore = "very slow"]
    c_err_too_many_locals,
    (0..=(u16::MAX as usize))
        .map(|i| format!("let l{};\n", i))
        .collect::<std::string::String>(),
    Err([e]) if e.is::<TooManyLocals>(),
}

compile! {
    c_err_if_scope,
    "
        if true {
            let a = 1;
        };
        a;
    ",
    Err([e]) if e.is::<UndefinedName>(),
}

run! {
    implicit_return_trailing_expr,
    "
        fn f() {
            1
        }
        assert f() == 1;

        fn g() {
            1;
        }
        assert g() == ();
    ",
    Ok(_),
}

run! {
    block_expr,
    "
        let a = {
            let a = 1;
            let b = 2;
            a + b
        };
        assert a == 3;
    ",
    Ok(_),
}

run! {
    if_expr,
    "
        let a = if true { 1 } else { 0 };
        let b = if false { 1 } else { 0 };
        assert a == 1;
        assert b == 0;

        let c = if true { 1 };
        let d = if false { 1 };
        assert c == 1;
        assert d == ();
    ",
    Ok(_),
}

run! {
    string_literals,
    r####"
        print ##"foo"##;
        print "bar";
        print ###"baz"###;
        print ##"a"#b"##;
    "####,
    Ok(_),
}

run! {
    string_interpolation,
    r#"
        let _ = "";
        let a = "a";
        let b = "b";
        assert "{a}{{abcd}}{b}" == r"a{abcd}b";
    "#,
    Ok(_),
}

run! {
    record,
    "
        let block = { (); };
        let record = { block, };
    ",
    Ok(_),
}

run! {
    tuple,
    "
        let parens = (1);
        let tuple = (1,);

        print parens;
        print tuple;
    ",
    Ok(_),
}
