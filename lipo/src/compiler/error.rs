use crate::diagnostic::{Diagnostic, Label, Severity};
use crate::span::FreeSpan;
use std::any::{Any, TypeId};
use std::fmt::{self, Debug};
use std::num::ParseFloatError;


pub trait Error: Debug + Any {
    fn message(&self) -> String;

    fn labels(&self) -> Vec<Label>;

    fn notes(&self) -> Vec<String>;
}


pub struct CompilerError(Box<dyn Error>);

impl CompilerError {
    pub(super) fn new(inner: impl Error) -> CompilerError {
        CompilerError(Box::new(inner))
    }

    pub fn is<E: Error>(&self) -> bool {
        // Propagate the type_id() method call down to the `dyn Error`
        (&*self.0).type_id() == TypeId::of::<E>()
    }
}

impl Debug for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Diagnostic for CompilerError {
    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn message(&self) -> String {
        self.0.message()
    }

    fn labels(&self) -> Vec<Label> {
        self.0.labels()
    }

    fn notes(&self) -> Vec<String> {
        self.0.notes()
    }
}


macro_rules! define_error {
    (
        $name:ident {
            $( $field:ident : $field_ty:ident ),* $(,)?
        },
        message: $message:expr,
        labels: [ $( $label:expr ),* $(,)? ],
        $( notes: [ $( $note:expr ),* $(,)? ], )?
    ) => {
        #[derive(Debug)]
        pub struct $name {
            $( pub $field : $field_ty ),*
        }

        impl Error for $name {
            fn message(&self) -> String {
                #[allow(unused_variables)]
                let $name { $($field),* } = self;
                $message.to_string()
            }

            fn labels(&self) -> Vec<Label> {
                #[allow(unused_variables)]
                let $name { $($field),* } = self;
                vec![ $($label),* ]
            }

            fn notes(&self) -> Vec<String> {
                #[allow(unused_variables)]
                let $name { $($field),* } = self;
                vec![ $($($note.to_string()),*)? ]
            }
        }
    };
}


define_error! {
    AssignImmutableBinding {
        bind_span: FreeSpan,
        assign_span: FreeSpan,
    },
    message: "cannot assign an immutable variable",
    labels: [
        Label::primary(assign_span, "assigned here"),
        Label::secondary(bind_span, "binding created here"),
    ],
    notes: [
        "help: to allow assigning a variable mark the binding as mutable, for example: `let mut foo;`",
    ],
}

define_error! {
    CaptureMutable {
        bind_span: FreeSpan,
        closure_def_span: FreeSpan,
        capture_span: FreeSpan,
    },
    message: "cannot capture a mutable variable",
    labels: [
        Label::primary(capture_span, "captured a mutable variable"),
        Label::secondary(closure_def_span, "captured by this closure"),
        Label::secondary(bind_span, "binding created here"),
    ],
}

define_error! {
    InvalidAssignmentTarget {
        span: FreeSpan,
    },
    message: "invalid assignment target",
    labels: [
        Label::primary(span, "tried to assign this expression"),
    ],
    notes: [
        "help: you can only assing variable names, for example: `foo = 1;`",
    ],
}

define_error! {
    InvalidNumberLiteral {
        cause: ParseFloatError,
        span: FreeSpan,
    },
    message: "failed to parse a number literal",
    labels: [
        Label::primary(span, cause)
    ],
}

define_error! {
    ReturnFromScript {
        return_span: FreeSpan,
    },
    message: "cannot use `return` outside of a function",
    labels: [
        Label::primary(return_span, "return statement here"),
    ],
}

define_error! {
    Shadowing {
        shadowing_span: FreeSpan,
        shadowed_span: FreeSpan,
    },
    message: "shadowing a variable in the same scope",
    labels: [
        Label::primary(shadowing_span, "shadowing variable"),
        Label::secondary(shadowed_span, "shadowed binding"),
    ],
    notes: [
        "help: you can shadow variables using a `{{` block `}}`",
    ],
}

define_error! {
    TooManyArguments {
        extra_arg_span: FreeSpan,
        call_span: FreeSpan,
        limit: usize,
    },
    message: "call expression exceeded the maximum number of arguments",
    labels: [
        Label::primary(extra_arg_span, "argument over the limit"),
        Label::secondary(call_span, "in function call"),
    ],
    notes: [
        format!("note: the maximum number of call arguments is {}", limit),
    ],
}

define_error! {
    TooManyLocals {
        span: FreeSpan,
        limit: usize,
    },
    message: "too many local variables",
    labels: [
        Label::primary(span, "binding over the limit"),
    ],
    notes: [
        format!("note: the maximum number of local variables is {}", limit),
    ],
}

define_error! {
    TooManyParameters {
        extra_param_span: FreeSpan,
        fn_params_span: FreeSpan,
        limit: usize,
    },
    message: "function exceeded the maximum number of parameters",
    labels: [
        Label::primary(extra_param_span, "parameter over the limit"),
        Label::secondary(fn_params_span, "in function definition"),
    ],
    notes: [
        format!("note: the maximum number of function parameters is {}", limit),
    ],
}

define_error! {
    UndefinedName {
        name_span: FreeSpan,
    },
    message: "undefined name",
    labels: [
        Label::primary(name_span, "name not found in the current scope"),
    ],
}