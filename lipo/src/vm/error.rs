use std::any::{Any, TypeId};
use std::fmt::{self, Debug};

use crate::diagnostic::{Diagnostic, Label, Severity};
use crate::span::FreeSpan;


pub trait Error: Debug + Any {
    fn message(&self) -> String;

    fn labels(&self) -> Vec<Label>;

    fn notes(&self) -> Vec<String>;
}


pub struct VmError(Box<dyn Error>);

impl VmError {
    pub(super) fn new(inner: impl Error) -> VmError {
        VmError(Box::new(inner))
    }

    pub fn is<E: Error>(&self) -> bool {
        // Propagate the type_id() method call down to the `dyn Error`
        (&*self.0).type_id() == TypeId::of::<E>()
    }
}

impl Debug for VmError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Diagnostic for VmError {
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
            $( $field:ident : $field_ty:ty ),* $(,)?
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
    AssertionError {
        span: FreeSpan,
    },
    message: "runtime assertion failed",
    labels: [
        Label::primary(span, "asserted here"),
    ],
}

define_error! {
    TypeError {
        span: FreeSpan,
        msg: &'static str,
    },
    message: "type error",
    labels: [
        Label::primary(span, msg),
    ],
}

define_error! {
    NativeError {
        msg: std::string::String,
    },
    message: "native error",
    labels: [],
    notes: [
        msg,
    ],
}

define_error! {
    WrongArity {
        span: FreeSpan,
        arity: usize,
        args: usize,
    },
    message: "mismatched arity",
    labels: [
        Label::primary(span, format!("called with {} arguments", args)),
    ],
    notes: [
        format!("note: callable arity is {}", arity),
    ],
}

define_error! {
    ValueNotCallable {
        span: FreeSpan,
        dbg: std::string::String,
    },
    message: "value not callable",
    labels: [
        Label::primary(span, "not callable"),
    ],
    notes: [
        format!("note: attempted to call {}", dbg),
    ],
}

define_error! {
    MathError {
        span: FreeSpan,
        msg: &'static str,
    },
    message: "math error",
    labels: [
        Label::primary(span, msg),
    ],
}
