//! Helpers for deriving the Trace trait
//!
//! These are used by the derive macro so they must be public but are hidden from the documentation
//! and should not be used outside of the generated code.

/// Helper trait for deriving Trace
pub trait MaybeTrace {
    fn maybe_mark(&self);
}

impl<T> MaybeTrace for T
where
    T: 'static,
{
    default fn maybe_mark(&self) {
        // nop, not a `Trace`able type
    }
}

impl<T> MaybeTrace for std::marker::PhantomData<T> {
    fn maybe_mark(&self) {
        // nop
    }
}

impl<T> MaybeTrace for T
where
    T: super::Trace,
{
    fn maybe_mark(&self) {
        self.mark();
    }
}

impl<T> MaybeTrace for Box<[T]>
where
    T: MaybeTrace,
{
    fn maybe_mark(&self) {
        self.iter().for_each(MaybeTrace::maybe_mark);
    }
}
