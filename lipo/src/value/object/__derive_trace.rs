//! Helpers for deriving the Trace trait
//!
//! These are used by the derive macro so they must be public but are hidden
//! from the documentation and should not be used outside of the generated code.

use std::marker::PhantomData;

pub use crate::value::object::gc::Trace;

/// Helper trait for deriving Trace
///
/// `specialization` is a bit broken and directly implementing `Trace` with
/// these blanket impls creates some false positive impl collisions.
pub unsafe trait ProxyTrace {
    fn proxy_mark(&self);
}

// SAFETY All Values carry the 'alloc lifetime, so a T: 'static cannot contain
// any GCd values.
unsafe impl<T: 'static> ProxyTrace for T {
    default fn proxy_mark(&self) {
        // nop
    }
}

// SAFETY PhantomData doesn't contain any actual Objects that'd need to be
// traced.
unsafe impl<T> ProxyTrace for PhantomData<T> {
    fn proxy_mark(&self) {
        // nop
    }
}

// SAFETY proxies Trace for objects that themselves have to uphold the trace
// invariants.
unsafe impl<T: Trace> ProxyTrace for T {
    fn proxy_mark(&self) {
        self.mark();
    }
}

// SAFETY marks all values within the array
unsafe impl<T: ProxyTrace> ProxyTrace for Box<[T]> {
    fn proxy_mark(&self) {
        self.iter().for_each(ProxyTrace::proxy_mark);
    }
}
