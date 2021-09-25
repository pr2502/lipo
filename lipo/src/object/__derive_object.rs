//! Implementation of vtable functions
//!
//! These are used by the derive macro so they must be public but are hidden from the documentation
//! and should not be used outside of the generated code.

use super::*;

pub use super::{DynObject, ObjectVtable};

unsafe fn downcast_unchecked<'alloc, O: Object>(this: ObjectRefAny<'alloc>) -> ObjectRef<'alloc, O> {
    match this.downcast::<O>() {
        Some(obj) => obj,
        None => {
            #[cfg(debug_assertions)] {
                unreachable!(
                    "BUG: incorrect receiver type. expected type {expect} got {found}",
                    expect = O::__vtable().typename,
                    found = this.vtable().typename,
                )
            }

            // SAFETY functions from the Object's vtable must be only called with that object
            // as a receiver (first parameter of every function).
            #[cfg(not(debug_assertions))]
            unsafe { std::hint::unreachable_unchecked() }
        }
    }

}

/// [`ObjectVtable::drop`]
///
/// # Safety
/// - Caller must provide correct receiver type, that is `this.is::<O>()` must be always true.
/// - Object behind `this` must be unreachable through the VM roots and never again used.
pub unsafe fn drop<O: Object>(this: ObjectRefAny<'static>) {
    // SAFETY caller must use correct receiver type
    let ObjectRef { _alloc, ptr } = unsafe { downcast_unchecked::<O>(this) };

    // SAFETY GC should only be calling this when the object is unreachable
    unsafe { Alloc::dealloc::<O>(ptr) }
}

/// [`ObjectVtable::mark`]
///
/// # Safety
/// - Caller must provide correct receiver type, that is `this.is::<O>()` must be always true.
pub unsafe fn mark<'alloc, O: Object>(this: ObjectRefAny<'alloc>) {
    // SAFETY caller must use correct receiver type
    let this = unsafe { downcast_unchecked::<O>(this) };

    this.mark();
    <O as Trace>::mark(&*this);
}

/// [`ObjectVtable::debug_fmt`]
///
/// # Safety
/// - Caller must provide correct receiver type, that is `this.is::<O>()` must be always true.
pub unsafe fn debug_fmt<'alloc, O: Object>(this: ObjectRefAny<'alloc>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    // SAFETY caller must use correct receiver type
    let this = unsafe { downcast_unchecked::<O>(this) };

    <O as Debug>::fmt(&*this, f)
}

/// [`ObjectVtable::partial_eq`]
///
/// # Safety
/// - Caller must provide correct receiver type, that is `this.is::<O>()` must be always true.
pub unsafe fn partial_eq<'alloc, O: Object>(this: ObjectRefAny<'alloc>, other: ObjectRefAny<'alloc>) -> Option<bool> {
    // SAFETY caller must use correct receiver type
    let this = unsafe { downcast_unchecked::<O>(this) };

    if <O as ObjectPartialEq>::supported() {
        if let Some(other) = other.downcast::<O>() {
            return Some(<O as ObjectPartialEq>::eq(&*this, other));
        }
    }

    // Types which don't support equality
    None
}

/// [`ObjectVtable::hash_code`]
///
/// # Safety
/// - Caller must provide correct receiver type, that is `this.is::<O>()` must be always true.
pub unsafe fn hash_code<'alloc, O: Object>(this: ObjectRefAny<'alloc>) -> usize {
    // SAFETY caller must use correct receiver type
    let this = unsafe { downcast_unchecked::<O>(this) };

    <O as ObjectHashCode>::hash_code(&*this)
}
