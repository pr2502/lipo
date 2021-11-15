macro_rules! debug_unreachable {
    ($($args:tt)*) => {{
        #[cfg(debug_assertions)]
        ::std::unreachable!($($args)*);

        // SAFETY Chunk is checked when the VM is constructed, all constant references
        // from OpCode::Closure must be valid and reference a Function object.
        #[cfg(not(debug_assertions))]
        unsafe { ::std::hint::unreachable_unchecked(); }
    }};
}


use std::cell::Cell;
use std::marker::PhantomData;

/// Marker type invariant over its `'lifetime` parameter
#[derive(Clone, Copy)]
pub struct Invariant<'lifetime>(PhantomData<Cell<&'lifetime ()>>);

// SAFETY Invariant is a marker type and should only affect the lifetime variance not Send&Sync but
// Cell which is used to enforce the invariance does, so we undo that
unsafe impl<'lifetime> Send for Invariant<'lifetime> {}
unsafe impl<'lifetime> Sync for Invariant<'lifetime> {}
