use super::{repr, TypeTag};
use crate::name::Name;
use std::fmt::{self, Debug};
use std::hash::Hash;
use std::num::NonZeroU64;


mod sealed {
    /// Limits implementations of the [`Primitive`](super::Primitive) trait
    pub trait Sealed {}
}


/// Marker trait for primitive types [`Value`](crate::Value) can represent
pub unsafe trait Primitive<'alloc>: sealed::Sealed + Debug + Hash + PartialEq + Eq {
    #[doc(hidden)]
    const TYPE_TAG: TypeTag;

    #[doc(hidden)]
    const NAME: &'static str;

    #[doc(hidden)]
    fn to_payload(self) -> u64;
    #[doc(hidden)]
    fn from_payload(payload: u64) -> Self;
}


#[derive(Clone, Copy)]
pub struct PrimitiveAny {
    repr: NonZeroU64,
}

impl PrimitiveAny {
    pub(super) unsafe fn from_repr(repr: NonZeroU64) -> PrimitiveAny {
        debug_assert!(repr::type_tag(repr) != TypeTag::OBJECT, "invalid repr for PrimitiveAny");
        PrimitiveAny { repr }
    }

    pub(super) fn is<'alloc, P: Primitive<'alloc>>(&self) -> bool {
        repr::type_tag(self.repr) == <P as Primitive>::TYPE_TAG
    }

    pub(super) fn downcast<'alloc, P: Primitive<'alloc>>(self) -> Option<P> {
        if self.is::<P>() {
            Some(<P as Primitive>::from_payload(repr::payload(self.repr)))
        } else {
            None
        }
    }
}


impl sealed::Sealed for () {}
unsafe impl Primitive<'static> for () {
    const TYPE_TAG: TypeTag = TypeTag::UNIT;

    const NAME: &'static str = "Unit";

    fn to_payload(self) -> u64 {
        0
    }

    #[allow(clippy::unused_unit)] // This is more descriptive
    fn from_payload(_: u64) -> () {
        ()
    }
}

impl sealed::Sealed for bool {}
unsafe impl Primitive<'static> for bool {
    const TYPE_TAG: TypeTag = TypeTag::BOOL;

    const NAME: &'static str = "Bool";

    fn to_payload(self) -> u64 {
        self as u64
    }

    fn from_payload(payload: u64) -> Self {
        payload != 0
    }
}

impl<'alloc> sealed::Sealed for Name<'alloc> {}
unsafe impl<'alloc> Primitive<'alloc> for Name<'alloc> {
    const TYPE_TAG: TypeTag = TypeTag::NAME;

    const NAME: &'static str = "Name";

    fn to_payload(self) -> u64 {
        self.to_u64()
    }

    fn from_payload(payload: u64) -> Self {
        unsafe {
            Self::from_u64(payload)
        }
    }
}


// Primitive type virtual dispatch tables. Indexed using `TypeTag` only if it's not `OBJECT` (`0x0000`).

struct Vtable<F, const N: usize>([F; N]);

impl<F: Copy, const N: usize> Vtable<F, N> {
    unsafe fn get(&self, tag: TypeTag) -> F {
        let offset = tag.as_usize();
        debug_assert!(offset > 0 && offset < N, "invalid TypeTag");

        // SAFETY type tag must be valid
        unsafe { *self.0.get_unchecked(offset) }
    }
}

struct PrimitiveVtables<const N: usize> {
    _name: Vtable<&'static str, N>,
    debug_fmt: Vtable<fn(PrimitiveAny, &mut fmt::Formatter<'_>) -> fmt::Result, N>,
    eq: Vtable<fn(PrimitiveAny, PrimitiveAny) -> bool, N>,
    hash_code: Vtable<fn(PrimitiveAny) -> usize, N>,
}

// NOTE The order of the arrays here is very important, the offset into the array must match the
// constants in `TypeTag`
static VTABLES: PrimitiveVtables<3> = PrimitiveVtables {
    _name: Vtable([
        "",
        <()>::NAME,
        <bool>::NAME,
    ]),

    // `drop` and `mark` are not implemented for primitive types

    debug_fmt: Vtable([
        |_, _| unreachable!(),
        debug_fmt::<()>,
        debug_fmt::<bool>,
    ]),

    eq: Vtable([
        |_, _| unreachable!(),
        eq::<()>,
        eq::<bool>,
    ]),

    hash_code: Vtable([
        |_| unreachable!(),
        hash::<()>,
        hash::<bool>,
    ]),
};

fn debug_fmt<'alloc, P: Primitive<'alloc>>(this: PrimitiveAny, f: &mut fmt::Formatter) -> fmt::Result {
    <P as Debug>::fmt(&P::from_payload(repr::payload(this.repr)), f)
}

fn eq<'alloc, P: Primitive<'alloc>>(this: PrimitiveAny, other: PrimitiveAny) -> bool {
    debug_assert!(repr::type_tag(this.repr) == repr::type_tag(other.repr));
    PartialEq::eq(
        &P::from_payload(repr::payload(this.repr)),
        &P::from_payload(repr::payload(other.repr)),
    )
}

fn hash<'alloc, P: Primitive<'alloc>>(this: PrimitiveAny) -> usize {
    fxhash::hash(&P::from_payload(repr::payload(this.repr)))
}


impl PrimitiveAny {
    pub(super) fn debug_fmt(self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let debug_fmt = unsafe { VTABLES.debug_fmt.get(repr::type_tag(self.repr)) };
        debug_fmt(self, f)
    }

    /// Returns `None` when types don't match
    pub(super) fn partial_eq(self, other: Self) -> Option<bool> {
        if repr::type_tag(self.repr) == repr::type_tag(other.repr) {
            let eq = unsafe { VTABLES.eq.get(repr::type_tag(self.repr)) };
            Some(eq(self, other))
        } else {
            None
        }
    }

    pub(super) fn hash_code(self) -> usize {
        let hash_code = unsafe { VTABLES.hash_code.get(repr::type_tag(self.repr)) };
        hash_code(self)
    }
}
