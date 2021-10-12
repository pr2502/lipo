use super::{repr, TypeTag};
use crate::name::Name;
use std::fmt::{self, Debug};
use std::hash::Hash;
use std::marker::PhantomData;
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
pub struct PrimitiveAny<'alloc> {
    _alloc: PhantomData<&'alloc ()>,
    repr: NonZeroU64,
}

impl<'alloc> PrimitiveAny<'alloc> {
    pub(super) unsafe fn from_repr(repr: NonZeroU64) -> PrimitiveAny<'alloc> {
        debug_assert!(repr::type_tag(repr) != TypeTag::OBJECT, "invalid repr for PrimitiveAny");
        PrimitiveAny {
            _alloc: PhantomData, // 'alloc in return type
            repr,
        }
    }

    pub(super) fn is<'p_alloc: 'alloc, P: Primitive<'p_alloc>>(&self) -> bool {
        repr::type_tag(self.repr) == <P as Primitive>::TYPE_TAG
    }

    pub(super) fn downcast<'p_alloc: 'alloc, P: Primitive<'p_alloc>>(self) -> Option<P> {
        /// Make sure that the lifetime bounds on this function are sufficient to not allow
        /// creating a non-'static `Primitive` type from a non-'static `Value`
        /// ```rust,compile_fail
        /// use lipo::{Alloc, Value};
        /// use lipo::builtins::{Name, String};
        ///
        /// let alloc = Alloc::new();
        /// let tst: Value<'_> = Value::from(String::new("hello", &alloc));
        /// let _ = tst.downcast::<Name<'static>>();
        /// ```
        fn _check_lifetime_bounds() {}

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
    _tags: Vtable<TypeTag, N>,
    _name: Vtable<&'static str, N>,
    debug_fmt: Vtable<fn(PrimitiveAny, &mut fmt::Formatter<'_>) -> fmt::Result, N>,
    eq: Vtable<fn(PrimitiveAny, PrimitiveAny) -> bool, N>,
    hash_code: Vtable<fn(PrimitiveAny) -> usize, N>,
}

impl<const N: usize> PrimitiveVtables<N> {
    #[allow(dead_code)] // Possibly false-positive, we're using it in a compile-time-assertion
    const fn size(&self) -> usize {
        N
    }
}

static VTABLES: PrimitiveVtables<4> = {
    // NOTE The order of the arrays here is very important, the offset into the array must match
    // the constants in `TypeTag`
    const _TYPE_TAG_SANITY_CHECK: () = {
        // Here we do a basic sanity check of the type tags being the correct position, it doesn't
        // catch the different fields not being in the same order but it does at least catch the
        // `_tags` field getting out of sync with the `TypeTag` associated constants.
        let mut i = 0usize;
        while i < VTABLES.size() {
            assert!(VTABLES._tags.0[i].as_usize() == i);
            i += 1;
        }
    };
    const VTABLES: PrimitiveVtables<4> = PrimitiveVtables {
        _tags: Vtable([
            TypeTag::OBJECT,
            TypeTag::UNIT,
            TypeTag::BOOL,
            TypeTag::NAME,
        ]),

        _name: Vtable([
            "",
            <()>::NAME,
            <bool>::NAME,
            <Name>::NAME,
        ]),

        // `drop` and `mark` are not implemented for primitive types

        debug_fmt: Vtable([
            |_, _| unreachable!(),
            debug_fmt::<()>,
            debug_fmt::<bool>,
            debug_fmt::<Name>,
        ]),

        eq: Vtable([
            |_, _| unreachable!(),
            eq::<()>,
            eq::<bool>,
            eq::<Name>,
        ]),

        hash_code: Vtable([
            |_| unreachable!(),
            hash::<()>,
            hash::<bool>,
            hash::<Name>,
        ]),
    };
    VTABLES
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


impl<'alloc> PrimitiveAny<'alloc> {
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
