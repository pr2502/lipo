use std::fmt::{self, Debug};
use std::hash::Hash;
use std::marker::PhantomData;
use std::num::NonZeroU64;

use super::{repr, TypeTag};
use crate::builtins::Type;
use crate::name::Name;
use crate::util::Invariant;
use crate::{Alloc, ObjectRef};


mod sealed {
    /// Limits implementations of the [`Primitive`](super::Primitive) trait
    pub trait Sealed {}
}


/// Marker trait for primitive types [`Value`](crate::Value) can represent
///
/// # Safety
/// - it has to be safe and lossless for `Primitive` types to do a roundtrip
///   through the masked bits of `u64` marked by `value::repr::PAYLOAD_MASK`
/// - the associated const `TYPE_TAG` has to be unique for every `Primitive`
///   type and different from `TypeTag::OBJECT`
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
    _alloc: PhantomData<Invariant<'alloc>>,
    repr: NonZeroU64,
}

#[cfg(test)]
#[test]
fn primitive_size() {
    use std::mem::size_of;

    assert_eq!(size_of::<PrimitiveAny>(), size_of::<u64>());
    assert_eq!(size_of::<PrimitiveAny>(), size_of::<Option<PrimitiveAny>>());
}

impl<'alloc> PrimitiveAny<'alloc> {
    pub(super) unsafe fn from_repr(repr: NonZeroU64) -> PrimitiveAny<'alloc> {
        debug_assert!(
            repr::type_tag(repr) != TypeTag::OBJECT,
            "invalid repr for PrimitiveAny"
        );
        PrimitiveAny {
            _alloc: PhantomData, // 'alloc in return type
            repr,
        }
    }

    pub(super) fn is<P: Primitive<'alloc>>(&self) -> bool {
        repr::type_tag(self.repr) == <P as Primitive>::TYPE_TAG
    }

    pub(super) fn downcast<P: Primitive<'alloc>>(self) -> Option<P> {
        if self.is::<P>() {
            Some(<P as Primitive>::from_payload(repr::payload(self.repr)))
        } else {
            None
        }
    }
}


impl sealed::Sealed for () {}
unsafe impl<'alloc> Primitive<'alloc> for () {
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
unsafe impl<'alloc> Primitive<'alloc> for bool {
    const TYPE_TAG: TypeTag = TypeTag::BOOL;

    const NAME: &'static str = "Bool";

    fn to_payload(self) -> u64 {
        self as u64
    }

    fn from_payload(payload: u64) -> Self {
        match payload {
            0 => false,
            1 => true,
            _ => {
                debug_unreachable!("BUG: atempted to create primitive bool from bits {payload:#b}")
            },
        }
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
        unsafe { Self::from_u64(payload) }
    }
}

impl sealed::Sealed for i32 {}
unsafe impl<'alloc> Primitive<'alloc> for i32 {
    const TYPE_TAG: TypeTag = TypeTag::INT32;

    const NAME: &'static str = "Int32";

    fn to_payload(self) -> u64 {
        // noop cast to an unsigned variant
        // and zero extend
        (self as u32) as u64
    }

    fn from_payload(payload: u64) -> Self {
        if payload & (u32::MAX as u64) != payload {
            debug_unreachable!("BUG: atempted to create primitive i32 from bits {payload:#b}");
        }
        // truncate (and we asserted there are no 1s in the top 32 bits)
        // and noop cast to signed
        (payload as u32) as i32
    }
}


// Primitive type virtual dispatch tables. Indexed using `TypeTag` only if it's
// not `OBJECT` (`0x0000`).

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
    tags: Vtable<TypeTag, N>,
    name: Vtable<&'static str, N>,
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

static VTABLES: PrimitiveVtables<5> = {
    // NOTE The order of the arrays here is very important, the offset into the
    // array must match the constants in `TypeTag`
    const _TYPE_TAG_SANITY_CHECK: () = {
        // Here we do a basic sanity check of the type tags being the correct position,
        // it doesn't catch the different fields not being in the same order but
        // it does at least catch the `_tags` field getting out of sync with the
        // `TypeTag` associated constants.
        let mut i = 0usize;
        while i < VTABLES.size() {
            assert!(VTABLES.tags.0[i].as_usize() == i);
            i += 1;
        }
    };
    const VTABLES: PrimitiveVtables<5> = PrimitiveVtables {
        tags: Vtable([
            TypeTag::OBJECT,
            TypeTag::UNIT,
            TypeTag::BOOL,
            TypeTag::NAME,
            TypeTag::INT32,
        ]),

        name: Vtable([
            "[Object]",
            <()>::NAME,
            <bool>::NAME,
            <Name>::NAME,
            <i32>::NAME,
        ]),

        // `drop` and `mark` are not implemented for primitive types
        debug_fmt: Vtable([
            |_, _| unreachable!(),
            debug_fmt::<()>,
            debug_fmt::<bool>,
            debug_fmt::<Name>,
            debug_fmt::<i32>,
        ]),

        eq: Vtable([
            |_, _| unreachable!(),
            eq::<()>,
            eq::<bool>,
            eq::<Name>,
            eq::<i32>,
        ]),

        hash_code: Vtable([
            |_| unreachable!(),
            hash::<()>,
            hash::<bool>,
            hash::<Name>,
            hash::<i32>,
        ]),
    };
    VTABLES
};

impl Debug for TypeTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.as_usize() < VTABLES.size() {
            // SAFETY we checked for overflow
            let name = unsafe { VTABLES.name.get(*self) };
            f.write_str(name)
        } else {
            let idx = self.as_usize();
            f.write_fmt(format_args!("UnknownTypeTag({idx})"))
        }
    }
}

fn debug_fmt<'alloc, P: Primitive<'alloc>>(
    this: PrimitiveAny,
    f: &mut fmt::Formatter,
) -> fmt::Result {
    <P as Debug>::fmt(&P::from_payload(repr::payload(this.repr)), f)
}

fn eq<'alloc, P: Primitive<'alloc>>(this: PrimitiveAny, other: PrimitiveAny) -> bool {
    if repr::type_tag(this.repr) != repr::type_tag(other.repr) {
        debug_unreachable!("BUG: comparing primitive values of different types");
    }
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

    pub(super) fn get_type(self, alloc: &Alloc<'_, 'alloc>) -> ObjectRef<'alloc, Type<'alloc>> {
        let tag = unsafe { VTABLES.tags.get(repr::type_tag(self.repr)) };
        Type::new_primitive(tag, alloc)
    }
}
