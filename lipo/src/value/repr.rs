use super::object::gc::ObjectHeader;
use super::object::ObjectRefAny;
use super::primitive::{Primitive, PrimitiveAny};
use std::cell::Cell;
use std::marker::PhantomData;
use std::num::NonZeroU64;
use std::ptr::NonNull;


#[derive(Clone, Copy)]
pub struct Value<'alloc> {
    /// Value is invariant over the `'alloc` lifetime.
    ///
    /// ```rust,compile_fail
    /// # use lipo::Value;
    /// fn shorter<'shorter>(int: &'shorter i32, value: Value<'shorter>) {}
    ///
    /// fn longer<'longer>(value: Value<'longer>) {
    ///     let int = 1;
    ///     shorter(&int, value);
    /// }
    /// ```
    ///
    /// This ensures we can't accidentally mix Values from different Allocators / VMs.
    ///
    /// ```rust,compile_fail
    /// # use lipo::Value;
    ///
    /// fn cmp<'alloc1, 'alloc2>(val1: Value<'alloc1>, val2: Value<'alloc2>) -> bool {
    ///     val1 == val2
    /// }
    /// ```
    _alloc: PhantomData<Cell<&'alloc ()>>,
    repr: NonZeroU64,
}

impl<'alloc> super::sealed::Sealed for Value<'alloc> {}

// SAFETY Cell is only included to enforce invariance over `'alloc` lifetime,
// Value is still immutable and Send & Sync are still safe
unsafe impl<'alloc> Send for Value<'alloc> {}
unsafe impl<'alloc> Sync for Value<'alloc> {}

#[cfg(test)]
#[test]
fn value_size() {
    use std::mem::size_of;

    assert_eq!(size_of::<Value>(), size_of::<u64>());
    assert_eq!(size_of::<Value>(), size_of::<Option<Value>>());
}


const TYPE_MASK: u64 = 0xFFFF_0000_0000_0000;
const PAYLOAD_MASK: u64  = !TYPE_MASK;
const TYPE_OFFSET: u8 = 48;


unsafe fn new_object(ptr: NonNull<ObjectHeader>) -> NonZeroU64 {
    let ptr = ptr.as_ptr() as u64;
    debug_assert!((ptr & PAYLOAD_MASK) == ptr, "platform ptr uses TYPE_MASK bits");
    // SAFETY `ptr` is NonNull therefore it's NonZero
    unsafe { NonZeroU64::new_unchecked(ptr) }
}

const unsafe fn new_primitive(tag: TypeTag, payload: u64) -> NonZeroU64 {
    debug_assert!((payload & PAYLOAD_MASK) == payload, "Primitive payload doesn't fit PAYLOAD_MASK");
    debug_assert!(tag.0 != TypeTag::OBJECT.0, "tried to create primitive with TypeTag::OBJECT");

    let tag = (tag.0 as u64) << TYPE_OFFSET;
    // SAFETY TypeTag 0 is used for Object, Primitives use TypeTag >= 1, we can safely assume
    // `tag | anything` is NonZero
    unsafe { NonZeroU64::new_unchecked(tag | payload) }
}

pub(super) const fn type_tag(repr: NonZeroU64) -> TypeTag {
    TypeTag((repr.get() >> TYPE_OFFSET) as u16)
}

pub(super) const fn payload(repr: NonZeroU64) -> u64 {
    repr.get() & PAYLOAD_MASK
}


#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct TypeTag(u16);

impl TypeTag {
    pub(super) const OBJECT: TypeTag    = TypeTag(0x0000);
    pub(super) const UNIT: TypeTag      = TypeTag(0x0001);
    pub(super) const BOOL: TypeTag      = TypeTag(0x0002);
    pub(super) const NAME: TypeTag      = TypeTag(0x0003);
    pub(super) const INT32: TypeTag     = TypeTag(0x0004);

    pub(super) const fn as_usize(self) -> usize {
        self.0 as usize
    }
}


pub(crate) enum ValueKind<'alloc> {
    Primitive(PrimitiveAny<'alloc>),
    Object(ObjectRefAny<'alloc>),
}

impl<'alloc> Value<'alloc> {
    pub const fn unit() -> Value<'alloc> {
        Value {
            _alloc: PhantomData,
            repr: unsafe { new_primitive(TypeTag::UNIT, 0) },
        }
    }

    pub(super) fn new_primitive<P: Primitive<'alloc>>(p: P) -> Value<'alloc> {
        Value {
            _alloc: PhantomData,
            repr: unsafe { new_primitive(P::TYPE_TAG, p.to_payload()) },
        }
    }

    pub(super) fn new_object(o: ObjectRefAny<'alloc>) -> Value<'alloc> {
        Value {
            _alloc: PhantomData,
            repr: unsafe { new_object(o.as_ptr()) },
        }
    }

    pub(crate) fn kind(self) -> ValueKind<'alloc> {
        match type_tag(self.repr) {
            TypeTag::OBJECT => ValueKind::Object(unsafe { ObjectRefAny::from_ptr(NonNull::new_unchecked(self.repr.get() as _)) }),
            _ => ValueKind::Primitive(unsafe { PrimitiveAny::from_repr(self.repr) }),
        }
    }
}
