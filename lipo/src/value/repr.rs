use super::object::gc::ObjectHeader;
use super::object::ObjectRefAny;
use super::primitive::{Primitive, PrimitiveAny};
use std::marker::PhantomData;
use std::num::NonZeroU64;
use std::ptr::NonNull;


#[derive(Clone, Copy)]
pub struct Value<'alloc> {
    _alloc: PhantomData<&'alloc ()>,
    repr: NonZeroU64,
}

impl<'alloc> super::sealed::Sealed for Value<'alloc> {}


const TYPE_MASK: u64 = 0xFFFF_0000_0000_0000;
const PAYLOAD_MASK: u64  = !TYPE_MASK;
const TYPE_OFFSET: u8 = 48;


unsafe fn new_object(ptr: NonNull<ObjectHeader>) -> NonZeroU64 {
    let ptr = ptr.as_ptr() as u64;
    debug_assert!((ptr & PAYLOAD_MASK) == ptr, "platform ptr uses TYPE_MASK bits");
    unsafe { NonZeroU64::new_unchecked(ptr) }
}

const unsafe fn new_primitive(tag: TypeTag, payload: u64) -> NonZeroU64 {
    debug_assert!((payload & PAYLOAD_MASK) == payload, "Primitive payload doesn't fit PAYLOAD_MASK");
    debug_assert!(tag.0 != TypeTag::OBJECT.0, "tried to create primitive with TypeTag::OBJECT");

    let tag = (tag.0 as u64) << TYPE_OFFSET;
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
    pub(super) const OBJECT: TypeTag   = TypeTag(0x0000);
    pub(super) const UNIT: TypeTag     = TypeTag(0x0001);
    pub(super) const BOOL: TypeTag     = TypeTag(0x0002);
    pub(super) const NAME: TypeTag     = TypeTag(0x0003);

    pub(super) const fn as_usize(self) -> usize {
        self.0 as usize
    }
}


pub(crate) enum ValueKind<'alloc> {
    Primitive(PrimitiveAny),
    Object(ObjectRefAny<'alloc>),
}

impl<'alloc> Value<'alloc> {
    pub const fn unit() -> Value<'static> {
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
            repr: unsafe { new_object(NonNull::new_unchecked(o.as_ptr())) },
        }
    }

    pub(crate) fn kind(self) -> ValueKind<'alloc> {
        match type_tag(self.repr) {
            TypeTag::OBJECT => ValueKind::Object(unsafe { ObjectRefAny::from_ptr(self.repr.get() as _) }),
            _ => ValueKind::Primitive(unsafe { PrimitiveAny::from_repr(self.repr) }),
        }
    }
}
