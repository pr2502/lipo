use crate::object::{Object, ObjectRef, ObjectRefAny};
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;


#[derive(Clone, Copy)]
pub struct Value<'alloc> {
    _alloc: PhantomData<&'alloc ()>,
    repr: u64,
}


const QUIET_NAN: u64    = 0x7FFC_0000_0000_0000; // bits 50..63 (the whole exponent plus bits 50..52 to avoid some intel behaviour)
const TAG_UNIT: u64     = 0x0000_0000_0000_0001;
const TAG_FALSE: u64    = 0x0000_0000_0000_0002;
const TAG_TRUE: u64     = 0x0000_0000_0000_0003;
const TAG_OBJECT: u64   = 0x8000_0000_0000_0000; // bit 63 (the sign bit)

const UNIT_REPR: u64 = QUIET_NAN | TAG_UNIT;
const TRUE_REPR: u64 = QUIET_NAN | TAG_TRUE;
const FALSE_REPR: u64 = QUIET_NAN | TAG_FALSE;


impl<'alloc> Value<'alloc> {
    pub fn new_unit() -> Value<'static> {
        Value {
            _alloc: PhantomData,
            repr: UNIT_REPR,
        }
    }

    pub fn new_bool(b: bool) -> Value<'static> {
        Value {
            _alloc: PhantomData,
            repr: if b { TRUE_REPR } else { FALSE_REPR },
        }
    }

    pub fn new_float(n: f64) -> Value<'static> {
        Value {
            _alloc: PhantomData,
            repr: n.to_bits(),
        }
    }

    pub fn new_object<O: Object>(o: ObjectRef<'alloc, O>) -> Value<'alloc> {
        Self::new_object_any(o.upcast())
    }

    pub fn new_object_any(o: ObjectRefAny<'alloc>) -> Value<'alloc> {
        let ptr = o.as_ptr() as usize as u64;

        debug_assert_eq!(
            ptr & (QUIET_NAN | TAG_OBJECT),
            0,
            "pointer high bits must be 0, what architecture is this?",
        );

        Value {
            _alloc: PhantomData,
            repr: QUIET_NAN | TAG_OBJECT | ptr,
        }
    }
}

impl<'alloc> Value<'alloc> {
    pub fn is_unit(&self) -> bool {
        self.repr == UNIT_REPR
    }

    pub fn to_bool(self) -> Option<bool> {
        match self.repr {
            TRUE_REPR => Some(true),
            FALSE_REPR => Some(false),
            _ => None,
        }
    }

    pub fn is_falsy(&self) -> bool {
        // PERF matching on `self.to_bool()` optimizes badly
        self.repr == UNIT_REPR || self.repr == FALSE_REPR
    }

    fn is_float(self) -> bool {
        (self.repr & QUIET_NAN) != QUIET_NAN
    }

    pub fn to_float(self) -> Option<f64> {
        // is number if at least some of the QUIET_NAN bits are not set
        if self.is_float() {
            Some(f64::from_bits(self.repr))
        } else {
            None
        }
    }

    fn is_object(self) -> bool {
        (self.repr & (QUIET_NAN | TAG_OBJECT)) == (QUIET_NAN | TAG_OBJECT)
    }

    pub fn to_object(self) -> Option<ObjectRefAny<'alloc>> {
        // is object if all QUIET_NAN and TAG_OBJECT bits are set
        if self.is_object() {
            // mask of the tagging bits and leave only original ptr
            let ptr = self.repr & !(QUIET_NAN | TAG_OBJECT);

            // SAFETY pointer was created from an ObjectRefAny with
            // the same lifetime in [`Value::new_object_any`]
            Some(unsafe { ObjectRefAny::from_ptr(ptr as usize as *mut _) })
        } else {
            None
        }
    }

    pub fn downcast<O: Object>(self) -> Option<ObjectRef<'alloc, O>> {
        self.to_object()
            .and_then(ObjectRefAny::downcast)
    }
}

impl<'alloc> Debug for Value<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_unit() {
            write!(f, "()")
        } else if let Some(b) = self.to_bool() {
            b.fmt(f)
        } else if let Some(n) = self.to_float() {
            n.fmt(f)
        } else if let Some(o) = self.to_object() {
            o.fmt(f)
        } else {
            unreachable!()
        }
    }
}

impl<'alloc> Value<'alloc> {
    pub fn partial_eq(&self, other: &Self) -> Option<bool> {
        if self.is_unit() && other.is_unit() {
            Some(true)
        } else if let (Some(lhs), Some(rhs)) = (self.to_bool(), other.to_bool()) {
            Some(lhs == rhs)
        } else if let (Some(lhs), Some(rhs)) = (self.to_float(), other.to_float()) {
            Some((lhs - rhs).abs() <= f64::EPSILON)
        } else if let (Some(lhs), Some(rhs)) = (self.to_object(), other.to_object()) {
            lhs.partial_eq(&rhs)
        } else {
            None
        }
    }
}

impl<'alloc> PartialEq for Value<'alloc> {
    fn eq(&self, other: &Self) -> bool {
        self.partial_eq(other).unwrap_or(false)
    }
}

impl<'alloc> Eq for Value<'alloc> {}

impl<'alloc> Hash for Value<'alloc> {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        if let Some(b) = self.to_bool() {
            state.write_u8(b as u8)
        } else if let Some(n) = self.to_float() {
            state.write_u64(n.to_bits())
        } else if let Some(o) = self.to_object() {
            o.hash(state)
        }
    }
}
