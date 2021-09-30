use crate::object::ObjectRefAny;
use std::marker::PhantomData;


#[derive(Clone, Copy)]
pub struct Value<'alloc> {
    _alloc: PhantomData<&'alloc ()>,
    repr: u64,
}

impl<'alloc> super::sealed::Sealed for Value<'alloc> {}


const QUIET_NAN: u64    = 0x7FFC_0000_0000_0000; // bits 50..63 (the whole exponent plus bits 50..52 to avoid some intel behaviour)
const TAG_UNIT: u64     = 0x0000_0000_0000_0001;
const TAG_FALSE: u64    = 0x0000_0000_0000_0002;
const TAG_TRUE: u64     = 0x0000_0000_0000_0003;
const TAG_OBJECT: u64   = 0x8000_0000_0000_0000; // bit 63 (the sign bit)

const UNIT_REPR: u64 = QUIET_NAN | TAG_UNIT;
const TRUE_REPR: u64 = QUIET_NAN | TAG_TRUE;
const FALSE_REPR: u64 = QUIET_NAN | TAG_FALSE;

impl<'alloc> Value<'alloc> {
    pub const fn unit() -> Value<'static> {
        Value {
            _alloc: PhantomData,
            repr: UNIT_REPR,
        }
    }

    pub(super) fn new_bool(b: bool) -> Value<'static> {
        Value {
            _alloc: PhantomData,
            repr: if b { TRUE_REPR } else { FALSE_REPR },
        }
    }

    pub(super) fn new_float(n: f64) -> Value<'static> {
        Value {
            _alloc: PhantomData,
            repr: n.to_bits(),
        }
    }

    pub(super) fn new_object(o: ObjectRefAny<'alloc>) -> Value<'alloc> {
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
    pub(super) fn is_unit(&self) -> bool {
        self.repr == UNIT_REPR
    }

    pub(super) fn is_bool(self) -> bool {
        matches!(self.repr, TRUE_REPR | FALSE_REPR)
    }

    pub(super) fn to_bool(self) -> Option<bool> {
        match self.repr {
            TRUE_REPR => Some(true),
            FALSE_REPR => Some(false),
            _ => None,
        }
    }

    pub(super) fn is_float(self) -> bool {
        (self.repr & QUIET_NAN) != QUIET_NAN
    }

    pub(super) fn to_float(self) -> Option<f64> {
        // is number if at least some of the QUIET_NAN bits are not set
        if self.is_float() {
            Some(f64::from_bits(self.repr))
        } else {
            None
        }
    }

    pub(super) fn is_object(self) -> bool {
        (self.repr & (QUIET_NAN | TAG_OBJECT)) == (QUIET_NAN | TAG_OBJECT)
    }

    pub(crate) fn to_object(self) -> Option<ObjectRefAny<'alloc>> {
        // is object if all QUIET_NAN and TAG_OBJECT bits are set
        if self.is_object() {
            // mask of the tagging bits and leave only original ptr
            let ptr = self.repr & !(QUIET_NAN | TAG_OBJECT);

            // We should never be able to truncate the data by casting a u64 to usize on either
            // 32bit or 64bit platforms. 64bit has the sizes of the types equal and on 32bit the
            // original pointer we got the Value.repr from a 32bit pointer.
            debug_assert!(
                ptr & (usize::MAX as u64) == ptr,
                "BUG: truncated Value.repr {:?} to {:?}",
                ptr,
                ptr as usize,
            );

            // SAFETY pointer was created from an ObjectRefAny with
            // the same lifetime in [`Value::new_object_any`]
            Some(unsafe { ObjectRefAny::from_ptr(ptr as usize as *mut _) })
        } else {
            None
        }
    }
}
