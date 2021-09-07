use crate::object::ObjectRefAny;


#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) struct ValueRepr(u64);

const QUIET_NAN: u64    = 0x7FFC_0000_0000_0000; // bits 50..63 (the whole exponent plus bits 50..52 to avoid some intel behaviour)
const TAG_UNIT: u64     = 0x0000_0000_0000_0001;
const TAG_FALSE: u64    = 0x0000_0000_0000_0002;
const TAG_TRUE: u64     = 0x0000_0000_0000_0003;
const TAG_OBJECT: u64   = 0x8000_0000_0000_0000; // bit 63 (the sign bit)

const UNIT_REPR: ValueRepr  = ValueRepr(QUIET_NAN | TAG_UNIT);
const TRUE_REPR: ValueRepr  = ValueRepr(QUIET_NAN | TAG_TRUE);
const FALSE_REPR: ValueRepr = ValueRepr(QUIET_NAN | TAG_FALSE);

impl super::ValueReprInternal for ValueRepr {
    fn new_unit() -> Self {
        UNIT_REPR
    }

    fn new_bool(b: bool) -> Self {
        if b { TRUE_REPR } else { FALSE_REPR }
    }

    fn new_float(n: f64) -> Self {
        ValueRepr(n.to_bits())
    }

    fn new_object(o: ObjectRefAny) -> Self {
        let ptr = o.ptr as usize as u64;
        debug_assert_eq!(
            ptr & (QUIET_NAN | TAG_OBJECT),
            0,
            "pointer high bits must be 0, what architecture is this?",
        );
        ValueRepr(QUIET_NAN | TAG_OBJECT | ptr)
    }

    fn is_unit(&self) -> bool {
        *self == UNIT_REPR
    }

    fn to_bool(self) -> Option<bool> {
        match self {
            TRUE_REPR => Some(true),
            FALSE_REPR => Some(false),
            _ => None,
        }
    }

    fn to_float(self) -> Option<f64> {
        // is number if at least some of the QUIET_NAN bits are not set
        if (self.0 & QUIET_NAN) != QUIET_NAN {
            Some(f64::from_bits(self.0))
        } else {
            None
        }
    }

    fn to_object(self) -> Option<ObjectRefAny> {
        // is object if all QUIET_NAN and TAG_OBJECT bits are set
        if (self.0 & (QUIET_NAN | TAG_OBJECT)) == (QUIET_NAN | TAG_OBJECT) {
            // mask of the tagging bits and leave only original ptr
            let ptr = self.0 & !(QUIET_NAN | TAG_OBJECT);
            Some(ObjectRefAny {
                ptr: ptr as usize as *mut _,
            })
        } else {
            None
        }
    }
}
