use crate::object::{Object, ObjectRef, ObjectRefAny};
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};


#[cfg(not(feature = "nan-boxing"))]
#[path = "value/repr_enum.rs"]
mod repr;

#[cfg(feature = "nan-boxing")]
#[path = "value/repr_nanbox.rs"]
mod repr;


/// Functionality required for Value representations
trait ValueReprInternal {
    fn new_nil() -> Self;
    fn new_bool(b: bool) -> Self;
    fn new_number(f: f64) -> Self;
    fn new_object(o: ObjectRefAny) -> Self;

    fn is_nil(&self) -> bool;
    fn to_bool(self) -> Option<bool>;
    fn to_number(self) -> Option<f64>;
    fn to_object(self) -> Option<ObjectRefAny>;
}


#[derive(Clone, Copy)]
pub struct Value {
    repr: repr::ValueRepr,
}


impl Value {
    pub fn new_nil() -> Self {
        Value {
            repr: repr::ValueRepr::new_nil()
        }
    }

    pub fn new_bool(b: bool) -> Self {
        Value {
            repr: repr::ValueRepr::new_bool(b),
        }
    }

    pub fn new_number(n: f64) -> Self {
        Value {
            repr: repr::ValueRepr::new_number(n),
        }
    }

    pub fn new_object<O: Object>(o: ObjectRef<O>) -> Self {
        Self::new_object_any(o.upcast())
    }

    pub fn new_object_any(o: ObjectRefAny) -> Self {
        Value {
            repr: repr::ValueRepr::new_object(o),
        }
    }
}

impl Value {
    pub fn is_nil(&self) -> bool {
        self.repr.is_nil()
    }

    pub fn to_bool(self) -> Option<bool> {
        self.repr.to_bool()
    }

    pub fn is_falsy(&self) -> bool {
        self.is_nil() || matches!(self.to_bool(), Some(false))
    }

    pub fn to_number(self) -> Option<f64> {
        self.repr.to_number()
    }

    pub fn to_object(self) -> Option<ObjectRefAny> {
        self.repr.to_object()
    }

    pub fn downcast<O: Object>(self) -> Option<ObjectRef<O>> {
        self.to_object()
            .and_then(ObjectRefAny::downcast)
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_nil() {
            write!(f, "nil")
        } else if let Some(b) = self.to_bool() {
            b.fmt(f)
        } else if let Some(n) = self.to_number() {
            n.fmt(f)
        } else if let Some(o) = self.to_object() {
            o.fmt(f)
        } else {
            unreachable!()
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        if self.is_nil() && other.is_nil() {
            true
        } else if let (Some(lhs), Some(rhs)) = (self.to_bool(), other.to_bool()) {
            lhs == rhs
        } else if let (Some(lhs), Some(rhs)) = (self.to_number(), other.to_number()) {
            (lhs - rhs).abs() <= f64::EPSILON
        } else if let (Some(lhs), Some(rhs)) = (self.to_object(), other.to_object()) {
            lhs == rhs
        } else {
            false
        }
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        if let Some(b) = self.to_bool() {
            state.write_u8(b as u8)
        } else if let Some(n) = self.to_number() {
            state.write_u64(n.to_bits())
        } else if let Some(o) = self.to_object() {
            o.hash(state)
        }
    }
}


#[cfg(test)]
#[test]
fn value_size() {
    use std::mem::size_of;

    #[cfg(not(feature = "nan-boxing"))]
    assert_eq!(
        size_of::<Value>(),
        size_of::<(usize, u64)>(),
    );

    #[cfg(feature = "nan-boxing")]
    assert_eq!(
        size_of::<Value>(),
        size_of::<(u64,)>(),
    );
}
