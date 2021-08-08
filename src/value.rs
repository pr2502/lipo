use std::fmt::{self, Debug};

#[derive(Clone, Copy, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
}

impl Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => "nil".fmt(f),
            Value::Bool(b) => b.fmt(f),
            Value::Number(n) => n.fmt(f),
        }
    }
}

mod bits {
    use super::Value;
    use std::hash::{Hash, Hasher};

    pub struct ValueBits(pub Value);

    impl PartialEq<ValueBits> for ValueBits {
        fn eq(&self, other: &ValueBits) -> bool {
            match (self.0, other.0) {
                (Value::Nil, Value::Nil) => true,
                (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
                (Value::Number(lhs), Value::Number(rhs)) => lhs.to_bits() == rhs.to_bits(),
                _ => false,
            }
        }
    }

    impl Eq for ValueBits {}

    impl Hash for ValueBits {
        fn hash<H>(&self, state: &mut H)
        where
            H: Hasher,
        {
            match self.0 {
                Value::Nil => {}
                Value::Bool(b) => state.write_u8(b as u8),
                Value::Number(n) => state.write_u64(n.to_bits()),
            }
        }
    }
}

pub use bits::ValueBits;
