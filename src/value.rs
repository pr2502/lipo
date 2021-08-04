use std::fmt::{self, Debug};

#[derive(Clone, Copy)]
pub struct Value {
    pub float: f64,
}

impl Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.float.fmt(f)
    }
}

mod bits {
    use super::Value;
    use std::hash::{Hash, Hasher};

    pub struct ValueBits(pub Value);

    impl PartialEq<ValueBits> for ValueBits {
        fn eq(&self, other: &ValueBits) -> bool {
            self.0.float.to_bits() == other.0.float.to_bits()
        }
    }

    impl Eq for ValueBits {}

    impl Hash for ValueBits {
        fn hash<H>(&self, state: &mut H)
        where
            H: Hasher,
        {
            state.write_u64(self.0.float.to_bits())
        }
    }
}

pub use bits::ValueBits;
