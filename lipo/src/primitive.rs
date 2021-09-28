mod sealed {
    /// Limits implementations of the [`Primitive`](super::Primitive) trait
    pub trait Sealed {}
}


/// Marker trait for primitive types [`crate::value::Value`] can represent
pub trait Primitive: sealed::Sealed {}


impl sealed::Sealed for () {}
impl Primitive for () {}

impl sealed::Sealed for bool {}
impl Primitive for bool {}

impl sealed::Sealed for f64 {}
impl Primitive for f64 {}
