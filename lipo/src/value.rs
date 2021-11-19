use crate::name::Name;
use crate::value::object::{Object, ObjectRef};
use std::fmt::{self, Debug};


pub mod primitive;
pub mod object;
mod repr;

pub use repr::{TypeTag, Value};
pub(crate) use repr::ValueKind;

use primitive::Primitive;


mod sealed {
    /// Limits implementations of the [`Downcast`](super::Downcast) trait
    pub trait Sealed {}
}


/// Allows `Value` to be downcast into a concrete Rust type
///
/// See methods [`Value::is`] and [`Value::downcast`].
pub trait Downcast<T>: sealed::Sealed {
    type Output: Sized;

    #[doc(hidden)]
    fn __is(&self) -> bool;

    #[doc(hidden)]
    fn __downcast(self) -> Option<Self::Output>;
}


impl<'alloc> Value<'alloc> {
    /// Returns `true` if the inner type of `Value` is the same as `T`.
    ///
    /// # Example
    /// ```rust
    /// # use lipo::Value;
    /// let unit = Value::unit();
    ///
    /// assert_eq!(unit.is::<()>(), true);
    /// assert_eq!(unit.is::<bool>(), false);
    /// ```
    pub fn is<T>(&self) -> bool
    where
        Self: Downcast<T>
    {
        self.__is()
    }

    /// Returns `Some` if the inner type of `Value` is the same as `T`.
    ///
    /// The `Output` type depends on the `T`
    /// - if `T` [implements `Object`](Object#implementors) then `Output` is `ObjectRef<T>` with
    /// the same lifetime as `Value`
    /// - If `T` [implements `Primitive`](Primitive#implementors) then `Output`
    /// is `T`
    ///
    /// # Example
    /// ```rust
    /// # use lipo::Value;
    /// let boolean = Value::from(true);
    ///
    /// assert_eq!(boolean.downcast::<bool>(), Some(true));
    /// assert_eq!(boolean.downcast::<()>(), None);
    /// ```
    pub fn downcast<T>(self) -> Option<<Self as Downcast<T>>::Output>
    where
        Self: Downcast<T>
    {
        self.__downcast()
    }

    /// Compares to `Value`s for equality
    ///
    /// Returns `None` if either `lhs` and `rhs` types don't match or if the concrete types (in
    /// case of `Object`s) don't support comparing for equality.
    pub fn partial_eq(&self, other: &Self) -> Option<bool> {
        match (self.kind(), other.kind()) {
            (ValueKind::Object(lhs), ValueKind::Object(rhs)) => {
                lhs.partial_eq(&rhs)
            }
            (ValueKind::Primitive(lhs), ValueKind::Primitive(rhs)) => {
                lhs.partial_eq(rhs)
            }
            _ => None,
        }
    }

    pub fn hash_code(&self) -> Option<usize> {
        match self.kind() {
            ValueKind::Object(o) => o.hash_code(),
            ValueKind::Primitive(p) => Some(p.hash_code()),
        }
    }
}


impl<'alloc> Debug for Value<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind() {
            ValueKind::Object(o) => o.fmt(f),
            ValueKind::Primitive(p) => p.debug_fmt(f),
        }
    }
}


impl<'alloc, P: Primitive<'alloc>> From<P> for Value<'alloc> {
    fn from(p: P) -> Self {
        Value::new_primitive(p)
    }
}

impl<'alloc, O: Object> From<ObjectRef<'alloc, O>> for Value<'alloc> {
    fn from(o: ObjectRef<'alloc, O>) -> Self {
        Value::new_object(o.upcast())
    }
}

// Rust can't yet express that two traits are mutually exclusive like `Object` and `Primitive`.
//
// Since `Object` is open to be implemented outside of this crate we have to use a blanket impl for
// it and because `Primitive` is implemented for a known set of types we're going to implement
// `Downcast` for those.
macro_rules! impl_downcast_primitive {
    ( $($P:ty),* $(,)? ) => { $(

        impl<'alloc> Downcast<$P> for Value<'alloc> {
            type Output = $P;

            default fn __is(&self) -> bool {
                match self.kind() {
                    ValueKind::Primitive(p) => p.is::<$P>(),
                    _ => false,
                }
            }

            default fn __downcast(self) -> Option<$P> {
                match self.kind() {
                    ValueKind::Primitive(p) => p.downcast::<$P>(),
                    _ => None,
                }
            }
        }

    )* };
}

impl_downcast_primitive! {
    (),
    bool,
    Name<'alloc>,
    i32,
}

impl<'alloc, O> Downcast<O> for Value<'alloc>
where
    O: Object,
{
    type Output = ObjectRef<'alloc, O>;

    fn __is(&self) -> bool {
        match self.kind() {
            ValueKind::Object(o) => o.is::<O>(),
            _ => false,
        }
    }

    fn __downcast(self) -> Option<ObjectRef<'alloc, O>> {
        match self.kind() {
            ValueKind::Object(o) => o.downcast::<O>(),
            _ => None,
        }
    }
}
