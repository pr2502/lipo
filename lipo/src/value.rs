use crate::object::{Object, ObjectRef, ObjectRefAny};
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};


mod repr;

pub use repr::Value;


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
    /// # use lipo::value::Value;
    /// let unit = Value::unit();
    ///
    /// assert_eq!(unit.is::<()>(), true);
    /// assert_eq!(unit.is::<f64>(), false);
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
    /// - If `T` [implements `Primitive`](crate::primitive::Primitive#implementors) then `Output`
    /// is `T`
    ///
    /// # Example
    /// ```rust
    /// # use lipo::value::Value;
    /// let float = Value::from(1.2f64);
    ///
    /// assert_eq!(float.downcast::<f64>(), Some(1.2));
    /// assert_eq!(float.downcast::<bool>(), None);
    /// ```
    pub fn downcast<T>(self) -> Option<<Self as Downcast<T>>::Output>
    where
        Self: Downcast<T>
    {
        self.__downcast()
    }

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


impl<'alloc> Debug for Value<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_unit() {
            write!(f, "()")
        } else if let Some(b) = self.downcast::<bool>() {
            b.fmt(f)
        } else if let Some(n) = self.downcast::<f64>() {
            n.fmt(f)
        } else if let Some(o) = self.to_object() {
            o.fmt(f)
        } else {
            unreachable!()
        }
    }
}


impl<'alloc> From<()> for Value<'alloc> {
    fn from(_: ()) -> Self {
        Value::unit()
    }
}

impl<'alloc> From<bool> for Value<'alloc> {
    fn from(b: bool) -> Self {
        Value::new_bool(b)
    }
}

impl<'alloc> From<f64> for Value<'alloc> {
    fn from(f: f64) -> Self {
        Value::new_float(f)
    }
}

impl<'alloc, O: Object> From<ObjectRef<'alloc, O>> for Value<'alloc> {
    fn from(o: ObjectRef<'alloc, O>) -> Self {
        Value::new_object(o.upcast())
    }
}


impl<'alloc> Downcast<()> for Value<'alloc> {
    type Output = ();

    fn __is(&self) -> bool {
        self.is_unit()
    }
    fn __downcast(self) -> Option<()> {
        self.is_unit().then(|| ())
    }
}

impl<'alloc> Downcast<bool> for Value<'alloc> {
    type Output = bool;

    fn __is(&self) -> bool {
        self.is_bool()
    }
    fn __downcast(self) -> Option<bool> {
        self.to_bool()
    }
}

impl<'alloc> Downcast<f64> for Value<'alloc> {
    type Output = f64;

    fn __is(&self) -> bool {
        self.is_float()
    }
    fn __downcast(self) -> Option<f64> {
        self.to_float()
    }
}

impl<'alloc, O> Downcast<O> for Value<'alloc>
where
    O: Object,
{
    type Output = ObjectRef<'alloc, O>;

    fn __is(&self) -> bool {
        self.to_object()
            .as_ref()
            .map(ObjectRefAny::is::<O>)
            .unwrap_or(false)
    }

    fn __downcast(self) -> Option<ObjectRef<'alloc, O>> {
        self.to_object()
            .and_then(ObjectRefAny::downcast)
    }
}


// FIXME std::PartialEq and std::Hash are implemented because of some internal requirements,
// we want to remove these in the future to prevent users relying on these implementations.
// Not all `Value`s are meant to be comparable and hashable, it depends on the underlying type.

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
            state.write_u8(b.into());
        } else if let Some(n) = self.to_float() {
            state.write_u64(n.to_bits());
        } else if let Some(o) = self.to_object() {
            o.hash(state);
        }
    }
}
