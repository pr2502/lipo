use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};

use crate::{Alloc, Object, ObjectRef, Trace};


/// Float
///
/// Restricted [`f64`] wrapper Object.
///
/// Float can be:
/// - normal
/// - `+` or `-` infinity
/// - `+0`
///
/// Float cannot be:
/// - `NaN`
/// - subnormal
/// - `-0`
#[derive(Object, Trace)]
pub struct Float {
    inner: f64,
}

impl Float {
    /// `-0` and subnormals will be coerced into `+0`, NaN returns `None`
    pub fn new<'alloc>(float: f64, alloc: &Alloc<'_, 'alloc>) -> Option<ObjectRef<'alloc, Float>> {
        use std::num::FpCategory;

        match float.classify() {
            FpCategory::Normal | FpCategory::Infinite => Some(alloc.alloc(Float { inner: float })),
            FpCategory::Zero | FpCategory::Subnormal => Some(alloc.alloc(Float { inner: 0.0_f64 })),
            FpCategory::Nan => None,
        }
    }

    pub fn inner(&self) -> f64 {
        self.inner
    }
}

impl PartialEq for Float {
    fn eq(&self, other: &Self) -> bool {
        (self.inner - other.inner).abs() <= f64::EPSILON
    }
}

impl Eq for Float {}

impl Hash for Float {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.inner.to_bits());
    }
}

impl Debug for Float {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl PartialOrd for Float {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match PartialOrd::partial_cmp(&self.inner, &other.inner) {
            Some(cmp) => Some(cmp),
            None => {
                #[cfg(debug_assertions)]
                unreachable!("BUG: Float cannot be NaN");

                // SAFETY the `Float::new` function ensures `Float` is always not-NaN
                #[cfg(not(debug_assertions))]
                unsafe {
                    std::hint::unreachable_unchecked();
                }
            },
        }
    }
}

impl Ord for Float {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        PartialOrd::partial_cmp(self, other).unwrap()
    }
}
