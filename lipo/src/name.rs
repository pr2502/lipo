use crate::util::Invariant;
use fxhash::FxHashMap as HashMap;
use std::fmt::{self, Debug, Display};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::{cmp, mem, ptr};


#[derive(Clone, Copy)]
pub struct Name<'interner> {
    /// Name is invariant over the `'interner` lifetime.
    ///
    /// ```rust,compile_fail
    /// # use lipo::builtins::Name;
    /// fn shorter<'shorter>(int: &'shorter i32, name: Name<'shorter>) {}
    ///
    /// fn longer<'longer>(name: Name<'longer>) {
    ///     let int = 1;
    ///     shorter(&int, name);
    /// }
    /// ```
    _interner: PhantomData<Invariant<'interner>>,
    string: &'static &'static str,
}

impl<'interner> Name<'interner> {
    fn as_ptr(self) -> *const &'static str {
        self.string as _
    }

    fn as_str(self) -> &'static str {
        *self.string
    }

    /// Only use for Primitive conversions
    pub(crate) fn to_u64(self) -> u64 {
        self.as_ptr() as u64
    }

    /// Only use for Primitive conversions
    ///
    /// # Safety
    /// The `'interner` lifetime must match the originally destroyed lifetime precisely.
    pub(crate) unsafe fn from_u64(bits: u64) -> Name<'interner> {
        // SAFETY only restoring from a previous call to `Name::to_u64`
        Name {
            _interner: PhantomData,
            string: unsafe { &*(bits as *const &'static str) },
        }
    }
}

impl<'interner> Debug for Name<'interner> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.as_str(), f)
    }
}

impl<'interner> Display for Name<'interner> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self.as_str(), f)
    }
}

impl<'interner> PartialEq for Name<'interner> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.as_ptr(), other.as_ptr())
    }
}

impl<'interner> Eq for Name<'interner> {}

impl<'interner> PartialOrd for Name<'interner> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(Ord::cmp(&self.as_ptr(), &other.as_ptr()))
    }
}

impl<'interner> Ord for Name<'interner> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        Ord::cmp(&self.as_ptr(), &other.as_ptr())
    }
}

impl<'interner> Hash for Name<'interner> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ptr().hash(state);
    }
}


#[derive(Default)]
pub(crate) struct NameInterner {
    dedup: HashMap<&'static str, Name<'static>>,
}

impl Drop for NameInterner {
    fn drop(&mut self) {
        for Name { string: outer_ref, .. } in mem::take(&mut self.dedup).into_values() {
            // SAFETY since NameInterner has been dropped all Names are gone, this is upheld by the
            // caller of `NameInterner::intern`
            unsafe {
                let outer_box = Box::from_raw(outer_ref as *const &'static str as *mut &'static str);
                let inner_ref = *outer_box;
                let inner_box = Box::from_raw(inner_ref as *const str as *mut str);
                drop(inner_box);
            }
        }
    }
}

impl NameInterner {
    /// # Safety
    /// The caller has to ensure that no returned Name outlives the NameInterner.
    pub(crate) unsafe fn intern<'interner>(&mut self, string: &str) -> Name<'interner> {
        // Caller ensures that `&mut self` lives for the `'interner` lifetime, Rust however doesn't
        // allow any kind of `'self` lifetime so we store Names with a `'static` lifetime and
        // manually change it to whatever the caller gives us.
        fn change_lifetime<'interner>(name: Name<'static>) -> Name<'interner> {
            Name {
                _interner: PhantomData,
                string: name.string,
            }
        }

        if let Some(n) = self.dedup.get(string) {
            return change_lifetime(*n);
        }

        let inner = &*Box::leak(Box::<str>::from(string));
        let outer = &*Box::leak(Box::new(inner));
        let name = Name { _interner: PhantomData, string: outer };

        self.dedup.insert(inner, name);

        change_lifetime(name)
    }
}
