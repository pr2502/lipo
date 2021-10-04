use fxhash::FxHashMap as HashMap;
use std::fmt::{self, Debug, Display};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::{mem, ptr};


#[derive(Clone, Copy)]
pub struct Name<'interner> {
    _alloc: PhantomData<&'interner NameInterner>,
    string: &'static &'static str,
}

impl<'interner> Name<'interner> {
    /// Returns a static **uninterned** name
    ///
    /// The returned Name is not guaranteed to be equal to any other Name except itself, this may
    /// be surprising but it still holds up to Eq requirements: Reflexivity, Symmetry and
    /// Transitivity.
    ///
    /// Multiple instances created using equivalent string literals may or may not be equal
    /// depending on compiler optimizations.
    pub fn unique_static(string: &'static &'static str) -> Name<'static> {
        // Because we don't intern the string we can't guarantee its equality properties with other
        // Names, but the caller can safely keep it for 'static because no interner will attempt to
        // deallocate it.
        Name {
            _alloc: PhantomData,
            string,
        }
    }

    /// Only use for Primitive conversions
    pub(crate) fn to_u64(self) -> u64 {
        self.string as *const &'static str as u64
    }

    /// Only use for Primitive conversions
    pub(crate) unsafe fn from_u64(bits: u64) -> Name<'interner> {
        // SAFETY only restoring from a previous call to `Name::to_u64`
        Name {
            _alloc: PhantomData,
            string: unsafe { &*(bits as *const &'static str) },
        }
    }
}

impl<'alloc> Debug for Name<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(*self.string, f)
    }
}

impl<'alloc> Display for Name<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(*self.string, f)
    }
}

impl<'interner> PartialEq for Name<'interner> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.string, other.string)
    }
}

impl<'interner> Eq for Name<'interner> {}

impl<'interner> Hash for Name<'interner> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.to_u64());
    }
}


#[derive(Default)]
pub(crate) struct NameInterner {
    strings: Vec<&'static &'static str>,
    dedup: HashMap<&'static str, Name<'static>>,
}

impl Drop for NameInterner {
    fn drop(&mut self) {
        for outer_ref in mem::take(&mut self.strings) {
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
    /// The returned lifetime is 'static but it refers to 'self. However if we returned '_ the
    /// interner wouldn't be able to intern any other Names.
    ///
    /// # Safety
    /// The caller has to ensure that no returned Name outlives the interner.
    pub(crate) unsafe fn intern(&mut self, string: &str) -> Name<'static> {
        if let Some(n) = self.dedup.get(string) {
            return *n;
        }

        let inner = &*Box::leak(Box::<str>::from(string));
        let outer = &*Box::leak(Box::new(inner));
        let name = Name { _alloc: PhantomData, string: outer };

        self.dedup.insert(inner, name);

        name
    }
}
