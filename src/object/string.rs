use crate::object::{Alloc, Object, ObjectRef, Trace};
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};


derive_Object!(String);
pub struct String {
    /// `fxhash::hash` of the `String`'s chars
    hash: usize,

    /// string data
    chars: Box<str>,
}

unsafe impl Trace for String {
    fn mark(&self) {
        // nop
    }
}

impl String {
    pub fn new<'alloc>(string: &str, alloc: &'alloc Alloc) -> ObjectRef<'alloc, String> {
        String::new_owned(string.into(), alloc)
    }

    pub fn new_owned<'alloc>(chars: Box<str>, alloc: &'alloc Alloc) -> ObjectRef<'alloc, String> {
        let hash = fxhash::hash(chars.as_bytes());
        Object::init(String { hash, chars }, alloc)
    }

    pub fn as_str(&self) -> &str {
        &self.chars
    }
}

impl Debug for String {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.chars.fmt(f)
    }
}

impl PartialEq for String {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash && self.chars == other.chars
    }
}

impl Eq for String {}

impl Hash for String {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.hash);
    }
}

impl super::ObjectHashCode for String {
    fn hash_code(&self) -> usize {
        // PERF avoid rehashing the cached hash in the default ObjectHashCode implementation
        self.hash
    }
}
