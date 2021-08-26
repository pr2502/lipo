use crate::object::{ObjectHeader, ObjectRef};
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};


#[repr(C)]
pub struct String {
    /// ObjectHeader
    header: ObjectHeader,

    /// `fxhash::hash` of the `String`'s chars
    hash: usize,

    /// string data
    chars: Box<str>,
}

impl_Object! { for String {
    fn debug_fmt(this, f) {
        Debug::fmt(&*this, f)
    }

    fn eq(this, other) {
        other.downcast::<String>()
            .map(|other| PartialEq::eq(&*this, &*other))
            .unwrap_or(false)
    }

    fn hash(this, hasher) {
        let mut hasher = hasher;
        this.hash(&mut hasher);
    }
}}

impl String {
    pub fn new(string: &str) -> ObjectRef<String> {
        String::new_owned(string.into())
    }

    pub fn new_owned(chars: Box<str>) -> ObjectRef<String> {
        let hash = fxhash::hash(chars.as_bytes());
        init_object!(String { hash, chars })
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
