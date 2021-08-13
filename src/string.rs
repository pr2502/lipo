use crate::object::{Object, ObjectHeader, ObjectRef, ObjectVtable};
use memoffset::offset_of;
use std::any::TypeId;
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


unsafe impl Object for String {
    fn vtable() -> &'static ObjectVtable {
        static VTABLE: ObjectVtable = ObjectVtable {
            typeid: TypeId::of::<String>(),
            debug_fmt: |this, f| {
                let this = unsafe { this.downcast::<String>().unwrap_unchecked() };
                Debug::fmt(&*this, f)
            },
            eq: |this, other| {
                let this = unsafe { this.downcast::<String>().unwrap_unchecked() };
                other.downcast::<String>()
                    .map(|other| PartialEq::eq(&*this, &*other))
                    .unwrap_or(false)
            },
            hash: |this, mut hasher| {
                let this = unsafe { this.downcast::<String>().unwrap_unchecked() };
                this.hash(&mut hasher);
            }
        };
        &VTABLE
    }
}

impl String {
    pub fn new(string: &str) -> ObjectRef<String> {
        String::new_owned(string.into())
    }

    pub fn new_owned(chars: Box<str>) -> ObjectRef<String> {
        let hash = fxhash::hash(chars.as_bytes());
        let mut uninit = Box::<String>::new_uninit();

        unsafe {
            let uninit_bytes = uninit.as_mut_ptr() as *mut u8;

            let hash_offset = offset_of!(String, hash);
            let hash_ptr = uninit_bytes.add(hash_offset) as *mut usize;
            hash_ptr.write(hash);

            let chars_offset = offset_of!(String, chars);
            let chars_ptr = uninit_bytes.add(chars_offset) as *mut Box<str>;
            chars_ptr.write(chars);

            ObjectHeader::init(uninit)
        }
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
