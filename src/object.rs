use std::any::{Any, TypeId};
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use std::mem::MaybeUninit;
use std::ops::Deref;
use std::ptr;
use std::sync::atomic::{AtomicPtr, Ordering};

struct Alloc {
    alloc_list: AtomicPtr<ObjectHeader>,
}

impl Alloc {
    const fn new() -> Alloc {
        Alloc {
            alloc_list: AtomicPtr::new(ptr::null_mut() as *mut ObjectHeader),
        }
    }
}

static GLOBAL_ALLOC: Alloc = Alloc::new();

/// # Safety
/// `Self` and the trait implementation must uphold the following invariants.
///
/// - `Self` must be marked `#[repr(C)]` and its first field must be of type [`ObjectHeader`].
/// - `vtable.typeid` must be the result of calling `std::any::TypeId::of::<Self>()`
pub unsafe trait Object: 'static + Any + Debug {
    fn vtable() -> &'static ObjectVtable;

    fn header(&mut self) -> &mut ObjectHeader {
        // SAFETY Both `Self` and `ObjectHeader` are `#[repr(C)]` and ObjectHeader is the first
        // field of `Self`. This is guaranteed by the implementor of the unsafe trait.
        unsafe {
            let ptr = self as *mut Self as *mut ObjectHeader;
            &mut *ptr
        }
    }
}

#[derive(Clone, Copy)]
pub struct ObjectRefAny {
    ptr: *mut ObjectHeader,
}

#[derive(Clone, Copy)]
pub struct ObjectRef<O: Object> {
    ptr: *mut O,
}

pub struct ObjectVtable {
    /// Must be the result of calling `std::any::TypeId::of::<Self>()` in [`Object`] trait impl.
    pub typeid: TypeId,

    /// First argument is always of type `Self`, this can be safely assumed.
    pub debug_fmt: fn(ObjectRefAny, &mut fmt::Formatter<'_>) -> fmt::Result,

    /// First argument is always of type `Self`, this can be safely assumed. Second argument can be
    /// of any type.
    pub eq: fn(ObjectRefAny, ObjectRefAny) -> bool,

    /// First argument is always of type `Self`, this can be safely assumed.
    pub hash: fn(ObjectRefAny, &mut dyn Hasher),
}

impl<O: Object> ObjectRef<O> {
    pub fn upcast(self) -> ObjectRefAny {
        ObjectRefAny { ptr: self.ptr as *mut ObjectHeader }
    }
}

impl ObjectRefAny {
    fn header(&self) -> &ObjectHeader {
        unsafe { &*self.ptr }
    }

    pub fn is<O: Object>(&self) -> bool {
        self.header().vtable.typeid == TypeId::of::<O>()
    }

    pub fn downcast<O: Object>(self) -> Option<ObjectRef<O>> {
        if self.is::<O>() {
            // SAFETY Just checked the type tag matches.
            Some(ObjectRef { ptr: self.ptr as *mut O })
        } else {
            None
        }
    }

    pub fn to_bits(self) -> usize {
        self.ptr as usize
    }
}

impl Debug for ObjectRefAny {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ObjectVtable { debug_fmt, .. } = unsafe { (*self.ptr).vtable };
        debug_fmt(*self, f)
    }
}

impl PartialEq for ObjectRefAny {
    fn eq(&self, other: &Self) -> bool {
        let ObjectVtable { eq, .. } = unsafe { (*self.ptr).vtable };
        eq(*self, *other)
    }
}

impl Hash for ObjectRefAny {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ObjectVtable { hash, .. } = unsafe { (*self.ptr).vtable };
        hash(*self, state)
    }
}

impl<O: Object> Deref for ObjectRef<O> {
    type Target = O;
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.ptr }
    }
}

#[repr(C)]
pub struct ObjectHeader {
    vtable: &'static ObjectVtable,
    next: AtomicPtr<ObjectHeader>,
}

impl ObjectHeader {
    /// Initialized uninitialized ObjectHeader.
    ///
    /// Links the underlying object into the global alloc list.
    ///
    /// # Safety
    /// `this` must be all initialized by the caller except for the header.
    pub unsafe fn init<O: Object>(mut this: Box<MaybeUninit<O>>) -> ObjectRef<O> {
        let mut head = GLOBAL_ALLOC.alloc_list.load(Ordering::Acquire);

        let header = this.as_mut_ptr() as *mut ObjectHeader;
        header.write(ObjectHeader {
            vtable: O::vtable(),
            next: AtomicPtr::new(head),
        });
        // SAFETY caller initialized everything but the header, now the whole object is initialized
        let mut this = this.assume_init();

        loop {
            match GLOBAL_ALLOC.alloc_list.compare_exchange_weak(head, header, Ordering::AcqRel, Ordering::Acquire) {
                Ok(_) => break ObjectRef { ptr: Box::into_raw(this) },
                Err(new_head) => {
                    head = new_head;
                    this.header().next.store(head, Ordering::Release);
                }
            }
        }
    }
}
