use std::any::{Any, TypeId};
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use std::mem::MaybeUninit;
use std::ops::Deref;
use std::ptr;
use std::sync::atomic::{AtomicPtr, Ordering};


/// Helper macro for unsafe initialization of Object types
macro_rules! init_object {
    ( $self_ty:ident {
        $( $field_name:ident ),* $(,)?
    } ) => {{
        let mut uninit = ::std::boxed::Box::<$self_ty>::new_uninit();

        // check that all fields except the `header` are initialized
        { let $self_ty { header: _, $($field_name: _),* }; };

        // check that the `header` field is at offset 0
        assert_eq!(
            // SAFETY we never read the uninitialized data behind the pointer
            unsafe { ::std::ptr::addr_of!((*uninit.as_ptr()).header) },
            uninit.as_ptr() as *const ObjectHeader,
            "`header` field must be the first field in the struct",
        );

        // check that `header` field is of type ObjectHeader
        fn _check_header_type(this: &$self_ty) -> &$crate::object::ObjectHeader {
            &this.header
        }

        // SAFETY
        //  - we checked that all fields except for header are initialized
        //  - we checked that header is the first field
        //  - we checked that header is of type ObjectHeader
        unsafe {
            let ptr = uninit.as_mut_ptr();
            $(
                ::std::ptr::addr_of_mut!((*ptr).$field_name)
                    .write($field_name);
            )*
            $crate::object::ObjectHeader::init(uninit)
        }
    }};
}


/// Helper macro for mostly safely implementing the unsafe Object trait
///
/// User still needs to ensure:
/// - `$object_ty` is marked `#[repr(C)]`
///
/// This macro ensures:
/// - `vtable.typeid` matches the type the trait is impld for (`$object_ty`)
/// - `vtable` return value is a unique static
macro_rules! impl_Object {
    ( for $object_ty:ty {
        $(
            fn $fn_name:ident ( $this:ident $(, $arg:ident $(: $arg_ty:ty)?)* $(,)? ) $(-> $fn_ret:ty)? { $($body:tt)* }
        )*
    } ) => {
        unsafe impl $crate::object::Object for $object_ty {
            fn vtable() -> &'static $crate::object::ObjectVtable {
                static VTABLE: $crate::object::ObjectVtable
                    = $crate::object::ObjectVtable {
                        typeid: ::std::any::TypeId::of::<$object_ty>(),
                        $(
                            $fn_name: |this, $($arg $(: $arg_ty)?),*| $(-> $fn_ret)? {
                                match unsafe { $crate::object::ObjectRefAny::downcast::<$object_ty>(this).unwrap_unchecked() } {
                                    $this => {
                                        $($body)*
                                    }
                                }
                            }
                        ),*
                    };
                &VTABLE
            }
        }
    };
}


pub mod function;
pub mod string;


struct Alloc {
    alloc_list: AtomicPtr<ObjectHeader>,
}

impl Alloc {
    const fn new() -> Alloc {
        Alloc {
            alloc_list: AtomicPtr::new(ptr::null_mut()),
        }
    }
}

static GLOBAL_ALLOC: Alloc = Alloc::new();


/// # Safety
/// `Self` and the trait implementation must uphold the following invariants.
///
/// - `Self` must be marked `#[repr(C)]` and its first field must be of type [`ObjectHeader`].
/// - `vtable.typeid` must be the result of calling `std::any::TypeId::of::<Self>()`
/// - `vtable` each implementation must return a reference to a unique ObjectVtable, and the return
///    value must never change
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
    pub(crate) ptr: *mut ObjectHeader,
}

pub struct ObjectRef<O: Object> {
    pub(crate) ptr: *mut O,
}

// Implement `Clone` and `Copy` manually, because deriving them inherits the `Clone+Copy`
// properties of the concrete `O` but `ObjectRef` is meant to be `Clone+Copy` for any `O: Object`
// regardless of whether `O` is `Clone` or `Copy`.
impl<O: Object> Clone for ObjectRef<O> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<O: Object> Copy for ObjectRef<O> {}


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
        // PERF Each Rust type implementing Object must have its own vtable. Therefore TypeIds are
        // equal if and only if the vtable pointers are equal.
        //
        // So here we compare the vtable pointers directly to save two dereferences.
        let ptr = |vtable| vtable as *const ObjectVtable as usize;
        ptr(self.header().vtable) == ptr(O::vtable())
    }

    pub fn downcast<O: Object>(self) -> Option<ObjectRef<O>> {
        if self.is::<O>() {
            // SAFETY Just checked the type tag matches.
            Some(ObjectRef { ptr: self.ptr as *mut O })
        } else {
            None
        }
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
        // SAFETY GC ensures the pointer stays valid.
        //        We never give mutable access to the value.
        unsafe { &*self.ptr }
    }
}

impl<O: Object + Debug> Debug for ObjectRef<O> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <O as Debug>::fmt(&*self, f)
    }
}

impl<O: Object + PartialEq> PartialEq for ObjectRef<O> {
    fn eq(&self, other: &Self) -> bool {
        <O as PartialEq>::eq(&*self, &*other)
    }
}

impl<O: Object + Eq> Eq for ObjectRef<O> {}

impl<O: Object + Hash> Hash for ObjectRef<O> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        <O as Hash>::hash(&*self, state)
    }
}


// Deliberately not Clone
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
        let header = this.as_mut_ptr() as *mut ObjectHeader;
        header.write(ObjectHeader {
            vtable: O::vtable(),
            next: AtomicPtr::new(ptr::null_mut()),
        });
        // SAFETY caller initialized everything but the header, now the whole object is initialized
        let mut this = this.assume_init();

        atomic_prepend(&GLOBAL_ALLOC.alloc_list, header, &this.header().next);

        ObjectRef { ptr: Box::into_raw(this) }
    }
}

fn atomic_prepend<P>(
    list_head: &AtomicPtr<P>,
    item: *mut P,
    item_next: &AtomicPtr<P>,
) {
    let mut head = list_head.load(Ordering::Acquire);

    loop {
        item_next.store(head, Ordering::Release);
        match list_head.compare_exchange_weak(head, item, Ordering::AcqRel, Ordering::Acquire) {
            Ok(_) => break,
            Err(new_head) => {
                head = new_head;
            }
        }
    }
}

#[cfg(test)]
#[test]
fn header_size() {
    use std::mem::size_of;

    assert_eq!(
        size_of::<ObjectHeader>(),
        size_of::<(usize, usize)>(),
    );
}
