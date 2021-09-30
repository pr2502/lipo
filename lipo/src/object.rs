use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr;


// Re-export the derive macros
pub use lipo_macro::{Object, Trace};

#[doc(hidden)]
pub mod __derive_object;
#[doc(hidden)]
pub mod __derive_trace;


mod gc;

pub use gc::Alloc;
pub use gc::Trace;
use gc::ObjectHeader;


////////////////////////////////////////////////////////////////////////////////
// Object traits

/// Marker trait for objects
pub trait Object: DynObject {}


/// Custom dynamic dispatch implementation for the [`Object`] trait.
///
/// **This trait should not be implemented manually**, use the [`derive_Object`] macro instead.
#[doc(hidden)]
pub unsafe trait DynObject: Trace + Debug + Sized + Send + Sync {
    /// # Safety
    /// Must return a unique static constant and must be filled correctly.
    /// See the [`ObjectVtable`] documentation and comments.
    fn __vtable() -> &'static ObjectVtable;
}


pub trait ObjectPartialEq: Object {
    /// Returns whether equality is supported for type
    fn supported() -> bool;

    /// Returns true if types are equal, panics if type doesn't support equality
    fn eq(&self, rhs: &Self) -> bool;
}

impl<O> ObjectPartialEq for O
where
    O: Object,
{
    default fn supported() -> bool {
        false
    }

    default fn eq(&self, rhs: &Self) -> bool {
        let _ = rhs;
        unimplemented!()
    }
}

impl<O> ObjectPartialEq for O
where
    O: Object,
    O: PartialEq<O>,
{
    default fn supported() -> bool {
        true
    }

    default fn eq(&self, rhs: &Self) -> bool {
        *self == *rhs
    }
}


pub trait ObjectHashCode: Object {
    /// Returns whether hashing is supported for type
    fn supported() -> bool;

    /// Returns the object `fxhash::hash`, panics if type doesn't support hashing
    fn hash_code(&self) -> usize;
}

impl<O> ObjectHashCode for O
where
    O: Object,
{
    default fn supported() -> bool {
        false
    }

    default fn hash_code(&self) -> usize {
        unimplemented!()
    }
}

impl<O> ObjectHashCode for O
where
    O: Object,
    O: Hash,
{
    default fn supported() -> bool {
        true
    }

    default fn hash_code(&self) -> usize {
        fxhash::hash(self)
    }
}


////////////////////////////////////////////////////////////////////////////////
// Object repr

/// Internal wrapper for object header and user data.
///
/// `#[repr(C)]` ensures that casting between `*mut ObjectHeader` and `*mut ObjectWrap<O>` is safe
/// as long as the type `O` matches.
#[repr(C)]
struct ObjectWrap<O: Object> {
    header: ObjectHeader,
    inner: O,
}

/// Type erased reference to a garbage collected Object
#[derive(Clone, Copy)]
pub struct ObjectRefAny<'alloc> {
    alloc: PhantomData<&'alloc ()>,
    ptr: *mut ObjectHeader,
}

/// Reference to a garbage collected Object
pub struct ObjectRef<'alloc, O: Object> {
    alloc: PhantomData<&'alloc ()>,
    ptr: *mut ObjectWrap<O>,
}

// Implement `Clone` and `Copy` manually, because deriving them inherits the `Clone+Copy`
// properties of the concrete `O` but `ObjectRef` is meant to be `Clone+Copy` for any `O: Object`
// regardless of whether `O` is `Clone` or `Copy`.
impl<'alloc, O: Object> Clone for ObjectRef<'alloc, O> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<'alloc, O: Object> Copy for ObjectRef<'alloc, O> {}


// SAFETY ObjectRef doesn't allow its contents to be mutated and all `Object`s are required to be
// Send and Sync themselves by the `DynObject` trait so allowing immutable access to them is safe
// from any thread.
//
// For the purposes of synchronization `ObjectRef<O>` behaves like `Arc<O>`.
unsafe impl<'alloc, O: Object> Send for ObjectRef<'alloc, O> {}
unsafe impl<'alloc, O: Object> Sync for ObjectRef<'alloc, O> {}


/// Custom [virtual method table](https://en.wikipedia.org/wiki/Virtual_method_table) for the
/// [`DynObject`] trait.
///
/// For every method the first argument (usually marked as `this`) is expected to be the receiver
/// and when called will receive upcast `ObjectRef<'_, Self>`, therefore for that argument it is
/// safe to downcast it without checking the type tag first.
#[doc(hidden)]
pub struct ObjectVtable {
    /// String representation for the type, used for debugging
    pub typename: &'static str,

    /// Deallocate Object, dispatch for [`Alloc::dealloc`]
    ///
    /// # Safety
    /// After calling `drop` the object behind `this` gets freed and cannot be used via this or any
    /// other reference. Caller must ensure that the Object is unreachable.
    ///
    /// The receiver (first argument) must be of upcast ObjectRef<'static, Self>.
    pub drop: unsafe fn(
        // this: Self
        ObjectRefAny<'static>,
    ),

    /// Mark Object as reachable, dispatch for [`Trace::mark`]
    ///
    /// # Safety
    /// The receiver (first argument) must be of upcast ObjectRef<'static, Self>.
    pub mark: unsafe fn(
        // this: Self
        ObjectRefAny,
    ),

    /// Format Object using the [`std::fmt::Debug`] formatter.
    ///
    /// # Safety
    /// The receiver (first argument) must be of upcast ObjectRef<'static, Self>.
    pub debug_fmt: unsafe fn(
        // this: Self
        ObjectRefAny,
        // f: debug formatter from std
        &mut fmt::Formatter<'_>
    ) -> fmt::Result,

    /// Compare Objects for equality, dispatch for [`ObjectPartialEq::eq`]
    ///
    /// Returns `None` when `this` doesn't support equality or when `rhs` type doesn't match.
    ///
    /// # Safety
    /// The receiver (first argument) must be of upcast ObjectRef<'static, Self>.
    pub partial_eq: unsafe fn(
        // this: Self
        ObjectRefAny,
        // rhs: Any
        ObjectRefAny,
    ) -> Option<bool>,

    /// Get Object hash code, dispatch for [`ObjectHashCode::hash_code`]
    ///
    /// # Safety
    /// The receiver (first argument) must be of upcast ObjectRef<'static, Self>.
    // TODO panics if hashing is not supported,
    // maybe return an Option<usize>
    pub hash_code: unsafe fn(
        // this: Self
        ObjectRefAny,
    ) -> usize,
}

impl<'alloc, O: Object> ObjectRef<'alloc, O> {
    pub fn upcast(self) -> ObjectRefAny<'alloc> {
        // SAFETY upcasting an ObjectRef is always safe, lifetime is maintained
        unsafe {
            ObjectRefAny::from_ptr(self.ptr.cast())
        }
    }
}

impl<'alloc> ObjectRefAny<'alloc> {
    pub(crate) fn as_ptr(self) -> *mut ObjectHeader {
        self.ptr
    }

    pub(crate) unsafe fn from_ptr(ptr: *mut ObjectHeader) -> ObjectRefAny<'alloc> {
        ObjectRefAny {
            alloc: PhantomData, // 'alloc - in return type
            ptr,
        }
    }

    fn header(&self) -> &ObjectHeader {
        unsafe { &*self.ptr }
    }

    fn vtable(&self) -> &ObjectVtable {
        self.header().vtable()
    }

    pub(crate) fn is<O: Object>(&self) -> bool {
        // PERF Each Rust type implementing Object must have its own vtable. Therefore TypeIds are
        // equal if and only if the vtable pointers are equal.
        //
        // So here we compare the vtable pointers directly to save two dereferences.
        ptr::eq(
            self.vtable(),
            O::__vtable(),
        )
    }

    pub(crate) fn downcast<O: Object>(self) -> Option<ObjectRef<'alloc, O>> {
        if self.is::<O>() {
            // SAFETY Just checked the type tag matches.
            Some(ObjectRef {
                alloc: self.alloc,
                ptr: self.ptr.cast::<ObjectWrap<O>>(),
            })
        } else {
            None
        }
    }
}

impl<'alloc> Debug for ObjectRefAny<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ObjectVtable { debug_fmt, .. } = self.vtable();

        // SAFETY we're using this Object's own vtable
        unsafe { debug_fmt(*self, f) }
    }
}

impl<'alloc> ObjectRefAny<'alloc> {
    pub fn partial_eq(&self, other: &ObjectRefAny<'alloc>) -> Option<bool> {
        let ObjectVtable { partial_eq, .. } = self.vtable();

        // SAFETY we're using this Object's own vtable
        unsafe { partial_eq(*self, *other) }
    }
}

impl<'alloc> PartialEq for ObjectRefAny<'alloc> {
    fn eq(&self, other: &Self) -> bool {
        self.partial_eq(other).unwrap_or(false)
    }
}

impl<'alloc> Hash for ObjectRefAny<'alloc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ObjectVtable { hash_code, .. } = self.vtable();

        // SAFETY we're using this Object's own vtable
        let code = unsafe { hash_code(*self) };

        state.write_usize(code);
    }
}

impl<'alloc, O: Object> Deref for ObjectRef<'alloc, O> {
    type Target = O;
    fn deref(&self) -> &Self::Target {
        // SAFETY GC ensures the pointer stays valid.
        //        We never give mutable access to the value.
        unsafe { &(*self.ptr).inner }
    }
}

impl<'alloc, O: Object + Debug> Debug for ObjectRef<'alloc, O> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <O as Debug>::fmt(&*self, f)
    }
}

impl<'alloc, O: Object + PartialEq> PartialEq for ObjectRef<'alloc, O> {
    fn eq(&self, other: &Self) -> bool {
        <O as PartialEq>::eq(&*self, &*other)
    }
}

impl<'alloc, O: Object + Eq> Eq for ObjectRef<'alloc, O> {}

impl<'alloc, O: Object + Hash> Hash for ObjectRef<'alloc, O> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        <O as Hash>::hash(&*self, state);
    }
}
