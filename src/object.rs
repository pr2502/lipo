use crate::value::Value;
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::ops::Deref;
use std::sync::atomic::{AtomicBool, AtomicPtr, Ordering};
use std::{mem, ptr};


/// Helper macro for implementing the Object trait
//
// TODO wrap this in a derive macro
#[macro_export]
macro_rules! derive_Object {
    ( [$($param:tt)*] $object_ty:ty ) => {
        unsafe impl<$($param)*> $crate::object::DynObject for $object_ty {
            // VTABLE is generated based on the traits automatically implemented for `Object`s
            fn __vtable() -> &'static $crate::object::ObjectVtable {

                fn downcast_unchecked<'alloc>(this: $crate::object::ObjectRefAny<'alloc>)
                    -> $crate::object::ObjectRef<'alloc, $object_ty>
                {
                    debug_assert!(this.is::<$object_ty>(), "incorrect receiver type");

                    // SAFETY within the context of this function, all receivers must be
                    // of type `$object_ty`
                    unsafe { this.downcast::<$object_ty>().unwrap_unchecked() }
                }

                // SAFETY `downcast_unchecked` must only be called on the receivers which are
                // guaranteed to be type `$object_ty` because the function is picked from _their_
                // vtable
                static VTABLE: $crate::object::ObjectVtable = $crate::object::ObjectVtable {
                    _typename: ::std::stringify!($object_ty),
                    drop: |this| $crate::object::__derive_drop(downcast_unchecked(this)),
                    mark: |this| $crate::object::__derive_mark(downcast_unchecked(this)),
                    debug_fmt: |this, f| $crate::object::__derive_debug_fmt(downcast_unchecked(this), f),
                    partial_eq: |this, other| $crate::object::__derive_partial_eq(downcast_unchecked(this), other),
                    hash_code: |this| $crate::object::__derive_hash_code(downcast_unchecked(this)),
                };
                &VTABLE
            }
        }

        impl<$($param)*> $crate::object::Object for $object_ty {}
    };

    ( $object_ty:ty ) => { derive_Object!([] $object_ty); };
}

#[doc(hidden)]
pub fn __derive_drop<O: Object>(this: ObjectRef<'static, O>) {
    let ObjectRef { _alloc, ptr } = this;
    // SAFETY GC should only be calling this when the object is unreachable
    unsafe { Alloc::dealloc(ptr) }
}

#[doc(hidden)]
pub fn __derive_mark<'alloc, O: Object>(this: ObjectRef<'alloc, O>) {
    this.mark();
    <O as Trace>::mark(&*this);
}

#[doc(hidden)]
pub fn __derive_debug_fmt<'alloc, O: Object>(this: ObjectRef<'alloc, O>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    <O as Debug>::fmt(&*this, f)
}

#[doc(hidden)]
pub fn __derive_partial_eq<'alloc, O: Object>(this: ObjectRef<'alloc, O>, other: ObjectRefAny<'alloc>) -> bool {
    if !<O as ObjectPartialEq>::supported() {
        todo!("type error: type $object_ty doesn't support equality");
    }

    if let Some(other) = other.downcast::<O>() {
        <O as ObjectPartialEq>::eq(&*this, other)
    } else {
        todo!("type error: rhs type doesn't match");
    }
}

#[doc(hidden)]
pub fn __derive_hash_code<'alloc, O: Object>(this: ObjectRef<'alloc, O>) -> usize {
    <O as ObjectHashCode>::hash_code(&*this)
}


pub mod builtins {
    mod function;
    mod string;

    pub use function::Function;
    pub use string::String;

    pub use crate::chunk::Chunk;
}


////////////////////////////////////////////////////////////////////////////////
// Alloc/GC

pub struct Alloc {
    head: AtomicPtr<ObjectHeader>,
}

impl Alloc {
    /// Create a new empty `Alloc`.
    pub const fn new() -> Alloc {
        Alloc {
            head: AtomicPtr::new(ptr::null_mut()),
        }
    }

    /// Take the current head of the alloc list and return an `Iterator` over all it's
    /// [`ObjectHeader`]s.
    fn take_iter(&self) -> impl Iterator<Item = *mut ObjectHeader> {
        struct Iter(*mut ObjectHeader);

        impl Iterator for Iter {
            type Item = *mut ObjectHeader;

            fn next(&mut self) -> Option<Self::Item> {
                if self.0.is_null() {
                    return None;
                }

                // SAFETY `Alloc::insert_atomic` maintains that `next` pointer
                // is either valid or null
                let current = unsafe { &*self.0 };

                // Relaxed doesn't race because we own the list while iterating.
                let next = current.next.load(Ordering::Relaxed);

                Some(mem::replace(&mut self.0, next))
            }
        }

        // This Acquire pairs with the Release part of the AcqRel on successful exchange
        // in [`insert_atomic`]. It ensures all writes to the list are synchronized
        // before we iterate over it.
        let list = self.head.swap(ptr::null_mut(), Ordering::Acquire);

        Iter(list)
    }

    /// Prepend a list of one or more [`ObjectHeader`]s to the list managed by this Alloc.
    ///
    /// # Safety
    /// `ptr` must point to a valid ObjectHeader object
    unsafe fn insert_atomic(
        &self,
        // object to insert at the beginning
        ptr: *mut ObjectHeader,
        // the `next` pointer of the last object to be inserted
        next: &AtomicPtr<ObjectHeader>,
    ) {
        let mut old_head = self.head.load(Ordering::Acquire);

        loop {
            // The next pointer is already synchronized by the Release-Acquire pair in the exchange
            // here and in the swap in [`take_iter`].
            next.store(old_head, Ordering::Relaxed);

            // The Release part of the AcqRel on success pairs with either the Acquire on failure,
            // the Acquire when inserting the next item(s) or with the Acquire in [`take_iter`].
            match self.head.compare_exchange_weak(old_head, ptr, Ordering::AcqRel, Ordering::Acquire) {
                Ok(_) => break,
                Err(new_head) => {
                    old_head = new_head;
                }
            }
        }
    }
}

impl Alloc {
    /// Allocate new object and track it
    fn alloc<'alloc, O: Object>(&'alloc self, inner: O) -> ObjectRef<'alloc, O> {
        let wrap = Box::new(ObjectWrap {
            header: ObjectHeader {
                vtable: O::__vtable(),
                next: AtomicPtr::new(ptr::null_mut()),
                mark: AtomicBool::new(false),
            },
            inner,
        });
        let obj = ObjectRef {
            _alloc: PhantomData, // 'alloc - in return type
            ptr: Box::into_raw(wrap),
        };
        // SAFETY ptr is valid because we obtained it from safe ObjectRef belonging to this Alloc
        unsafe { self.insert_atomic(obj.upcast().ptr, &obj.upcast().header().next); }
        obj
    }

    /// Deallocate a previously allocated object
    ///
    /// # Safety
    /// `ptr` must originate from a previous call to `Alloc::alloc` on the same `Alloc` instance.
    unsafe fn dealloc<O: Object>(ptr: *mut ObjectWrap<O>) {
        // SAFETY ptr was created from a `Box<ObjectWrap<O>>` in `Alloc::alloc`
        let _drop = unsafe { Box::from_raw(ptr) };
    }

    /// Collect unmarked objects
    ///
    /// # Safety
    /// Every reachable object must have been marked prior to calling this function. This function
    /// will unmark objects as it traverses the heap so objects have to be re-marked before every
    /// call to `sweep`.
    pub unsafe fn sweep(&self) {
        // Extracts the next field from a `*mut ObjectHeader`
        let header = |ptr: *mut ObjectHeader| {
            // SAFETY must audit uses of the lambda
            unsafe { &*ptr }
        };

        // Linked list of objects left after collection
        let mut retain_head = ptr::null_mut::<ObjectHeader>();
        let mut retain_tail = ptr::null_mut();

        // Store object into the retain list.
        let mut retain = |ptr: *mut ObjectHeader| {
            assert!(!ptr.is_null(), "cannot retain nullptr");

            if retain_head.is_null() {
                // insert first element
                retain_head = ptr;
                retain_tail = ptr;
            } else {
                // SAFETY tail was set in the other branch
                header(retain_tail).next.store(ptr, Ordering::Relaxed);
                retain_tail = ptr;
            }
        };

        for object in self.take_iter() {
            // SAFETY iterator only yields non-null pointers
            if header(object).mark.swap(false, Ordering::Relaxed) {
                // object is marked, keeping
                retain(object);
            } else {
                // unmarked object, dropping

                // SAFETY object is not null so it must be valid `
                let to_drop: ObjectRefAny<'static> = unsafe { ObjectRefAny::from_ptr(object) };

                log::info!("collecting Obj@{:?} {:?}", to_drop.ptr, &to_drop);

                // SAFETY Object behind `current` will can never be used again
                let ObjectVtable { drop, .. } = to_drop.vtable();
                unsafe { drop(to_drop) }
            }
        }

        if !retain_head.is_null() {
            // some objects were left after collection, insert them back into the alloc list

            // SAFETY reinserting only items from the list which were not dropped
            unsafe { self.insert_atomic(retain_head, &header(retain_tail).next); }
        }
    }
}

impl Drop for Alloc {
    /// Deallocates all objects registered to this `Alloc`
    fn drop(&mut self) {
        for object in self.take_iter() {
            // SAFETY `Alloc::alloc` maintains a linked list, ptr either points to a valid object
            // or is null and we checked for null.
            //
            // We use 'static lifetime here because we don't have any name for 'self or similar,
            // this object reference is valid until we drop it here.
            let object: ObjectRefAny<'static> = unsafe { ObjectRefAny::from_ptr(object) };

            let ObjectVtable { drop, .. } = object.vtable();
            // SAFETY Object behind `current` will can never be used again
            unsafe { drop(object) }
        }
    }
}


pub unsafe trait Trace {
    fn mark(&self);
}

unsafe impl<'alloc> Trace for ObjectRefAny<'alloc> {
    fn mark(&self) {
        let prev_marked = self.header().mark.swap(true, Ordering::Relaxed);
        if !prev_marked {
            // if the object wasn't marked previously mark recursively all it's children
            let ObjectVtable { mark, .. } = self.vtable();
            mark(*self);
        }
    }
}

unsafe impl<'alloc, O: Object> Trace for ObjectRef<'alloc, O> {
    fn mark(&self) {
        let prev_marked = self.upcast().header().mark.swap(true, Ordering::Relaxed);
        if !prev_marked {
            self.deref().mark();
        }
    }
}

unsafe impl<'alloc> Trace for Value<'alloc> {
    fn mark(&self) {
        if let Some(o) = self.to_object() {
            o.mark();
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
// Object traits

pub trait Object: DynObject {
    /// Moves the object to the heap and registers it in the `alloc` allocator.
    ///
    /// Do not override this method, use the default implementation.
    fn init<'alloc>(this: Self, alloc: &'alloc Alloc) -> ObjectRef<'alloc, Self> {
        alloc.alloc(this)
    }
}


/// Custom dynamic dispatch implementation for the [`Object`] trait.
///
/// **This trait should not be implemented manually**, use the [`derive_Object`] macro instead.
#[doc(hidden)]
pub unsafe trait DynObject: Trace + Debug + Sized {
    /// # Safety
    /// Must return a unique static constant and must be filled correctly.
    /// See the [`ObjectVtable`] documentation and comments.
    fn __vtable() -> &'static ObjectVtable;
}


pub trait ObjectPartialEq: Object {
    /// Returns whether equality is supported for type
    fn supported() -> bool;

    /// Returns true if types are equal, panics if type doesn't support equality
    fn eq(&self, rhs: ObjectRef<'_, Self>) -> bool;
}

impl<O> ObjectPartialEq for O
where
    O: Object,
{
    default fn supported() -> bool {
        false
    }

    default fn eq(&self, rhs: ObjectRef<'_, O>) -> bool {
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

    default fn eq(&self, rhs: ObjectRef<'_, O>) -> bool {
        *self == *rhs
    }
}


pub trait ObjectHashCode: Object {
    /// Returns whether hashing is supported for type
    fn supported() -> bool;

    /// Returns the object fxhash::hash, panics if type doesn't support hashing
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

/// Header stores the vtable pointer and a intrusive linked list for GC.
pub(crate) struct ObjectHeader {
    vtable: &'static ObjectVtable,
    next: AtomicPtr<ObjectHeader>,
    mark: AtomicBool,
}

/// Type erased reference to a garbage collected Object
#[derive(Clone, Copy)]
pub struct ObjectRefAny<'alloc> {
    _alloc: PhantomData<&'alloc ()>,
    ptr: *mut ObjectHeader,
}

/// Reference to a garbage collected Object
pub struct ObjectRef<'alloc, O: Object> {
    _alloc: PhantomData<&'alloc ()>,
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


/// Custom [virtual method table](https://en.wikipedia.org/wiki/Virtual_method_table) for the
/// [`DynObject`] trait.
///
/// For every method the first argument (usually marked as `this`) is expected to be the receiver
/// and when called will receive upcast `ObjectRef<'_, Self>`, therefore for that argument it is
/// safe to downcast it without checking the type tag first.
#[doc(hidden)]
pub struct ObjectVtable {
    /// String representation for the type, used for debugging
    pub _typename: &'static str,

    /// Deallocate Object, dispatch for [`Alloc::dealloc`]
    ///
    /// # Safety
    /// After calling `drop` the object behind `this` gets freed and cannot be used via this or any
    /// other reference. Caller must ensure that the Object is unreachable.
    pub drop: unsafe fn(
        // this: Self
        ObjectRefAny<'static>,
    ),

    /// Mark Object as reachable, dispatch for [`Trace::mark`]
    pub mark: fn(
        // this: Self
        ObjectRefAny,
    ),

    /// Format Object using the [`std::fmt::Debug`] formatter.
    pub debug_fmt: fn(
        // this: Self
        ObjectRefAny,
        // f: debug formatter from std
        &mut fmt::Formatter<'_>
    ) -> fmt::Result,

    /// Compare Objects for equality, dispatch for [`ObjectPartialEq::eq`]
    // TODO panics if equality is not supported or the rhs type doesn't match,
    // maybe return a Result<bool, TypeError> or Option<Result<bool, TypeError>>
    pub partial_eq: fn(
        // this: Self
        ObjectRefAny,
        // rhs: Any
        ObjectRefAny,
    ) -> bool,

    /// Get Object hash code, dispatch for [`ObjectHashCode::hash_code`]
    // TODO panics if hashing is not supported,
    // maybe return an Option<usize>
    pub hash_code: fn(
        // this: Self
        ObjectRefAny,
    ) -> usize,
}

impl<'alloc, O: Object> ObjectRef<'alloc, O> {
    pub fn upcast(self) -> ObjectRefAny<'alloc> {
        // SAFETY upcasting an ObjectRef is always safe, lifetime is maintained
        unsafe {
            ObjectRefAny::from_ptr(self.ptr as _)
        }
    }
}

impl<'alloc> ObjectRefAny<'alloc> {
    pub(crate) fn as_ptr(self) -> *mut ObjectHeader {
        self.ptr
    }

    pub(crate) unsafe fn from_ptr(ptr: *mut ObjectHeader) -> ObjectRefAny<'alloc> {
        ObjectRefAny {
            _alloc: PhantomData, // 'alloc - in return type
            ptr,
        }
    }

    fn header(&self) -> &ObjectHeader {
        unsafe { &*self.ptr }
    }

    fn vtable(&self) -> &ObjectVtable {
        self.header().vtable
    }

    pub fn is<O: Object>(&self) -> bool {
        // PERF Each Rust type implementing Object must have its own vtable. Therefore TypeIds are
        // equal if and only if the vtable pointers are equal.
        //
        // So here we compare the vtable pointers directly to save two dereferences.
        ptr::eq(
            self.header().vtable,
            O::__vtable(),
        )
    }

    pub fn downcast<O: Object>(self) -> Option<ObjectRef<'alloc, O>> {
        if self.is::<O>() {
            // SAFETY Just checked the type tag matches.
            Some(ObjectRef {
                _alloc: self._alloc,
                ptr: self.ptr as *mut ObjectWrap<O>,
            })
        } else {
            None
        }
    }
}

impl<'alloc> Debug for ObjectRefAny<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ObjectVtable { debug_fmt, .. } = self.vtable();
        debug_fmt(*self, f)
    }
}

impl<'alloc> PartialEq for ObjectRefAny<'alloc> {
    fn eq(&self, other: &Self) -> bool {
        let ObjectVtable { partial_eq, .. } = self.vtable();
        partial_eq(*self, *other)
    }
}

impl<'alloc> Hash for ObjectRefAny<'alloc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ObjectVtable { hash_code, .. } = self.vtable();
        let code = hash_code(*self);
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
        <O as Hash>::hash(&*self, state)
    }
}
