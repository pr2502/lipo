use std::lazy::SyncOnceCell;
use std::marker::PhantomData;
use std::ptr::{self, NonNull};
use std::sync::atomic::{AtomicBool, AtomicPtr, Ordering};
use std::sync::Mutex;

use tracing::info;

use super::{Object, ObjectRef, ObjectRefAny, ObjectVtable, ObjectWrap};
use crate::name::{Name, NameInterner};
use crate::util::Invariant;
use crate::value::{Value, ValueKind};
use crate::Primitive;


/// Header stores the vtable pointer and a intrusive linked list for GC.
pub(crate) struct ObjectHeader {
    /// Virtual method table
    vtable: &'static ObjectVtable,

    /// Next link of intrusive linked free list
    next: AtomicPtr<ObjectHeader>,

    /// GC marker
    ///
    /// Set to `true` during tracing and objects which are left at `false` get
    /// dropped.
    mark: AtomicBool,
}

impl ObjectHeader {
    pub(super) fn vtable(&self) -> &'static ObjectVtable {
        self.vtable
    }
}


/// Alloc keeps two lifetimes.
///
/// One is the `'root` allocator lifetime. This is the lifetimes all allocated
/// `Object`s will live for.
///
/// The second is `'parent` and it refers to the lifetime of the parent `Alloc`
/// for nested `Alloc`s.
///
/// ## Allocator nesting
/// Because garbage collection requries knowledge of all root objects this would
/// prevent anyone except for the owner of the root allocator from calling
/// `sweep` safely. For this purposes systems like the [`VM`](crate::VM) will
/// create their own local allocator that will be used for all objects allocated
/// within the VM and only call sweep on this subset of objects that were
/// allocated within it.
///
/// Because `sweep` requires `&mut self` it's only possible to call it on the
/// leaf Allocator preventing issues with objects allocated in child allocators
/// pointing back to a parent allocator which is getting sweeped. This can
/// however become a problem with interior mutability but all (for now) builtin
/// lipo types are immutable. Further workaround (like migrating objects to
/// parent allocators on insert) might be required to make even interior
/// mutability safe.
///
/// ```rust
/// # use lipo::{Trace, Alloc, builtins::Float};
///
/// let mut root = Alloc::new();
/// let root_obj = Float::new(1.0, &root).unwrap();
///
/// // mark and sweep the root allocator
/// root_obj.mark();
/// unsafe { root.sweep() }
/// assert_eq!(root_obj.inner(), 1.0);
///
/// let mut nested = root.nested();
/// let nested_obj = Float::new(2.0, &nested).unwrap();
///
/// // mark and sweep only the nested allocator,
/// // both objects have to stay alive even though only the object from the
/// // nested allocator was marked
/// nested_obj.mark();
/// unsafe { nested.sweep() }
/// assert_eq!(root_obj.inner(), 1.0);
/// assert_eq!(nested_obj.inner(), 2.0);
///
/// // when we drop the nested allocator its remaining objects get migrated to
/// // the parent
/// drop(nested);
/// assert_eq!(root_obj.inner(), 1.0);
/// assert_eq!(nested_obj.inner(), 2.0);
///
/// // we can now sweep the parent again (because it's not borrowed by the
/// // child allocator anymore) but we have to mark both objects for them to
/// // stay alive
/// root_obj.mark();
/// nested_obj.mark();
/// unsafe { root.sweep() }
/// assert_eq!(root_obj.inner(), 1.0);
/// assert_eq!(nested_obj.inner(), 2.0);
/// ```
pub struct Alloc<'parent, 'root> {
    root: PhantomData<Invariant<'root>>,
    parent: Option<&'parent Alloc<'parent, 'root>>,
    alloc_list: AtomicPtr<ObjectHeader>,
    name_interner: SyncOnceCell<Mutex<NameInterner>>,
}

impl<'parent, 'root> Alloc<'parent, 'root> {
    /// Create a new root `Alloc`.
    pub const fn new() -> Alloc<'static, 'root> {
        Alloc {
            root: PhantomData,
            parent: None,
            alloc_list: AtomicPtr::new(ptr::null_mut()),
            name_interner: SyncOnceCell::new(),
        }
    }

    /// Create a new nested `Alloc`.
    pub const fn nested(&'parent self) -> Alloc<'parent, 'root> {
        Alloc {
            root: self.root,
            parent: Some(self),
            alloc_list: AtomicPtr::new(ptr::null_mut()),
            name_interner: SyncOnceCell::new(),
        }
    }

    /// Take the current head of the alloc list and return an `Iterator` over
    /// all it's [`ObjectHeader`]s.
    fn take_iter(&self) -> impl Iterator<Item = NonNull<ObjectHeader>> {
        struct Iter(*mut ObjectHeader);

        impl Iterator for Iter {
            type Item = NonNull<ObjectHeader>;

            fn next(&mut self) -> Option<Self::Item> {
                let current = NonNull::new(self.0)?;
                // SAFETY `Alloc::insert_atomic` maintains that `next` pointer
                // is either valid or null
                let current_ref = unsafe { current.as_ref() };
                // Relaxed doesn't race because we own the list while iterating.
                self.0 = current_ref.next.load(Ordering::Relaxed);
                Some(current)
            }
        }

        // This Acquire pairs with the Release part of the AcqRel on successful exchange
        // in [`insert_atomic`]. It ensures all writes to the list are synchronized
        // before we iterate over it.
        let list = self.alloc_list.swap(ptr::null_mut(), Ordering::Acquire);

        Iter(list)
    }

    /// Prepend a list of one or more [`ObjectHeader`]s to the list managed by
    /// this Alloc.
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
        let mut old_head = self.alloc_list.load(Ordering::Acquire);

        loop {
            // The next pointer is already synchronized by the Release-Acquire pair in the
            // exchange here and in the swap in [`take_iter`].
            next.store(old_head, Ordering::Relaxed);

            // The Release part of the AcqRel on success pairs with either the Acquire on
            // failure, the Acquire when inserting the next item(s) or with the
            // Acquire in [`take_iter`].
            match self.alloc_list.compare_exchange_weak(
                old_head,
                ptr,
                Ordering::AcqRel,
                Ordering::Acquire,
            ) {
                Ok(_) => break,
                Err(new_head) => {
                    old_head = new_head;
                },
            }
        }
    }
}

impl<'parent, 'root> Alloc<'parent, 'root> {
    /// Allocate new object and track it
    pub fn alloc<O: Object>(&self, inner: O) -> ObjectRef<'root, O> {
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
            // SAFETY Box::into_raw returns a NonNull pointer
            ptr: unsafe { NonNull::new_unchecked(Box::into_raw(wrap)) },
        };
        // SAFETY ptr is valid because we obtained it from safe ObjectRef belonging to
        // this Alloc
        unsafe {
            self.insert_atomic(obj.upcast().ptr.as_ptr(), &obj.upcast().header().next);
        }
        obj
    }

    /// Deallocate a previously allocated object
    ///
    /// # Safety
    /// `ptr` must originate from a previous call to `Alloc::alloc` on the same
    /// `Alloc` instance.
    pub(super) unsafe fn dealloc<O: Object>(ptr: NonNull<ObjectWrap<O>>) {
        // SAFETY ptr was created from a `Box<ObjectWrap<O>>` in `Alloc::alloc`
        let _drop = unsafe { Box::from_raw(ptr.as_ptr()) };
    }

    /// Collect unmarked objects
    ///
    /// # Safety
    /// Every reachable object must have been marked prior to calling this
    /// function. This function will unmark objects as it traverses the heap
    /// so objects have to be re-marked before every call to `sweep`.
    pub unsafe fn sweep(&mut self) {
        // Extracts the next field from a `*mut ObjectHeader`
        let header = |ptr: NonNull<ObjectHeader>| {
            // SAFETY must audit uses of the lambda
            unsafe { ptr.as_ref() }
        };

        // Linked list of objects left after collection
        let mut retain_head = Option::<NonNull<ObjectHeader>>::None;
        let mut retain_tail = None;

        // Store object into the retain list.
        let mut retain = |ptr: NonNull<ObjectHeader>| {
            // assert!(!ptr.is_null(), "cannot retain nullptr");

            if retain_head.is_none() {
                // insert first element
                retain_head = Some(ptr);
                retain_tail = Some(ptr);
            } else {
                // SAFETY tail was set in the other branch
                header(retain_tail.unwrap())
                    .next
                    .store(ptr.as_ptr(), Ordering::Relaxed);
                retain_tail = Some(ptr);
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


                info!("collecting Obj@{:?} {}", to_drop.ptr, {
                    let fmt = format!("{:?}", &to_drop);
                    if fmt.len() > 32 {
                        format!("{:.30}...", fmt)
                    } else {
                        fmt
                    }
                });

                // SAFETY Object behind `current` will can never be used again
                let ObjectVtable { drop, .. } = to_drop.vtable();
                unsafe { drop(to_drop) }
            }
        }

        if let Some(retain_head) = retain_head {
            // some objects were left after collection, insert them back into the alloc list

            // SAFETY reinserting only items from the list which were not dropped
            unsafe {
                self.insert_atomic(retain_head.as_ptr(), &header(retain_tail.unwrap()).next);
            }
        }
    }
}

impl<'parent, 'root> Drop for Alloc<'parent, 'root> {
    /// Deallocates all objects registered to this `Alloc` if it is the root
    /// `Alloc`, or moves it's objects into the parent.
    fn drop(&mut self) {
        if let Some(parent) = self.parent {
            let mut iter = self.take_iter();
            if let Some(first) = iter.next() {
                let last = iter.last().unwrap_or(first);

                // SAFETY reinserting all items from current list into the parent list
                unsafe { parent.insert_atomic(first.as_ptr(), &last.as_ref().next) }
            }
        } else {
            // root Alloc, we can drop all objects
            for object in self.take_iter() {
                // SAFETY `Alloc::alloc` maintains a linked list, ptr either points to a valid
                // object or is null and we checked for null.
                //
                // We use 'static lifetime here because we don't have any name for 'self or
                // similar, this object reference is valid until we drop it here.
                let object: ObjectRefAny<'static> = unsafe { ObjectRefAny::from_ptr(object) };

                let ObjectVtable { drop, .. } = object.vtable();
                // SAFETY Object behind `current` will can never be used again
                unsafe { drop(object) }
            }
        }
    }
}


/// Marks any contained GCd Values
///
/// Usually derived using the [`lipo::Trace`](crate::Trace) derive macro.
///
/// # Safety
/// Manual implementations of `Trace` must mark every GCd `Value<'a>`,
/// `ObjectRefAny<'a>` and `ObjectRef<'a, T>` contained within it. (Note that if
/// the lifetime is `'static` they're _not_ GCd.)
pub unsafe trait Trace {
    fn mark(&self);
}

unsafe impl<'alloc> Trace for ObjectRefAny<'alloc> {
    fn mark(&self) {
        let prev_marked = self.header().mark.swap(true, Ordering::Relaxed);
        if !prev_marked {
            // if the object wasn't marked previously mark recursively all it's children
            let ObjectVtable { mark, .. } = self.vtable();

            // SAFETY we're using this Object's own vtable
            unsafe {
                mark(*self);
            }
        }
    }
}

unsafe impl<'alloc, O: Object> Trace for ObjectRef<'alloc, O> {
    fn mark(&self) {
        let prev_marked = self.upcast().header().mark.swap(true, Ordering::Relaxed);
        if !prev_marked {
            <O as Trace>::mark(&*self)
        }
    }
}

unsafe impl<'alloc> Trace for Value<'alloc> {
    fn mark(&self) {
        if let ValueKind::Object(o) = self.kind() {
            o.mark();
        }
    }
}

// SAFETY Primitives don't own any memory and cannot contain any other
// Values because of memory constraints alone.
unsafe impl<'alloc, P: Primitive<'alloc>> Trace for P {
    fn mark(&self) {
        // nop
    }
}


impl<'parent, 'root> Alloc<'parent, 'root> {
    pub(crate) fn intern_name(&self, string: &str) -> Name<'root> {
        if let Some(parent) = self.parent {
            // we're not root, request the parent allocator to intern
            parent.intern_name(string)
        } else {
            let interner = self.name_interner.get_or_init(Default::default);
            let mut interner = interner.lock().unwrap();
            // SAFETY we're the root Allocator and we're shortening the lifetime to the
            // 'root lifetime and with that ensuring that the returned Name
            // won't outlive the Alloc which owns the NameInterner
            unsafe { interner.intern(string) }
        }
    }
}
