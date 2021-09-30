use super::{Object, ObjectRef, ObjectRefAny, ObjectVtable, ObjectWrap};
use crate::value::{Value, ValueKind};
use std::marker::PhantomData;
use std::sync::atomic::{AtomicBool, AtomicPtr, Ordering};
use std::{mem, ptr};


/// Header stores the vtable pointer and a intrusive linked list for GC.
pub(crate) struct ObjectHeader {
    /// Virtual method table
    vtable: &'static ObjectVtable,

    /// Next link of intrusive linked free list
    next: AtomicPtr<ObjectHeader>,

    /// GC marker
    ///
    /// Set to `true` during tracing and objects which are left at `false` get dropped.
    mark: AtomicBool,
}

impl ObjectHeader {
    pub(super) fn vtable(&self) -> &'static ObjectVtable {
        self.vtable
    }
}


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
    pub fn alloc<'alloc, O: Object>(&'alloc self, inner: O) -> ObjectRef<'alloc, O> {
        let wrap = Box::new(ObjectWrap {
            header: ObjectHeader {
                vtable: O::__vtable(),
                next: AtomicPtr::new(ptr::null_mut()),
                mark: AtomicBool::new(false),
            },
            inner,
        });
        let obj = ObjectRef {
            alloc: PhantomData, // 'alloc - in return type
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
    pub(super) unsafe fn dealloc<O: Object>(ptr: *mut ObjectWrap<O>) {
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


                log::info!(
                    "collecting Obj@{:?} {}",
                    to_drop.ptr,
                    {
                        let fmt = format!("{:?}", &to_drop);
                        if fmt.len() > 32 {
                            format!("{:.30}...", fmt)
                        } else {
                            fmt
                        }
                    }
                );

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

            // SAFETY we're using this Object's own vtable
            unsafe { mark(*self); }
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
