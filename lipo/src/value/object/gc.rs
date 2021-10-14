use super::{Object, ObjectRef, ObjectRefAny, ObjectVtable, ObjectWrap};
use crate::name::{Name, NameInterner};
use crate::value::{Value, ValueKind};
use crate::Primitive;
use std::lazy::SyncOnceCell;
use std::marker::PhantomData;
use std::ptr::{self, NonNull};
use std::sync::atomic::{AtomicBool, AtomicPtr, Ordering};
use std::sync::Mutex;
use tracing::info;


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
    alloc_list: AtomicPtr<ObjectHeader>,
    name_interner: SyncOnceCell<Mutex<NameInterner>>,
}

impl Alloc {
    /// Create a new empty `Alloc`.
    pub const fn new() -> Alloc {
        Alloc {
            alloc_list: AtomicPtr::new(ptr::null_mut()),
            name_interner: SyncOnceCell::new(),
        }
    }

    /// Take the current head of the alloc list and return an `Iterator` over all it's
    /// [`ObjectHeader`]s.
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
        let mut old_head = self.alloc_list.load(Ordering::Acquire);

        loop {
            // The next pointer is already synchronized by the Release-Acquire pair in the exchange
            // here and in the swap in [`take_iter`].
            next.store(old_head, Ordering::Relaxed);

            // The Release part of the AcqRel on success pairs with either the Acquire on failure,
            // the Acquire when inserting the next item(s) or with the Acquire in [`take_iter`].
            match self.alloc_list.compare_exchange_weak(old_head, ptr, Ordering::AcqRel, Ordering::Acquire) {
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
            // SAFETY Box::into_raw returns a NonNull pointer
            ptr: unsafe { NonNull::new_unchecked(Box::into_raw(wrap)) }
        };
        // SAFETY ptr is valid because we obtained it from safe ObjectRef belonging to this Alloc
        unsafe { self.insert_atomic(obj.upcast().ptr.as_ptr(), &obj.upcast().header().next); }
        obj
    }

    /// Deallocate a previously allocated object
    ///
    /// # Safety
    /// `ptr` must originate from a previous call to `Alloc::alloc` on the same `Alloc` instance.
    pub(super) unsafe fn dealloc<O: Object>(ptr: NonNull<ObjectWrap<O>>) {
        // SAFETY ptr was created from a `Box<ObjectWrap<O>>` in `Alloc::alloc`
        let _drop = unsafe { Box::from_raw(ptr.as_ptr()) };
    }

    /// Collect unmarked objects
    ///
    /// # Safety
    /// Every reachable object must have been marked prior to calling this function. This function
    /// will unmark objects as it traverses the heap so objects have to be re-marked before every
    /// call to `sweep`.
    pub unsafe fn sweep(&self) {
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
                header(retain_tail.unwrap()).next.store(ptr.as_ptr(), Ordering::Relaxed);
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


                info!(
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

        if let Some(retain_head) = retain_head {
            // some objects were left after collection, insert them back into the alloc list

            // SAFETY reinserting only items from the list which were not dropped
            unsafe { self.insert_atomic(retain_head.as_ptr(), &header(retain_tail.unwrap()).next); }
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

unsafe impl<'alloc, P: Primitive<'alloc>> Trace for P {
    fn mark(&self) {
        // nop
    }
}


impl Alloc {
    pub(crate) fn intern_name<'alloc>(&'alloc self, string: &str) -> Name<'alloc> {
        let interner = self.name_interner.get_or_init(Default::default);
        let mut interner = interner.lock().unwrap();
        // SAFETY we're shortening the lifetime to the 'alloc lifetime and with that ensuring that
        // the returned Name won't outlive the Alloc which owns the NameInterner
        unsafe {
            interner.intern(string)
        }
    }
}
