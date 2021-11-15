use crate::util::Invariant;
use crate::{Alloc, Object, ObjectRef, Trace, Value};
use std::fmt::{self, Debug};
use std::marker::PhantomData;
use std::mem;
use std::sync::atomic::{AtomicU64, Ordering};


#[derive(Object)]
pub struct ConstCell<'alloc> {
    _alloc: PhantomData<Invariant<'alloc>>,
    cell: AtomicU64,
}

const NONE: u64 = 0;

impl<'alloc> ConstCell<'alloc> {
    pub fn new(alloc: &'alloc Alloc) -> ObjectRef<'alloc, ConstCell<'alloc>> {
        alloc.alloc(ConstCell {
            _alloc: PhantomData,
            cell: AtomicU64::new(NONE),
        })
    }

    pub fn get(&self) -> Option<Value<'alloc>> {
        let repr = self.cell.load(Ordering::Acquire);
        unsafe { mem::transmute(repr) }
    }

    pub fn set(&self, value: Value<'alloc>) -> Result<(), Value<'alloc>> {
        let repr = unsafe { mem::transmute(value) };
        self.cell.compare_exchange(NONE, repr, Ordering::AcqRel, Ordering::Acquire)
            .map(|_| ())
            .map_err(|prev| unsafe { mem::transmute(prev) })
    }
}

unsafe impl<'alloc> Trace for ConstCell<'alloc> {
    fn mark(&self) {
        if let Some(val) = self.get() {
            val.mark();
        }
    }
}

impl<'alloc> Debug for ConstCell<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut w = f.debug_tuple("Constant");
        if let Some(val) = self.get() {
            w.field(&val);
        } else {
            w.field(&format_args!("NONE"));
        }
        w.finish()
    }
}
