use std::fmt::{self, Debug};

use crate::name::Name;
use crate::{Alloc, Object, ObjectRef, Trace, Value};


#[derive(Object, Trace)]
pub struct Record<'alloc> {
    // TODO PERF we want to keep both of these arrays in a single allocated block, but Rust doesn't
    // support DSTs like ([u64],[u64]) so we'll have to cheat
    keys: Box<[Name<'alloc>]>,
    vals: Box<[Value<'alloc>]>,
}

impl<'alloc> Record<'alloc> {
    pub fn new(
        items: Vec<(Name<'alloc>, Value<'alloc>)>,
        alloc: &Alloc<'_, 'alloc>,
    ) -> ObjectRef<'alloc, Record<'alloc>> {
        let mut items = items;
        items.sort_unstable_by_key(|(k, _v)| *k);

        let mut keys = Vec::new();
        let mut vals = Vec::new();
        keys.reserve_exact(items.len());
        vals.reserve_exact(items.len());
        items.into_iter().for_each(|(k, v)| {
            keys.push(k);
            vals.push(v);
        });
        let keys = keys.into_boxed_slice();
        let vals = vals.into_boxed_slice();

        alloc.alloc(Record { keys, vals })
    }

    /// Constructs a Record from a pre-sorted slice of items and keys.
    ///
    /// This is used by the `OpCode::Record` instruction to make constructing
    /// Records as cheap as possible.
    ///
    /// - the slice length must be a multiple of 2,
    /// - the first half must only contain `Name` keys
    /// - keys are sorderd,
    /// - for key at index `i` its value is at index `i + len/2`
    /// - the number of keys must not exceed [`u8::MAX`]
    ///
    /// These invariants are only checked with `debug_assertions`, they should
    /// be enforced by [`ChunkBuf::check`].
    ///
    /// # Safety
    /// First half of the slice must be all `Name`s, the cast is unchecked
    /// without `debug_assertions`.
    pub(crate) unsafe fn new_from_sorted(
        keys_vals: &[Value<'alloc>],
        alloc: &Alloc<'_, 'alloc>,
    ) -> ObjectRef<'alloc, Record<'alloc>> {
        let n = keys_vals.len() / 2;

        debug_assert!(keys_vals.len() % 2 == 0);
        debug_assert!(n <= u8::MAX as usize);
        debug_assert!(keys_vals[..n].is_sorted_by_key(|v| v.downcast::<Name>().unwrap()));

        let keys = keys_vals[..n]
            .iter()
            .map(|key| match key.downcast::<Name>() {
                Some(key) => key,
                None => {
                    // SAFETY the caller ensures first half of `keys_vals` slice only contains
                    // `Name`s
                    unsafe { std::hint::unreachable_unchecked() }
                },
            })
            .collect();
        let vals = Box::from(&keys_vals[n..]);

        alloc.alloc(Record { keys, vals })
    }

    pub fn get(&self, name: Name<'alloc>) -> Option<Value<'alloc>> {
        debug_assert!(self.keys.len() == self.vals.len());
        let idx = self.keys.iter().position(|k| *k == name)?;
        // SAFETY self.keys and self.vals always have the same length
        Some(unsafe { *self.vals.get_unchecked(idx) })
    }
}

impl<'alloc> Debug for Record<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut w = f.debug_map();
        for (key, val) in self.keys.iter().zip(self.vals.iter()) {
            w.entry(&key, &val);
        }
        w.finish()
    }
}
