use std::fmt::{self, Debug};
use std::ops::Deref;

use crate::{Alloc, Object, ObjectRef, Trace, Value};


#[derive(Object, Trace)]
pub struct Tuple<'alloc> {
    items: Box<[Value<'alloc>]>,
}

impl<'alloc> Tuple<'alloc> {
    pub fn new(
        items: Box<[Value<'alloc>]>,
        alloc: &'alloc Alloc,
    ) -> ObjectRef<'alloc, Tuple<'alloc>> {
        alloc.alloc(Tuple { items })
    }
}

impl<'alloc> Debug for Tuple<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <[Value] as Debug>::fmt(self, f)
    }
}

impl<'alloc> Deref for Tuple<'alloc> {
    type Target = [Value<'alloc>];
    fn deref(&self) -> &Self::Target {
        &self.items
    }
}
