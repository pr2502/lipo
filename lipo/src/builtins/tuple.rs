use std::fmt::{self, Debug};
use std::ops::Deref;

use crate::builtins::Type;
use crate::value::object::ObjectType;
use crate::{Alloc, Object, ObjectRef, Trace, Value};


#[derive(Object, Trace)]
pub struct Tuple<'alloc> {
    items: Box<[Value<'alloc>]>,
}

impl<'alloc> Tuple<'alloc> {
    pub fn new(
        items: Box<[Value<'alloc>]>,
        alloc: &Alloc<'_, 'alloc>,
    ) -> ObjectRef<'alloc, Tuple<'alloc>> {
        alloc.alloc(Tuple { items })
    }

    pub fn iter(&self) -> impl Iterator<Item = Value<'alloc>> + '_ {
        self.items.iter().copied()
    }
}

impl<'alloc> ObjectType<'alloc> for Tuple<'alloc> {
    fn get_type(&self, alloc: &Alloc<'_, 'alloc>) -> ObjectRef<'alloc, Type<'alloc>> {
        Type::new_tuple(self.iter(), alloc)
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
