use std::fmt::{self, Debug};
use std::marker::PhantomData;

use crate::{Alloc, Object, ObjectRef, Trace, Value};


// Native function arguments
type Args<'stack, 'alloc> = &'stack [Value<'alloc>];
// Native function result
type Result<'alloc> = std::result::Result<Value<'alloc>, NativeError<'alloc>>;


#[derive(Object, Trace)]
pub struct NativeFunction {
    name: &'static str,
    fn_impl: for<'alloc> fn(Args<'_, 'alloc>) -> Result<'alloc>,
}

impl NativeFunction {
    pub fn new<'alloc>(
        name: &'static str,
        fn_impl: for<'call_alloc> fn(Args<'_, 'call_alloc>) -> Result<'call_alloc>,
        alloc: &Alloc<'_, 'alloc>,
    ) -> ObjectRef<'alloc, NativeFunction> {
        alloc.alloc(NativeFunction { name, fn_impl })
    }
}

impl Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NativeFunction({})", self.name)
    }
}

impl NativeFunction {
    pub fn call<'alloc>(&self, args: Args<'_, 'alloc>) -> Result<'alloc> {
        (self.fn_impl)(args)
    }
}


#[derive(Object, Trace)]
pub struct NativeError<'alloc> {
    pub(crate) msg: String,
    _todo: PhantomData<Value<'alloc>>, // some objects in the future maybe
}

impl<'alloc> Debug for NativeError<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error({})", self.msg)
    }
}
