use crate::object::{Alloc, Object, ObjectRef, Trace};
use crate::value::Value;
use std::fmt::{self, Debug};
use std::marker::PhantomData;


// Native function arguments
type Args<'stack, 'alloc> = &'stack [Value<'alloc>];
// Native function result
type Result<'alloc> = std::result::Result<Value<'alloc>, NativeError<'alloc>>;

#[derive(Object)]
pub struct NativeFunction {
    name: &'static str,
    fn_impl: for<'alloc> fn(Args<'_, 'alloc>) -> Result<'alloc>,
}

unsafe impl Trace for NativeFunction {
    fn mark(&self) {
        // nop
    }
}

impl NativeFunction {
    pub fn new<'alloc>(
        name: &'static str,
        fn_impl: for<'call_alloc> fn(Args<'_, 'call_alloc>) -> Result<'call_alloc>,
        alloc: &'alloc Alloc,
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


#[derive(Object)]
pub struct NativeError<'alloc> {
    msg: String,
    _todo: PhantomData<&'alloc ()>, // some objects in the future maybe
}

unsafe impl<'alloc> Trace for NativeError<'alloc> {
    fn mark(&self) {
        // nop, for now at least
    }
}

impl<'alloc> Debug for NativeError<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error({})", self.msg)
    }
}
