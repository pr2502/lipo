use std::fmt::{self, Debug};
use std::marker::PhantomData;

use lipo_macro::{Object, Trace};

use crate::name::Name;
use crate::value::object::{ObjectRef, ObjectVtable};
use crate::value::primitive::Primitive;
use crate::value::TypeTag;
use crate::{Alloc, Value};

#[derive(Object, Trace)]
pub struct Type<'alloc> {
    // prevent `Type` from being constructed outside this module when all other fields are public
    _non_exhaustive: PhantomData<()>,

    pub ty: Ty<'alloc>,
    // TODO parametrization
    //
    // Some types like `List` for example can be used with or without a type parameter.
    // `List` without a parameter is the same as `List(Any)` with the exception that `List` on its
    // own can be called like a function and further parametrized while `List(Any)` is already
    // fully parametrized.
    //
    // In tye `Type` itself we probably want to store an optional `Function` that will accept a
    // list of type parameters and return another `Type` with the parameters embedded and likely
    // without the parametrization function.
}

// TODO paramatrized types
// TODO sum types
#[derive(Trace)]
pub enum Ty<'alloc> {
    /// wildcard type, every type including itself is a subtype of Any
    /// `assert T :: Any;` will be true for any Type T
    Any,

    /// Builtin primitive types carry a unique TypeTag which can be used to
    /// index the Primitive vtables
    Primitive(TypeTag),

    /// Objects have their own associated vtables
    Object(&'static ObjectVtable),

    /// Tuple of `Type`s
    ///
    /// They are constructed in the language using tuples:
    /// ```lipo
    /// type MyType = (Int, Float, String);
    /// ```
    Tuple(Box<[Ty<'alloc>]>),

    /// Record of `Type`s
    ///
    /// They are constructed in the language using records:
    /// ```lipo
    /// type MyType = {
    ///     a: Int,
    ///     b: Float,
    ///     c: String,
    /// };
    /// ```
    Record(Box<[Name<'alloc>]>, Box<[Ty<'alloc>]>),
}

impl<'alloc> Type<'alloc> {
    pub fn new(ty: Ty<'alloc>, alloc: &Alloc<'_, 'alloc>) -> ObjectRef<'alloc, Type<'alloc>> {
        alloc.alloc(Type { _non_exhaustive: PhantomData, ty })
    }

    pub fn from_value(
        value: Value<'alloc>,
        alloc: &Alloc<'_, 'alloc>,
    ) -> ObjectRef<'alloc, Type<'alloc>> {
        if let Some(ty) = value.downcast::<Type>() {
            return ty;
        }
        if value.is::<()>() {
            return Type::new(Ty::Primitive(<() as Primitive>::TYPE_TAG), alloc);
        }
        todo!("more complex type conversions")
    }
}

impl<'alloc> Debug for Type<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Type::")?;
        self.ty.fmt(f)
    }
}

impl<'alloc> Debug for Ty<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Any => f.write_str("Any"),
            Ty::Primitive(type_tag) => f.write_fmt(format_args!("Primitive({type_tag:?})")),
            Ty::Object(vtable) => {
                let name = vtable.typename;
                f.write_fmt(format_args!("Object({name})"))
            },
            Ty::Tuple(types) => {
                f.write_str("Tuple")?;
                f.debug_list().entries(types.iter()).finish()
            },
            Ty::Record(names, types) => {
                f.write_str("Record")?;
                f.debug_map()
                    .entries(names.iter().zip(types.iter()))
                    .finish()
            },
        }
    }
}
