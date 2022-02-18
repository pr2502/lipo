use std::fmt::{self, Debug};
use std::ptr;

use crate::builtins::{Record, Tuple};
use crate::name::Name;
use crate::value::object::{ObjectRef, ObjectVtable};
use crate::value::primitive::Primitive;
use crate::value::TypeTag;
use crate::{Alloc, Object, Trace, Value};


// TODO parametrization
//
// Some types like `List` for example can be used with or without a type
// parameter. `List` without a parameter is the same as `List(Any)` with the
// exception that `List` on its own can be called like a function and further
// parametrized while `List(Any)` is already fully parametrized.
//
// In tye `Type` itself we probably want to store an optional `Function` that
// will accept a list of type parameters and return another `Type` with the
// parameters embedded and likely without the parametrization function.
#[derive(Object, Trace)]
pub struct Type<'alloc> {
    ty: Ty<'alloc>,
}

// TODO paramatrized types
#[derive(Trace)]
enum Ty<'alloc> {
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
    Tuple(Box<[ObjectRef<'alloc, Type<'alloc>>]>),

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
    Record(Box<[Name<'alloc>]>, Box<[ObjectRef<'alloc, Type<'alloc>>]>),

    /// A set of types
    ///
    /// Type T is a subtype of self if it is a subtype of any of the type in
    /// self.
    ///
    /// They're constructed in the language using the type or operator `|`:
    /// ```lipo
    /// type MyType = Int | String;
    /// ```
    Sum(TypeSet<'alloc>),
}

#[derive(Trace, Clone)]
pub struct TypeSet<'alloc> {
    types: Box<[ObjectRef<'alloc, Type<'alloc>>]>,
}

fn type_set_add<'alloc>(
    list: Box<[ObjectRef<'alloc, Type<'alloc>>]>,
    ty: ObjectRef<'alloc, Type<'alloc>>,
) -> Box<[ObjectRef<'alloc, Type<'alloc>>]> {
    if list.iter().any(|item| Type::is_subtype(*item, ty)) {
        list
    } else {
        let mut v = list.to_vec();
        v.push(ty);
        v.into_boxed_slice()
    }
}

impl<'alloc> Type<'alloc> {
    fn new(ty: Ty<'alloc>, alloc: &Alloc<'_, 'alloc>) -> ObjectRef<'alloc, Type<'alloc>> {
        alloc.alloc(Type { ty })
    }

    pub(crate) fn new_primitive(
        type_tag: TypeTag,
        alloc: &Alloc<'_, 'alloc>,
    ) -> ObjectRef<'alloc, Type<'alloc>> {
        Type::new(Ty::Primitive(type_tag), alloc)
    }

    pub(crate) fn new_object<O: Object>(
        alloc: &Alloc<'_, 'alloc>,
    ) -> ObjectRef<'alloc, Type<'alloc>> {
        Type::new(Ty::Object(O::__vtable()), alloc)
    }

    pub(crate) fn new_tuple(
        elems: impl Iterator<Item = Value<'alloc>>,
        alloc: &Alloc<'_, 'alloc>,
    ) -> ObjectRef<'alloc, Type<'alloc>> {
        let types = elems.map(|e| e.get_type(alloc)).collect();
        Type::new(Ty::Tuple(types), alloc)
    }

    pub(crate) fn new_record(
        elems: impl Iterator<Item = (Name<'alloc>, Value<'alloc>)>,
        alloc: &Alloc<'_, 'alloc>,
    ) -> ObjectRef<'alloc, Type<'alloc>> {
        let mut keys = Vec::new();
        let mut vals = Vec::new();
        for (key, val) in elems {
            keys.push(key);
            vals.push(val.get_type(alloc));
        }
        Type::new(
            Ty::Record(keys.into_boxed_slice(), vals.into_boxed_slice()),
            alloc,
        )
    }

    /// Converts convertible Value into a Type.
    ///
    /// Convertible Values are:
    /// - Type
    /// - ()
    /// - Tuple of convertible Values
    /// - Record of convertible Values
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
        if let Some(tuple) = value.downcast::<Tuple>() {
            let tuple = tuple
                .iter()
                .map(|value| Type::from_value(value, alloc))
                .collect();
            return Type::new(Ty::Tuple(tuple), alloc);
        }
        if let Some(record) = value.downcast::<Record>() {
            let keys = record.keys().collect();
            let vals = record
                .values()
                .map(|value| Type::from_value(value, alloc))
                .collect();
            return Type::new(Ty::Record(keys, vals), alloc);
        }
        todo!("type error: value cannot be converted to Type")
    }

    pub fn or(
        self: ObjectRef<'alloc, Self>,
        other: ObjectRef<'alloc, Type<'alloc>>,
        alloc: &Alloc<'_, 'alloc>,
    ) -> ObjectRef<'alloc, Type<'alloc>> {
        match (&self.ty, &other.ty) {
            // `Any | T` always results in `Any`
            (Ty::Any, _) | (_, Ty::Any) => Type::new(Ty::Any, alloc),

            // Set intersection
            (Ty::Sum(set1), Ty::Sum(set2)) => {
                let types = set1.types.clone();
                let types = set2
                    .types
                    .iter()
                    .cloned()
                    .fold(types, |acc, ty| type_set_add(acc, ty));
                Type::new(Ty::Sum(TypeSet { types }), alloc)
            },

            // Add one element to a set
            (Ty::Sum(set), _) => {
                let types = type_set_add(set.types.clone(), other);
                Type::new(Ty::Sum(TypeSet { types }), alloc)
            },
            (_, Ty::Sum(set)) => {
                let types = type_set_add(set.types.clone(), self);
                Type::new(Ty::Sum(TypeSet { types }), alloc)
            },

            // If one type is a subtype of the other, return the supertype
            _ if Type::is_subtype(self, other) => other,
            _ if Type::is_subtype(other, self) => self,

            // For to basic (non-sum) types, create a new set with two elements
            _ => {
                let types = Box::from([self, other]);
                Type::new(Ty::Sum(TypeSet { types }), alloc)
            },
        }
    }

    pub fn is_subtype(
        self: ObjectRef<'alloc, Type<'alloc>>,
        of: ObjectRef<'alloc, Type<'alloc>>,
    ) -> bool {
        fn recur<'alloc>(
            (lhs, rhs): (
                &ObjectRef<'alloc, Type<'alloc>>,
                &ObjectRef<'alloc, Type<'alloc>>,
            ),
        ) -> bool {
            match (&lhs.ty, &rhs.ty) {
                // `T :: Any` is always true
                (_, Ty::Any) => true,

                (Ty::Primitive(lhs), Ty::Primitive(rhs)) => lhs == rhs,

                (Ty::Object(lhs), Ty::Object(rhs)) => {
                    // ObjectVtable references must be unique for each `Object` type
                    ptr::eq(lhs, rhs)
                },

                // Tuples must match exactly in length
                (Ty::Tuple(lhs), Ty::Tuple(rhs)) => {
                    lhs.len() == rhs.len() && lhs.iter().zip(rhs.iter()).all(recur)
                },

                // Record fields must match exactly
                (Ty::Record(lhs1, lhs2), Ty::Record(rhs1, rhs2)) => {
                    lhs1.len() == rhs1.len()
                        && lhs1.iter().zip(rhs1.iter()).all(|(lhs, rhs)| lhs == rhs)
                        && lhs2.iter().zip(rhs2.iter()).all(recur)
                },

                // T is a subtype of a TypeSet if it's a subtype of any of its elements
                (_, Ty::Sum(set)) => set.types.iter().any(|rhs| recur((lhs, rhs))),

                _ => false,
            }
        }

        recur((&self, &of))
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
            Ty::Sum(set) => {
                f.write_str("Sum")?;
                f.debug_set().entries(set.types.iter()).finish()
            },
        }
    }
}
