use crate::object::ObjectRefAny;


#[derive(Clone, Copy)]
pub(super) enum ValueRepr {
    Unit,
    Bool(bool),
    Float(f64),
    Object(ObjectRefAny),
}

impl super::ValueReprInternal for ValueRepr {
    fn new_unit() -> Self {
        ValueRepr::Unit
    }

    fn new_bool(b: bool) -> Self {
        ValueRepr::Bool(b)
    }

    fn new_float(n: f64) -> Self {
        ValueRepr::Float(n)
    }

    fn new_object(o: ObjectRefAny) -> Self {
        ValueRepr::Object(o)
    }

    fn is_unit(&self) -> bool {
        matches!(self, ValueRepr::Unit)
    }

    fn to_bool(self) -> Option<bool> {
        match self {
            ValueRepr::Bool(b) => Some(b),
            _ => None,
        }
    }

    fn to_float(self) -> Option<f64> {
        match self {
            ValueRepr::Float(n) => Some(n),
            _ => None,
        }
    }

    fn to_object(self) -> Option<ObjectRefAny> {
        match self {
            ValueRepr::Object(o) => Some(o),
            _ => None,
        }
    }
}
