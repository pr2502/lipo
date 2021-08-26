use crate::object::ObjectRefAny;


#[derive(Clone, Copy)]
pub(super) enum ValueRepr {
    Nil,
    Bool(bool),
    Number(f64),
    Object(ObjectRefAny),
}

impl super::ValueReprInternal for ValueRepr {
    fn new_nil() -> Self {
        ValueRepr::Nil
    }

    fn new_bool(b: bool) -> Self {
        ValueRepr::Bool(b)
    }

    fn new_number(n: f64) -> Self {
        ValueRepr::Number(n)
    }

    fn new_object(o: ObjectRefAny) -> Self {
        ValueRepr::Object(o)
    }

    fn is_nil(&self) -> bool {
        matches!(self, ValueRepr::Nil)
    }

    fn to_bool(self) -> Option<bool> {
        match self {
            ValueRepr::Bool(b) => Some(b),
            _ => None,
        }
    }

    fn to_number(self) -> Option<f64> {
        match self {
            ValueRepr::Number(n) => Some(n),
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
