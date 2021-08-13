use crate::object::ObjectRefAny;
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};

#[derive(Clone, Copy)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    Object(ObjectRefAny),
}

impl Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => b.fmt(f),
            Value::Number(n) => n.fmt(f),
            Value::Object(r) => r.fmt(f),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Number(lhs), Value::Number(rhs)) => (lhs - rhs).abs() <= f64::EPSILON,
            (Value::Object(lhs), Value::Object(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            Value::Nil => {}
            Value::Bool(b) => state.write_u8(*b as u8),
            Value::Number(n) => state.write_u64(n.to_bits()),
            Value::Object(o) => o.hash(state),
        }
    }
}
