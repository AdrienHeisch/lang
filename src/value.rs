use std::mem::size_of;

use crate::ast::{Identifier, IdentifierTools};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Pointer(u32, Box<Type>), //TODO u32 or usize ?
    Array { addr: u32, len: u32, t: Box<Type> },
    Int(i32),
    Float(f32),
    Bool(bool),
    Void,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Pointer(Box<Type>),
    Array { len: u32, t: Box<Type> },
    Int,
    Float,
    Bool,
    Fn(Box<[Type]>, Box<Type>),
    Void,
}

impl Value {
    pub fn as_type(&self) -> Type {
        match self {
            Value::Pointer(_, t) => Type::Pointer(t.clone()),
            Value::Array { len, t, .. } => Type::Array {
                len: *len,
                t: t.clone(),
            },
            Value::Int(_) => Type::Int,
            Value::Float(_) => Type::Float,
            Value::Bool(_) => Type::Bool,
            Value::Void => Type::Void,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;
        match self {
            Pointer(p, t) => write!(fmt, "{} @ {}", t, p),
            Array { addr, len, t } => write!(fmt, "{} & {}[{}]", t, addr, len),
            Int(i) => write!(fmt, "{}", i),
            Float(f) => write!(fmt, "{}", f),
            Bool(b) => write!(fmt, "{}", b),
            Void => write!(fmt, "void"),
        }
    }
}

macro_rules! type_id_pattern {
    () => {
        b"int\0\0\0\0\0" | b"float\0\0\0" | b"bool\0\0\0\0"
    };
}
pub(crate) use type_id_pattern;

impl Type {
    pub fn from_identifier(id: &Identifier) -> Self {
        use Type::*;
        match id {
            b"int\0\0\0\0\0" => Int,
            b"float\0\0\0" => Float,
            b"bool\0\0\0\0" => Bool,
            b"void\0\0\0\0" => Void,
            _ => panic!("Invalid type identifier : {}", id.to_string()),
        }
    }

    pub fn from_identifier_ptr(id: &Identifier) -> Self {
        Self::Pointer(Box::new(Self::from_identifier(id)))
    }

    pub fn to_value(&self) -> Value {
        use Type::*;
        match self {
            Pointer(t) => Value::Pointer(0, Box::new(*t.clone())),
            Array { len, t } => Value::Array {
                addr: 0,
                len: *len,
                t: Box::new(*t.clone()),
            },
            Int => Value::Int(Default::default()),
            Float => Value::Float(Default::default()),
            Bool => Value::Bool(Default::default()),
            Fn(_, _) => panic!(),
            Void => Value::Void,
        }
    }

    pub fn get_size(&self) -> usize {
        use Type::*;
        match self {
            Pointer(_) => size_of::<u32>(),
            Array { .. } => size_of::<u32>() * 2,
            Int => size_of::<i32>(),
            Float => size_of::<f32>(),
            Bool => size_of::<bool>(),
            Fn(_, _) => panic!(),
            Void => 0,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Type::*;
        match self {
            Pointer(t) => write!(fmt, "{:?}*", t),
            Array { len, t } => write!(fmt, "{:?}[{}]", t, len),
            _ => write!(fmt, "{:?}", self),
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Self::Void
    }
}
