use std::mem::size_of;

use crate::ast::{Identifier, IdentifierTools};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Fn(Identifier, Type),
    Pointer(u32, Box<Type>), //TODO u32 or usize ? //TODO remove box
    Array { addr: u32, len: u32, t: Box<Type> }, //TODO remove box
    Int(i32),
    Char(char),
    Float(f32),
    Bool(bool),
    Void,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Fn(Box<[Type]>, Box<Type>), //TODO is this a pointer or a value
    Pointer(Box<Type>),
    Array { len: u32, t: Box<Type> },
    Int,
    Char,
    Float,
    Bool,
    Void,
}

pub struct TypeError {
    pub msg: String,
}

impl Value {
    pub fn as_type(&self) -> Type {
        match self {
            Value::Fn(_, t) => t.clone(),
            Value::Pointer(_, t) => Type::Pointer(t.clone()),
            Value::Array { len, t, .. } => Type::Array {
                len: *len,
                t: t.clone(),
            },
            Value::Int(_) => Type::Int,
            Value::Char(_) => Type::Char,
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
            Fn(_, t) => write!(fmt, "{}", t),
            Pointer(p, t) => write!(fmt, "{} @ {}", t, p),
            Array { addr, len, t } => write!(fmt, "{}[{}] @ {}", t, len, addr),
            Int(i) => write!(fmt, "{}", i),
            Char(c) => write!(fmt, "{}", c),
            Float(f) => write!(fmt, "{}", f),
            Bool(b) => write!(fmt, "{}", b),
            Void => write!(fmt, "void"),
        }
    }
}

macro_rules! type_id_pattern {
    () => {
        b"int\0\0\0\0\0" | b"float\0\0\0" | b"bool\0\0\0\0" | b"char\0\0\0\0"
    };
}
pub(crate) use type_id_pattern;

impl Type {
    pub fn from_identifier(id: &Identifier) -> Result<Self, TypeError> {
        use Type::*;
        Ok(match id {
            b"int\0\0\0\0\0" => Int,
            b"char\0\0\0\0" => Char,
            b"float\0\0\0" => Float,
            b"bool\0\0\0\0" => Bool,
            b"void\0\0\0\0" => Void,
            _ => {
                return Err(TypeError {
                    msg: format!("Invalid type identifier : {}", id.to_string()),
                })
            }
        })
    }

    pub fn to_value(&self) -> Result<Value, TypeError> {
        use Type::*;
        Ok(match self {
            Pointer(t) => Value::Pointer(0, Box::new(*t.clone())),
            Array { len, t } => Value::Array {
                addr: 0,
                len: *len,
                t: Box::new(*t.clone()),
            },
            Int => Value::Int(Default::default()),
            Char => Value::Char(Default::default()),
            Float => Value::Float(Default::default()),
            Bool => Value::Bool(Default::default()),
            Fn(_, _) => {
                return Err(TypeError {
                    msg: "".to_string(),
                })
            }
            Void => Value::Void,
        })
    }

    pub fn get_size(&self) -> usize {
        use Type::*;
        match self {
            Pointer(_) => size_of::<u32>(),
            Array { .. } => size_of::<u32>() * 2,
            Int => size_of::<i32>(),
            Char => size_of::<char>(),
            Float => size_of::<f32>(),
            Bool => size_of::<bool>(),
            Fn(_, _) => size_of::<Identifier>(),
            Void => 0,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Type::*;
        match self {
            Pointer(t) => write!(fmt, "{}*", t),
            Array { len, t } => write!(fmt, "{}[{}]", t, len),
            _ => write!(fmt, "{:?}", self),
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Self::Void
    }
}
