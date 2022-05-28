use crate::ast::{Identifier, IdentifierTools};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i32),
    Float(f32),
    Bool(bool),
    Void,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Fn_, //TODO typed functions
    // Fn(Box<[Type]>, Type),
    Void,
}

impl Value {
    pub fn as_type(&self) -> Type {
        match self {
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
            Int(i) => write!(fmt, "{}", i),
            Float(f) => write!(fmt, "{}", f),
            Bool(b) => write!(fmt, "{}", b),
            Void => write!(fmt, "void"),
        }
    }
}

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

    pub fn get_size(&self) -> usize {
        use Type::*;
        match self {
            Int => 4,
            Float => 4,
            Bool => 1,
            Fn_ => panic!(),
            Void => 0,
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Type::Void
    }
}
