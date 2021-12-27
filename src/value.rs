use crate::ast::Identifier;
use crate::memory::Pointer; //TODO remove this
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Value //TODO should belong to interp
{
    Int(i32),
    Float(f32),
    // Str(String), //TODO get rid of this
    Bool(bool),
    Obj(Pointer), //TODO objects
    FnPtr(usize),
    Void
}

impl Value
{

    pub fn as_type (&self) -> Type
    {
        match self
        {
            Value::Int(_)     => Type::Int,
            Value::Float(_)   => Type::Float,
            // LangVal::Str(_)     => LangType::Str,
            Value::Bool(_)    => Type::Bool,
            Value::Obj(_)     => unimplemented!(),
            Value::FnPtr(_)   => unimplemented!(),
            Value::Void       => Type::Void,
        }
    }

}

impl std::fmt::Display for Value
{
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        use Value::*;
        match self
        {
            Int(i) => write!(fmt, "{}", i),
            Float(f) => write!(fmt, "{}", f),
            // Str(s) => write!(f, "{}", s),
            Bool(b) => write!(fmt, "{}", b),
            Obj(o) => write!(fmt, "{:?}", o),
            FnPtr(p) => write!(fmt, "{:?}", p),
            Void => write!(fmt, "void")
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Type
{
    Int,
    Float,
    // Str,
    Bool,
    Obj, //TODO objects
    FnPtr,
    Void
}

impl Default for Type
{
    fn default () -> Self
    {
        Type::Void
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LangObj
{
    fields: HashMap<Identifier, Value>
}