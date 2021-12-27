use crate::ast::Identifier;
use crate::memory::Pointer; //TODO remove this
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum LangVal //TODO should belong to interp
{
    Int(i32),
    Float(f32),
    // Str(String), //TODO get rid of this
    Bool(bool),
    Obj(Pointer), //TODO objects
    FnPtr(usize),
    Void
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum LangType
{
    Int,
    Float,
    // Str,
    Bool,
    Obj, //TODO objects
    FnPtr,
    Void
}

#[derive(Debug, Clone, PartialEq)]
pub struct LangObj
{
    fields: HashMap<Identifier, LangVal>
}

impl LangVal
{

    pub fn as_type (&self) -> LangType
    {
        match self
        {
            LangVal::Int(_)  => LangType::Int,
            LangVal::Float(_)  => LangType::Float,
            // LangVal::Str(_)     => LangType::Str,
            LangVal::Bool(_)    => LangType::Bool,
            LangVal::Obj(_)     => unimplemented!(),
            LangVal::FnPtr(_)     => unimplemented!(),
            LangVal::Void       => LangType::Void,
        }
    }

}

impl std::fmt::Display for LangVal
{
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        use LangVal::*;
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

impl Default for LangType
{
    fn default () -> Self
    {
        LangType::Void
    }
}