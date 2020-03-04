use crate::ast::Identifier;
use crate::memory::Pointer;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum LangVal
{
    Number(f32),
    Str(String), //TODO get rid of this
    Bool(bool),
    #[allow(dead_code)]
    Obj(Pointer), //TODO objects
    Void
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum LangType
{
    Number,
    Str,
    Bool,
    #[allow(dead_code)]
    Obj, //TODO objects
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
            LangVal::Number(_)  => LangType::Number,
            LangVal::Str(_)     => LangType::Str,
            LangVal::Bool(_)    => LangType::Bool,
            LangVal::Obj(_)     => unimplemented!(),
            LangVal::Void       => LangType::Void,
        }
    }

}

impl std::fmt::Display for LangVal
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        use LangVal::*;
        match self
        {
            Number(f_) => write!(f, "{}", f_),
            Str(s) => write!(f, "{}", s),
            Bool(b) => write!(f, "{}", b),
            Obj(o) => write!(f, "{:?}", o),
            Void => write!(f, "void")
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