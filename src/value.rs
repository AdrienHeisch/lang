#[derive(Debug, Clone, PartialEq)]
pub enum Value //TODO should belong to interp
{
    Int(i32),
    Float(f32),
    Bool(bool),
    Void
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Type
{
    Int,
    Float,
    Bool,
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
            Value::Bool(_)    => Type::Bool,
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
            Bool(b) => write!(fmt, "{}", b),
            Void => write!(fmt, "void")
        }
    }
}

impl Type {

    pub fn get_size (&self) -> usize {
        match self {
            Type::Int => 4,
            Type::Float => 4,
            Type::Bool => 1,
            Type::Void => 0
        }
    }

}

impl Default for Type
{
    fn default () -> Self
    {
        Type::Void
    }
}