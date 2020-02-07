#[derive(Debug, Clone, PartialEq)]
pub enum Const
{
    Number(f32),
    Str(String),
    Bool(bool),
    Void
}

impl std::fmt::Display for Const
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        match self
        {
            Const::Number(f_) => write!(f, "{}", f_),
            Const::Str(s) => write!(f, "{}", s),
            Const::Bool(b) => write!(f, "{}", b),
            Const::Void => write!(f, "{}", "void")
        }
    }
}