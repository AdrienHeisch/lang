#[derive(Debug, Clone, PartialEq)]
pub enum LangVal
{
    Number(f32),
    Str(String),
    Bool(bool),
    Void
}

impl std::fmt::Display for LangVal
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        match self
        {
            Self::Number(f_) => write!(f, "{}", f_),
            Self::Str(s) => write!(f, "{}", s),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Void => write!(f, "void")
        }
    }
}