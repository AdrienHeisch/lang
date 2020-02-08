#[derive(Debug, Clone)]
pub enum Expr<'a>
{
    Const(crate::cst::Const),
    Id(String),
    Var(String, &'a Expr<'a>),
    UnOp(String, &'a Expr<'a>),
    BinOp(String, &'a Expr<'a>, &'a Expr<'a>),
    Parent(&'a Expr<'a>),
    Call(&'a Expr<'a>, Vec<&'a Expr<'a>>),
    Block(Vec<&'a Expr<'a>>),
    If(&'a Expr<'a>, &'a Expr<'a>),
    While(&'a Expr<'a>, &'a Expr<'a>),
    End
}