use crate::op::Op;

//Use with an arena
#[derive(Debug, Clone)]
pub enum Expr<'a>
{
    Const   (crate::cst::Const),
    Id      (String),
    Var     (String, &'a Expr<'a>),
    UnOp    (Op, &'a Expr<'a>),
    BinOp   (Op, &'a Expr<'a>, &'a Expr<'a>),
    Parent  (&'a Expr<'a>),
    Call    (&'a Expr<'a>, Vec<&'a Expr<'a>>),
    Block   (Vec<&'a Expr<'a>>),
    If      (&'a Expr<'a>, &'a Expr<'a>),
    While   (&'a Expr<'a>, &'a Expr<'a>),
    End
}