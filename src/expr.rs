use crate::op::Op;

//Use with an arena
#[derive(Debug, Clone)]
pub enum Expr<'a>
{
    Const   (crate::cst::Const),
    Id      (String), //TODO replace string with a numeric id
    Var     (String, &'a Expr<'a>), //TODO shouldn't this string be a Expr::Id ?
    UnOp    (Op, &'a Expr<'a>),
    BinOp   (Op, bool, &'a Expr<'a>, &'a Expr<'a>),
    Parent  (&'a Expr<'a>),
    Call    (&'a Expr<'a>, Vec<&'a Expr<'a>>),
    Block   (Vec<&'a Expr<'a>>),
    If      (&'a Expr<'a>, &'a Expr<'a>),
    While   (&'a Expr<'a>, &'a Expr<'a>),
    End
}