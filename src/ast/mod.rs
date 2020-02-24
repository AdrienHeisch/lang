mod parser;
mod lexer;

use typed_arena::Arena;

pub struct Ast<'e>
{
    #[allow(dead_code)] // This is where all the Expr instances live, they are accessed using top_level
    arena: Arena<Expr<'e>>,
    top_level: &'e Expr<'e>
}

impl<'e> Ast<'e>
{

    pub fn from_str (program:&str) -> Self
    {
        let arena = Arena::new();
        let top_level;
        unsafe {
            let arena_ref = &*(&arena as *const Arena<Expr>);
            top_level = parser::parse(arena_ref, &lexer::lex(program));
        }
        Self
        {
            arena,
            top_level
        }
    }

    pub fn get_top_level (&self) -> &Expr
    {
        self.top_level
    }

}

// #region EXPR
#[derive(Debug, Clone)]
pub enum Expr<'a>
{
    Const   (Const),
    Id      (String), //TODO replace string with a numeric id
    Var     (String, &'a Expr<'a>), //TODO shouldn't this string be a Expr::Id ?
    UnOp    (Op, &'a Expr<'a>),
    BinOp   (Op, bool, &'a Expr<'a>, &'a Expr<'a>),
    Parent  (&'a Expr<'a>),
    Call    (&'a Expr<'a>, Vec<&'a Expr<'a>>),
    Block   (Vec<&'a Expr<'a>>),
    If      (&'a Expr<'a>, &'a Expr<'a>, Option<&'a Expr<'a>>),
    While   (&'a Expr<'a>, &'a Expr<'a>),
    End
}

impl<'a> Expr<'a>
{

    pub fn is_block (&self) -> bool
    {
        match self
        {
            Expr::Block(_) => true,
            Expr::If(_, expr, _) => Self::is_block(expr),
            Expr::While(_, expr) => Self::is_block(expr),
            _ => false
        }
    }

}
// #endregion

// #region CONST
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
            Const::Void => write!(f, "void")
        }
    }
}
// #endregion

// #region OP
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op
{
    Not,
    Add,
    Mult,
    Div,
    Sub,
    Mod,
    Equal,
    NotEqual,
    Gt,
    Gte,
    Lt,
    Lte,
    BoolAnd,
    BoolOr,
    Assign
}

impl Op
{

    pub fn from_string (string:&str) -> (Op, bool)
    {
        let mut is_assign = false;
        let op = match string
        {
            "!" =>  Op::Not,
            "==" => Op::Equal,
            "!=" => Op::NotEqual,
            ">" =>  Op::Gt,
            ">=" => Op::Gte,
            "<" =>  Op::Lt,
            "<=" => Op::Lte,
            "&&" => Op::BoolAnd,
            "||" => Op::BoolOr,
            "=" =>  Op::Assign,
            //Check if this is an assign operator other than "="
            op if op.ends_with('=') => {
                is_assign = true;
                Op::from_string(&op[0..(op.len() - 1)]).0
            },
            //All operators below can have an assign version like "+="
            "%" =>  Op::Mod,
            "+" =>  Op::Add,
            "-" =>  Op::Sub,
            "*" =>  Op::Mult,
            "/" =>  Op::Div,
            op => {
                eprintln!("Invalid unop : {}", op);
                panic!();
            }
        };

        (op, is_assign)
    }

    pub fn priority (self) -> u8
    {
        match self
        {
            Op::Not => 0,
            Op::Mod => 1,
            Op::Mult => 2,
            Op::Div => 2,
            Op::Add => 3,
            Op::Sub => 3,
            Op::Equal => 4,
            Op::NotEqual => 4,
            Op::Gt => 4,
            Op::Gte => 4,
            Op::Lt => 4,
            Op::Lte => 4,
            Op::BoolAnd => 5,
            Op::BoolOr => 6,
            Op::Assign => 7
        }
    }

}
// #endregion

// #region TOKEN
#[derive(Debug, PartialEq)]
pub enum Token<'a>
{
    Id(&'a str),
    Const(Const), //String ?
    Op(Op, bool),
    DelimOpen(Delimiter),
    DelimClose(Delimiter),
    Comma,
    Semicolon,
    Eof,
    Nil
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Delimiter
{
    Pr,
    Br,
    SqBr
}
// #endregion


// #[cfg(test)]
#[allow(dead_code)]
pub mod benchmarks
{
    use crate::BENCHMARK_ITERATIONS as ITERATIONS;
    use super::{ lexer, parser };
    use std::time::Instant;

    pub fn benchmark_lexer ()
    {
        let program = std::fs::read_to_string("./code.lang").unwrap();
        let now = Instant::now();
        for _ in 0..ITERATIONS { lexer::lex(&program); }
        println!("Lexing: {}ms", now.elapsed().as_millis());
    }

    pub fn benchmark_parser ()
    {
        let program = std::fs::read_to_string("./code.lang").unwrap();
        let tokens = lexer::lex(&program);
        let expr_arena = typed_arena::Arena::new();
        let now = Instant::now();
        for _ in 0..ITERATIONS { parser::parse(&expr_arena, &tokens); }
        println!("Parsing: {}ms", now.elapsed().as_millis());
    }

}