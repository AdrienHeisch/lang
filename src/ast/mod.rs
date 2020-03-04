// mod analyzer;
mod parser;
mod lexer;

use crate::langval::{ LangVal };
use std::collections::VecDeque;
use typed_arena::Arena;

pub struct Ast<'e>
{
    #[allow(dead_code)] // This is where all the Expr instances live, they are accessed using top_level
    arena: Arena<Expr<'e>>,
    top_level: Vec<&'e Expr<'e>>
}

impl<'e> Ast<'e>
{

    pub fn new (tokens:&VecDeque<Token>) -> Self
    {
        let arena = Arena::new();
        let top_level;
        unsafe {
            let arena_ref = &*(&arena as *const Arena<Expr>);
            top_level = parser::parse(arena_ref, &tokens);
        }
        let ast = Self
        {
            arena,
            top_level
        };
        // analyzer::analyze(&ast);
        
        #[cfg(not(benchmark))]
        println!("exprs:  {:?}\n", ast.top_level);

        ast
    }

    pub fn from_str (program:&str) -> Self
    {
        Self::new(&lexer::lex(program))
    }

    pub fn get_top_level (&self) -> &Vec<&Expr>
    {
        &self.top_level
    }

}

// #region IDENTIFIER
pub type Identifier = [u8; 16];

pub fn make_identifier (id:&str) -> Identifier
{
    let mut identifier = Identifier::default();
    identifier[0..id.len()].copy_from_slice(id.as_bytes());
    identifier
}
// #endregion

// #region EXPR
#[derive(Debug)]
pub enum Expr<'a>
{
    Const   (LangVal),
    Id      (Identifier),
    Var     (Identifier, &'a Expr<'a>), //TODO allow declaration without initialization
    Block   (Box<[&'a Expr<'a>]>),
    Parent  (&'a Expr<'a>),
    // --- Operation
    Field   (&'a Expr<'a>, &'a Expr<'a>),
    UnOp    (Op, &'a Expr<'a>),
    BinOp   { op: Op, is_assign: bool, left: &'a Expr<'a>, right: &'a Expr<'a> },
    Call    { name: &'a Expr<'a>, args: Box<[&'a Expr<'a>]> },
    // --- Declaration
    FnDecl  { arity:u8, body:&'a Expr<'a> },
    // Struct  { name:Identifier, fields: Box<[/* ( */Identifier/* , LangType) */]> },
    // --- Control Flow
    If      { cond: &'a Expr<'a>, then: &'a Expr<'a>, elze: Option<&'a Expr<'a>>  },
    While   { cond: &'a Expr<'a>, body: &'a Expr<'a> },
    End //TODO this seems to be useless
}

impl<'a> Expr<'a>
{

    pub fn is_block (&self) -> bool
    {
        match self
        {
            Expr::Block{..}         => true,
            Expr::If{then, elze, ..}      => {
                if let Some(elze) = elze {
                    Self::is_block(elze)
                } else {
                    Self::is_block(then)
                }
            },
            Expr::While{body, ..}   => Self::is_block(body),
            Expr::FnDecl{body, ..}  => Self::is_block(body),
            _ => false
        }
    }

}
// #endregion

// #region TOKEN
#[derive(Debug, PartialEq)]
pub enum Token<'a>
{
    Id(&'a str),
    Const(LangVal),
    Op(Op, bool),
    DelimOpen(Delimiter),
    DelimClose(Delimiter),
    Comma,
    Dot,
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
        use Op::*;
        match self
        {
            Not => 0,
            Mod => 1,
            Mult => 2,
            Div => 2,
            Add => 3,
            Sub => 3,
            Equal => 4,
            NotEqual => 4,
            Gt => 4,
            Gte => 4,
            Lt => 4,
            Lte => 4,
            BoolAnd => 5,
            BoolOr => 6,
            Assign => 7
        }
    }

    /* pub fn returns_bool (self) -> bool
    {
        use Op::*;
        match self
        {
            Equal | NotEqual | Gt | Gte | Lt | Lte | BoolAnd | BoolOr => true,
            _ => false
        }
    } */

}
// #endregion


// #[cfg(test)]
#[allow(dead_code)]
pub mod benchmarks
{
    use crate::benchmarks::ITERATIONS;
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