// mod analyzer;
mod parser;
mod lexer;

use crate::langval::{ LangVal };
use std::collections::VecDeque;
use typed_arena::Arena;

pub struct Ast<'e, 's>
{
    #[allow(dead_code)] // This is where all the Expr instances live, they are accessed using top_level
    arena: Arena<Expr<'e, 's>>,
    top_level: Vec<&'e Expr<'e, 's>>
}

impl<'e, 's> Ast<'e, 's>
{

    pub fn from_str (program:&'s str) -> (Self, Vec<Error>)
    {
        let (tokens, mut errors) = lexer::lex(program);

        if cfg!(not(lang_benchmark)) {
            println!("tokens: {:?}\n", tokens.iter().map(|tk| &tk.def).collect::<Vec<_>>());
        }

        let (ast, mut more_errors) = Self::from_tokens(&tokens);
        errors.append(&mut more_errors);
        (ast, errors)
    }

    //TODO probably useless
    pub fn from_tokens (tokens:&VecDeque<Token<'_, 's>>) -> (Self, Vec<Error>)
    {
        let arena = Arena::new();
        let top_level; let errors;
        unsafe {
            let arena_ref = &*(&arena as *const Arena<Expr<'e, 's>>);
            let pair = parser::parse(arena_ref, &tokens);
            top_level = pair.0;
            errors = pair.1;
        }
        
        if cfg!(not(lang_benchmark)) {
            println!("exprs:");
            for e in top_level.iter() {
                println!("\t{:?}", e.def);
            }
            println!();
        }

        (Ast { arena, top_level }, errors)
    }

    pub fn get_top_level (&self) -> &Vec<&Expr>
    {
        &self.top_level
    }

}

// #region IDENTIFIER
pub type Identifier = [u8; 16];

trait IdentifierTools
{
    fn make (id:&str) -> Self;
}

impl IdentifierTools for Identifier
{
    fn make (id:&str) -> Identifier
    {
        let mut identifier = Identifier::default();
        identifier[0..id.len()].copy_from_slice(id.as_bytes());
        identifier
    }
}
/* 
impl std::fmt::Display for Identifier
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result
    {
        write!(f, "{}", crate::utils::slice_to_string(self))
    }
} */
// #endregion

// #region EXPR
pub type Expr<'e, 's> = WithPosition<'s, ExprDef<'e, 's>>;

#[derive(Debug, Clone)]
pub enum ExprDef<'e, 's>
{
    // --- Values
    Const       (LangVal),
    Id          (Identifier),
    // --- Control Flow
    If          { cond: &'e Expr<'e, 's>, then: &'e Expr<'e, 's>, elze: Option<&'e Expr<'e, 's>>  },
    While       { cond: &'e Expr<'e, 's>, body: &'e Expr<'e, 's> },
    // --- Operations
    Field       (&'e Expr<'e, 's>, &'e Expr<'e, 's>),
    UnOp        (Op, &'e Expr<'e, 's>),
    BinOp       { op: Op, left: &'e Expr<'e, 's>, right: &'e Expr<'e, 's> },
    Call        { id: &'e Expr<'e, 's>, args: Box<[&'e Expr<'e, 's>]> },
    // --- Declarations
    Var         (Identifier, &'e Expr<'e, 's>), //TODO allow declaration without initialization
    FnDecl      { id:Identifier, params:Box<[Identifier]>, body:&'e Expr<'e, 's> },
    StructDecl  { id:Identifier, fields:Box<[Identifier]> },
    // --- Others
    Block       (Box<[&'e Expr<'e, 's>]>),
    Parent      (&'e Expr<'e, 's>),
    Return      (&'e Expr<'e, 's>),
    Invalid,
    End //TODO this seems to be useless
}

impl<'e, 's> Expr<'e, 's>
{

    pub fn is_block (&self) -> bool
    {
        match self.def
        {
            ExprDef::Block{..}          => true,
            ExprDef::If{then, elze, ..} => {
                if let Some(elze) = elze {
                    Self::is_block(elze)
                } else {
                    Self::is_block(then)
                }
            },
            ExprDef::While{body, ..}    => Self::is_block(body),
            ExprDef::FnDecl{body, ..}   => Self::is_block(body),
            ExprDef::StructDecl{..}     => true,
            _ => false
        }
    }

}
// #endregion

// #region TOKEN
pub type Token<'t, 's> = WithPosition<'s, TokenDef<'t>>;

#[derive(Debug, PartialEq)]
pub enum TokenDef<'s>
{
    Id(&'s str),
    Const(LangVal),
    Op(Op),
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

impl Delimiter
{
    pub fn to_str (&self, closing:bool) -> &str
    {
        use Delimiter::*;

        if closing
        {
            match self
            {
                Pr => ")",
                Br => "}",
                SqBr => "]",
            }
        } else {
            match self
            {
                Pr => "(",
                Br => "{",
                SqBr => "[",
            }
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
    AddAssign,
    Mult,
    MultAssign,
    Div,
    DivAssign,
    Sub,
    SubAssign,
    Mod,
    ModAssign,
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

    pub fn from_string (string:&str) -> Op
    {
        use Op::*;
        match string
        {
            "!" =>  Not,
            "==" => Equal,
            "!=" => NotEqual,
            ">" =>  Gt,
            ">=" => Gte,
            "<" =>  Lt,
            "<=" => Lte,
            "&&" => BoolAnd,
            "||" => BoolOr,
            "=" =>  Assign,
            "%" =>  Mod,
            "%=" => ModAssign,
            "+" =>  Add,
            "+=" => AddAssign,
            "-" => Sub,
            "-=" => SubAssign,
            "*" =>  Mult,
            "*=" => MultAssign,
            "/" =>  Div,
            "/=" => DivAssign,
            _ => panic!("Invalid operator : {}", string)
        }
    }

    pub fn to_string (self) -> &'static str
    {
        use Op::*;
        match self
        {
            Not => "!" ,
            Equal => "==",
            NotEqual => "!=",
            Gt => ">" ,
            Gte => ">=",
            Lt => "<" ,
            Lte => "<=",
            BoolAnd => "&&",
            BoolOr => "||",
            Assign => "=" ,
            Mod => "%" ,
            ModAssign => "%=",
            Add => "+" ,
            AddAssign => "+=",
            Sub => "-",
            SubAssign => "-=",
            Mult => "*" ,
            MultAssign => "*=",
            Div => "/" ,
            DivAssign => "/=",
        }
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
            Assign => 7,
            AddAssign => 7,
            SubAssign => 7,
            MultAssign => 7,
            DivAssign => 7,
            ModAssign => 7
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

// #region POSITION
#[derive(Clone)]
pub struct WithPosition<'s, T> where T : std::fmt::Debug
{
    pub def: T,
    pub src: &'s str, //TODO probably useless, store in Ast
    pub pos: Position,
}

#[derive(Debug, Clone, Copy)]
pub struct Position(usize, usize);

#[derive(Debug)]
pub struct FullPosition
{
    line: usize,
    column: usize,
    len: usize
}

impl<'s, T> std::fmt::Debug for WithPosition<'s, T> where T : std::fmt::Debug
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result
    {
        write!(f, "{:?}", self.def)
    }
}

impl<'s, T> WithPosition<'s, T> where T : std::fmt::Debug
{
    
    pub fn get_full_pos (&self) -> FullPosition
    {
        self.pos.get_full(self.src)
    }

    /* pub fn add_as_full_pos<U> (left:&Self, right:&WithPosition<U>) -> FullPosition where U : std::fmt::Debug
    {
        if left.src != right.src {
            panic!("Tried to add positions of items with differents sources : {:?}, {:?}]", left, right);
        } else {
            (left.pos + right.pos).get_full(left.src)
        }
    } */

    pub fn downcast<U> (&self, def:U) -> WithPosition<U> where U : std::fmt::Debug
    {
        WithPosition
        {
            def,
            src: self.src,
            pos: self.pos
        }
    }

}

impl Position
{
    pub fn zero () -> Self
    {
        Position(0, 0)
    }

    pub fn get_full (self, source:&str) -> FullPosition
    {
        let (line, column) = source.chars().take(self.0).fold((1, 1), |(line, column), c| if c == '\n' { (line + 1, 1) } else { (line, column + 1) });
        FullPosition
        {
            line,
            column,
            len: self.1 - self.0
        }
    }
}

impl std::ops::Add for Position
{
    type Output = Position;

    fn add (self, other:Self) -> Self::Output
    {
        Position(usize::min(self.0, other.0), usize::max(self.1, other.1))
    }
}

impl std::ops::AddAssign for Position
{
    fn add_assign (&mut self, other:Self)
    {
        *self = *self + other
    }
}

/* impl FullPosition
{
    pub fn zero () -> Self
    {
        FullPosition
        {
            line: 0,
            column: 0,
            len: 0
        }
    }
} */

impl std::fmt::Display for FullPosition
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result
    {
        write!(f, "At ln {}, col {}, len {}", self.line, self.column, self.len)
    }
}
// #endregion

// #region OP
#[derive(Debug)]
pub struct Error
{
    pub msg: String,
    pub pos: FullPosition
}

impl std::fmt::Display for Error
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result
    {
        write!(f, "{} -> {}", self.pos, self.msg)
    }
}
// #endregion


// #[cfg(test)]
#[allow(dead_code)]
pub mod benchmarks
{
    pub use super::lexer::benchmark as benchmark_lexer;
    pub use super::parser::benchmark as benchmark_parser;
}