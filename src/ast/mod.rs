mod lexer;
mod parser;

use crate::value::{Type, Value};
use typed_arena::Arena;

//TODO revoir visibilité

pub struct Ast<'a> {
    pub source: String,
    pub top_level: Vec<&'a Expr<'a>>,
    #[allow(dead_code)] //TODO why is this marked as dead code ?
    arena: Arena<Expr<'a>>,
}

impl<'ast> Ast<'ast> {
    pub fn from_str(source: &str) -> Result<Self, Vec<Error>> {
        // TODO should return (Ast, Vec<Error>)
        match lexer::lex(source) {
            Ok(tokens) => {
                if cfg!(lang_print_lexer_output) && cfg!(not(lang_benchmark)) {
                    println!(
                        "tokens: {:?}\n",
                        tokens.iter().map(|tk| &tk.def).collect::<Vec<_>>()
                    );
                }

                //TODO get rid of unsafe code
                let arena = Arena::new();
                let arena_ref = unsafe { &*(&arena as *const Arena<Expr<'ast>>) };

                match parser::parse(arena_ref, &tokens) {
                    Ok(top_level) => {
                        if cfg!(lang_print_parser_output) && cfg!(not(lang_benchmark)) {
                            println!("statements:");
                            for e in top_level.iter() {
                                println!("\t{:#?}", e.def);
                            }
                            println!();
                        };
                        Ok(Ast {
                            source: source.to_owned(),
                            arena,
                            top_level,
                        })
                    }
                    Err(errors) => Err(errors),
                }
            }
            Err(errors) => Err(errors),
        }
    }
}

// #region IDENTIFIER
pub type Identifier = [u8; 8]; //TODO should this be [char; 8] ?

pub trait IdentifierTools {
    fn make(id: &str) -> Self;
    fn to_string(&self) -> String;
}

impl IdentifierTools for Identifier {
    fn make(id: &str) -> Identifier {
        let mut identifier = Identifier::default();
        identifier[0..id.len()].copy_from_slice(id.as_bytes());
        identifier
    }

    fn to_string(&self) -> String {
        //unwrap accepted because this is only used for error handling
        std::str::from_utf8(self).unwrap().to_owned()
    }
}
// #endregion

// #region EXPR
pub type Expr<'e> = WithPosition<ExprDef<'e>>; //TODO derive display

#[derive(Debug, Clone)]
pub enum ExprDef<'e> {
    // --- Values
    Const(Value),
    Id(Identifier),
    StringLit(Box<[char]>),
    //TODO what about scalar initializers ?
    ArrayLit {
        items: Box<[&'e Expr<'e>]>,
        t: Box<Type>,
    },
    // --- Control Flow
    If {
        //TODO else if
        cond: &'e Expr<'e>,
        then: &'e Expr<'e>,
        elze: Option<&'e Expr<'e>>,
    },
    While {
        cond: &'e Expr<'e>,
        body: &'e Expr<'e>,
    },
    // --- Operations
    Field(&'e Expr<'e>, &'e Expr<'e>),
    UnOp {
        op: Op,
        e: &'e Expr<'e>,
    },
    BinOp {
        op: Op,
        left: &'e Expr<'e>,
        right: &'e Expr<'e>,
    },
    Call {
        function: &'e Expr<'e>,
        args: Box<[&'e Expr<'e>]>,
    },
    // --- Declarations
    VarDecl(Identifier, Type, Option<&'e Expr<'e>>),
    FnDecl {
        id: Identifier,
        params: Box<[(Type, Identifier)]>,
        return_t: Type,
        body: &'e Expr<'e>,
    },
    StructDecl {
        id: Identifier,
        fields: Box<[Identifier]>,
    },
    // --- Others
    Block(Box<[&'e Expr<'e>]>),
    Parent(&'e Expr<'e>),
    Return(&'e Expr<'e>),
    Invalid,
    End,
}

impl<'e> Expr<'e> {
    pub fn is_block(&self) -> bool {
        match self.def {
            ExprDef::Block { .. } => true,
            ExprDef::If { then, elze, .. } => {
                if let Some(elze) = elze {
                    Self::is_block(elze)
                } else {
                    Self::is_block(then)
                }
            }
            ExprDef::While { body, .. } => Self::is_block(body),
            ExprDef::FnDecl { body, .. } => Self::is_block(body),
            ExprDef::StructDecl { .. } => true,
            _ => false,
        }
    }
}
// #endregion

// #region TOKEN
pub type Token = WithPosition<TokenDef>;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenDef {
    Id(Identifier),
    Const(Value),
    StringLit(Box<[char]>),
    Op(Op),
    DelimOpen(Delimiter),
    DelimClose(Delimiter),
    If,
    While,
    Return,
    Comma,
    Dot,
    Semicolon,
    Eof,
    Nil,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Delimiter {
    Pr,
    Br,
    SqBr,
}

impl Delimiter {
    pub fn to_str<'s>(self, closing: bool) -> &'s str {
        use Delimiter::*;

        if closing {
            match self {
                Pr => ")",
                Br => "}",
                SqBr => "]",
            }
        } else {
            match self {
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
pub enum Op {
    Not,
    Add,
    AddAssign,
    MultOrDeref,
    MultAssign,
    Div,
    DivAssign,
    SubOrNeg,
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
    Assign,
    Addr,
    Index,
}

macro_rules! op_chars_pattern {
    () => {
        '=' | '+' | '-' | '*' | '/' | '%' | '<' | '>' | '|' | '&' | '!'
    };
}
pub(crate) use op_chars_pattern;

impl Op {
    const MAX_LENGTH: usize = 2;

    pub fn from_string(string: &str) -> Option<Op> {
        use Op::*;
        Some(match string {
            "!" => Not,
            "==" => Equal,
            "!=" => NotEqual,
            ">" => Gt,
            ">=" => Gte,
            "<" => Lt,
            "<=" => Lte,
            "&&" => BoolAnd,
            "||" => BoolOr,
            "=" => Assign,
            "%" => Mod,
            "%=" => ModAssign,
            "+" => Add,
            "+=" => AddAssign,
            "-" => SubOrNeg,
            "-=" => SubAssign,
            "*" => MultOrDeref,
            "*=" => MultAssign,
            "/" => Div,
            "/=" => DivAssign,
            "&" => Addr,
            "[" => Index,
            _ => return None,
        })
    }

    pub fn to_string(self) -> &'static str {
        use Op::*;
        match self {
            Not => "!",
            Equal => "==",
            NotEqual => "!=",
            Gt => ">",
            Gte => ">=",
            Lt => "<",
            Lte => "<=",
            BoolAnd => "&&",
            BoolOr => "||",
            Assign => "=",
            Mod => "%",
            ModAssign => "%=",
            Add => "+",
            AddAssign => "+=",
            SubOrNeg => "-",
            SubAssign => "-=",
            MultOrDeref => "*",
            MultAssign => "*=",
            Div => "/",
            DivAssign => "/=",
            Addr => "&",
            Index => "[",
        }
    }

    pub fn precedence(self) -> u8 {
        use Op::*;
        match self {
            Addr => 0,
            Not => 0,
            Index => 1,
            Mod => 1,
            MultOrDeref => 2,
            Div => 2,
            Add => 3,
            SubOrNeg => 3,
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
            ModAssign => 7,
        }
    }

    pub fn is_assign(&self) -> bool {
        use Op::*;
        matches!(
            self,
            Assign | AddAssign | SubAssign | MultAssign | DivAssign | ModAssign
        )
    }

    pub fn is_unop(&self) -> bool {
        use Op::*;
        matches!(self, Not | SubOrNeg | MultOrDeref | Addr)
    }

    pub fn is_binop(&self) -> bool {
        use Op::*;
        !matches!(self, Not | Addr)
    }
}
// #endregion

// #region POSITION
#[derive(Clone)]
pub struct WithPosition<T>
where
    T: std::fmt::Debug,
{
    pub def: T,
    pub pos: Position,
}

#[derive(Debug, Clone, Copy)]
pub struct Position(usize, usize);

#[derive(Debug, Default)]
pub struct FullPosition {
    line: usize,
    column: usize,
    len: usize,
}

impl<T> std::fmt::Debug for WithPosition<T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.def)
    }
}

impl<T> WithPosition<T>
where
    T: std::fmt::Debug,
{
    pub fn get_full_pos(&self) -> FullPosition {
        FullPosition::default()
    }

    pub fn downcast_position<U>(&self, def: U) -> WithPosition<U>
    where
        U: std::fmt::Debug,
    {
        WithPosition { def, pos: self.pos }
    }
}

impl Position {
    pub fn zero() -> Self {
        Position(0, 0)
    }

    pub fn join(&self, other:Self) -> Self {
        Position(usize::min(self.0, other.0), usize::max(self.1, other.1))
    }

    pub fn get_full(self, source: &str) -> FullPosition {
        let (line, column) = source
            .chars()
            .take(self.0)
            .fold((1, 1), |(line, column), c| {
                if c == '\n' {
                    (line + 1, 1)
                } else {
                    (line, column + 1)
                }
            });
        FullPosition {
            line,
            column,
            len: self.1 - self.0,
        }
    }
}

impl std::ops::Add for Position {
    type Output = Position;

    fn add(self, other: Self) -> Self::Output {
        Position(usize::min(self.0, other.0), usize::max(self.1, other.1))
    }
}

impl std::ops::AddAssign for Position {
    fn add_assign(&mut self, other: Self) {
        *self = *self + other
    }
}

impl std::fmt::Display for FullPosition {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "At ln {}, col {}, len {}",
            self.line, self.column, self.len
        )
    }
}
// #endregion

#[derive(Debug, Clone)]
pub struct Error {
    pub msg: String,
    pub pos: Position,
}

impl Error {
    pub fn display_with_source(&self, source: &str) -> String {
        format!("{} -> {}", self.pos.get_full(source), self.msg)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?} -> {}", self.pos, self.msg)
    }
}
