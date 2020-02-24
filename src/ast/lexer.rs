use crate::langval::LangVal;
use super::{ Op, Token, Delimiter };
use std::collections::VecDeque;

//DESIGN define how strict the lexer should be (unexpected characters are currently ignored)
pub fn lex (program:&str) -> VecDeque<Token>
{
    let mut pos:usize = 0;
    let mut tokens = VecDeque::new();

    while pos < program.len()
    {
        let (token, len) = get_token(program, pos);
        pos += len;
        match token
        {
            Token::Eof => break,
            Token::Nil => (),
            _ => tokens.push_back(token)
        }
    }
    
    tokens.push_back(Token::Eof);

    #[cfg(not(benchmark))]
    println!("tokens: {:?}\n", tokens);

    tokens
}

// #[inline(never)] //used for profiling
#[allow(clippy::cognitive_complexity)] //TODO split into smaller functions ?
fn get_token (program:&str, mut pos:usize) -> (Token, usize)
{
    let mut len:usize = 1;
    let bytes = program.as_bytes();

    // #region MACROS
    macro_rules! get_char {
        () => {
            if pos + len < bytes.len() {
                bytes[pos + len] as char
            } else {
                EOF
            }
        };
    }
    
    macro_rules! read_cursor {
        () => {
            &program[pos..(pos + len)]
        };
    }
    // #endregion
    
    let token = match bytes[pos] as char
    {
        c if c.is_lowercase() || c == '_' => {
            loop
            {
                let c = get_char!();
                if !(c.is_lowercase() || c == '_' || c.is_numeric()) { break; }
                len += 1;
            }
            let id = read_cursor!();
            match id
            {
                "true" => Token::Const(LangVal::Bool(true)),
                "false" => Token::Const(LangVal::Bool(false)),
                id => Token::Id(id)
            }
        },
        c if c.is_numeric() => {
            let mut is_float = false;
            loop
            {
                let c = get_char!();
                if c == '.' {
                    if !is_float {
                        is_float = true;
                    } else {
                        unexpected_char(c);
                    }
                } else if !c.is_numeric() { break; }
                len += 1;
            }
            Token::Const(LangVal::Number(read_cursor!().parse().unwrap()))
        },
        c if c.is_operator() => {
            match get_char!()
            {
                '/' => { //COMMENT
                    while get_char!() != '\n' && get_char!() != EOF { len += 1; }
                    Token::Nil
                },
                _ => { //OP
                    loop
                    {
                        let c = get_char!();
                        if !c.is_operator() { break; }
                        len += 1;
                    }
                    let op = Op::from_string(read_cursor!());
                    Token::Op(op.0, op.1)
                }
            }
        },
        '"' => {
            pos += 1;
            loop
            {
                let c = get_char!();
                if c == '"' { break; }
                len += 1;
            }
            let tk = Token::Const(LangVal::Str(String::from(read_cursor!())));
            len += 2;
            tk
        },
        c if c.is_delimiter_open() => Token::DelimOpen(Delimiter::from_char(c)),
        c if c.is_delimiter_close() => Token::DelimClose(Delimiter::from_char(c)),
        ',' => Token::Comma,
        ';' => Token::Semicolon,
        EOF => Token::Eof,
        c if c.is_whitespace() => {
            loop
            {
                let c = get_char!();
                if !c.is_whitespace() { break; }
                len += 1;
            }
            Token::Nil
        },
        c => {
            unexpected_char(c);
            Token::Nil
        }
    };

    (token, len)
}

fn unexpected_char (c:char)
{
    eprintln!("Unexpected char : {}", c);
    panic!();
}

trait CharExt
{
    fn is_operator (&self) -> bool;
    fn is_delimiter_open (&self) -> bool;
    fn is_delimiter_close (&self) -> bool;
}

impl CharExt for char
{
    fn is_operator (&self) -> bool
    {
        match self
        {
            '=' | '+' | '-' | '*' | '/' | '<' | '>' | '|' | '&' | '!' => true,
            _ => false
        }
    }

    fn is_delimiter_open (&self) -> bool
    {
        *self == '(' || *self == '[' || *self == '{'
    }

    fn is_delimiter_close (&self) -> bool
    {
        *self == ')' || *self == ']' || *self == '}'
    }
}

impl Delimiter
{

    fn from_char (c:char) -> Delimiter
    {
        match c
        {
            '(' | ')' => Delimiter::Pr,
            '{' | '}' => Delimiter::Br,
            '[' | ']' => Delimiter::SqBr,
            _ => {
                eprintln!("Invalid delimiter : {}", c);
                panic!();
            }
        }
    }

}

const EOF:char = '\u{0}';