use std::collections::VecDeque;

#[derive(Debug)]
pub enum Token
{
    Id(String),
    Const(Const),
    Op(String),
    Eof,
    Nil
}

#[derive(Debug)]
#[derive(Clone)]
pub enum Const
{
    Number(f32)
}

pub fn lex (program:&str) -> VecDeque<Token>
{
    let mut pos:usize = 0;
    let mut tokens = VecDeque::new();

    while pos < program.len()
    {
        let (token, len) = get_token(program, pos);
        pos = pos + len;
        match token
        {
            Token::Nil => (),
            Token::Eof => break,
            _ => tokens.push_back(token)
        }
    }

    // tokens.push_back(Token::Eof);

    // println!("tokens: {:?}", tokens);
    tokens
}

fn get_token (program:&str, pos:usize) -> (Token, usize)
{
    let mut len:usize = 1;
    let bytes = program.as_bytes();

    macro_rules! get_char {
        () => { //TODO is this ok ?
            if pos + len < bytes.len() {
                bytes[pos + len] as char
            } else {
                EOF
            }
        }
    }
    macro_rules! read_cursor {
        () => {
            &program[pos..(pos + len)]
        }
    }
    
    let out = match bytes[pos] as char
    {
        c if c.is_lowercase() => {
            loop
            {
                let c = get_char!();
                if !c.is_lowercase() { break; }
                len += 1;
            }
            Token::Id(String::from(read_cursor!()))
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
            Token::Const(Const::Number(read_cursor!().parse().unwrap()))
        },
        c if c.is_operator() => {
            loop
            {
                let c = get_char!();
                if !c.is_operator() { break; }
                len += 1;
            }
            Token::Op(String::from(read_cursor!()))
        },
        EOF => Token::Eof,
        _ => Token::Nil
    };

    (out, len)
}

fn unexpected_char (c:char)
{
    eprintln!("Unexpected char : {}", c.to_string());
    panic!();
}

trait CharExt
{
    fn is_operator (&self) -> bool;
}

impl CharExt for char
{
    fn is_operator (&self) -> bool
    {
        *self == '=' || *self == '+' || *self == '-' || *self == '*' || *self == '/'
    }
}

const EOF:char = '\u{0}';