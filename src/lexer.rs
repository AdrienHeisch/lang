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
    Int(i32),
    Float(f32)
}

pub fn lex (path:&String) -> VecDeque<Token>
{
    let program:String;
    let mut pos:usize = 0;
    let mut tokens = VecDeque::new();

    let res = std::fs::read_to_string(&path);
    match res
    {
        Ok(p) => program = p,
        Err(_) => {
            eprintln!("Program not found at {}", &path);
            panic!();
        }
    }

    while pos < program.len()
    {
        let (token, len) = get_token(&program, pos);
        pos = pos + len;
        match token
        {
            Token::Nil => (),
            _ => tokens.push_back(token)
        }
    }

    tokens.push_back(Token::Eof);

    println!("tokens: {:?}", tokens);
    tokens
}

fn get_token (program:&String, pos:usize) -> (Token, usize)
{
    let mut len:usize = 1;
    let bytes = &program.as_bytes();

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
            String::from(&program[(pos as usize)..((pos + len) as usize)])
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
            Token::Id(read_cursor!())
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
            if is_float {
                Token::Const(Const::Float(read_cursor!().parse().unwrap()))
            } else {
                Token::Const(Const::Int(read_cursor!().parse().unwrap()))
            }
        },
        c if c.is_operator() => {
            loop
            {
                let c = get_char!();
                if !c.is_operator() { break; }
                len += 1;
            }
            Token::Op(read_cursor!())
        }
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