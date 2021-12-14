use crate::langval::LangVal;
use super::{ Op, Token, TokenDef, Position, Delimiter, Error };
use std::collections::VecDeque;

//DESIGN define how strict the lexer should be (unexpected characters are currently ignored)
pub fn lex (program:&str) -> (VecDeque<Token>, Vec<Error>) //TODO retest vecdeque vs vec
{
    let mut tokens = VecDeque::new();
    let mut errors = Vec::new();
    let mut pos:usize = 0;

    while pos < program.len()
    {
        match get_token(program, pos)
        {
            (Ok(token_def), len) => {
                match token_def
                {
                    TokenDef::Eof => break,
                    TokenDef::Nil => (),
                    _ => {
                        tokens.push_back(Token {
                            def: token_def,
                            src: program,
                            pos: Position(pos, pos + len)
                        })
                    }
                }
                pos += len;
            },
            (Err(chars), len) => {
                let error = Error {
                    msg: format!("Unexpected characters : {:?}", chars),
                    pos: Position(pos, pos + len).get_full(program)
                };

                if cfg!(lang_panic_on_error) {
                    panic!("{}", error);
                } else {
                    errors.push(error);
                }
                pos += len;
            }
        }
    }
    
    tokens.push_back(Token { def: TokenDef::Eof, src: program, pos: Position::zero() });

    (tokens, errors)
}

// #[inline(never)] //used for profiling
#[allow(clippy::cognitive_complexity)] //TODO split into smaller functions ?
fn get_token (program:&str, mut pos:usize) -> (Result<TokenDef, String>, usize)
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
        'a'..='z' | 'A'..='Z' | '_' => {
            loop
            {
                let c = get_char!();
                if !(c.is_lowercase() || c == '_' || c.is_numeric()) { break; }
                len += 1;
            }
            match read_cursor!()
            {
                "true" => TokenDef::Const(LangVal::Bool(true)),
                "false" => TokenDef::Const(LangVal::Bool(false)),
                id => TokenDef::Id(id)
            }
        },
        '0'..='9' => {
            let mut is_float = false;
            loop
            {
                let c = get_char!();
                if c == '.' {
                    if !is_float {
                        is_float = true;
                    } else {
                        return (Err(collect_unexpected_chars(program, c, &mut pos, &mut len)), len);
                    }
                } else if !c.is_numeric() { break; }
                len += 1;
            }
            TokenDef::Const(LangVal::Number(read_cursor!().parse().unwrap()))
        },
        '/' if { get_char!() == '/' } => { //COMMENT
            while get_char!() != '\n' && get_char!() != EOF { len += 1; }
            TokenDef::Nil
        },
        '=' | '+' | '-' | '*' | '/' | '<' | '>' | '|' | '&' | '!' => {
            while let '=' | '+' | '-' | '*' | '/' | '<' | '>' | '|' | '&' | '!' = get_char!() {
                len += 1;
            }
            TokenDef::Op(Op::from_string(read_cursor!()))
        },
        /* '"' => {
            pos += 1;
            loop
            {
                let c = get_char!();
                if c == '"' { break; }
                len += 1;
            }
            let tk = TokenDef::Const(LangVal::Str(String::from(read_cursor!())));
            len += 2;
            tk
        }, */
        c @ '(' | c @ '[' | c @ '{' => TokenDef::DelimOpen (Delimiter::from_char(c)),
        c @ ')' | c @ ']' | c @ '}' => TokenDef::DelimClose(Delimiter::from_char(c)),
        ',' => TokenDef::Comma,
        '.' => TokenDef::Dot,
        ';' => TokenDef::Semicolon,
        EOF => TokenDef::Eof,
        ' ' | '\x09'..='\x0d' => {
            loop
            {
                let c = get_char!();
                if !c.is_whitespace() { break; }
                len += 1;
            }
            TokenDef::Nil
        },
        c => return (Err(collect_unexpected_chars(program, c, &mut pos, &mut len)), len)
    };

    (Ok(token), len)
}

fn collect_unexpected_chars (program:&str, first_char:char, pos:&mut usize, len:&mut usize) -> String
{
    let mut chars = first_char.to_string();
    *pos += 1;
    while let (Err(chars_), len_) = get_token(program, *pos) {
        chars += &chars_;
        *pos += 1;
        *len += len_;
    }
    chars
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
            _ => panic!("Invalid delimiter : {}", c)
        }
    }

}

const EOF:char = '\u{0}';

#[allow(dead_code)]
pub fn benchmark ()
{
    use crate::benchmarks::ITERATIONS;
    use std::time::{ Instant };
    
    let program = std::fs::read_to_string("./code.lang").unwrap();
    let now = Instant::now();
    for _ in 0..ITERATIONS { lex(&program); }
    println!("Lexing: {}ms", now.elapsed().as_millis());
}