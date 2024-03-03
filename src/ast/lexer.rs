use super::{Delimiter, Error, Op, Position, Token, TokenDef};
use crate::{
    ast::{op_chars_pattern, Identifier, IdentifierTools},
    value::Value,
};
use std::collections::VecDeque;

type LexErr = (String, LexErrCode);

pub fn lex(program: &str) -> Result<VecDeque<Token>, Vec<Error>> {
    let mut tokens = VecDeque::new();
    let mut errors = Vec::new();
    let mut pos: usize = 0;

    while pos < program.len() {
        match get_token(program, pos) {
            Ok((token_def, len)) => {
                match token_def {
                    TokenDef::Eof => break,
                    TokenDef::Nil => (),
                    _ => tokens.push_back(Token {
                        def: token_def,
                        pos: Position(pos, pos + len),
                    }),
                }
                pos += len;
            }
            Err(((chars, err_code), len)) => {
                let error = Error {
                    msg: format!("{} : {:?}", err_code.to_string(), chars),
                    pos: Position(pos, pos + len),
                };

                if cfg!(lang_panic_on_error) {
                    panic!("{}", error);
                }

                errors.push(error);
                pos += len;
            }
        }
    }

    tokens.push_back(Token {
        def: TokenDef::Eof,
        pos: Position(pos, pos),
    });

    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err(errors)
    }
}

// #[inline(never)] //used for profiling
#[allow(clippy::cognitive_complexity)]
fn get_token(program: &str, mut pos: usize) -> Result<(TokenDef, usize), (LexErr, usize)> {
    let mut len: usize = 1;
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

    let token = match bytes[pos] as char {
        'a'..='z' | 'A'..='Z' | '_' => {
            loop {
                let c = get_char!();
                if !(c.is_lowercase() || c == '_' || c.is_numeric()) {
                    break;
                }
                len += 1;
            }
            match read_cursor!() {
                "true" => TokenDef::Const(Value::Bool(true)),
                "false" => TokenDef::Const(Value::Bool(false)),
                "if" => TokenDef::If,
                "while" => TokenDef::While,
                "return" => TokenDef::Return,
                id => {
                    if id.len() <= 8 {
                        TokenDef::Id(Identifier::make(id))
                    } else {
                        return Err(((id.to_owned(), LexErrCode::IdTooLong), len));
                    }
                }
            }
        }
        '0'..='9' => {
            let mut is_float = false;
            loop {
                let c = get_char!();
                if c == '.' {
                    if !is_float {
                        is_float = true;
                    } else {
                        return Err((collect_unexpected_chars(
                                program,
                                c,
                                &mut pos,
                                &mut len,
                                LexErrCode::UnexpectedChar,
                            ),
                            len
                        ));
                    }
                } else if !c.is_numeric() {
                    break;
                }
                len += 1;
            }

            let cursor = &read_cursor!();
            let c = cursor.chars().next().ok_or((
                (cursor.to_string(), LexErrCode::InvalidCursor),
                len
            ))?;

            if is_float {
                TokenDef::Const(Value::Float(cursor.parse::<f32>().map_err(|_| {
                    (
                        collect_unexpected_chars(
                            program,
                            c,
                            &mut pos,
                            &mut len,
                            LexErrCode::UnexpectedChar,
                        ),
                        len
                    )
                })?))
            } else {
                TokenDef::Const(Value::Int(cursor.parse::<i32>().map_err(|_| {
                    (
                        collect_unexpected_chars(
                            program,
                            c,
                            &mut pos,
                            &mut len,
                            LexErrCode::UnexpectedChar,
                        ),
                        len
                    )
                })?))
            }
        }
        '\'' => {
            let char = get_char!();
            len += 1;
            let c = get_char!();
            if c != '\'' {
                return Err((collect_unexpected_chars(
                        program,
                        c,
                        &mut pos,
                        &mut len,
                        LexErrCode::UnexpectedChar,
                    ),
                    len
                ));
            }
            len += 1;
            TokenDef::Const(Value::Char(char))
        }
        '"' => {
            pos += 1;
            loop {
                let c = get_char!();
                if c == '"' {
                    break;
                }
                len += 1;
            }
            // let tk = TokenDef::Const(Value::Str(String::from(read_cursor!())));
            let tk = TokenDef::StringLit(read_cursor!().chars().collect());
            len += 2;
            tk
        }
        '/' if { get_char!() == '/' } => {
            //COMMENT
            while get_char!() != '\n' && get_char!() != EOF {
                len += 1;
            }
            TokenDef::Nil
        }
        c @ op_chars_pattern!() => {
            len = Op::MAX_LENGTH;
            let mut op = None;
            'while_len: while len > 0 {
                let str = read_cursor!();
                for byte in str.as_bytes() {
                    if let op_chars_pattern!() = *byte as char {
                    } else {
                        len -= 1;
                        continue 'while_len;
                    }
                }
                if let op_ @ Some(_) = Op::from_string(str) {
                    op = op_;
                    break;
                }
                len -= 1;
            }
            if let Some(op) = op {
                TokenDef::Op(op)
            } else {
                return Err((collect_unexpected_chars(
                        //TODO len is always zero here
                        program,
                        c,
                        &mut pos,
                        &mut len,
                        LexErrCode::UnexpectedChar,
                    ),
                    len
                ));
            }
        }
        c @ '(' | c @ '[' | c @ '{' => TokenDef::DelimOpen(match Delimiter::from_char(c) {
            Ok(delimiter) => delimiter,
            Err(err) => return Err((err, len)),
        }),
        c @ ')' | c @ ']' | c @ '}' => TokenDef::DelimClose(match Delimiter::from_char(c) {
            Ok(delimiter) => delimiter,
            Err(err) => return Err((err, len)),
        }),
        ',' => TokenDef::Comma,
        '.' => TokenDef::Dot,
        ';' => TokenDef::Semicolon,
        EOF => TokenDef::Eof,
        ' ' | '\x09'..='\x0d' => {
            loop {
                let c = get_char!();
                if !c.is_whitespace() {
                    break;
                }
                len += 1;
            }
            TokenDef::Nil //TODO remove this for optimization ? (maybe recursive get_token call)
        }
        c => {
            return Err((collect_unexpected_chars(
                    program,
                    c,
                    &mut pos,
                    &mut len,
                    LexErrCode::UnexpectedChar,
                ),
                len,
            ))
        }
    };

    Ok((token, len))
}

fn collect_unexpected_chars(
    program: &str,
    first_char: char, //TODO get rid of this parameter
    pos: &mut usize,
    len: &mut usize,
    err_code: LexErrCode,
) -> LexErr {
    let mut chars = first_char.to_string();
    *pos += 1;
    while let Err(((chars_, _), len_)) = get_token(program, *pos) {
        chars += &chars_;
        *pos += 1;
        *len += len_;
    }
    (chars, err_code)
}

enum LexErrCode {
    UnexpectedChar,
    InvalidDelimiter,
    IdTooLong,
    NumberParseError,
    InvalidCursor
}

impl LexErrCode {
    fn to_string(&self) -> &str {
        use LexErrCode::*;
        match self {
            UnexpectedChar => "Unexpected character",
            InvalidDelimiter => "Invalid delimiter",
            IdTooLong => "Id too long",
            NumberParseError => "Number parse error",
            InvalidCursor => "Invalid cursor"
        }
    }
}

impl Delimiter {
    fn from_char(c: char) -> Result<Delimiter, LexErr> {
        Ok(match c {
            '(' | ')' => Delimiter::Pr,
            '{' | '}' => Delimiter::Br,
            '[' | ']' => Delimiter::SqBr,
            _ => return Err((c.to_string(), LexErrCode::InvalidDelimiter)),
        })
    }
}

const EOF: char = '\u{0}';
