use crate::lexer::{ Const, Token };
use std::collections::VecDeque;

#[derive(Debug)]
#[derive(Clone)]
pub enum Expr
{
    Const(Const),
    Id(String),
    Var(String, Box<Expr>),
    BinOp(String, Box<Expr>, Box<Expr>),
    Invalid
}

macro_rules! next {
    ($tokens:ident) => {
        if let Some(item) = $tokens.next() {
            item
        } else {
            &Token::Eof
        }
    };
}

macro_rules! peek {
    ($tokens:ident) => {
        if let Some(item) = $tokens.peek() {
            item
        } else {
            &Token::Eof
        }
    };
}

pub fn parse (tokens:&VecDeque<Token>) -> VecDeque<Expr>
{
    let mut exprs:VecDeque<Expr> = VecDeque::new();
    let _tokens:VecDeque<&Token> = tokens.iter().collect();
    let mut tk_iter:TkIter = _tokens.iter().peekable();

    loop
    {
        let expr = parse_expr(&mut tk_iter);
        exprs.push_back(expr);
        if let None = tk_iter.peek() { break; }
    }

    println!("exprs:  {:?}", exprs);
    exprs
}

fn parse_expr (tokens:&mut TkIter) -> Expr
{
    return match next!(tokens)
    {
        Token::Const(c) => parse_expr_next(tokens, Expr::Const(c.clone())), //TODO Copy would be better than Clone
        Token::Id(id) => {
            parse_structure(tokens, id)
        },
        _ => Expr::Invalid
    }
}

fn parse_expr_next (tokens:&mut TkIter, e:Expr) -> Expr
{
    match peek!(tokens)
    {
        Token::Op(op) => {
            next!(tokens);
            make_binop(op, e, parse_expr(tokens))
        },
        _ => e
    }
}

fn parse_structure (tokens:&mut TkIter, id:&str) -> Expr
{
    return match id
    {
        "var" => {
            match next!(tokens)
            {
                Token::Id(id) => {
                    Expr::Var(
                        id.clone(),
                        Box::new(match peek!(tokens)
                        { 
                            Token::Op(op) if &op[..] == "=" => {
                                next!(tokens);
                                parse_expr(tokens)
                            } 
                            _ => Expr::Invalid
                        })
                    )
                }
                _ => Expr::Invalid
            }
        }
        id => parse_expr_next(tokens, Expr::Id(String::from(id)))
    }
}

fn make_binop (op:&str, el:Expr, er:Expr) -> Expr
{
    return match er.clone()
    {
        Expr::BinOp(op_, el_, er_) => {
            if priorities(op) <= priorities(&op_) {
                Expr::BinOp(op_.clone(), Box::new(make_binop(op, el, *el_)), er_)
            } else {
                Expr::BinOp(String::from(op), Box::new(el), Box::new(er))
            }
        },
        _ => Expr::BinOp(String::from(op), Box::new(el), Box::new(er))
    }
}

fn priorities (op:&str) -> i32
{
    macro_rules! map(
        { $($key:expr => $value:expr),+ } => {
            {
                let mut m = ::std::collections::HashMap::new();
                $(
                    m.insert($key, $value);
                )+
                m
            }
        };
    );

    let map = map!{ "*" => 1, "/" => 1, "+" => 2, "-" => 2, "=" => 9 }; //TODO can't do this every time
    if let Some(p) = map.get(op) {
        *p
    } else {
        eprintln!("Invalid operator : {}", op);
        0
    }
}

type TkIter<'l> = std::iter::Peekable<std::collections::vec_deque::Iter<'l, &'l crate::lexer::Token>>;