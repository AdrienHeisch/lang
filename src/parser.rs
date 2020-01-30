use std::collections::VecDeque;
use crate::lexer::Token;
use crate::lexer::Const;

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
        println!("expr:{:?}", expr);
        /* match expr
        {
            Expr::Invalid => break,
            _ => ()
        } */
        if let None = tk_iter.peek() { break; }
        exprs.push_back(expr);
    }

    println!("exprs:{:?}", exprs);
    exprs
}

fn parse_expr (tokens:&mut TkIter) -> Expr
{
    println!("token:{:?}", tokens.peek());
    return match next!(tokens)
    {
        Token::Const(c) => parse_expr_next(tokens, Expr::Const(c.clone())), //TODO clone ?
        Token::Id(id) => {
            parse_structure(tokens, id.clone())
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

fn parse_structure (tokens:&mut TkIter, id:String) -> Expr
{
    return match &id[..]
    {
        "var" => {
            match next!(tokens)
            {
                Token::Id(id) => {
                    Expr::Var(
                        id.clone(),
                        Box::new(match peek!(tokens)
                        { 
                            Token::Op(op) => {
                                println!("{}", op);
                                next!(tokens);
                                parse_expr(tokens)
                            } 
                            _ =>  Expr::Invalid
                        })
                    )
                }
                _ => Expr::Invalid
            }
        }
        id => parse_expr_next(tokens, Expr::Id(String::from(id)))
    }
}

fn make_binop (op:&String, e1:Expr, e:Expr) -> Expr //TODO avoid &String (use &str)
{
    return match e
    {
        Expr::BinOp(op_, e2, e3) => {
            if priorities(op) <= priorities(&op_) {
                Expr::BinOp(op_.clone(), Box::new(make_binop(op, e1, *e2)), e3)//Box::new((*e3).clone()))
            } else {
                Expr::BinOp(op.clone(), Box::new(e1), e3)
            }
        },
        _ => Expr::BinOp(op.clone(), Box::new(e1), Box::new(e))
    }
}

fn priorities (op:&String) -> i32
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

    let map = map!{ "*" => 1, "/" => 1, "+" => 2, "-" => 2, "=" => 9 };
    if let Some(p) = map.get(op as &str) {
        *p
    } else {
        eprintln!("Invalid operator : {}", op);
        0
    }
}

type TkIter<'l> = std::iter::Peekable<std::collections::vec_deque::Iter<'l, &'l crate::lexer::Token>>;