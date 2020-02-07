use crate::lexer::{ Const, Token };
use std::collections::VecDeque;

#[derive(Debug, Clone)]
pub enum Expr
{
    Const(Const),
    Id(String),
    Var(String, Box<Expr>),
    UnOp(String, Box<Expr>),
    BinOp(String, Box<Expr>, Box<Expr>),
    Parent(Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Block(Vec<Expr>),
    If(Box<Expr>, Box<Expr>),
    While(Box<Expr>, Box<Expr>),
    End
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

//DESIGN should blocks return last expression only if there is no semicolon like rust ?
pub fn parse (tokens:&VecDeque<Token>) -> Expr
{
    let mut exprs = Vec::new();
    let _tokens:VecDeque<&Token> = tokens.iter().collect();
    let mut tk_iter:TkIter = _tokens.iter().peekable();

    loop
    {
        match parse_full_expr(&mut tk_iter)
        {
            // Expr::End => break,
            expr => exprs.push(expr)
        }
        if let None = tk_iter.peek() { break; }
    }

    // #[cfg(not(features = "benchmark"))]
    println!("exprs:  {:?}\n", exprs);

    // #[cfg(not(features = "benchmark"))]
    exprs.push(Expr::Call(Box::new(Expr::Id(String::from("printmem"))), Vec::new()));

    Expr::Block(exprs)
}

fn parse_full_expr (tokens:&mut TkIter) -> Expr
{
    let expr = parse_expr(tokens);

    let tk = peek!(tokens);
    if *tk == Token::Semicolon {
        next!(tokens);
    } else
    {
        match &expr
        {
            e if is_block(e) => (),
            Expr::End => (),
            _ => {
                eprintln!("Expected Semicolon, got : {:?}", tk);
                panic!();
            }
        }
    }

    expr
}

fn parse_expr (tokens:&mut TkIter) -> Expr
{
    match next!(tokens)
    {
        Token::Op(op) => Expr::UnOp(String::from(op), Box::new(parse_expr(tokens))),
        Token::Const(c) => parse_expr_next(tokens, Expr::Const(c.clone())), //RESEARCH Copy vs Clone
        Token::Id(id) => {
            parse_structure(tokens, id)
        },
        Token::DelimOpen('(') => {
            let e = parse_expr(tokens);
            match next!(tokens)
            {
                Token::DelimClose(')') => parse_expr_next(tokens, Expr::Parent(Box::new(e))),
                _ => {
                    eprintln!("Unclosed delimiter \"(\"");
                    panic!();
                }
            }
        },
        Token::DelimOpen('{') => {
            let mut exprs = Vec::new();
            while *peek!(tokens) != Token::DelimClose('}') {
                exprs.push(parse_full_expr(tokens));
            }
            next!(tokens);
            Expr::Block(exprs)
        },
        Token::Eof => Expr::End,
        tk => {
            eprintln!("Unexpected token : {:?}", tk);
            panic!();
        }
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
        Token::DelimOpen('(') => {
            next!(tokens);
            Expr::Call(Box::new(e), make_expr_list(tokens, ')'))
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
                            tk => {
                                eprintln!("Expected assign operator, got : {:?}", tk);
                                panic!();
                            }
                        })
                    )
                }
                tk => {
                    eprintln!("Expected identifier, got : {:?}", tk);
                    panic!();
                }
            }
        },
        "if" => {
            let cond = parse_expr(tokens);
            let then = parse_expr(tokens);
            Expr::If(Box::new(cond), Box::new(then))
        },
        "while" => {
            let cond = parse_expr(tokens);
            let then = parse_expr(tokens);
            Expr::While(Box::new(cond), Box::new(then))
        },
        id => parse_expr_next(tokens, Expr::Id(String::from(id)))
    }
}

fn make_binop (op:&str, el:Expr, er:Expr) -> Expr
{
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

        let map = map!{ "*" => 1, "/" => 1, "+" => 2, "-" => 2, "=" => 9 }; //TODO can't do this every time -> lazy_static! ?
        if let Some(p) = map.get(op) {
            *p
        } else {
            eprintln!("Invalid operator : {}", op);
            0
        }
    }

    match er.clone()
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

fn make_expr_list (tokens:&mut TkIter, close_on:char) -> Vec<Expr>
{
    let mut expr_list = Vec::new();
    if *peek!(tokens) == Token::DelimClose(close_on) {
        return expr_list;
    }

    loop
    {
        expr_list.push(parse_expr(tokens));
        match peek!(tokens)
        {
            Token::Comma => {
                next!(tokens);
            },
            Token::DelimClose(c) if *c == close_on => {
                next!(tokens);
                break;
            },
            Token::Eof => {
                eprintln!("Unclosed parenthese.");
                panic!();
            },
            tk => {
                eprintln!("Expected Comma or DelimClose('{}'), got  {:?}", close_on, tk);
                panic!();
            }
        }
    }
    
    expr_list
}

fn is_block (e:&Expr) -> bool
{
    match e
    {
		Expr::Block(_) => true,
		Expr::If(_, expr) => is_block(expr),
		Expr::While(_, expr) => is_block(expr),
		_ => false
    }
}

/* fn unexpected_expr (e:Expr)
{
    eprintln!("Unexpected expression : {:?}", e);
    panic!();
} */

type TkIter<'l> = std::iter::Peekable<std::collections::vec_deque::Iter<'l, &'l crate::lexer::Token>>;