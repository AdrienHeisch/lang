use crate::{
    expr::Expr,
    lexer::Token
};
use std::collections::VecDeque;
use typed_arena::Arena;

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
pub fn parse<'a> (arena:&'a Arena<Expr<'a>>, tokens:&VecDeque<Token>) -> &'a Expr<'a>
{
    let mut exprs:Vec<&Expr> = Vec::new();
    let _tokens:VecDeque<&Token> = tokens.iter().collect();
    let mut tk_iter:TkIter = _tokens.iter().peekable();

    loop
    {
        /* match parse_full_expr(&mut tk_iter)
        {
            // Expr::End => break,
            expr => exprs.push(expr)
        }; */
        exprs.push(arena.alloc(parse_full_expr(arena, &mut tk_iter)));
        if tk_iter.peek().is_none() { break; }
    }

    #[cfg(not(benchmark))]
    {
        println!("exprs:  {:?}\n", exprs);
        exprs.push(arena.alloc(Expr::Call(arena.alloc(Expr::Id(String::from("printmem"))), Vec::new())));
    }

    arena.alloc(Expr::Block(exprs))
}

fn parse_full_expr<'a> (arena:&'a Arena<Expr<'a>>, tokens:&mut TkIter) -> Expr<'a>
{
    let expr = parse_expr(arena, tokens);

    let tk = peek!(tokens);
    if *tk == Token::Semicolon {
        next!(tokens);
    } else {
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

fn parse_expr<'a> (arena:&'a Arena<Expr<'a>>, tokens:&mut TkIter) -> Expr<'a>
{
    match next!(tokens)
    {
        Token::Op(op) => Expr::UnOp(String::from(op), arena.alloc(parse_expr(arena, tokens))),
        Token::Const(c) => parse_expr_next(arena, tokens, Expr::Const(c.clone())), //RESEARCH Copy vs Clone
        Token::Id(id) => {
            parse_structure(arena, tokens, id)
        },
        Token::DelimOpen('(') => {
            let e = parse_expr(arena, tokens);
            match next!(tokens)
            {
                Token::DelimClose(')') => parse_expr_next(arena, tokens, Expr::Parent(arena.alloc(e))),
                _ => {
                    eprintln!("Unclosed delimiter \"(\"");
                    panic!();
                }
            }
        },
        Token::DelimOpen('{') => {
            let mut exprs:Vec<&Expr> = Vec::new();
            while *peek!(tokens) != Token::DelimClose('}') {
                exprs.push(arena.alloc(parse_full_expr(arena, tokens)));
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

fn parse_expr_next<'a> (arena:&'a Arena<Expr<'a>>, tokens:&mut TkIter, e:Expr<'a>) -> Expr<'a>
{
    match peek!(tokens)
    {
        Token::Op(op) => {
            next!(tokens);
            make_binop(arena, op, e, parse_expr(arena, tokens))
        },
        Token::DelimOpen('(') => {
            next!(tokens);
            Expr::Call(arena.alloc(e), make_expr_list(arena, tokens, ')'))
        },
        _ => e
    }
}

fn parse_structure<'a> (arena:&'a Arena<Expr<'a>>, tokens:&mut TkIter, id:&str) -> Expr<'a>
{
    match id
    {
        "var" => {
            match next!(tokens)
            {
                Token::Id(id) => {
                    Expr::Var(
                        id.clone(),
                        arena.alloc(match peek!(tokens)
                        { 
                            Token::Op(op) if &op[..] == "=" => {
                                next!(tokens);
                                parse_expr(arena, tokens)
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
            let cond = parse_expr(arena, tokens);
            let then = parse_expr(arena, tokens);
            Expr::If(arena.alloc(cond), arena.alloc(then))
        },
        "while" => {
            let cond = parse_expr(arena, tokens);
            let then = parse_expr(arena, tokens);
            Expr::While(arena.alloc(cond), arena.alloc(then))
        },
        id => parse_expr_next(arena, tokens, Expr::Id(String::from(id)))
    }
}

fn make_binop<'a> (arena:&'a Arena<Expr<'a>>, op:&str, el:Expr<'a>, er:Expr<'a>) -> Expr<'a>
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
                Expr::BinOp(op_, arena.alloc(make_binop(arena, op, el, el_.clone())), er_)
            } else {
                Expr::BinOp(String::from(op), arena.alloc(el), arena.alloc(er))
            }
        },
        _ => Expr::BinOp(String::from(op), arena.alloc(el), arena.alloc(er))
    }
}

fn make_expr_list<'a> (arena:&'a Arena<Expr<'a>>, tokens:&mut TkIter, close_on:char) -> Vec<&'a Expr<'a>>
{
    let mut expr_list = Vec::new();
    if *peek!(tokens) == Token::DelimClose(close_on) {
        return expr_list;
    }

    loop
    {
        expr_list.push(arena.alloc(parse_expr(arena, tokens)));
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