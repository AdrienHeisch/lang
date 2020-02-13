use crate::{
    expr::Expr,
    op::Op,
    lexer::{ Token, Delimiter }
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

//TODO reduce number of args ?
//DESIGN should blocks return last expression only if there is no semicolon like rust ?
pub fn parse<'a, 'b> (arena:&'a Arena<Expr<'a>>, tokens:&'b VecDeque<Token<'b>>, identifiers:&mut Vec<String>) -> &'a Expr<'a>
{
    let mut exprs:Vec<&Expr> = Vec::new();
    let mut tk_iter:TkIter = tokens.iter().peekable();

    loop
    {
        exprs.push(parse_full_expr(arena, &mut tk_iter, identifiers));
        if tk_iter.peek().is_none() { break; }
    }

    #[cfg(not(benchmark))]
    {
        println!("exprs:  {:?}\n", exprs);
        exprs.push(arena.alloc(Expr::Call(arena.alloc(Expr::Id(make_id(identifiers, "printmem"))), Vec::new())));
    }

    arena.alloc(Expr::Block(exprs))
}

fn parse_full_expr<'a> (arena:&'a Arena<Expr<'a>>, tokens:&mut TkIter, identifiers:&mut Vec<String>) -> &'a Expr<'a>
{
    let expr = parse_expr(arena, tokens, identifiers);

    if peek!(tokens) == &Token::Semicolon {
        next!(tokens);
    } else {
        match &expr
        {
            e if is_block(e) => (),
            Expr::End => (),
            _ => {
                eprintln!("Expected Semicolon, got : {:?}", peek!(tokens));
                panic!();
            }
        }
    }

    expr
}

fn parse_expr<'a> (arena:&'a Arena<Expr<'a>>, tokens:&mut TkIter, identifiers:&mut Vec<String>) -> &'a Expr<'a>
{
    match next!(tokens)
    {
        Token::Op(op, _) => arena.alloc(Expr::UnOp(*op, parse_expr(arena, tokens, identifiers))),
        Token::Const(c) => parse_expr_next(arena, tokens, identifiers, arena.alloc(Expr::Const(c.clone()))), //RESEARCH Copy vs Clone
        Token::Id(id) => {
            parse_structure(arena, tokens, identifiers, id)
        },
        Token::DelimOpen(Delimiter::Pr) => {
            let e = parse_expr(arena, tokens, identifiers);
            match next!(tokens)
            {
                Token::DelimClose(Delimiter::Pr) => parse_expr_next(arena, tokens, identifiers, arena.alloc(Expr::Parent(e))),
                _ => {
                    eprintln!("Unclosed delimiter \"(\"");
                    panic!();
                }
            }
        },
        Token::DelimOpen(Delimiter::Br) => {
            let mut exprs:Vec<&Expr> = Vec::new();
            while *peek!(tokens) != Token::DelimClose(Delimiter::Br) {
                exprs.push(&parse_full_expr(arena, tokens, identifiers));
            }
            next!(tokens);
            arena.alloc(Expr::Block(exprs))
        },
        Token::Eof => arena.alloc(Expr::End),
        tk => {
            eprintln!("Unexpected token : {:?}", tk);
            panic!();
        }
    }
}

fn parse_expr_next<'a> (arena:&'a Arena<Expr<'a>>, tokens:&mut TkIter, identifiers:&mut Vec<String>, e:&'a Expr<'a>) -> &'a Expr<'a>
{
    match peek!(tokens)
    {
        Token::Op(op, is_assign) => {
            next!(tokens);
            make_binop(arena, op, *is_assign, e, parse_expr(arena, tokens, identifiers))
        },
        Token::DelimOpen(Delimiter::Pr) => {
            next!(tokens);
            arena.alloc(Expr::Call(e, make_expr_list(arena, tokens, identifiers, Delimiter::Pr)))
        },
        _ => e
    }
}

fn parse_structure<'a> (arena:&'a Arena<Expr<'a>>, tokens:&mut TkIter, identifiers:&mut Vec<String>, id:&str) -> &'a Expr<'a>
{
    match id
    {
        "var" => {
            match next!(tokens)
            {
                Token::Id(id) => {
                    arena.alloc(Expr::Var(
                        make_id(identifiers, id),
                        match peek!(tokens)
                        {
                            Token::Op(Op::Assign, _) => {
                                next!(tokens);
                                parse_expr(arena, tokens, identifiers)
                            },
                            tk => {
                                eprintln!("Expected assign operator, got : {:?}", tk);
                                panic!();
                            }
                        }
                    ))
                }
                tk => {
                    eprintln!("Expected identifier, got : {:?}", tk);
                    panic!();
                }
            }
        },
        "if" => {
            let cond = parse_expr(arena, tokens, identifiers);
            let then = parse_expr(arena, tokens, identifiers);
            arena.alloc(Expr::If(cond, then))
        },
        "while" => {
            let cond = parse_expr(arena, tokens, identifiers);
            let then = parse_expr(arena, tokens, identifiers);
            arena.alloc(Expr::While(cond, then))
        },
        id => {
            let id_ = make_id(identifiers, id);
            parse_expr_next(arena, tokens, identifiers, arena.alloc(Expr::Id(id_)))
        }
    }
}

fn make_binop<'a> (arena:&'a Arena<Expr<'a>>, op:&Op, is_assign:bool, el:&'a Expr<'a>, er:&'a Expr<'a>) -> &'a Expr<'a>
{
    let priority = if is_assign {
        std::u8::MAX
    } else {
        op.priority()
    };

    match er
    {
        Expr::BinOp(op_, is_assign_, el_, er_) => {
            if priority <= op_.priority() {
                arena.alloc(Expr::BinOp(*op_, *is_assign_, make_binop(arena, op, is_assign, el, el_), er_))
            } else {
                arena.alloc(Expr::BinOp(*op, is_assign, el, er))
            }
        },
        _ => arena.alloc(Expr::BinOp(*op, is_assign, el, er))
    }
}

fn make_expr_list<'a> (arena:&'a Arena<Expr<'a>>, tokens:&mut TkIter, identifiers:&mut Vec<String>, delimiter:Delimiter) -> Vec<&'a Expr<'a>>
{
    let mut expr_list = Vec::new();
    if *peek!(tokens) == Token::DelimClose(delimiter) {
        return expr_list;
    }

    loop
    {
        expr_list.push(parse_expr(arena, tokens, identifiers));
        match peek!(tokens)
        {
            Token::Comma => {
                next!(tokens);
            },
            Token::DelimClose(c) if *c == delimiter => {
                next!(tokens);
                break;
            },
            Token::Eof => {
                eprintln!("Unclosed parenthese.");
                panic!();
            },
            tk => {
                eprintln!("Expected Comma or DelimClose('{:?}'), got  {:?}", delimiter, tk);
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

fn make_id (identifiers:&mut Vec<String>, id:&str) -> usize
{
    if let Some(i) = identifiers.iter().position(|s| s.as_str() == id) { //TODO maybe HashMap<String, u32(would replace usize)> would be faster ?
        i
    } else {
        identifiers.push(String::from(id));
        identifiers.len() - 1
    }
}

/* fn unexpected_expr (e:Expr)
{
    eprintln!("Unexpected expression : {:?}", e);
    panic!();
} */

type TkIter<'l> = std::iter::Peekable<std::collections::vec_deque::Iter<'l, crate::lexer::Token<'l>>>;