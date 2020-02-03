mod memory;

use crate::{
    lexer::Const,
    parser::Expr
};
use dynamic::Dynamic;
use memory::{
    Memory,
    // dyn_memory::DynamicMemory as MemType,
    static_memory::StaticMemory as MemType,
};
use std::collections::VecDeque;

pub fn interpret (exprs:VecDeque<Expr>)
{
    let mut mem = MemType::new();
    
    for e in exprs
    {
        expr(&mut mem, &e);
    }

    mem.print_memory();
}

fn expr<T:Memory> (mem:&mut T, e:&Expr) -> Box<Dynamic>
{
    let e = e.clone();
    match e
    {
        Expr::Const(cst) => {
            match cst
            {
                Const::Number(n) => Dynamic::new(n),
                Const::Str(s) =>    Dynamic::new(s)
            }
        },
        Expr::Id(id) => mem.get_var(&id),
        Expr::Var(id, assign_expr) => assign(mem, &Expr::Id(id), &*assign_expr), //TODO should re assignation be allowed ?
        Expr::BinOp(op, e1, e2) => operation(mem, &op, &*e1, &*e2),
        // Expr::EParent(e): expr(e),
        /* Expr::Invalid */e => {
            eprintln!("Invalid expression : {:?}", e);
            panic!();
        },
    }
}

fn operation<T:Memory> (mem:&mut T, op:&str, e1:&Expr, e2:&Expr) -> Box<Dynamic> //TODO FIX OPERATIONS PRECEDENCE (* and +)
{
    let expr1 = expr(mem, e1);
    let expr2 = expr(mem, e2);
    match (expr1, expr2)
    {
        (expr1, expr2) if expr1.is::<f32>() && expr2.is::<f32>() => { //TODO match types instead of using guards (Dynamic.id)
            let e1_val = *expr1.downcast_ref::<f32>().unwrap();
            let e2_val = *expr2.downcast_ref::<f32>().unwrap();
            match &op[..]
            {
                "+" => Dynamic::new(e1_val + e2_val),
                "-" => Dynamic::new(e1_val - e2_val),
                "*" => Dynamic::new(e1_val * e2_val),
                "/" => Dynamic::new(e1_val / e2_val),
                //EXTRACT TO METHOD FROM HERE
                "=" => assign(mem, e1, e2),
                _ => {
                    let last_char = if let Some(c) = op.chars().nth(op.len() - 1) { c } else { ' ' };
                    if op.len() > 1 && last_char == '=' {
                        let value:f32 = *operation(mem, &op[0..op.len() - 1], e1, e2).downcast_ref().unwrap();
                        assign(mem, e1, &Expr::Const(Const::Number(value)))
                    } else {
                        eprintln!("Invalid operator : {}", op);
                        panic!()
                    }
                }
                //TO HERE
            }
        },
        (expr1, expr2) if expr1.is::<String>() && expr2.is::<String>() => { //TODO match types instead of using guards (Dynamic.id)
            let e1_val = expr1.downcast_ref::<String>().unwrap();
            let e2_val = expr2.downcast_ref::<String>().unwrap();
            match &op[..]
            {
                "+" => Dynamic::new(format!("{}{}", e1_val, e2_val)), //TODO check performance
                "=" => assign(mem, e1, e2),
                _ => {
                    let last_char = if let Some(c) = op.chars().nth(op.len() - 1) { c } else { ' ' };
                    if op.len() > 1 && last_char == '=' {
                        let value:f32 = *operation(mem, &op[0..op.len() - 1], e1, e2).downcast_ref().unwrap();
                        assign(mem, e1, &Expr::Const(Const::Number(value)))
                    } else {
                        eprintln!("Invalid operator : {}", op);
                        panic!()
                    }
                }
            }
        },
        _ => {
            eprintln!("Invalid operation !! : {}", op);
            panic!()
        }
    }
}

fn assign<T:Memory> (mem:&mut T, to:&Expr, from:&Expr) -> Box<Dynamic>
{
    let value = expr(mem, from);
    match to
    {
        Expr::Id(id) => {
            mem.set_var(id, &value);
        },
        _ => {
            eprintln!("Can't assign {:?} to {:?}", from, to);
            panic!()
        }
    }
    value
}