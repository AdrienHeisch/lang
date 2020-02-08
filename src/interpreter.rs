mod memory;

use crate::{
    cst::Const,
    parser::Expr,
    utils
};
use memory::{
    Memory,
    // dyn_memory::DynamicMemory as MemType,
    // table_memory::TableMemory as MemType,
    static_memory::StaticMemory as MemType,
};

const F32_EQUALITY_THRESHOLD:f32 = 1e-6;

pub fn interpret (expr_:Expr)
{
    let mut mem = MemType::new();
    
    #[cfg(not(benchmark))]
    println!("Program stdout :");
    
    expr(&mut mem, &expr_);
}

fn expr<T:Memory> (mem:&mut T, e:&Expr) -> Const
{
    let e = e.clone();
    match e
    {
        Expr::Const(cst) => cst,
        Expr::Id(id) => mem.get_var(&id),
        Expr::Var(id, assign_expr) => assign(mem, &Expr::Id(id), &*assign_expr), //DESIGN should re assignation be allowed ?
        Expr::UnOp(op, e) => unop(mem, &op, &*e),
        Expr::BinOp(op, e1, e2) => binop(mem, &op, &*e1, &*e2),
        Expr::Parent(e) => expr(mem, &*e),
        Expr::Call(e, args) => call(mem, &*e, &args),
        Expr::Block(exprs) => {
            mem.open_scope();
            let out = if !exprs.is_empty()
            {
                for e in exprs.iter().take(exprs.len() - 1) {
                    expr(mem, e);
                }
                expr(mem, exprs.iter().last().unwrap())
            }
            else {
                Const::Void
            };
            mem.close_scope();
            out
        },
        Expr::If(cond, e) => {
            match expr(mem, &*cond)
            {
                Const::Bool(b) => {
                    if b {
                        expr(mem, &*e)
                    } else {
                        Const::Void
                    }
                },
                _ => {
                    eprintln!("Invalid condition : {:?}", cond); //TODO this should not be checked at runtime
                    panic!();
                }
            }
        },
        Expr::While(cond, e) => {
            loop
            {
                match expr(mem, &*cond)
                {
                    Const::Bool(b) => {
                        if b {
                            expr(mem, &*e);
                        } else {
                            break;
                        }
                    },
                    _ => {
                        eprintln!("Invalid condition : {:?}", cond); //TODO this should not be checked at runtime
                        panic!();
                    }
                }
            }
            Const::Void
        },
        Expr::End => Const::Void,
        /* Expr::Invalid => {
            eprintln!("Invalid expression : {:?}", e);
            panic!();
        } */
    }
}

fn unop<T:Memory> (mem:&mut T, op:&str, e:&Expr) -> Const
{
    match expr(mem, e)
    {
        Const::Number(f) => {
            match op
            {
                "-" => Const::Number(-f),
                _ => {
                    eprintln!("Invalid operator : {}", op);
                    panic!();
                }
            }
        },
        Const::Bool(b) => {
            match op
            {
                "-" => Const::Bool(!b),
                _ => {
                    eprintln!("Invalid operator : {}", op);
                    panic!();
                }
            }
        },
        e => {
            eprintln!("Invalid operation : {}{:?}", op, e);
            panic!();
        }
    }
}

fn binop<T:Memory> (mem:&mut T, op:&str, e1:&Expr, e2:&Expr) -> Const
{
    match (expr(mem, e1), expr(mem, e2))
    {
        (Const::Number(f1), Const::Number(f2)) => { //TODO match types instead of using guards (Dynamic.id) ?
            match op
            {
                "==" => Const::Bool( utils::compare_floats(f1, f2, F32_EQUALITY_THRESHOLD)),
                "!=" => Const::Bool(!utils::compare_floats(f1, f2, F32_EQUALITY_THRESHOLD)),
                ">" =>  Const::Bool(f1 > f2),
                ">=" => Const::Bool(f1 >= f2),
                "<" =>  Const::Bool(f1 < f2),
                "<=" => Const::Bool(f1 <= f2),
                "+" =>  Const::Number(f1 + f2),
                "-" =>  Const::Number(f1 - f2),
                "*" =>  Const::Number(f1 * f2),
                "/" =>  Const::Number(f1 / f2),
                //TODO EXTRACT TO METHOD FROM HERE ?
                "=" =>  assign(mem, e1, e2),
                _ => {
                    if op.len() > 1 && op.ends_with('=') {
                        let value = binop(mem, &op[0..(op.len() - 1)], e1, e2);
                        assign(mem, e1, &Expr::Const(value))
                    } else {
                        eprintln!("Invalid operator : {}", op);
                        panic!();
                    }
                }
                //TO HERE
            }
        },
        (Const::Str(s1), Const::Str(s2)) => {
            match &op[..]
            {
                "==" => Const::Bool(s1 == s2),
                "!=" => Const::Bool(s1 == s2),
                "+" =>  Const::Str(format!("{}{}", s1, s2)), //TODO check performance
                "=" =>  assign(mem, e1, e2),
                _ => {
                    if op.len() > 1 && op.ends_with('=') {
                        let value = binop(mem, &op[0..(op.len() - 1)], e1, e2);
                        assign(mem, e1, &Expr::Const(value))
                    } else {
                        eprintln!("Invalid operator : {}", op);
                        panic!();
                    }
                }
            }
        },
        (Const::Bool(b1), Const::Bool(b2)) => {
            match &op[..]
            {
                "==" => Const::Bool(b1 == b2),
                "!=" => Const::Bool(b1 == b2),
                "&&" => Const::Bool(b1 && b2),
                "||" => Const::Bool(b1 || b2),
                "=" =>  assign(mem, e1, e2),
                _ => {
                    eprintln!("Invalid operator : {}", op);
                    panic!();
                }
            }
        },
        (e1, e2) => {
            eprintln!("Invalid operation : {:?} {} {:?}", e1, op, e2);
            panic!();
        }
    }
}

fn assign<T:Memory> (mem:&mut T, to:&Expr, from:&Expr) -> Const
{
    let value = expr(mem, from);
    match to
    {
        Expr::Id(id) => {
            mem.set_var(id, &value);
        },
        _ => {
            eprintln!("Can't assign {:?} to {:?}", from, to);
            panic!();
        }
    }
    value
}

fn call<T:Memory> (mem:&mut T, e:&Expr, args:&[Expr]) -> Const
{
    match e
    {
        Expr::Id(id) => {
            match id.as_str()
            {
                "print" => {
                    #[cfg(not(benchmark))]
                    {
                        print!("> ");
                        args.iter().for_each(|arg| print!("{} ", expr(mem, arg)));
                        println!();
                    }
                    Const::Void
                },
                "printmem" => {
                    #[cfg(not(benchmark))]
                    {
                        println!("\nMemory state :");
                        mem.print_memory();
                    }
                    Const::Void
                },
                id => {
                    eprintln!("Unknown function identifier : {}", id);
                    panic!();
                }
            }
        },
        e => {
            eprintln!("Expected an identifier : {:?}", e);
            panic!();
        }
    }
}