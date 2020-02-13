mod memory;

use crate::{
    cst::Const,
    expr::Expr,
    op::Op,
    utils
};
use memory::{
    Memory,
    // dyn_memory::DynamicMemory as MemType,
    // table_memory::TableMemory as MemType,
    static_memory::StaticMemory as MemType,
    // dumb_memory::DumbMemory as MemType,
};

const F32_EQUALITY_THRESHOLD:f32 = 1e-6;

//TODO identifiers won't need to be passed here as soon as function definitions are implemented
pub fn interpret (expr_:&Expr, identifiers:&[String])
{
    let mut mem = MemType::new();
    
    #[cfg(not(benchmark))]
    println!("Program stdout :");
    
    expr(&mut mem, identifiers, expr_);
}

fn expr<T:Memory> (mem:&mut T, identifiers:&[String], e:&Expr) -> Const
{
    match e
    {
        Expr::Const(cst) => cst.clone(),
        Expr::Id(id) => mem.get_var(*id),
        Expr::Var(id, assign_expr) => assign(mem, identifiers, &Expr::Id(*id), assign_expr), //DESIGN should re assignation be allowed ?
        Expr::UnOp(op, e) => unop(mem, identifiers, *op, e),
        Expr::BinOp(op, is_assign, e1, e2) => binop(mem, identifiers, *op, *is_assign, e1, e2),
        Expr::Parent(e) => expr(mem, identifiers, e),
        Expr::Call(e, args) => call(mem, identifiers, e, args),
        Expr::Block(exprs) => {
            mem.open_scope();
            let out = if !exprs.is_empty()
            {
                for e in exprs.iter().take(exprs.len() - 1) {
                    expr(mem, identifiers, e);
                }
                expr(mem, identifiers, exprs.iter().last().unwrap())
            }
            else {
                Const::Void
            };
            mem.close_scope();
            out
        },
        Expr::If(cond, e) => {
            match expr(mem, identifiers, cond)
            {
                Const::Bool(b) => {
                    if b {
                        expr(mem, identifiers, e)
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
                match expr(mem, identifiers, cond)
                {
                    Const::Bool(b) => {
                        if b {
                            expr(mem, identifiers, e);
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

fn unop<T:Memory> (mem:&mut T, identifiers:&[String], op:Op, e:&Expr) -> Const
{
    match expr(mem, identifiers, e)
    {
        Const::Number(f) => {
            match op
            {
                Op::Sub => Const::Number(-f),
                _ => {
                    eprintln!("Invalid operator : {:?}", op);
                    panic!();
                }
            }
        },
        Const::Bool(b) => {
            match op
            {
                Op::Not => Const::Bool(!b),
                _ => {
                    eprintln!("Invalid operator : {:?}", op);
                    panic!();
                }
            }
        },
        cst => {
            eprintln!("Invalid operation : {:?}{}", op, cst);
            panic!();
        }
    }
}

fn binop<T:Memory> (mem:&mut T, identifiers:&[String], op:Op, is_assign:bool, e1:&Expr, e2:&Expr) -> Const
{
    match (expr(mem, identifiers, e1), expr(mem, identifiers, e2))
    {
        (Const::Number(f1), Const::Number(f2)) => { //TODO match types instead of using guards (Dynamic.id) ?
            if is_assign {
                let value = binop(mem, identifiers, op, false, e1, e2);
                assign(mem, identifiers, e1, &Expr::Const(value))
            } else {
                match op
                {
                    Op::Assign      =>  assign(mem, identifiers, e1, e2),
                    Op::Equal       => Const::Bool( utils::compare_floats(f1, f2, F32_EQUALITY_THRESHOLD)),
                    Op::NotEqual    => Const::Bool(!utils::compare_floats(f1, f2, F32_EQUALITY_THRESHOLD)),
                    Op::Gt          =>  Const::Bool(f1 > f2),
                    Op::Gte         => Const::Bool(f1 >= f2),
                    Op::Lt          =>  Const::Bool(f1 < f2),
                    Op::Lte         => Const::Bool(f1 <= f2),
                    Op::Add         =>  Const::Number(f1 + f2),
                    Op::Sub         =>  Const::Number(f1 - f2),
                    Op::Mult        =>  Const::Number(f1 * f2),
                    Op::Div         =>  Const::Number(f1 / f2),
                    _ => {
                        eprintln!("Invalid operator : {:?}", op);
                        panic!();
                    }
                }
            }
        },
        (Const::Str(s1), Const::Str(s2)) => {
            if is_assign {
                let value = binop(mem, identifiers, op, false, e1, e2);
                assign(mem, identifiers, e1, &Expr::Const(value))
            } else {
                match op
                {
                    Op::Assign      =>  assign(mem, identifiers, e1, e2),
                    Op::Equal       => Const::Bool(s1 == s2),
                    Op::NotEqual    => Const::Bool(s1 == s2),
                    Op::Add         =>  Const::Str(format!("{}{}", s1, s2)), //TODO check performance
                    _ => {
                        eprintln!("Invalid operator : {:?}", op);
                        panic!();
                    }
                }
            }
        },
        (Const::Bool(b1), Const::Bool(b2)) => {
            match op
            {
                Op::Assign      => assign(mem, identifiers, e1, e2),
                Op::Equal       => Const::Bool(b1 == b2),
                Op::NotEqual    => Const::Bool(b1 == b2),
                Op::BoolAnd     => Const::Bool(b1 && b2),
                Op::BoolOr      => Const::Bool(b1 || b2),
                _ => {
                    eprintln!("Invalid operator : {:?}", op);
                    panic!();
                }
            }
        },
        (e1, e2) => {
            eprintln!("Invalid operation : {:?} {:?} {:?}", e1, op, e2);
            panic!();
        }
    }
}

fn assign<T:Memory> (mem:&mut T, identifiers:&[String], to:&Expr, from:&Expr) -> Const
{
    let value = expr(mem, identifiers, from);
    match to
    {
        Expr::Id(id) => {
            mem.set_var(*id, &value);
        },
        _ => {
            eprintln!("Can't assign {:?} to {:?}", from, to);
            panic!();
        }
    }
    value
}

fn call<T:Memory> (mem:&mut T, identifiers:&[String], e:&Expr, args:&[&Expr]) -> Const
{
    match e
    {
        Expr::Id(id) => {
            match identifiers[*id].as_str()
            {
                "print" => {
                    #[cfg(not(benchmark))]
                    {
                        print!("> ");
                        args.iter().for_each(|arg| print!("{} ", expr(mem, identifiers, arg)));
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