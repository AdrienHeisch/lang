mod memory;

use crate::{
    lexer::Const,
    parser::Expr
};
use dynamic::Dynamic;
use memory::{
    Memory,
    // dyn_memory::DynamicMemory as MemType,
    // table_memory::TableMemory as MemType,
    static_memory::StaticMemory as MemType,
};

pub fn interpret (expr_:Expr)
{
    let mut mem = MemType::new();
    
    #[cfg(not(benchmark))]
    println!("Program stdout :");
    
    expr(&mut mem, &expr_);
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
                Const::Str(s) =>    Dynamic::new(s),
                Const::Bool(b) =>   Dynamic::new(b)
            }
        },
        Expr::Id(id) => mem.get_var(&id),
        Expr::Var(id, assign_expr) => assign(mem, &Expr::Id(id), &*assign_expr), //DESIGN should re assignation be allowed ?
        Expr::UnOp(op, e) => unop(mem, &op, &*e),
        Expr::BinOp(op, e1, e2) => binop(mem, &op, &*e1, &*e2),
        Expr::Parent(e) => expr(mem, &*e),
        Expr::Call(e, args) => call(mem, &*e, &args),
        Expr::Block(exprs) => {
            mem.open_scope();
            let out = if exprs.len() > 0
            {
                for i in 0..(exprs.len() - 1) {
                    expr(mem, &exprs[i]);
                }
                expr(mem, exprs.iter().last().unwrap())
            }
            else {
                Dynamic::new(Void)
            };
            mem.close_scope();
            out
        },
        Expr::If(cond, e) => {
            let cond_val = expr(mem, &*cond);
            if cond_val.is::<bool>()
            {
                if *cond_val.downcast_ref::<bool>().unwrap() {
                    expr(mem, &*e)
                } else {
                    Dynamic::new(Void)
                }
            } 
            else
            {
                eprintln!("Invalid condition : {:?}", cond); //TODO this should not be checked at runtime
                panic!();
            }
        },
        Expr::While(cond, e) => {
            loop
            {
                let cond_val = expr(mem, &*cond);
                if cond_val.is::<bool>()
                {
                    if *cond_val.downcast_ref::<bool>().unwrap() {
                        expr(mem, &*e);
                    } else {
                        break;
                    }
                } 
                else
                {
                    eprintln!("Invalid condition : {:?}", cond); //TODO this should not be checked at runtime
                    panic!();
                }
            }
            Dynamic::new(Void)
        },
        Expr::End => Dynamic::new(Void),
        /* Expr::Invalid => {
            eprintln!("Invalid expression : {:?}", e);
            panic!();
        } */
    }
}

fn unop<T:Memory> (mem:&mut T, op:&str, e:&Expr) -> Box<Dynamic>
{
    match expr(mem, e)
    {
        e if e.is::<f32>() => {
            let e_val = *e.downcast_ref::<f32>().unwrap();
            match op
            {
                "-" => Dynamic::new(-e_val),
                _ => {
                    eprintln!("Invalid operator : {}", op);
                    panic!();
                }
            }
        },
        e if e.is::<bool>() => {
            let e_val = *e.downcast_ref::<bool>().unwrap();
            match op
            {
                "-" => Dynamic::new(!e_val),
                _ => {
                    eprintln!("Invalid operator : {}", op);
                    panic!();
                }
            }
        },
        e => {
            eprintln!("Invalid operation : {}{}", op, crate::dyn_box_to_string!(&e));
            panic!();
        }
    }
}

fn binop<T:Memory> (mem:&mut T, op:&str, e1:&Expr, e2:&Expr) -> Box<Dynamic>
{
    match (expr(mem, e1), expr(mem, e2))
    {
        (expr1, expr2) if expr1.is::<f32>() && expr2.is::<f32>() => { //TODO match types instead of using guards (Dynamic.id) ?
            let e1_val = *expr1.downcast_ref::<f32>().unwrap();
            let e2_val = *expr2.downcast_ref::<f32>().unwrap();
            match op
            {
                "==" => Dynamic::new(e1_val == e2_val),
                "!=" => Dynamic::new(e1_val != e2_val),
                ">" =>  Dynamic::new(e1_val > e2_val),
                ">=" => Dynamic::new(e1_val >= e2_val),
                "<" =>  Dynamic::new(e1_val < e2_val),
                "<=" => Dynamic::new(e1_val <= e2_val),
                "+" =>  Dynamic::new(e1_val + e2_val),
                "-" =>  Dynamic::new(e1_val - e2_val),
                "*" =>  Dynamic::new(e1_val * e2_val),
                "/" =>  Dynamic::new(e1_val / e2_val),
                //TODO EXTRACT TO METHOD FROM HERE ?
                "=" => assign(mem, e1, e2),
                _ => {
                    if op.len() > 1 && op.ends_with("=") {
                        let value = *binop(mem, &op[0..op.len() - 1], e1, e2).downcast_ref::<f32>().unwrap();
                        assign(mem, e1, &Expr::Const(Const::Number(value)))
                    } else {
                        eprintln!("Invalid operator : {}", op);
                        panic!();
                    }
                }
                //TO HERE
            }
        },
        (expr1, expr2) if expr1.is::<String>() && expr2.is::<String>() => {
            let e1_val = expr1.downcast_ref::<String>().unwrap();
            let e2_val = expr2.downcast_ref::<String>().unwrap();
            match &op[..]
            {
                "==" => Dynamic::new(e1_val == e2_val),
                "!=" => Dynamic::new(e1_val != e2_val),
                "+" => Dynamic::new(format!("{}{}", e1_val, e2_val)), //TODO check performance
                "=" => assign(mem, e1, e2),
                _ => {
                    if op.len() > 1 && op.ends_with("=") {
                        let value = binop(mem, &op[0..op.len() - 1], e1, e2).downcast_ref::<String>().unwrap().clone();
                        assign(mem, e1, &Expr::Const(Const::Str(value)))
                    } else {
                        eprintln!("Invalid operator : {}", op);
                        panic!();
                    }
                }
            }
        },
        (expr1, expr2) if expr1.is::<bool>() && expr2.is::<bool>() => {
            let e1_val = *expr1.downcast_ref::<bool>().unwrap();
            let e2_val = *expr2.downcast_ref::<bool>().unwrap();
            match &op[..]
            {
                "==" => Dynamic::new(e1_val == e2_val),
                "!=" => Dynamic::new(e1_val != e2_val),
                "&&" => Dynamic::new(e1_val && e2_val),
                "||" => Dynamic::new(e1_val || e2_val),
                "=" => assign(mem, e1, e2),
                _ => {
                    eprintln!("Invalid operator : {}", op);
                    panic!();
                }
            }
        },
        (expr1, expr2) => {
            eprintln!("Invalid operation : {} {} {}", crate::dyn_box_to_string!(&expr1), op, crate::dyn_box_to_string!(&expr2));
            panic!();
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
            panic!();
        }
    }
    value
}

fn call<T:Memory> (mem:&mut T, e:&Expr, args:&Vec<Expr>) -> Box<Dynamic>
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
                        args.iter().for_each(|arg| print!("{} ", crate::dyn_box_to_string!(expr(mem, arg))));
                        println!();
                    }
                    Dynamic::new(Void)
                },
                "printmem" => {
                    #[cfg(not(benchmark))]
                    {
                        println!("\nMemory state :");
                        mem.print_memory();
                    }
                    Dynamic::new(Void)
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

struct Void;