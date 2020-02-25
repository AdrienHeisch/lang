pub mod memory;

use crate::{
    ast::{ Expr, Op },
    langval::LangVal,
    utils
};
use memory::{
    Memory,
    // vec_memory::VecMemory as MemType,
    // table_memory::TableMemory as MemType,
    // hashmap_memory::HashMapMemory as MemType,
    static_memory::StaticMemory as MemType,
    // dumb_memory::DumbMemory as MemType,
};
use std::collections::HashMap;

const F32_EQUALITY_THRESHOLD:f32 = 1e-6;

pub struct Interpreter
{
    memory: MemType,
    globals: HashMap<String, ()>
}

impl Interpreter
{

    pub fn new () -> Self
    {
        Self
        {
            memory: MemType::new(),
            globals: HashMap::new()
        }
    }

    pub fn interpret (&mut self, exprs:&[&Expr])
    {
        self.memory = MemType::new(); //TODO REMOVE THIS

        #[cfg(not(benchmark))]
        println!("Program stdout :");
        
        for e in exprs {
            self.expr(e);
        }

        #[cfg(not(benchmark))]
        println!();
    }

    pub fn get_memory (&self) -> &impl Memory
    {
        &self.memory
    }

    fn expr (&mut self, e:&Expr) -> LangVal
    {
        match e
        {
            Expr::Const(value) => value.clone(),
            Expr::Id(id) => self.memory.get_var(id),
            Expr::Var(id, assign_expr) => {
                // mem.get_var(id); //TODO check if variable exists
                self.assign(&Expr::Id(*id), assign_expr) //FIXME re assigning variable with different size can lead to problems -> forbid re assigning ?
            }, //DESIGN should re assignation be allowed ?
            Expr::UnOp(op, e) => self.unop(*op, e),
            Expr::BinOp(op, is_assign, e1, e2) => self.binop(*op, *is_assign, e1, e2),
            Expr::Parent(e) => self.expr(e),
            Expr::Call(e, args) => self.call(e, args),
            Expr::Block(exprs) => {
                self.memory.open_scope();
                let out = if !exprs.is_empty()
                {
                    for e in exprs.iter().take(exprs.len() - 1) {
                        self.expr(e);
                    }
                    self.expr(exprs.iter().last().unwrap())
                }
                else {
                    LangVal::Void
                };
                //TODO only in debug mode ?
                //if there is no more scopes to close, prints memory
                self.memory.close_scope();
                out
            },
            Expr::If(cond, e, else_) => {
                match self.expr(cond)
                {
                    LangVal::Bool(b) => {
                        if b {
                            self.expr(e)
                        } else if else_.is_some() {
                            self.expr(else_.unwrap())
                        } else {
                            LangVal::Void
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
                    match self.expr(cond)
                    {
                        LangVal::Bool(b) => {
                            if b {
                                self.expr(e);
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
                LangVal::Void
            },
            Expr::End => LangVal::Void, //TODO ?
            /* Expr::Invalid => {
                eprintln!("Invalid expression : {:?}", e);
                panic!();
            } */
        }
    }

    fn unop (&mut self, op:Op, e:&Expr) -> LangVal
    {
        match self.expr(e)
        {
            LangVal::Number(f) => {
                match op
                {
                    Op::Sub => LangVal::Number(-f),
                    _ => {
                        eprintln!("Invalid operator : {:?}", op);
                        panic!();
                    }
                }
            },
            LangVal::Bool(b) => {
                match op
                {
                    Op::Not => LangVal::Bool(!b),
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

    fn binop (&mut self, op:Op, is_assign:bool, e1:&Expr, e2:&Expr) -> LangVal
    {
        match (self.expr(e1), self.expr(e2))
        {
            (LangVal::Number(f1), LangVal::Number(f2)) => { //TODO match types instead of using guards (Dynamic.id) ?
                if is_assign {
                    let value = self.binop(op, false, e1, e2);
                    self.assign(e1, &Expr::Const(value))
                } else {
                    match op
                    {
                        Op::Assign      => self.assign(e1, e2),
                        Op::Equal       => LangVal::Bool( utils::compare_floats(f1, f2, F32_EQUALITY_THRESHOLD)),
                        Op::NotEqual    => LangVal::Bool(!utils::compare_floats(f1, f2, F32_EQUALITY_THRESHOLD)),
                        Op::Gt          => LangVal::Bool(f1 > f2),
                        Op::Gte         => LangVal::Bool(f1 > f2 || utils::compare_floats(f1, f2, F32_EQUALITY_THRESHOLD)),
                        Op::Lt          => LangVal::Bool(f1 < f2 || utils::compare_floats(f1, f2, F32_EQUALITY_THRESHOLD)),
                        Op::Lte         => LangVal::Bool(f1 <= f2),
                        Op::Add         => LangVal::Number(f1 + f2),
                        Op::Sub         => LangVal::Number(f1 - f2),
                        Op::Mult        => LangVal::Number(f1 * f2),
                        Op::Div         => LangVal::Number(f1 / f2),
                        _ => {
                            eprintln!("Invalid operator : {:?}", op);
                            panic!();
                        }
                    }
                }
            },
            (LangVal::Str(s1), LangVal::Str(s2)) => {
                if is_assign {
                    let value = self.binop(op, false, e1, e2);
                    self.assign(e1, &Expr::Const(value))
                } else {
                    match op
                    {
                        Op::Assign      => self.assign(e1, e2),
                        Op::Equal       => LangVal::Bool(s1 == s2),
                        Op::NotEqual    => LangVal::Bool(s1 != s2),
                        Op::Add         => LangVal::Str(s1 + &s2),
                        _ => {
                            eprintln!("Invalid operator : {:?}", op);
                            panic!();
                        }
                    }
                }
            },
            (LangVal::Bool(b1), LangVal::Bool(b2)) => {
                match op
                {
                    Op::Assign      => self.assign(e1, e2),
                    Op::Equal       => LangVal::Bool(b1 == b2),
                    Op::NotEqual    => LangVal::Bool(b1 != b2),
                    Op::BoolAnd     => LangVal::Bool(b1 && b2),
                    Op::BoolOr      => LangVal::Bool(b1 || b2),
                    _ => {
                        eprintln!("Invalid operator : {:?}", op);
                        panic!();
                    }
                }
            },
            (e1, e2) => {
                eprintln!("Invalid operation : {:?} {:?} {:?}", e1, op, e2);
                panic!();
                // Const::Void
            }
        }
    }

    fn assign (&mut self, to:&Expr, from:&Expr) -> LangVal
    {
        let value = self.expr(from);
        match to
        {
            Expr::Id(id) => {
                self.memory.set_var(id, &value);
            },
            _ => {
                eprintln!("Can't assign {:?} to {:?}", from, to);
                panic!();
            }
        }
        value
    }

    fn call (&mut self, e:&Expr, args:&[&Expr]) -> LangVal
    {
        match e
        {
            Expr::Id(id) => {
                match id
                {
                    //"print"
                    [0x70, 0x72, 0x69, 0x6e, 0x74, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/* , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 */] => {
                        #[cfg(not(benchmark))]
                        {
                            print!("> ");
                            args.iter().for_each(|arg| print!("{} ", self.expr(arg)));
                            println!();
                        }
                        LangVal::Void
                    },
                    //"printmem"
                    [0x70, 0x72, 0x69, 0x6E, 0x74, 0x6D, 0x65, 0x6D, 0, 0, 0, 0, 0, 0, 0, 0/* , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 */] => {
                        #[cfg(not(benchmark))]
                        {
                            println!("\nMemory state :");
                            self.memory.print_memory();
                        }
                        LangVal::Void
                    },
                    id => {
                        eprintln!("Unknown function identifier : {}", std::str::from_utf8(id).ok().unwrap());
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

}