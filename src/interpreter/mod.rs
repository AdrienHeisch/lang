use crate::{
    ast::{ Identifier, Expr, Op },
    env::Environment,
    memory::{ Memory, Pointer },
    langval::{ LangVal, LangType },
    utils
};

const F32_EQ_THRESHOLD:f32 = 1e-6;

pub struct Interpreter
{
    memory: Memory,
    stack: [Pointer; 256],
    env: Environment
}

impl Interpreter
{
    
    pub fn new () -> Self
    {
        Self
        {
            memory: Memory::new(),
            stack: [Default::default(); 256],
            env: Default::default()
        }
    }

    // ----- INTERP

    pub fn interpret (&mut self, exprs:&[&Expr])
    {
        #[cfg(benchmark)]
        {
            self.stack = [Default::default(); 256];
            self.env = Environment
            {
                locals: [([0; 16], 0); 256],
                locals_count: 0,
                scope_depth: 0
            };
            self.memory = Memory::new();
        }

        #[cfg(not(benchmark))]
        println!("Program stdout :");
        
        for e in exprs {
            self.expr(e);
        }

        #[cfg(not(benchmark))]
        println!();
    }

    fn expr (&mut self, e:&Expr) -> LangVal
    {
        use Expr::*;
        match e
        {
            Const(value) => value.clone(),
            Id(id) => self.memory.get_var(&self.get_pointer(id).unwrap()),
            Var(id, assign_expr) => {
                //TODO check if variable exists ?
                let value = self.expr(assign_expr);
                let ptr = self.declare(id, value.as_type());
                self.stack[self.env.locals_count as usize - 1] = self.memory.set_var(ptr, &value);
                value
                //FIXME re assigning variable with different size can lead to problems -> forbid re assigning ?
            }, //DESIGN should re assignation be allowed ?
            UnOp(op, e) => self.unop(*op, e),
            BinOp { op, is_assign, left, right } => self.binop(*op, *is_assign, left, right),
            Parent(e) => self.expr(e),
            Call { name, args } => self.call(name, args),
            FnDecl {..} => {
                LangVal::Void
            },
            // Struct {..} => unimplemented!(),
            Field(_, _) => unimplemented!(),
            Block(exprs) => {
                if !exprs.is_empty()
                {
                    self.env.open_scope();
                    for e in exprs.iter().take(exprs.len() - 1) {
                        self.expr(e);
                    }
                    let ret = self.expr(exprs.iter().last().unwrap());
                    self.env.close_scope();
                    ret
                }
                else {
                    LangVal::Void
                }
            },
            If { cond, then, elze } => {
                match self.expr(cond)
                {
                    LangVal::Bool(b) => {
                        if b {
                            self.expr(then)
                        } else if let Some(elze) = elze {
                            self.expr(elze)
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
            While { cond, body } => {
                loop
                {
                    match self.expr(cond) //TODO this should not be checked at runtime
                    {
                        LangVal::Bool(b) => {
                            if b {
                                self.expr(body);
                            } else {
                                break;
                            }
                        },
                        _ => panic!("Invalid condition : {:?}", cond)
                    }
                }
                LangVal::Void
            },
            End => LangVal::Void,
            /* Invalid => {
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
                    _ => panic!("Invalid operator : {:?}", op)
                }
            },
            LangVal::Bool(b) => {
                match op
                {
                    Op::Not => LangVal::Bool(!b),
                    _ => panic!("Invalid operator : {:?}", op)
                }
            },
            value => panic!("Invalid operation : {:?}{:?}", op, value)
        }
    }

    fn binop (&mut self, op:Op, is_assign:bool, e1:&Expr, e2:&Expr) -> LangVal
    {
        use { Op::*, LangVal::* };
        match (self.expr(e1), self.expr(e2))
        {
            (Number(f1), Number(f2)) => {
                if is_assign {
                    let value = self.binop(op, false, e1, e2);
                    self.assign(e1, value)
                } else {
                    use utils::compare_floats;
                    match op
                    {
                        Assign => {
                            let value = self.expr(e2);
                            self.assign(e1, value)
                        },
                        Equal       => Bool( compare_floats(f1, f2, F32_EQ_THRESHOLD)),
                        NotEqual    => Bool(!compare_floats(f1, f2, F32_EQ_THRESHOLD)),
                        Gt          => Bool(f1 > f2),
                        Gte         => Bool(f1 > f2 || compare_floats(f1, f2, F32_EQ_THRESHOLD)),
                        Lt          => Bool(f1 < f2 || compare_floats(f1, f2, F32_EQ_THRESHOLD)),
                        Lte         => Bool(f1 <= f2),
                        Add         => Number(f1 + f2),
                        Sub         => Number(f1 - f2),
                        Mult        => Number(f1 * f2),
                        Div         => Number(f1 / f2),
                        _ => panic!("Invalid operator : {:?}", op)
                    }
                }
            },
            (Str(s1), Str(s2)) => {
                if is_assign {
                    let value = self.binop(op, false, e1, e2);
                    self.assign(e1, value)
                } else {
                    match op
                    {
                        Assign => {
                            let value = self.expr(e2);
                            self.assign(e1, value)
                        },
                        Equal       => Bool(s1 == s2),
                        NotEqual    => Bool(s1 != s2),
                        Add         => Str(s1 + &s2),
                        _ => panic!("Invalid operator : {:?}", op)
                    }
                }
            },
            (Bool(b1), Bool(b2)) => {
                match op
                {
                    Assign => {
                        let value = self.expr(e2);
                        self.assign(e1, value)
                    },
                    Equal       => Bool(b1 == b2),
                    NotEqual    => Bool(b1 != b2),
                    BoolAnd     => Bool(b1 && b2),
                    BoolOr      => Bool(b1 || b2),
                    _ => panic!("Invalid operator : {:?}", op)
                }
            },
            (e1, e2) => {
                panic!("Invalid operation : {:?} {:?} {:?}", e1, op, e2);
                // Void
            }
        }
    }

    fn assign (&mut self, to:&Expr, value:LangVal) -> LangVal
    {
        if let Expr::Id(id) = to {
            self.stack[self.env.locals_count as usize - 1] = self.memory.set_var(self.get_pointer(id).unwrap(), &value);
        } else {
            panic!("Can't assign {:?} to {:?}", value, to);
        }
        /* match to
        {
            Expr::Id(id) => self.stack[self.env.locals_count as usize] = self.memory.set_var(self.get_pointer(id).unwrap(), &value),
            _ => panic!("Can't assign {:?} to {:?}", value, to)
        } */
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
                            self.print_locals();
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

    // ----- VARIABLES
    
    pub fn declare (&mut self, id:&Identifier, t:LangType) -> Pointer
    {
        let ptr = self.memory.make_pointer_for_type(t);
        self.env.locals[self.env.locals_count as usize] = (*id, self.env.scope_depth);
        self.stack[self.env.locals_count as usize] = ptr;
        if let Some(n) = self.env.locals_count.checked_add(1) {
            self.env.locals_count = n;
        } else {
            panic!("Too many locals.");
        }
        ptr
    }

    fn get_pointer (&self, id:&Identifier) -> Option<Pointer>
    {
        for i in (0..self.env.locals_count as usize).rev()
        {
            let (id_, _) = self.env.locals[i];
            if *id == id_ {
                return Some(self.stack[i]);
            }
        }
        None
    }
    
    #[allow(dead_code)]
    pub fn print_locals (&self)
    {
        println!("Locals:");
        let mut current_depth = if self.env.scope_depth > 0 {
            self.env.scope_depth + 1
        } else {
            0
        };

        for i in (0..self.env.locals_count as usize).rev()
        {
            let (id, depth) = self.env.locals[i];

            if current_depth > depth {
                current_depth = depth;
                println!("----- Depth: {} -----", current_depth);
            }

            let id_str = String::from_utf8(id.iter().take_while(|i| **i != 0).cloned().collect()).ok().unwrap();
            let ptr = self.get_pointer(&id).unwrap();
            println!("{} => {:?} => {:?}", id_str, ptr, self.memory.get_var(&ptr));
        }

        println!();
        self.memory.print_ram();
    }

}