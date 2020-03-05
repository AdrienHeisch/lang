use crate::{
    ast::{ Identifier, Expr, Op },
    env::Environment,
    memory::{ Memory, Pointer },
    langval::{ LangVal, LangType },
    utils
};

const F32_EQ_THRESHOLD:f32 = 1e-6;

pub struct Interpreter<'e>
{
    memory: Memory,
    stack: [PtrOrFn<'e>; 256], //TODO STACK_SIZE
    frame_ptr: u8,
    env: Environment
}

#[derive(Debug, Clone)]
enum PtrOrFn<'e>
{
    Ptr(Pointer),
    Fn(Box<[Identifier]>, Expr<'e>)
}

impl<'e> Interpreter<'e>
{
    
    pub fn new () -> Self
    {
        Self
        {
            memory: Memory::new(),
            stack: unsafe {
                let mut arr:[_; 256] = std::mem::MaybeUninit::uninit().assume_init();
                let value = PtrOrFn::Ptr(Default::default());
                for item in &mut arr[..] {
                    *item = std::mem::transmute_copy(&value);
                }
                arr
            },
            frame_ptr: 0,
            env: Environment::new()
        }
    }

    // ----- INTERP

    pub fn interpret (&mut self, exprs:&[&Expr<'e>])
    {
        #[cfg(not(benchmark))]
        println!("Program stdout :");
        
        for e in exprs {
            self.expr(e);
        }

        #[cfg(not(benchmark))]
        println!();
    }

    #[allow(dead_code)]
    pub fn reset (&mut self)
    {
        self.memory = Memory::new();
        unsafe {
            let value = PtrOrFn::Ptr(Default::default());
            for item in &mut self.stack[..] {
                std::ptr::copy_nonoverlapping(&value, item, 1);
            }
        };
        self.frame_ptr = 0;
        self.env = Environment::new();
    }

    fn expr (&mut self, e:&Expr<'e>) -> LangVal
    {
        use Expr::*;
        match e
        {
            // --- Values
            Const(value) => value.clone(),
            Id(id) => {
                self.memory.get_var(&if let PtrOrFn::Ptr(ptr) = self.get_pointer(id).unwrap()
                    {
                        ptr
                    } else {
                        panic!("Tried to use function as value.") //TODO //DESIGN functions as values ?
                    }
                )
            },
            // --- Control Flow
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
            // --- Operations
            UnOp(op, e) => self.unop(*op, e),
            BinOp { op, is_assign, left, right } => self.binop(*op, *is_assign, left, right),
            Call { name, args } => self.call(name, args),
            Field(_, _) => unimplemented!(),
            // --- Declarations
            Var(id, assign_expr) => {
                //TODO check if variable exists ?
                //DESIGN should re assignation be allowed ?
                //FIXME re assigning variable with different size can lead to problems -> forbid re assigning ?
                let value = self.expr(assign_expr);
                let ptr = self.declare_var(id, value.as_type());
                //TODO remove this hack
                self.stack[self.frame_ptr as usize + self.env.locals_count as usize - 1] = PtrOrFn::Ptr(self.memory.set_var(ptr, &value));
                value
            }, 
            FnDecl { id, params, body } => {
                self.declare_fn(id, params.clone(), body);
                // println!("{:?}", self.stack[self.env.locals_count as usize - 1]);
                LangVal::Void
            },
            // Struct {..} => unimplemented!(),
            // --- Others
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
            Parent(e) => self.expr(e),
            End => LangVal::Void,
            /* Invalid => {
                eprintln!("Invalid expression : {:?}", e);
                panic!();
            } */
        }
    }

    fn unop (&mut self, op:Op, e_right:&Expr<'e>) -> LangVal
    {
        match self.expr(e_right)
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

    fn binop (&mut self, op:Op, is_assign:bool, e_left:&Expr<'e>, e_right:&Expr<'e>) -> LangVal
    {
        use { Op::*, LangVal::* };
        match (self.expr(e_left), self.expr(e_right))
        {
            (Number(f1), Number(f2)) => {
                if is_assign {
                    let value = self.binop(op, false, e_left, e_right);
                    self.assign(e_left, value)
                } else {
                    use utils::compare_floats;
                    match op
                    {
                        Assign => {
                            let value = self.expr(e_right);
                            self.assign(e_left, value)
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
                    let value = self.binop(op, false, e_left, e_right);
                    self.assign(e_left, value)
                } else {
                    match op
                    {
                        Assign => {
                            let value = self.expr(e_right);
                            self.assign(e_left, value)
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
                        let value = self.expr(e_right);
                        self.assign(e_left, value)
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

    fn assign (&mut self, e_to:&Expr, value:LangVal) -> LangVal
    {
        if let Expr::Id(id) = e_to {
            self.memory.set_var(if let PtrOrFn::Ptr(ptr) = self.get_pointer(id).unwrap()
            {
                ptr
            } else {
                panic!("Tried to use function as value.") //TODO //DESIGN functions as values ?
            }, &value);
        } else {
            panic!("Can't assign {:?} to {:?}", value, e_to);
        }
        value
    }

    fn call (&mut self, e_id:&Expr<'e>, args:&[&Expr<'e>]) -> LangVal
    {
        match e_id
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
                        // eprintln!("Unknown function identifier : {}", std::str::from_utf8(id).ok().unwrap());
                        let (params, body) = if let PtrOrFn::Fn(params, body) = self.get_pointer(id).unwrap() //TODO env / args
                        {
                            (params, body)
                        } else {
                            panic!("Tried to call on a value.") //TODO //DESIGN functions as values ?
                        };

                        let values_and_params = args.iter().map(|arg| self.expr(arg)).zip(params.iter()).collect::<Vec<_>>();

                        let prev_frame_ptr = self.frame_ptr;
                        self.frame_ptr += self.env.locals_count;

                        let prev_env = std::mem::replace(&mut self.env, Environment::new());
                        
                        if params.len() != args.len() {
                            panic!("Invalid number of arguments.");
                        }

                        for (value, param) in values_and_params {
                            let ptr = self.declare_var(param, value.as_type());
                            self.memory.set_var(ptr, &value);
                        }

                        let out = self.expr(&body);

                        self.frame_ptr = prev_frame_ptr;
                        std::mem::replace(&mut self.env, prev_env);

                        out
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

    pub fn declare_fn (&mut self, id:&Identifier, params:Box<[Identifier]>, body:&Expr<'e>)
    {
        self.env.locals[self.env.locals_count as usize] = (*id, self.env.scope_depth);
        self.stack[self.frame_ptr as usize + self.env.locals_count as usize] = PtrOrFn::Fn(params, body.clone());
        if let Some(n) = self.env.locals_count.checked_add(1) {
            self.env.locals_count = n;
            /* if self.frame_ptr.checked_add(self.env.locals_count).is_none() {
                panic!("Stack overflow.");
            } */
        } else {
            panic!("Too many locals.");
        }
    }
    
    pub fn declare_var (&mut self, id:&Identifier, t:LangType) -> Pointer
    {
        let ptr = self.memory.make_pointer_for_type(t);
        self.env.locals[self.env.locals_count as usize] = (*id, self.env.scope_depth);
        self.stack[self.frame_ptr as usize + self.env.locals_count as usize] = PtrOrFn::Ptr(ptr);
        if let Some(n) = self.env.locals_count.checked_add(1) {
            self.env.locals_count = n;
            /* if self.frame_ptr.checked_add(self.env.locals_count).is_none() {
                panic!("Stack overflow.");
            } */
        } else {
            panic!("Too many locals.");
        }
        ptr
    }

    fn get_pointer (&self, id:&Identifier) -> Option<PtrOrFn<'e>>
    {
        for i in (0..self.env.locals_count as usize).rev()
        {
            let (id_, _) = self.env.locals[i];
            if *id == id_ {
                return Some(self.stack[self.frame_ptr as usize + i].clone());
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
            let ptr = if let PtrOrFn::Ptr(ptr) = self.get_pointer(&id).unwrap() {
                ptr
            } else {
                continue;
            };
            println!("{} => {:?} => {:?}", id_str, ptr, self.memory.get_var(&ptr));
        }

        println!();
        self.memory.print_ram();
    }

}