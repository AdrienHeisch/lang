use crate::{
    ast::{ Identifier, Expr, ExprDef, Op, Error, FullPosition, WithPosition },
    env::{ Environment, Context },
    memory::{ Memory, Pointer },
    langval::{ LangVal, LangType },
    utils
};

const F32_EQ_THRESHOLD:f32 = 1e-6;

pub struct Interpreter<'e, 's>
{
    memory: Memory,
    stack: [PtrOrFn<'e, 's>; 256], //TODO STACK_SIZE
    frame_ptr: u8,
    env: Environment
}

#[derive(Debug, Clone)]
enum PtrOrFn<'e, 's> //TODO function pointers in memory, points to a value in a table in the interpreter
{
    Ptr(Pointer),
    Fn(Box<[Identifier]>, Expr<'e, 's>)
}

enum ResultErr
{
    Error(Error),
    Return(LangVal),
    Nothing
}

impl<'e, 's> Interpreter<'e, 's>
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
            env: Environment::new(Context::TopLevel)
        }
    }

    // ----- INTERP

    pub fn interpret (&mut self, exprs:&[&Expr<'e, 's>]) -> Result<(), Error>
    {
        if cfg!(not(lang_benchmark)) {
            println!("Program stdout :");
        }
        
        for e in exprs {
            if let Err(ResultErr::Error(error)) = self.expr(e) { return Err(error); }
        }

        if cfg!(not(lang_benchmark)) {
            println!();
        }

        Ok(())
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
        self.env = Environment::new(Context::TopLevel);
    }

    fn throw (&mut self, msg:String, pos:FullPosition) -> ResultErr
    {
        let error = Error { msg, pos };
        if cfg!(lang_panic_on_error) {
            panic!("{}", error);
        } else {
            ResultErr::Error(error)
        }
    }

    fn expr (&mut self, expr:&Expr<'e, 's>) -> Result<LangVal, ResultErr>
    {
        use ExprDef::*;
        Ok(match &expr.def
        {
            // --- Values
            Const(value) => value.clone(),
            Id(id) => {
                if let PtrOrFn::Ptr(ptr) = self.get_pointer(id).unwrap() { //TODO remove this unwrap !!
                    self.memory.get_var(&ptr)
                } else {
                    return Err(self.throw("Tried to use function as value.".to_owned(), expr.get_full_pos())); //DESIGN functions as values ?
                }
            },
            // --- Control Flow
            If { cond, then, elze } => {
                match self.expr(cond)?
                {
                    LangVal::Bool(b) => {
                        if b {
                            self.expr(then)?
                        } else if let Some(elze) = elze {
                            self.expr(elze)?
                        } else {
                            LangVal::Void
                        }
                    },
                    _ => return Err(self.throw(format!("Invalid condition : {:?}", cond), expr.get_full_pos()))
                }
                
            },
            While { cond, body } => {
                loop
                {
                    match self.expr(cond)?
                    {
                        LangVal::Bool(b) => {
                            if b {
                                self.expr(body)?;
                            } else {
                                break;
                            }
                        },
                        _ => return Err(self.throw(format!("Invalid condition : {:?}", cond), expr.get_full_pos()))
                    }
                }
                LangVal::Void
            },
            // --- Operations
            UnOp(op, right) => {
                match self.unop(*op, right)
                {
                    Ok(val) => val,
                    Err(ResultErr::Nothing) => return Err(self.throw(format!("Invalid operation : {}{:?}", op.to_string(), right.def), expr.get_full_pos())),
                    err @ Err(_) => return err
                }
            },
            BinOp { op, left, right } => {
                match self.binop(*op, left, right)
                {
                    Ok(val) => val,
                    Err(ResultErr::Nothing) => return Err(self.throw(format!("Invalid operation : {:?} {} {:?}", left.def, op.to_string(), right.def), expr.get_full_pos())),
                    err @ Err(_) => return err
                }
            },
            Call { id, args } => self.call(id, args)?,
            Field(_, _) => unimplemented!(),
            // --- Declarations
            Var(id, assign_expr) => {
                //DESIGN should re assignation be allowed ?
                /* if self.get_pointer(id).is_some() {
                    self.throw("There is already a variable named ", pos: FullPosition);
                } */
                let value = self.expr(assign_expr)?;
                let ptr = match self.declare_var(id, value.as_type())
                {
                    Ok(ptr) => ptr,
                    Err(message) => return Err(self.throw(message, expr.get_full_pos()))
                };
                //TODO remove this hack
                self.stack[self.frame_ptr as usize + self.env.locals_count as usize - 1] = PtrOrFn::Ptr(self.memory.set_var(ptr, &value));
                value
            }, 
            FnDecl { id, params, body } => {
                if let Err(message) = self.declare_fn(id, params.clone(), body) {
                    return Err(self.throw(message, expr.get_full_pos()));
                }
                LangVal::Void
            },
            StructDecl {..} => unimplemented!(),
            // --- Others
            Block(exprs) => {
                if !exprs.is_empty()
                {
                    self.env.open_scope();
                    let mut ret = LangVal::Void;
                    for expr in exprs.iter() {
                        ret = match self.expr(expr)
                        {
                            Ok(val) => val,
                            err @ Err(_) => return err
                        };
                    }
                    // let ret = self.expr(exprs.iter().last().unwrap())?;
                    let n_vars = self.env.close_scope();
                    self.free_unused_stack(n_vars as usize);
                    ret
                }
                else {
                    LangVal::Void
                }
            },
            Parent(e) => self.expr(e)?,
            Return(e) => {
                if let Context::Function = self.env.get_context() {
                    return Err(ResultErr::Return(self.expr(e)?));
                } else {
                    return Err(self.throw("Can't return from top-level".to_owned(), e.get_full_pos()));
                }
            },
            End => LangVal::Void,
            Invalid => return Err(self.throw(format!("Invalid expression : {:?}", expr), expr.get_full_pos()))
        })
    }

    fn unop (&mut self, op:Op, e_right:&Expr<'e, 's>) -> Result<LangVal, ResultErr>
    {
        use ResultErr::Nothing;

        let value = self.expr(e_right)?;

        Ok(match value
        {
            LangVal::Number(f) => {
                match op
                {
                    Op::Sub => LangVal::Number(-f),
                    _ => return Err(Nothing)
                }
            },
            LangVal::Bool(b) => {
                match op
                {
                    Op::Not => LangVal::Bool(!b),
                    _ => return Err(Nothing)
                }
            },
            _ => return Err(Nothing)
        })
    }

    fn binop (&mut self, op:Op, e_left:&Expr<'e, 's>, e_right:&Expr<'e, 's>) -> Result<LangVal, ResultErr>
    {
        use ResultErr::Nothing;
        
        let value_left = self.expr(e_left)?;
        let value_right = self.expr(e_right)?;

        use { Op::*, LangVal::* };
        Ok(match (value_left, value_right)
        {
            (Number(l), Number(r)) => {
                use utils::compare_floats;
                match op
                {
                    Assign => {
                        let value = self.expr(e_right)?;
                        self.assign(e_left, e_right.downcast(value))?
                    },
                    Equal       =>   Bool( compare_floats(l, r, F32_EQ_THRESHOLD)),
                    NotEqual    =>   Bool(!compare_floats(l, r, F32_EQ_THRESHOLD)),
                    Gt          =>   Bool(l > r),
                    Gte         =>   Bool(l > r || compare_floats(l, r, F32_EQ_THRESHOLD)),
                    Lt          =>   Bool(l < r),
                    Lte         =>   Bool(l < r || compare_floats(l, r, F32_EQ_THRESHOLD)),
                    Add         => Number(l + r),
                    Sub         => Number(l - r),
                    Mult        => Number(l * r),
                    Div         => Number(l / r),
                    Mod         => Number(l % r),
                    AddAssign   => self.assign(e_left, e_right.downcast(Number(l + r)))?,
                    SubAssign   => self.assign(e_left, e_right.downcast(Number(l - r)))?,
                    MultAssign  => self.assign(e_left, e_right.downcast(Number(l * r)))?,
                    DivAssign   => self.assign(e_left, e_right.downcast(Number(l / r)))?,
                    ModAssign   => self.assign(e_left, e_right.downcast(Number(l % r)))?,
                    _ => return Err(Nothing)
                }
            },
            (Str(l), Str(r)) => {
                match op
                {
                    Assign => {
                        let value = self.expr(e_right)?;
                        self.assign(e_left, e_right.downcast(value))?
                    },
                    Equal       => Bool(l == r),
                    NotEqual    => Bool(l != r),
                    Add         =>  Str(l + &r),
                    AddAssign   => self.assign(e_left, e_right.downcast(Str(l + &r)))?,
                    _ => return Err(Nothing)
                }
            },
            (Bool(l), Bool(r)) => {
                match op
                {
                    Assign => {
                        let value = self.expr(e_right)?;
                        self.assign(e_left, e_right.downcast(value))?
                    },
                    Equal       => Bool(l == r),
                    NotEqual    => Bool(l != r),
                    BoolAnd     => Bool(l && r),
                    BoolOr      => Bool(l || r),
                    _ => return Err(Nothing)
                }
            },
            (_, _) => return Err(Nothing)
        })
    }

    fn assign (&mut self, e_to:&Expr, value:WithPosition<LangVal>) -> Result<LangVal, ResultErr>
    {
        if let ExprDef::Id(id) = &e_to.def {
            self.memory.set_var(if let PtrOrFn::Ptr(ptr) = self.get_pointer(id).unwrap()
            {
                ptr
            } else {
                panic!("Tried to use function as value.") //TODO //DESIGN functions as values ?
            }, &value.def);
        } else {
            return Err(self.throw(format!("Can't assign {:?} to {:?}", value, e_to), value.get_full_pos()));
        }
        Ok(value.def)
    }

    fn call (&mut self, e_id:&Expr<'e, 's>, args:&[&Expr<'e, 's>]) -> Result<LangVal, ResultErr> //TODO globals
    {
        Ok(match &e_id.def
        {
            ExprDef::Id(id) => {
                match id
                {
                    _ if &id[..5] == b"print" => {
                        if cfg!(not(lang_benchmark))
                        {
                            print!("> ");
                            for arg in args {
                                print!("{} ", self.expr(arg)?);
                            }
                            println!();
                        }
                        LangVal::Void
                    },
                    _ if &id[..8] == b"printmem" => {
                        if cfg!(not(lang_benchmark))
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

                        let args_pos = if args.is_empty() {
                            e_id.get_full_pos()
                        } else {
                            args.iter().skip(1).fold(args[0].pos, |acc, arg| acc + arg.pos).get_full(e_id.src)
                        };

                        let prev_frame_ptr = self.frame_ptr;
                        self.frame_ptr += self.env.locals_count;

                        let prev_env = std::mem::replace(&mut self.env, Environment::new(Context::Function));

                        // self.declare_fn(id, params: Box<[Identifier]>, body: &Expr<'e, 's>)
                        
                        if params.len() != args.len() {
                            // panic!("Invalid number of arguments.");
                            return Err(self.throw("Invalid number of arguments.".to_owned(), args_pos));
                        }

                        for (value, param) in values_and_params {
                            let value = value?;
                            let ptr = match self.declare_var(param, value.as_type())
                            {
                                Ok(ptr) => ptr,
                                Err(message) => return Err(self.throw(message, e_id.get_full_pos()))
                            };
                            self.memory.set_var(ptr, &value);
                        }

                        let out = match self.expr(&body)
                        {
                            Ok(val) => val,
                            Err(e) => match e
                            {
                                ResultErr::Return(val) => val,
                                error @ ResultErr::Error(_) => return Err(error),
                                ResultErr::Nothing => panic!("Should not happen.")
                            }
                        };

                        let n_vars = self.env.clear();
                        self.free_unused_stack(n_vars as usize);

                        self.frame_ptr = prev_frame_ptr;
                        std::mem::replace(&mut self.env, prev_env);

                        out
                    }
                }
            },
            _ => return Err(self.throw(format!("Expected an identifier, got : {:?}", e_id), e_id.get_full_pos()))
        })
    }

    // ----- VARIABLES

    pub fn declare_fn (&mut self, id:&Identifier, params:Box<[Identifier]>, body:&Expr<'e, 's>) -> Result<(), String>
    {
        self.env.locals[self.env.locals_count as usize] = (*id, self.env.scope_depth);
        self.stack[self.frame_ptr as usize + self.env.locals_count as usize] = PtrOrFn::Fn(params, body.clone());
        if let Some(n) = self.env.locals_count.checked_add(1) {
            self.env.locals_count = n;
        } else {
            return Err("Too many locals.".to_owned());
        }
        Ok(())
    }
    
    pub fn declare_var (&mut self, id:&Identifier, t:LangType) -> Result<Pointer, String>
    {
        let ptr = self.memory.make_pointer_for_type(t);
        self.env.locals[self.env.locals_count as usize] = (*id, self.env.scope_depth);
        self.stack[self.frame_ptr as usize + self.env.locals_count as usize] = PtrOrFn::Ptr(ptr);
        if let Some(n) = self.env.locals_count.checked_add(1) {
            self.env.locals_count = n;
        } else {
            return Err("Too many locals.".to_owned());
        }
        Ok(ptr)
    }

    fn get_pointer (&self, id:&Identifier) -> Option<PtrOrFn<'e, 's>>
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

    fn free_unused_stack (&mut self, n_variables:usize)
    {
        for ptr in self.stack[(self.frame_ptr as usize + self.env.locals_count as usize)..].iter().take(n_variables)
        {
            match ptr {
                PtrOrFn::Ptr(ptr) => self.memory.free_ptr(&ptr),
                PtrOrFn::Fn(_, _) => ()
            }
        }
    }
    
    #[allow(dead_code)]
    pub fn print_locals (&self)
    {
        // println!("Env ({}): {}", self.env.locals_count, crate::utils::slice_to_string_debug(&self.env.locals));

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

impl From<Error> for ResultErr
{
    fn from (error:Error) -> Self
    {
        ResultErr::Error(error)
    }
}