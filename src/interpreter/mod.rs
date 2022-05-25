use crate::{
    ast::{Error, Expr, ExprDef, Identifier, IdentifierTools, Op, Position, WithPosition},
    env::{Context, Environment},
    memory::RawMemory,
    utils,
    value::{Type, Value},
};

mod memory;
use memory::{Memory, Pointer};

const F32_EQ_THRESHOLD: f32 = 1e-6;
const STACK_SIZE: usize = 8;

pub struct Interpreter<'e> {
    memory: RawMemory,
    stack: [Reference<'e>; STACK_SIZE],
    frame_ptr: u8,
    env: Environment,
}

#[derive(Debug, Clone)]
enum Reference<'e>
//TODO function pointers in memory, points to a value in a table in the interpreter
{
    Ptr(Pointer),
    Fn(Box<[Identifier]>, Expr<'e>),
}

enum ResultErr {
    Error(Error),
    Return(Value),
    Nothing,
}

impl<'e> Interpreter<'e> {
    pub fn new() -> Self {
        Self {
            memory: Memory::new(),
            stack: [(); STACK_SIZE].map(|_| Reference::Ptr(Default::default())),
            frame_ptr: 0,
            env: Environment::new(Context::TopLevel),
        }
    }

    // ----- INTERP

    pub fn run(&mut self, statements: &[&Expr<'e>]) -> Result<(), Error> {
        if cfg!(not(lang_benchmark)) {
            println!("Program stdout :");
        }

        for e in statements {
            if let Err(ResultErr::Error(error)) = self.expr(e) {
                return Err(error);
            }
        }

        if cfg!(not(lang_benchmark)) {
            println!();
        }

        Ok(())
    }

    #[allow(dead_code)]
    pub fn reset(&mut self) {
        self.memory = Memory::new();
        self.stack = [(); STACK_SIZE].map(|_| Reference::Ptr(Default::default()));
        self.frame_ptr = 0;
        self.env = Environment::new(Context::TopLevel);
    }

    pub fn get_var_by_name(&self, name: &str) -> Option<Value> {
        if let Some(Reference::Ptr(ptr)) = self.get_pointer(&Identifier::make(name)) {
            Some(self.memory.get_var(ptr))
        } else {
            None
        }
    }

    fn throw(&mut self, msg: String, pos: Position) -> ResultErr {
        let error = Error { msg, pos };
        if cfg!(lang_panic_on_error) {
            self.print_locals();
            panic!("{}", error); //TODO any way to get full position ?
        } else {
            ResultErr::Error(error)
        }
    }

    fn expr(&mut self, expr: &Expr<'e>) -> Result<Value, ResultErr> {
        use ExprDef::*;
        Ok(match &expr.def {
            // --- Values
            Const(value) => value.clone(),
            Id(id) => {
                match self.get_pointer(id) {
                    Some(Reference::Ptr(ptr)) => self.memory.get_var(ptr),
                    Some(Reference::Fn(_, _)) => {
                        return Err(
                            self.throw("Tried to use function as value.".to_owned(), expr.pos)
                        )
                    } //DESIGN functions as values ?
                    None => {
                        return Err(self
                            .throw(format!("Unknown identifier : {}", id.to_string()), expr.pos));
                    }
                }
            }
            // --- Control Flow
            If { cond, then, elze } => match self.expr(cond)? {
                Value::Bool(b) => {
                    if b {
                        self.expr(then)?
                    } else if let Some(elze) = elze {
                        self.expr(elze)?
                    } else {
                        Value::Void
                    }
                }
                _ => return Err(self.throw(format!("Invalid condition : {:?}", cond), expr.pos)),
            },
            While { cond, body } => {
                loop {
                    match self.expr(cond)? {
                        Value::Bool(b) => {
                            if b {
                                self.expr(body)?;
                            } else {
                                break;
                            }
                        }
                        _ => {
                            return Err(
                                self.throw(format!("Invalid condition : {:?}", cond), expr.pos)
                            )
                        }
                    }
                }
                Value::Void
            }
            // --- Operations
            UnOp { op, e } => match self.unop(*op, e) {
                Ok(val) => val,
                Err(ResultErr::Nothing) => {
                    return Err(self.throw(
                        format!("Invalid operation : {}{:?}", op.to_string(), e.def),
                        expr.pos,
                    ))
                }
                err @ Err(_) => return err,
            },
            BinOp { op, left, right } => match self.binop(*op, left, right) {
                Ok(val) => val,
                Err(ResultErr::Nothing) => {
                    return Err(self.throw(
                        format!(
                            "Invalid operation : {:?} {} {:?}",
                            left.def,
                            op.to_string(),
                            right.def
                        ),
                        expr.pos,
                    ))
                }
                err @ Err(_) => return err,
            },
            Call { id, args } => self.call(id, args)?,
            Field(_, _) => unimplemented!(),
            // --- Declarations
            VarDecl(id, assign_expr) => {
                //DESIGN should re assignation be allowed ?
                /* if self.get_pointer(id).is_some() {
                    self.throw("There is already a variable named ", pos: FullPosition);
                } */
                let value = self.expr(assign_expr)?;
                let ptr = match self.declare_var(id, value.as_type()) {
                    Ok(ptr) => ptr,
                    Err(message) => return Err(self.throw(message, expr.pos)),
                };
                //TODO remove this hack
                self.stack[self.frame_ptr as usize + self.env.locals_count as usize - 1] =
                    Reference::Ptr(self.memory.set_var(ptr, &value));
                value
            }
            FnDecl { id, params, body } => {
                if let Err(message) = self.declare_fn(id, params.clone(), body) {
                    return Err(self.throw(message, expr.pos));
                }
                Value::Void
            }
            StructDecl { .. } => unimplemented!(),
            // --- Others
            Block(exprs) => {
                if !exprs.is_empty() {
                    self.env.open_scope();
                    let mut ret = Value::Void;
                    for expr in exprs.iter() {
                        ret = match self.expr(expr) {
                            Ok(val) => val,
                            err @ Err(_) => return err,
                        };
                    }
                    // let ret = self.expr(exprs.iter().last().unwrap())?;
                    let n_vars = self.env.close_scope();
                    self.free_unused_stack(n_vars as usize);
                    ret
                } else {
                    Value::Void
                }
            }
            Parent(e) => self.expr(e)?,
            Return(e) => {
                if let Context::Function = self.env.get_context() {
                    return Err(ResultErr::Return(self.expr(e)?));
                } else {
                    return Err(self.throw("Can't return from top-level".to_owned(), e.pos));
                }
            }
            End => Value::Void,
            Invalid => return Err(self.throw(format!("Invalid expression : {:?}", expr), expr.pos)),
        })
    }

    fn unop(&mut self, op: Op, e_right: &Expr<'e>) -> Result<Value, ResultErr> {
        let value = self.expr(e_right)?;

        Ok(match value {
            Value::Int(i) => match op {
                Op::Sub => Value::Int(-i),
                _ => return Err(ResultErr::Nothing),
            },
            Value::Float(f) => match op {
                Op::Sub => Value::Float(-f),
                _ => return Err(ResultErr::Nothing),
            },
            Value::Bool(b) => match op {
                Op::Not => Value::Bool(!b),
                _ => return Err(ResultErr::Nothing),
            },
            _ => return Err(ResultErr::Nothing),
        })
    }

    fn binop(&mut self, op: Op, e_left: &Expr<'e>, e_right: &Expr<'e>) -> Result<Value, ResultErr> {
        let value_left = self.expr(e_left)?;
        let value_right = self.expr(e_right)?;

        use {Op::*, Value::*};
        Ok(match (value_left, value_right) {
            (Int(l), Int(r)) => match op {
                Assign => {
                    let value = self.expr(e_right)?;
                    self.assign(e_left, e_right.downcast(value))?
                }
                Equal => Bool(l == r),
                NotEqual => Bool(l != r),
                Gt => Bool(l > r),
                Gte => Bool(l >= r),
                Lt => Bool(l < r),
                Lte => Bool(l <= r),
                Add => Int(l + r),
                Sub => Int(l - r),
                Mult => Int(l * r),
                Div => Int(l / r),
                Mod => Int(l % r),
                AddAssign => self.assign(e_left, e_right.downcast(Int(l + r)))?,
                SubAssign => self.assign(e_left, e_right.downcast(Int(l - r)))?,
                MultAssign => self.assign(e_left, e_right.downcast(Int(l * r)))?,
                DivAssign => self.assign(e_left, e_right.downcast(Int(l / r)))?,
                ModAssign => self.assign(e_left, e_right.downcast(Int(l % r)))?,
                _ => return Err(ResultErr::Nothing),
            },
            (Float(l), Float(r)) => {
                use utils::compare_f32;
                match op {
                    Assign => {
                        let value = self.expr(e_right)?;
                        self.assign(e_left, e_right.downcast(value))?
                    }
                    Equal => Bool(compare_f32(l, r, F32_EQ_THRESHOLD)),
                    NotEqual => Bool(!compare_f32(l, r, F32_EQ_THRESHOLD)),
                    Gt => Bool(l > r),
                    Gte => Bool(l > r || compare_f32(l, r, F32_EQ_THRESHOLD)),
                    Lt => Bool(l < r),
                    Lte => Bool(l < r || compare_f32(l, r, F32_EQ_THRESHOLD)),
                    Add => Float(l + r),
                    Sub => Float(l - r),
                    Mult => Float(l * r),
                    Div => Float(l / r),
                    Mod => Float(l % r),
                    AddAssign => self.assign(e_left, e_right.downcast(Float(l + r)))?,
                    SubAssign => self.assign(e_left, e_right.downcast(Float(l - r)))?,
                    MultAssign => self.assign(e_left, e_right.downcast(Float(l * r)))?,
                    DivAssign => self.assign(e_left, e_right.downcast(Float(l / r)))?,
                    ModAssign => self.assign(e_left, e_right.downcast(Float(l % r)))?,
                    _ => return Err(ResultErr::Nothing),
                }
            }
            (Bool(l), Bool(r)) => match op {
                Assign => {
                    let value = self.expr(e_right)?;
                    self.assign(e_left, e_right.downcast(value))?
                }
                Equal => Bool(l == r),
                NotEqual => Bool(l != r),
                BoolAnd => Bool(l && r),
                BoolOr => Bool(l || r),
                _ => return Err(ResultErr::Nothing),
            },
            (value_left, value_right) if op == Op::Assign => {
                return Err(self.throw(
                    format!(
                        "Invalid assignment : tried to assign {:?} to {:?}",
                        value_left.as_type(),
                        value_right.as_type()
                    ),
                    e_left.pos,
                ))
            }
            (_, _) => return Err(ResultErr::Nothing),
        })
    }

    fn assign(&mut self, e_to: &Expr, value: WithPosition<Value>) -> Result<Value, ResultErr> {
        if let ExprDef::Id(id) = &e_to.def {
            self.memory.set_var(
                if let Reference::Ptr(ptr) = self.get_pointer(id).unwrap() {
                    ptr
                } else {
                    panic!("Tried to use function as value.") //DESIGN functions as values ?
                },
                &value.def,
            );
        } else {
            return Err(self.throw(format!("Can't assign {:?} to {:?}", value, e_to), value.pos));
        }
        Ok(value.def) //TODO should be void ?
    }

    fn call(&mut self, e_id: &Expr<'e>, args: &[&Expr<'e>]) -> Result<Value, ResultErr> {
        Ok(match &e_id.def {
            ExprDef::Id(id) => {
                match id {
                    _ if &id[..5] == b"print" => {
                        if cfg!(not(lang_benchmark)) {
                            print!("> ");
                            for arg in args {
                                print!("{} ", self.expr(arg)?);
                            }
                            println!();
                        }
                        Value::Void
                    }
                    _ if &id[..8] == b"printmem" => {
                        if cfg!(not(lang_benchmark)) {
                            println!("\nMemory state :");
                            self.print_locals();
                        }
                        Value::Void
                    }
                    id => {
                        // eprintln!("Unknown function identifier : {}", std::str::from_utf8(id).ok().unwrap());
                        let (params, body) =
                            if let Reference::Fn(params, body) = self.get_pointer(id).unwrap() {
                                (params, body) //TODO env / args
                            } else {
                                panic!("Tried to call on a value.") //DESIGN functions as values ?
                            };

                        let values_and_params = args
                            .iter()
                            .map(|arg| self.expr(arg))
                            .zip(params.iter())
                            .collect::<Vec<_>>();

                        let args_pos = if args.is_empty() {
                            e_id.pos
                        } else {
                            args.iter()
                                .skip(1)
                                .fold(args[0].pos, |acc, arg| acc + arg.pos)
                        };

                        let prev_frame_ptr = self.frame_ptr;
                        self.frame_ptr += self.env.locals_count;

                        let prev_env =
                            std::mem::replace(&mut self.env, Environment::new(Context::Function));

                        // self.declare_fn(id, params: Box<[Identifier]>, body: &Expr<'e>)

                        if params.len() != args.len() {
                            // panic!("Invalid number of arguments.");
                            return Err(
                                self.throw("Invalid number of arguments.".to_owned(), args_pos)
                            );
                        }

                        for (value, param) in values_and_params {
                            let value = value?;
                            let ptr = match self.declare_var(param, value.as_type()) {
                                Ok(ptr) => ptr,
                                Err(message) => return Err(self.throw(message, e_id.pos)),
                            };
                            self.memory.set_var(ptr, &value);
                        }

                        let out = match self.expr(&body) {
                            Ok(val) => val,
                            Err(e) => match e {
                                ResultErr::Return(val) => val,
                                error @ ResultErr::Error(_) => return Err(error),
                                ResultErr::Nothing => panic!("Should not happen."),
                            },
                        };

                        let n_vars = self.env.clear();
                        self.free_unused_stack(n_vars as usize);

                        self.frame_ptr = prev_frame_ptr;
                        // std::mem::replace(&mut self.env, prev_env); //what was that
                        self.env = prev_env;

                        out
                    }
                }
            }
            _ => {
                return Err(self.throw(
                    format!("Expected an identifier, got : {:?}", e_id),
                    e_id.pos,
                ))
            }
        })
    }

    // ----- VARIABLES

    fn declare_fn(
        &mut self,
        id: &Identifier,
        params: Box<[Identifier]>,
        body: &Expr<'e>,
    ) -> Result<(), String> {
        self.env.locals[self.env.locals_count as usize] = (*id, self.env.scope_depth);
        self.stack[self.frame_ptr as usize + self.env.locals_count as usize] =
            Reference::Fn(params, body.clone());
        if let Some(n) = self.env.locals_count.checked_add(1) {
            self.env.locals_count = n;
        } else {
            return Err("Too many locals.".to_owned());
        }
        Ok(())
    }

    fn declare_var(&mut self, id: &Identifier, t: Type) -> Result<Pointer, String> {
        let ptr = self.memory.make_pointer_for_type(t);
        self.env.locals[self.env.locals_count as usize] = (*id, self.env.scope_depth);
        self.stack[self.frame_ptr as usize + self.env.locals_count as usize] = Reference::Ptr(ptr);
        if let Some(n) = self.env.locals_count.checked_add(1) {
            self.env.locals_count = n;
        } else {
            return Err("Too many locals.".to_owned());
        }
        Ok(ptr)
    }

    fn get_pointer(&self, id: &Identifier) -> Option<Reference<'e>> {
        for i in (0..self.env.locals_count as usize).rev() {
            let (id_, _) = self.env.locals[i];
            if *id == id_ {
                return Some(self.stack[self.frame_ptr as usize + i].clone());
            }
        }
        None
    }

    fn free_unused_stack(&mut self, n_variables: usize) {
        for ptr in self.stack[(self.frame_ptr as usize + self.env.locals_count as usize)..]
            .iter()
            .take(n_variables)
        {
            match ptr {
                Reference::Ptr(ptr) => self.memory.free_ptr(*ptr),
                Reference::Fn(_, _) => (),
            }
        }
    }

    #[allow(dead_code)]
    pub fn print_locals(&self) {
        if cfg!(lang_benchmark) {
            return;
        }
        // println!("Env ({}): {}", self.env.locals_count, crate::utils::slice_to_string_debug(&self.env.locals));

        println!("Locals:");
        let mut current_depth = if self.env.scope_depth > 0 {
            self.env.scope_depth + 1
        } else {
            0
        };

        for i in (0..self.env.locals_count as usize).rev() {
            let (id, depth) = self.env.locals[i];

            if current_depth > depth {
                current_depth = depth;
                println!("----- Depth: {} -----", current_depth);
            }

            let id_str = String::from_utf8(id.iter().take_while(|i| **i != 0).cloned().collect())
                .ok()
                .unwrap();
            let ptr = if let Reference::Ptr(ptr) = self.get_pointer(&id).unwrap() {
                ptr
            } else {
                continue;
            };
            println!("{} => {:?} => {:?}", id_str, ptr, self.memory.get_var(ptr));
        }

        println!();
        self.memory.print_ram();
    }
}

impl From<Error> for ResultErr {
    fn from(error: Error) -> Self {
        ResultErr::Error(error)
    }
}
