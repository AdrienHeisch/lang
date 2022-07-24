use crate::{
    ast::{Error, Expr, ExprDef, Identifier, IdentifierTools, Op, Position, WithPosition},
    env::{Context, Environment, Local},
    memory::{Address, RawMemory},
    utils,
    value::{Type, Value},
};

mod memory;
use memory::{Memory, Variable};

const F32_EQ_THRESHOLD: f32 = 1e-6;
const STACK_SIZE: usize = 8;

pub struct Interpreter<'e> {
    memory: RawMemory,
    stack: [Reference<'e>; STACK_SIZE],
    frame_ptr: u8,
    globals: Vec<Reference<'e>>,
    env: Environment,
}

#[derive(Debug, Clone)]
enum Reference<'e>
//TODO function pointers in memory, points to a value in a table in the interpreter
{
    Var(Variable),
    Fn(Box<[(Type, Identifier)]>, Type, Expr<'e>),
}

enum ResultErr {
    Error(Error),
    Return(Value),
    Nothing,
}

//TODO remove all unnecessary tests already performed by the parser
impl<'e> Interpreter<'e> {
    pub fn new() -> Self {
        Self {
            memory: Memory::new(),
            stack: [(); STACK_SIZE].map(|_| Reference::Var(Default::default())),
            frame_ptr: 0,
            globals: Vec::new(),
            env: Environment::new(Context::TopLevel),
        }
    }

    // ----- INTERP

    pub fn run(&mut self, top_level: &[&Expr<'e>]) -> Result<(), Error> {
        if cfg!(not(lang_benchmark)) {
            println!("Program stdout :");
        }

        for e in top_level {
            if let Err(ResultErr::Error(error)) = self.expr(e) {
                return Err(error);
            }
        }

        if let Err(ResultErr::Error(error)) =
            self.call_id(&IdentifierTools::make("main"), &[], Position::zero())
        {
            return Err(error);
        }

        if cfg!(not(lang_benchmark)) {
            println!();
        }
        if cfg!(lang_print_interpreter) {
            self.print_locals();
        }

        Ok(())
    }

    #[allow(dead_code)]
    pub fn reset(&mut self) {
        self.memory = Memory::new();
        self.stack = [(); STACK_SIZE].map(|_| Reference::Var(Default::default()));
        self.frame_ptr = 0;
        self.env = Environment::new(Context::TopLevel);
    }

    #[allow(dead_code)] //USED BY TESTS
    pub fn get_var_by_name(&self, name: &str) -> Option<Value> {
        if let Some(Reference::Var(ptr)) = self.get_ref(&Identifier::make(name)) {
            Some(self.memory.get_var(&ptr))
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
                match self.get_ref(id) {
                    Some(Reference::Var(var)) => self.memory.get_var(&var),
                    Some(Reference::Fn(args, ret_t, _)) => Value::Fn(
                        *id,
                        Type::Fn(
                            args.iter().map(|arg| arg.0.clone()).collect(),
                            Box::new(ret_t),
                        ),
                    ), //DESIGN functions as values ?
                    None => {
                        return Err(self
                            .throw(format!("Unknown identifier : {}", id.to_string()), expr.pos));
                    }
                }
            }
            ArrayLit { items, t } => {
                let t_len = t.get_size();
                let arr_len = items.len();
                let ptr = self.memory.alloc(t_len * arr_len);
                let mut pos = ptr.pos;
                for item in items.iter() {
                    let value = self.expr(item)?;
                    self.memory.set_var(
                        &Variable {
                            t: *t.clone(),
                            raw: Address { pos, len: t_len },
                        },
                        &value,
                    );
                    pos += t_len;
                }
                Value::Array {
                    addr: ptr.pos as u32,
                    len: arr_len as u32,
                    t: t.clone(),
                }
            }
            StringLit(chars) => {
                let t_len = Type::Char.get_size();
                let arr_len = chars.len();
                let ptr = self.memory.alloc(t_len * arr_len);
                let mut pos = ptr.pos;
                for char in chars.iter() {
                    let value = Value::Char(*char);
                    self.memory.set_var(
                        &Variable {
                            t: Type::Char,
                            raw: Address { pos, len: t_len },
                        },
                        &value,
                    );
                    pos += t_len;
                }
                Value::Array {
                    addr: ptr.pos as u32,
                    len: arr_len as u32,
                    t: Box::new(Type::Char),
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
            Call { function: id, args } => self.call(id, args)?,
            Field(_, _) => unimplemented!(),
            // --- Declarations
            VarDecl(id, t, assign_expr) => {
                //TODO should already be checked by parser (test)
                /* if self.get_ref(id).is_some() {
                    self.throw("There is already a variable named ".to_owned(), expr.pos);
                } */

                let ptr = match self.declare_var(t, id) {
                    Ok(ptr) => ptr,
                    Err(message) => return Err(self.throw(message, expr.pos)),
                };

                /* //TODO remove this hack
                self.stack[self.frame_ptr as usize + self.env.locals_count as usize - 1] =
                    Reference::Var(ptr.clone()); */

                if let Some(assign_expr) = assign_expr {
                    let value = self.expr(assign_expr)?;
                    let t_ = value.as_type();
                    if t != &t_ {
                        return Err(
                            self.throw(format!("Can't assign {:?} to {:?}", t, t_), expr.pos)
                        );
                    }
                    self.memory.set_var(&ptr, &value)
                } else if let Type::Array { len, t } = &t {
                    let addr = self.memory.alloc(t.get_size() * *len as usize);
                    self.memory.set_var(
                        &ptr,
                        &Value::Array {
                            addr: addr.pos as u32,
                            len: *len,
                            t: t.clone(),
                        },
                    )
                }
                Value::Void
            }
            FnDecl {
                id,
                params,
                return_t,
                body,
            } => {
                if let Err(message) = self.declare_fn(id, params.clone(), return_t, body) {
                    return Err(self.throw(message, expr.pos));
                }
                Value::Void
            }
            StructDecl { .. } => unimplemented!(),
            // --- Others
            Block(exprs) => {
                if !exprs.is_empty() {
                    self.env.open_scope();
                    for expr in exprs.iter() {
                        match self.expr(expr) {
                            Ok(val) => val,
                            err @ Err(_) => return err,
                        };
                    }
                    self.env.close_scope();
                    self.free_unused_stack();
                }
                Value::Void
            }
            Parent(e) => self.expr(e)?,
            Return(e) => {
                if let Context::Function = self.env.context {
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
        if let Op::Addr = op {
            if let ExprDef::Id(id) = e_right.def {
                match self.get_ref(&id) {
                    Some(Reference::Var(var)) => {
                        return Ok(Value::Pointer(
                            var.raw.pos.try_into().unwrap(),
                            Box::new(var.t),
                        ))
                    }
                    Some(Reference::Fn(_, _, _)) => todo!(),
                    None => {
                        return Err(self.throw(
                            format!("Unknown identifier : {}", id.to_string()),
                            e_right.pos,
                        ));
                    }
                }
            }
        }

        let value = self.expr(e_right)?;

        Ok(match value {
            Value::Pointer(addr, t) => match op {
                Op::MultOrDeref => self.memory.get_var(&Variable {
                    t: *t.clone(),
                    raw: Address {
                        pos: addr.try_into().unwrap(),
                        len: t.get_size(),
                    },
                }),
                _ => return Err(ResultErr::Nothing),
            },
            Value::Int(i) => match op {
                Op::SubOrNeg => Value::Int(-i),
                _ => return Err(ResultErr::Nothing),
            },
            Value::Float(f) => match op {
                Op::SubOrNeg => Value::Float(-f),
                _ => return Err(ResultErr::Nothing),
            },
            Value::Bool(b) => match op {
                Op::Not => Value::Bool(!b),
                _ => return Err(ResultErr::Nothing),
            },
            _ => return Err(ResultErr::Nothing),
        })
    }

    // TODO remove all unnecessary check (performed in parser)
    fn binop(&mut self, op: Op, e_left: &Expr<'e>, e_right: &Expr<'e>) -> Result<Value, ResultErr> {
        let value_left = self.expr(e_left)?;
        let value_right = self.expr(e_right)?;

        if op == Op::Assign {
            // TODO move to self.assign ?
            match e_left.def {
                ExprDef::UnOp {
                    op: Op::MultOrDeref,
                    e,
                } => {
                    if let Pointer(addr, t) = self.expr(e)? {
                        if value_right.as_type() == *t {
                            self.memory.set_var(
                                &Variable {
                                    t: *t.clone(),
                                    raw: Address {
                                        pos: addr.try_into().unwrap(),
                                        len: t.get_size(),
                                    },
                                },
                                &value_right,
                            );
                            return Ok(Value::Void);
                        }
                    }
                }
                ExprDef::BinOp {
                    op: Op::Index,
                    left,
                    right,
                } => {
                    if let Array { addr, len, t } = self.expr(left)? {
                        if let Int(index) = self.expr(right)? {
                            let index = index as u32;
                            if index >= len {
                                panic!()
                            }
                            let value = self.expr(e_right)?;
                            self.memory.set_var(
                                &Variable {
                                    t: *t.clone(),
                                    raw: Address {
                                        pos: addr as usize + t.get_size() * index as usize,
                                        len: t.get_size(),
                                    },
                                },
                                &value,
                            );
                            return Ok(Value::Void);
                        } else {
                            panic!()
                        }
                    } else {
                        panic!()
                    }
                }
                _ => (),
            }
        }

        // TODO remove multiple "self.expr(e_right)" calls
        use {Op::*, Value::*};
        Ok(match (value_left, value_right) {
            (Pointer(_, ptr_t_l), Pointer(_, ptr_t_r)) if *ptr_t_l == *ptr_t_r => match op {
                Assign => {
                    let value = self.expr(e_right)?;
                    self.assign(e_left, e_right.downcast_position(value))?
                }
                _ => return Err(ResultErr::Nothing),
            },
            (Array { addr, t, .. }, Int(i)) => match op {
                Index =>
                /* Pointer(addr + (i * t.get_size() as i32) as u32, t), */
                {
                    self.memory.get_var(&Variable {
                        t: *t.clone(),
                        raw: Address {
                            pos: t.get_size() * i as usize + addr as usize,
                            len: t.get_size(),
                        },
                    })
                }
                _ => return Err(ResultErr::Nothing),
            },
            (Int(l), Int(r)) => match op {
                Assign => {
                    let value = self.expr(e_right)?;
                    self.assign(e_left, e_right.downcast_position(value))?
                }
                Equal => Bool(l == r),
                NotEqual => Bool(l != r),
                Gt => Bool(l > r),
                Gte => Bool(l >= r),
                Lt => Bool(l < r),
                Lte => Bool(l <= r),
                Add => Int(l + r),
                SubOrNeg => Int(l - r),
                MultOrDeref => Int(l * r),
                Div => Int(l / r),
                Mod => Int(l % r),
                AddAssign => self.assign(e_left, e_right.downcast_position(Int(l + r)))?,
                SubAssign => self.assign(e_left, e_right.downcast_position(Int(l - r)))?,
                MultAssign => self.assign(e_left, e_right.downcast_position(Int(l * r)))?,
                DivAssign => self.assign(e_left, e_right.downcast_position(Int(l / r)))?,
                ModAssign => self.assign(e_left, e_right.downcast_position(Int(l % r)))?,
                _ => return Err(ResultErr::Nothing),
            },
            (Float(l), Float(r)) => {
                use utils::eq_f32;
                match op {
                    Assign => {
                        let value = self.expr(e_right)?;
                        self.assign(e_left, e_right.downcast_position(value))?
                    }
                    Equal => Bool(eq_f32(l, r, F32_EQ_THRESHOLD)),
                    NotEqual => Bool(!eq_f32(l, r, F32_EQ_THRESHOLD)),
                    Gt => Bool(l > r),
                    Gte => Bool(l > r || eq_f32(l, r, F32_EQ_THRESHOLD)),
                    Lt => Bool(l < r),
                    Lte => Bool(l < r || eq_f32(l, r, F32_EQ_THRESHOLD)),
                    Add => Float(l + r),
                    SubOrNeg => Float(l - r),
                    MultOrDeref => Float(l * r),
                    Div => Float(l / r),
                    Mod => Float(l % r),
                    AddAssign => self.assign(e_left, e_right.downcast_position(Float(l + r)))?,
                    SubAssign => self.assign(e_left, e_right.downcast_position(Float(l - r)))?,
                    MultAssign => self.assign(e_left, e_right.downcast_position(Float(l * r)))?,
                    DivAssign => self.assign(e_left, e_right.downcast_position(Float(l / r)))?,
                    ModAssign => self.assign(e_left, e_right.downcast_position(Float(l % r)))?,
                    _ => return Err(ResultErr::Nothing),
                }
            }
            (Bool(l), Bool(r)) => match op {
                Assign => {
                    let value = self.expr(e_right)?;
                    self.assign(e_left, e_right.downcast_position(value))?
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
                        "Invalid assignment : can't assign {:?} to {:?}",
                        value_right.as_type(),
                        value_left.as_type()
                    ),
                    e_left.pos,
                ))
            }
            (_, _) => return Err(ResultErr::Nothing),
        })
    }

    fn assign(&mut self, e_to: &Expr, value: WithPosition<Value>) -> Result<Value, ResultErr> {
        match &e_to.def {
            ExprDef::Id(id) if !matches!(value.def, Value::Void) => {
                self.memory.set_var(
                    &if let Reference::Var(ptr) = self.get_ref(id).unwrap() {
                        ptr
                    } else {
                        panic!("Tried to use function as value.") //DESIGN functions as values ?
                    },
                    &value.def,
                );
            }
            _ => {
                return Err(self.throw(format!("Can't assign {:?} to {:?}", value, e_to), value.pos))
            }
        }
        Ok(Value::Void)
    }

    fn call(&mut self, e_id: &Expr<'e>, args: &[&Expr<'e>]) -> Result<Value, ResultErr> {
        Ok(match &e_id.def {
            ExprDef::Id(id) => {
                match id {
                    b"print\0\0\0" => {
                        if cfg!(not(lang_benchmark)) {
                            println!("> {}", self.expr(args[0])?);
                            // Implementation for any number of arguments
                            /* args.iter()
                            .map(|arg| self.expr(arg))
                            .collect::<Result<Vec<Value>, _>>()?
                            .iter()
                            .for_each(|val| println!("> {}", val)); */
                        }
                        Value::Void
                    }
                    b"printmem" => {
                        if cfg!(not(lang_benchmark)) {
                            println!("> Memory :");
                            self.print_locals();
                        }
                        Value::Void
                    }
                    id => self.call_id(id, args, e_id.pos)?,
                }
            }
            //TODO any other cases ?
            _ => {
                return Err(self.throw(
                    format!("Expected an identifier, got : {:?}", e_id),
                    e_id.pos,
                ));
            }
        })
    }

    fn call_id(
        &mut self,
        id: &[u8; 8],
        args: &[&Expr<'e>],
        id_pos: Position,
    ) -> Result<Value, ResultErr> {
        let (params, body) = {
            //TODO unwrap panics on recursion
            let (params, body);
            let mut reference = if let Some(reference) = self.get_ref(id) {
                reference
            } else {
                return Err(self.throw(format!("Unknown identifier {}", id.to_string()), id_pos));
            };
            loop {
                match reference {
                    Reference::Fn(params_, _, body_) => {
                        (params, body) = (params_, body_);
                        break;
                    }
                    Reference::Var(var) => {
                        if let Value::Fn(id, _) = self.memory.get_var(&var) {
                            reference = self.get_ref(&id).unwrap();
                        } else {
                            panic!()
                        }
                    }
                }
            }
            (params, body)
        };
        let values_and_params = args
            .iter()
            .map(|arg| self.expr(arg))
            .zip(params.iter())
            .collect::<Vec<_>>();
        let args_pos = if args.is_empty() {
            id_pos
        } else {
            args.iter()
                .skip(1)
                .fold(args[0].pos, |acc, arg| acc + arg.pos)
        };
        let prev_frame_ptr = self.frame_ptr;
        self.frame_ptr += self.env.locals_count;
        let prev_context = self.env.context;
        self.env.context = Context::Function;
        if params.len() != args.len() {
            // panic!("Invalid number of arguments.");
            return Err(self.throw("Invalid number of arguments.".to_owned(), args_pos));
        }
        for (value, param) in values_and_params {
            let value = value?;
            let ptr = match self.declare_var(&param.0, &param.1) {
                Ok(ptr) => ptr,
                Err(message) => return Err(self.throw(message, id_pos)),
            };
            self.memory.set_var(&ptr, &value);
        }
        let out = match self.expr(&body) {
            Ok(val) => val,
            Err(e) => match e {
                ResultErr::Return(val) => val,
                error @ ResultErr::Error(_) => return Err(error),
                ResultErr::Nothing => panic!("Should not happen."),
            },
        };
        self.free_unused_stack();
        self.frame_ptr = prev_frame_ptr;
        self.env.context = prev_context;
        Ok(out)
    }

    // ----- VARIABLES

    fn declare_fn(
        &mut self,
        id: &Identifier,
        params: Box<[(Type, Identifier)]>,
        return_t: &Type,
        body: &Expr<'e>,
    ) -> Result<(), String> {
        if let Context::TopLevel = self.env.context {
            self.env.globals.push(Local {
                id: *id,
                t: Type::Fn(
                    params.iter().map(|param| param.0.clone()).collect(),
                    Box::new(return_t.clone()),
                ),
                depth: self.env.scope_depth,
            });
            self.globals
                .push(Reference::Fn(params, return_t.clone(), body.clone()));
        } else {
            self.env.locals[self.env.locals_count as usize] = Local {
                id: *id,
                t: Type::Fn(
                    params.iter().map(|param| param.0.clone()).collect(),
                    Box::new(return_t.clone()),
                ),
                depth: self.env.scope_depth,
            };
            self.stack[self.frame_ptr as usize + self.env.locals_count as usize] =
                Reference::Fn(params, return_t.clone(), body.clone());
            if let Some(n) = self.env.locals_count.checked_add(1) {
                self.env.locals_count = n;
            } else {
                return Err("Too many locals.".to_owned());
            }
        }
        Ok(())
    }

    fn declare_var(&mut self, t: &Type, id: &Identifier) -> Result<Variable, String> {
        let ptr = self.memory.make_pointer_for_type(t);

        if let Context::TopLevel = self.env.context {
            self.env.globals.push(Local {
                id: *id,
                t: t.clone(),
                depth: 0,
            });
            self.globals.push(Reference::Var(ptr.clone()));
        } else {
            self.env.locals[self.env.locals_count as usize] = Local {
                id: *id,
                t: t.clone(),
                depth: self.env.scope_depth,
            };
            self.stack[self.frame_ptr as usize + self.env.locals_count as usize] =
                Reference::Var(ptr.clone());

            if let Some(n) = self.env.locals_count.checked_add(1) {
                self.env.locals_count = n;
            } else {
                return Err("Too many locals.".to_owned());
            }
        }

        Ok(ptr)
    }

    fn get_ref(&self, id: &Identifier) -> Option<Reference<'e>> {
        for i in (0..self.env.locals_count as usize).rev() {
            let Local { id: id_, .. } = self.env.locals[i];
            if *id == id_ {
                return Some(self.stack[self.frame_ptr as usize + i].clone());
            }
        }
        for (index, global) in self.env.globals.iter().enumerate() {
            if *id == global.id {
                return Some(self.globals[index].clone());
            }
        }
        None
    }

    fn free_unused_stack(&mut self) {
        match &self.stack[(self.frame_ptr as usize + self.env.locals_count as usize)] {
            Reference::Var(ptr) => self.memory.free_from(ptr.raw.pos),
            Reference::Fn(_, _, _) => (),
        }
    }

    #[allow(dead_code)]
    pub fn print_locals(&self) {
        if cfg!(lang_benchmark) {
            return;
        }

        let mut current_depth = if self.env.scope_depth > 0 {
            self.env.scope_depth + 1
        } else {
            0
        };

        for i in (0..self.env.locals_count as usize).rev() {
            let Local { id, depth, .. } = self.env.locals[i];

            if current_depth > depth {
                current_depth = depth;
                println!("----- Depth: {} -----", current_depth);
            }

            let id_str = String::from_utf8(id.iter().take_while(|i| **i != 0).cloned().collect())
                .ok()
                .unwrap();
            match self.get_ref(&id).unwrap() {
                Reference::Var(ptr) => {
                    let value = self.memory.get_var(&ptr);
                    print!("{id_str} => {ptr:?} => {value}");
                    match value {
                        Value::Pointer(addr, t) => println!(
                            " => {}",
                            self.memory.get_var(&Variable {
                                t: *t.clone(),
                                raw: Address {
                                    pos: addr.try_into().unwrap(),
                                    len: t.get_size(),
                                },
                            })
                        ),
                        Value::Array { addr, len, t } => println!(
                            " => [{}]",
                            (0..len)
                                .into_iter()
                                .map(|i| {
                                    format!(
                                        "{}",
                                        self.memory.get_var(&Variable {
                                            t: *t.clone(),
                                            raw: Address {
                                                pos: t.get_size() * i as usize + addr as usize,
                                                len: t.get_size(),
                                            },
                                        })
                                    )
                                })
                                .reduce(|acc, str| format!("{acc}, {str}"))
                                .unwrap_or_default()
                        ),
                        _ => println!(),
                    }
                }
                Reference::Fn(params, ret_t, _) => {
                    println!("{id_str} => {params:?} => {ret_t}");
                }
            };
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
