use super::*;
use crate::ast::{Error, Expr, ExprDef, Identifier, IdentifierTools, Op};
use crate::env::{Context, Environment, Local};
use crate::value::Value;

#[derive(Debug, Clone)]
enum IdentifierValue {
    Var(u16),
    Fn {
        offset: u16,
        args: Option<Vec<Identifier>>, //TODO if not needed replace identifiervalue by address/reference
    },
}

type IdentifierList = Vec<(Identifier, IdentifierValue)>;

trait IdentifierListTools {
    fn get_value(&self, id: Identifier) -> Option<IdentifierValue>;
}

impl IdentifierListTools for IdentifierList {
    fn get_value(&self, id: Identifier) -> Option<IdentifierValue> {
        self.iter()
            .rev()
            .find(|(id_, _)| id == *id_)
            .map(|(_, value)| value.clone())
    }
}

pub fn compile(
    statements: &[&Expr],
) -> Result<(Chunk, Option<Address>, Vec<Instruction>, DebugInfo), Error> {
    let mut chunk = Chunk::new();
    let mut identifiers = IdentifierList::new();
    let mut env = Environment::new(Context::TopLevel);
    let mut debug_info = DebugInfo::new();

    for s in statements {
        expr(s, &mut chunk, &mut env, &mut identifiers, false)?;
    }

    let entrypoint: Option<Address> =
        IdentifierListTools::get_value(&identifiers, Identifier::make("main")).map(
            |val| match val {
                IdentifierValue::Var(address) => address,
                IdentifierValue::Fn {
                    offset: address, ..
                } => address,
            },
        );

    if cfg!(lang_print_vm_compiler) && !cfg!(lang_benchmark) {
        println!("========= ASM =========");
        for (offset, instruction) in chunk.iter().enumerate() {
            print!(
                "{:04} {:>11}   {:04X}   |   {}, {:?}",
                offset,
                instruction.to_asm(),
                instruction.code,
                format!("{:?}", instruction.debug_info.def)
                    .split(['{', '(', '['])
                    .next()
                    .unwrap()
                    .trim()
                    .replace('\"', ""),
                instruction.debug_info.pos
            );
            println!();
        }
        println!("=======================");
        // println!();
        // println!("{:?}", env);
        println!();
        println!("Entrypoint: {:?}", entrypoint);
        println!();
    }

    let mut globals = vec![];
    for global in env.globals {
        globals.push(Instruction {
            code: IdentifierListTools::get_value(&identifiers, global.id)
                .map(|value| match value {
                    IdentifierValue::Var(value) => value,
                    IdentifierValue::Fn { offset, .. } => offset,
                })
                .unwrap(),
            debug_info: InstructionDebugInfo {
                def: String::default(),
                pos: Position::zero(),
            },
        })
    }

    // let identifiers: HashMap<Identifier, u16> = identifiers.into_iter().map(|(k, v)| (k, v.address)).collect();
    Ok((chunk, entrypoint, globals, debug_info))
}

fn expr(
    e: &Expr,
    chunk: &mut Chunk,
    env: &mut Environment,
    identifiers: &mut Vec<(Identifier, IdentifierValue)>,
    mut main: bool, //HACK find a better way to store entrypoint
) -> Result<(), Error> {
    macro_rules! instr {
        ($e: expr) => {
            chunk.push(Instruction {
                code: $e,
                debug_info: InstructionDebugInfo {
                    pos: e.pos,
                    def: format!("{:?}", e.def),
                },
            })
        };
    }

    macro_rules! todo {
        () => {
            return Err(Error {
                msg: "Not yet implemented.".to_owned(),
                pos: e.pos,
            })
        };
        ($msg: literal) => {
            return Err(Error {
                msg: $msg.to_owned(),
                pos: e.pos,
            })
        };
    }

    macro_rules! PUSH_D {
        () => {
            instr!(SP); // A = SP
            instr!(0b1001110000100000); // A = *A
            instr!(0b1000001100001000); // *A = D
            instr!(SP); // A = SP
            instr!(0b1001110111001000); // *A = *A + 1
        };
    }

    macro_rules! POP_D {
        () => {
            instr!(SP); // A = SP
            instr!(0b1001110010001000); // *A = *A - 1
            instr!(0b1001110000100000); // A = *A
            instr!(0b1001110000010000); // D = *A
        };
    }

    macro_rules! POP_A {
        () => {
            instr!(SP); // A = SP
            instr!(0b1001110010001000); // *A = *A - 1
            instr!(0b1001110000100000); // A = *A
            instr!(0b1001110000100000); // A = *A
        };
    }

    macro_rules! PUSH_VALUE {
        ($value: literal) => {
            instr!($value); // A = ARGS
            instr!(0b1000110000010000); // D = A
            PUSH_D!(); // PUSH_D
        };
    }

    macro_rules! PUSH_STATIC {
        ($address: ident) => {
            instr!($address); // A = ARGS
            instr!(0b1001110000010000); // D = *A
            PUSH_D!(); // PUSH_D
        };
    }

    use ExprDef::*;
    match &e.def {
        Const(value) => {
            let mut instruction = match value {
                Value::Fn(_, _) => todo!("Functions as constants are not implemented yet."),
                Value::Pointer(p, _) if *p <= MEM_SIZE.try_into().unwrap() => {
                    todo!("Pointers are not implemented yet.")
                }
                Value::Pointer(_, _) => return Err(Error { msg: format!("Pointer out of range"), pos: e.pos }),
                Value::Int(i) if *i <= 0x7fff => *i as u16,
                Value::Int(_) => return Err(Error { msg: format!("Int out of range"), pos: e.pos }),
                Value::Char(c) => *c as u16,
                Value::Float(_) => todo!("Floats are not implemented yet."), //TODO two's complement
                Value::Bool(false) => 0,
                Value::Bool(true) => 0x7fff_u16,
                Value::Void => return Err(Error { msg: format!("Tried to use void as value"), pos: e.pos }),
                Value::Array { .. } => todo!("Arrays are not implemented yet."),
            };
            instruction &= !(1 << 15); // make sure bit 15 is 0 (data instruction)
            instr!(instruction);
        }
        Id(id) => {
            match IdentifierListTools::get_value(identifiers, *id) {
                Some(IdentifierValue::Var(address)) => {
                    match env.get_from_id(id) {
                        Some(local) if local.depth > 0 => {
                            println!("Get identifier {id:?} -> {address}");
                            instr!(LOCALS); // A = LOCALS
                            instr!(0b1001110000010000); // D = *A
                            instr!(address); // A = address
                            instr!(0b1000000010100000); // A = D + A
                            instr!(0b1001110000100000); // A = *A
                        }
                        Some(_) => todo!("Gloval variables not implemented yet"),
                        None => return Err(Error { msg: format!(
                            "Corrupted environment, can't find identifier : {}",
                            id.to_string()
                        ), pos: e.pos }),
                    }
                }
                Some(IdentifierValue::Fn {
                    offset: address, ..
                }) => {
                    instr!(address); // A = address
                    match env.get_from_id(id) {
                        Some(Local {
                            t: crate::value::Type::Fn(_, _),
                            ..
                        }) => {}
                        _ => return Err(Error { msg: format!(
                            "Corrupted environment, can't find identifier : {}",
                            id.to_string()
                        ), pos: e.pos }),
                    }
                }
                None => return Err(Error { msg: format!("Unknown identifier : {}", id.to_string()), pos: e.pos }),
            }
        }
        ArrayLit { .. } => todo!("Array literals are not implemented yet."),
        StringLit { .. } => todo!("String literals are not implemented yet."),
        If { cond, then, elze } => {
            let before_if = chunk.len();
            instr!(0); // A = tbd
            expr(cond, chunk, env, identifiers, main)?;
            expr(then, chunk, env, identifiers, main)?;

            let skip_else = chunk.len();
            if elze.is_some() {
                instr!(0);
                instr!(0b1000000000000111); // JMP
            }

            chunk[before_if].code = (chunk.len() - 1) as u16; // A =

            if let Some(elze) = elze {
                expr(elze, chunk, env, identifiers, main)?;
                chunk[skip_else].code = (chunk.len() - 1) as u16;
            }
        }
        While { cond, body } => {
            let loop_start = chunk.len();
            instr!(0);
            expr(cond, chunk, env, identifiers, main)?;
            expr(body, chunk, env, identifiers, main)?;
            instr!((loop_start - 1) as u16);
            instr!(0b1000000000000111); // JMP

            chunk[loop_start].code = (chunk.len() - 1) as u16;
        }
        UnOp { op, e } => {
            expr(e, chunk, env, identifiers, main)?; // A = e
            match *op {
                Op::SubOrNeg => instr!(0b100001101001000),     // D = -A
                Op::Not => instr!(0b1000001100010000),         // D = ~D
                Op::MultOrDeref => instr!(0b1001110000010000), // D = *A
                Op::Addr => instr!(0b1000110000010000),        // D = A //TODO should be checked
                _ => todo!("This unary operator is not implemented yet."),
            };
        }
        BinOp { op, left, right } => {
            macro_rules! simple_binop {
                () => {
                    expr(left, chunk, env, identifiers, main)?; // A = left
                    instr!(0b1000110000010000); // D = A
                    expr(right, chunk, env, identifiers, main)?; // A = right
                };
            }

            macro_rules! cond_binop {
                () => {
                    instr!(0b1000110000010000); // D = A
                    instr!(JMP_ADDRESS); // A = JMP_ADDRESS
                    instr!(0b1000001100001000); // *A = D
                    expr(left, chunk, env, identifiers, main)?; // A = left
                    instr!(0b1000110000010000); // D = A
                    expr(right, chunk, env, identifiers, main)?; // A = right
                    instr!(0b1000010011010000); // D = D - A
                    instr!(JMP_ADDRESS); // A = JMP_ADDRESS
                    instr!(0b1001110000100000); // A = *A
                };
            }

            match *op {
                Op::Assign => {
                    match &left.def {
                        Id(id) => {
                            let address = match IdentifierListTools::get_value(identifiers, *id) {
                                Some(
                                    IdentifierValue::Var(address)
                                    | IdentifierValue::Fn {
                                        offset: address, ..
                                    },
                                ) => address,
                                None => return Err(Error { msg: format!(
                                        "Unkown identifier : {}",
                                        id.to_string()
                                    ), pos: e.pos })
                            };

                            expr(right, chunk, env, identifiers, main)?;
                            if let Const(_) = right.def {
                                instr!(0b1000110000010000); // D = A
                            }
                            instr!(address); // A = address
                            instr!(0b1000001100001000); // *A = D
                        }
                        UnOp {
                            op: Op::MultOrDeref,
                            e,
                        } => {
                            expr(right, chunk, env, identifiers, main)?;
                            if let Const(_) = right.def {
                                instr!(0b1000110000010000); // D = A
                            }
                            expr(e, chunk, env, identifiers, main)?; // A = address
                            instr!(0b1000001100001000); // *A = D
                        }
                        _ => return Err(Error { msg: format!("Can't assign {:?} to {:?}", right, left), pos: e.pos })
                    }
                }
                Op::Add => {
                    simple_binop!();
                    instr!(0b1000000010010000); // D = D + A
                }
                Op::SubOrNeg => {
                    simple_binop!();
                    instr!(0b1000010011010000); // D = D - A
                }
                Op::BoolAnd => {
                    simple_binop!();
                    instr!(0b1000000000010000); // D = D & A
                }
                Op::BoolOr => {
                    simple_binop!();
                    instr!(0b1000010101010000); // D = D & A
                }
                Op::Equal => {
                    cond_binop!();
                    instr!(0b1000001010000101); // D; JNE
                }
                Op::NotEqual => {
                    cond_binop!();
                    instr!(0b1000001010000010); // D; JEQ
                }
                Op::Gt => {
                    cond_binop!();
                    instr!(0b1000001010000110); // D; JLE
                }
                Op::Gte => {
                    cond_binop!();
                    instr!(0b1000001010000100); // D; JLT
                }
                Op::Lt => {
                    cond_binop!();
                    instr!(0b1000001010000011); // D; JGE
                }
                Op::Lte => {
                    cond_binop!();
                    instr!(0b1000001010000001); // D; JGT
                }
                _ => todo!("This binary operator is not implemented yet."),
            };
        }
        Call { function, args } => {
            PUSH_STATIC!(ARGS); // PUSH_STATIC ARGS

            PUSH_STATIC!(LOCALS); // PUSH_STATIC LOCALS

            let return_label = chunk.len();
            PUSH_VALUE!(0); // PUSH_VALUE RETURN_LABEL

            instr!(SP); // A = SP
            instr!(0b1001110000010000); // D = *A
            instr!(args.len() as u16); // A = args.len()
            instr!(0b1000010011010000); // D = D - A
            instr!(0); // A = 3
            instr!(0b1000010011010000); // D = D - A
            instr!(ARGS); // A = ARGS
            instr!(0b1000001100001000); // *A = D

            expr(function, chunk, env, identifiers, main)?;
            instr!(0b1000000000000111); // JMP
            chunk[return_label].code = chunk.len() as u16;

            instr!(ARGS); // A = ARGS
            instr!(0b1001110000010000); // D = *A
            instr!(TEMP_0); // A = TEMP_0
            instr!(0b1000001100001000); // *A = D

            POP_D!(); // POP_D
            instr!(LOCALS); // A = LOCALS
            instr!(0b1000001100001000); // *A = D

            POP_D!(); // POP_D
            instr!(ARGS); // A = ARGS
            instr!(0b1000001100001000); // *A = D

            instr!(TEMP_0); // A = TEMP_0
            instr!(0b1001110000010000); // D = *A
            instr!(SP); // A = SP
            instr!(0b1000001100001000); // *A = D

            instr!(RETVAL); // A = RETVAL
            instr!(0b1001110000010000); // D = *A
            PUSH_D!(); // PUSH_D
        }
        Field(_, _) => todo!("Field access is not implemented yet."),
        VarDecl(.., None) => todo!("Uninitialized variables are not implemented yet."), //TODO uninitialized var ?
        VarDecl(id, t, Some(assign_expr)) => {
            //TODO type checking ? //TODO global var ?
            if let Context::TopLevel = env.context {
                todo!()
            } else {
                env.locals[env.locals_count as usize] = Local {
                    id: *id,
                    t: t.clone(),
                    depth: env.scope_depth,
                };

                identifiers.push((*id, IdentifierValue::Var(env.locals_count as u16)));

                if let Some(n) = env.locals_count.checked_add(1) {
                    env.locals_count = n;
                } else {
                    return Err(Error {
                        msg: "Too many locals.".to_owned(),
                        pos: e.pos,
                    });
                }
            }
            // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            // identifiers.push((*id, IdentifierValue::Var(*sp)));
            // println!("Pushing var {:?} at {}", id);

            expr(assign_expr, chunk, env, identifiers, main)?;
            if let Const(_) = assign_expr.def {
                instr!(0b1000110000010000); // D = A
            }
            PUSH_D!();
        }
        FnDecl {
            id,
            params,
            return_t,
            body,
        } => {
            main = id == b"main\0\0\0\0";
            if let Context::TopLevel = env.context {
                env.globals.push(Local {
                    id: *id,
                    t: crate::value::Type::Fn(
                        params.iter().map(|param| param.0.clone()).collect(),
                        Box::new(return_t.clone()),
                    ),
                    depth: env.scope_depth,
                });
            } else {
                todo!("Local functions not implemented")
            }

            env.context = Context::Function;

            let address = chunk.len() as u16;
            identifiers.push((
                *id,
                IdentifierValue::Fn {
                    offset: address,
                    args: None, /* Some(params.iter().map(|(_, v)| *v).collect()) */
                },
            ));

            instr!(SP); // A = SP
            instr!(0b1001110000010000); // D = *A
            instr!(LOCALS); // A = LOCALS
            instr!(0b1000001100001000); // *A = D
            instr!(0); // A = localsCount ????
            instr!(0b1000000010010000); // D = D + A
            instr!(SP); // A = SP
            instr!(0b1000001100001000); // *A = D
            expr(body, chunk, env, identifiers, main)?;
        }
        StructDecl { .. } => todo!("Structure declarations are not implemented yet."),
        Block(exprs) => {
            // let sp_now = *sp;
            if let Err(err) = env.open_scope() {
                return Err(Error {
                    msg: err.msg,
                    pos: e.pos,
                });
            }

            for e in exprs.iter() {
                expr(e, chunk, env, identifiers, main)?;
            }

            let n_locals = match env.close_scope() {
                Ok(n_locals) => n_locals,
                Err(err) => {
                    return Err(Error {
                        msg: err.msg,
                        pos: e.pos,
                    })
                }
            };
            for _ in 0..n_locals {
                identifiers.pop();
            }
            // *sp = sp_now;
        }
        Parent(e) => {
            expr(e, chunk, env, identifiers, main)?;
        }
        Return(return_expr) => {
            //HACK assuming return expr puts the return value in register A
            expr(return_expr, chunk, env, identifiers, main)?;
            instr!(0b1000110000010000); // D = A
            instr!(RETVAL); // A = RETVAL
            instr!(0b1000001100001000); // *A = D
            if main {
                instr!(0x7FFF); // A = -1
            } else {
                instr!(LOCALS); // A = LOCALS
                instr!(0b1001110000010000); // D = *A
                instr!(SP); // A = SP
                instr!(0b1000001100001000); // *A = D
                POP_A!(); // POP_A
            }
            instr!(0b1000000000000111); // JMP
            env.context = Context::TopLevel;
        }
        End => (),
        Invalid => return Err(Error {
            msg: format!("Found invalid expression"),
            pos: e.pos,
        })
    }

    Ok(())
}
