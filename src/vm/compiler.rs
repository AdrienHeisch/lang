use super::*;
use crate::ast::{Error, Expr, ExprDef, Identifier, IdentifierTools, Op};
use crate::value::Value;
use std::collections::HashMap;

#[derive(Debug)]
struct IdentifierValue {
    address: u16,
    args: Option<Vec<Identifier>>
}

pub fn compile(statements: &[&Expr]) -> Result<(Chunk, Option<Address>, DebugInfo), Error> {
    let mut chunk = Chunk::new();
    let mut identifiers = HashMap::<Identifier, IdentifierValue>::new();
    let mut sp = SP_INIT; // necessary to keep track of variables on the stack at compile time

    for s in statements {
        expr(s, &mut chunk, &mut identifiers, &mut sp)?;
    }

    let entrypoint = identifiers.get(&Identifier::make("main")).map(|id_val| id_val.address);

    if cfg!(lang_print_vm_compiler) && !cfg!(lang_benchmark) {
        println!("========= ASM =========");
        for (offset, instruction) in chunk.iter().enumerate() {
            print!(
                "{:04} {:>11}   {:04X}   |   {}, {:?}",
                offset,
                instruction.to_asm(),
                instruction.code,
                format!("{:?}", instruction.debug_info.def).split(['{', '(', '[']).nth(0).unwrap().trim().replace("\"", "").to_string(),
                instruction.debug_info.pos
            );
            println!();
        }
        println!("=======================");
        println!();
        println!("{:?}", identifiers);
        println!();
        println!("Entrypoint: {:?}", entrypoint);
        println!();
    }

    let identifiers: HashMap<Identifier, u16> = identifiers.into_iter().map(|(k, v)| (k, v.address)).collect();
    Ok((chunk, entrypoint, DebugInfo { identifiers }))
}

fn expr(e: &Expr, chunk: &mut Chunk, identifiers: &mut HashMap<Identifier, IdentifierValue>, sp: &mut u16) -> Result<(), Error>{
    macro_rules! instr {
        ($e: expr) => {
            chunk.push(Instruction { code: $e, debug_info: InstructionDebugInfo { pos: e.pos, def: format!("{:?}", e.def) } })
        };
    }

    macro_rules! todo {
        () => {
            return Err(Error { msg: "Not yet implemented.".to_owned(), pos: e.pos })
        };
        ($msg: literal) => {
            return Err(Error { msg: $msg.to_owned(), pos: e.pos })
        }
    }

    macro_rules! PUSH_D {
        () => {
            instr!(SP);                 // A = SP
            instr!(0b1001110000100000); // A = *A
            instr!(0b1000001100001000); // *A = D
            instr!(SP);                 // A = SP
            instr!(0b1001110111001000); // *A = *A + 1
            *sp += 1;
        };
    }

    macro_rules! POP_D {
        () => {
            instr!(SP);                 // A = SP
            instr!(0b1001110010001000); // *A = *A - 1
            instr!(0b1001110000100000); // A = *A
            instr!(0b1001110000010000); // D = *A
            *sp -= 1;
        };
    }

    macro_rules! POP_A {
        () => {
            instr!(SP);                 // A = SP
            instr!(0b1001110010001000); // *A = *A - 1
            instr!(0b1001110000100000); // A = *A
            instr!(0b1001110000100000); // A = *A
            *sp -= 1;
        };
    }
    
    macro_rules! PUSH_VALUE {
        ($value: literal) => {
            instr!($value);           // A = ARGS
            instr!(0b1000110000010000); // D = A
            PUSH_D!();                             // PUSH_D
        };
    }
    
    macro_rules! PUSH_STATIC {
        ($address: ident) => {
            instr!($address);           // A = ARGS
            instr!(0b1001110000010000); // D = *A
            PUSH_D!();                             // PUSH_D
        };
    }

    use ExprDef::*;
    match &e.def {
        Const(value) => {
            let mut instruction = match value {
                Value::Fn(_, _) => todo!("Functions as constants are not implemented yet."),
                Value::Pointer(p, _) if *p <= MEM_SIZE.try_into().unwrap() => todo!("Pointers are not implemented yet."),
                Value::Pointer(_, _) => panic!("Pointer out of range"),
                Value::Int(i) if *i <= 0x7fff => *i as u16,
                Value::Int(_) => panic!("Int out of range"),
                Value::Char(c) => *c as u16,
                Value::Float(_) => todo!("Floats are not implemented yet."), //TODO two's complement
                Value::Bool(false) => 0,
                Value::Bool(true) => 0x7fff_u16,
                Value::Void => panic!(),
                Value::Array { .. } => todo!("Arrays are not implemented yet."),
            };
            instruction &= !(1 << 15); // make sure bit 15 is 0 (data instruction)
            instr!(instruction);
        }
        Id(id) => {
            if let Some(IdentifierValue { address, .. }) = identifiers.get(id) {
                println!("Get identifier {id:?} -> {address}");
                instr!(*address);           // A = address
                instr!(0b1001110000100000); // A = *A
            } else {
                panic!("Unknown identifier : {}", id.to_string())
            }
        }
        ArrayLit { .. } => todo!("Array literals are not implemented yet."),
        StringLit { .. } => todo!("String literals are not implemented yet."),
        If { cond, then, elze } => {
            let before_if = chunk.len();
            instr!(0); // A = tbd
            expr(cond, chunk, identifiers, sp)?;
            expr(then, chunk, identifiers, sp)?;

            let skip_else = chunk.len();
            if elze.is_some() {
                instr!(0);
                instr!(0b1000000000000111); // JMP
            }

            chunk[before_if].code = (chunk.len() - 1) as u16; // A =

            if let Some(elze) = elze {
                expr(elze, chunk, identifiers, sp)?;
                chunk[skip_else].code = (chunk.len() - 1) as u16;
            }
        }
        While { cond, body } => {
            let loop_start = chunk.len();
            instr!(0);
            expr(cond, chunk, identifiers, sp)?;
            expr(body, chunk, identifiers, sp)?;
            instr!((loop_start - 1) as u16);
            instr!(0b1000000000000111);     // JMP

            chunk[loop_start].code = (chunk.len() - 1) as u16;
        }
        UnOp { op, e } => {
            expr(e, chunk, identifiers, sp)?;   // A = e
            match *op {
                Op::SubOrNeg =>     instr!(0b100001101001000),  // D = -A
                Op::Not =>          instr!(0b1000001100010000), // D = ~D
                Op::MultOrDeref =>  instr!(0b1001110000010000), // D = *A
                Op::Addr =>         instr!(0b1000110000010000), // D = A //TODO should be checked
                _ => todo!("This unary operator is not implemented yet."),
            };
        }
        BinOp { op, left, right } => {
            macro_rules! simple_binop {
                () => {
                    expr(left, chunk, identifiers, sp)?;    // A = left
                    instr!(0b1000110000010000);         // D = A
                    expr(right, chunk, identifiers, sp)?;   // A = right
                };
            }

            macro_rules! cond_binop {
                () => {
                    instr!(0b1000110000010000);         // D = A
                    instr!(JMP_ADDRESS);                // A = JMP_ADDRESS
                    instr!(0b1000001100001000);         // *A = D
                    expr(left, chunk, identifiers, sp)?;    // A = left
                    instr!(0b1000110000010000);         // D = A
                    expr(right, chunk, identifiers, sp)?;   // A = right
                    instr!(0b1000010011010000);         // D = D - A
                    instr!(JMP_ADDRESS);                // A = JMP_ADDRESS
                    instr!(0b1001110000100000);         // A = *A
                };
            }

            match *op {
                Op::Assign => {
                    match &left.def {
                        Id(id) => {
                            let address = if let Some(IdentifierValue { address, .. }) = identifiers.get(id) {
                                *address
                            } else {
                                panic!(
                                    "Unkown identifier : {}",
                                    String::from_utf8(Vec::from(id)).unwrap()
                                );
                            };

                            expr(right, chunk, identifiers, sp)?;
                            if let Const(_) = right.def {
                                instr!(0b1000110000010000); // D = A
                            }
                            instr!(address);                // A = address
                            instr!(0b1000001100001000);     // *A = D
                        }
                        UnOp { op: Op::MultOrDeref, e } => {

                            expr(right, chunk, identifiers, sp)?;
                            if let Const(_) = right.def {
                                instr!(0b1000110000010000); // D = A
                            }
                            expr(e, chunk, identifiers, sp)?;   // A = address
                            instr!(0b1000001100001000);     // *A = D
                        }
                        _ => {
                            panic!("Can't assign {:?} to {:?}", right, left);
                        }
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
            PUSH_STATIC!(ARGS);                    // PUSH_STATIC ARGS

            PUSH_STATIC!(LOCALS);                  // PUSH_STATIC LOCALS

            let return_label = chunk.len();
            PUSH_VALUE!(0);                        // PUSH_VALUE RETURN_LABEL

            instr!(SP);                 // A = SP
            instr!(0b1001110000010000); // D = *A
            instr!(args.len() as u16);  // A = args.len()
            instr!(0b1000010011010000); // D = D - A
            instr!(0);                  // A = 3
            instr!(0b1000010011010000); // D = D - A
            instr!(ARGS);               // A = ARGS
            instr!(0b1000001100001000); // *A = D

            expr(function, chunk, identifiers, sp)?;
            instr!(0b1000000000000111); // JMP
            chunk[return_label].code = chunk.len() as u16;

            instr!(ARGS);               // A = ARGS
            instr!(0b1001110000010000); // D = *A
            instr!(TEMP_0);             // A = TEMP_0
            instr!(0b1000001100001000); // *A = D

            POP_D!();                       // POP_D
            instr!(LOCALS);             // A = LOCALS
            instr!(0b1000001100001000); // *A = D

            POP_D!();                       // POP_D
            instr!(ARGS);               // A = ARGS
            instr!(0b1000001100001000); // *A = D

            instr!(TEMP_0);             // A = TEMP_0
            instr!(0b1001110000010000); // D = *A
            instr!(SP);                 // A = SP
            instr!(0b1000001100001000); // *A = D

            instr!(RETVAL);             // A = RETVAL
            instr!(0b1001110000010000); // D = *A
            PUSH_D!();                      // PUSH_D
        },
        Field(_, _) => todo!("Field access is not implemented yet."),
        VarDecl(.., None) => todo!("Uninitialized variables are not implemented yet."), //TODO uninitialized var ?
        VarDecl(id, _, Some(assign_expr)) => { //TODO type checking ?
            identifiers.insert(*id, IdentifierValue { address: *sp, args: None });
            expr(assign_expr, chunk, identifiers, sp)?;
            if let Const(_) = assign_expr.def {
                instr!(0b1000110000010000); // D = A
            }
            PUSH_D!();
        }
        FnDecl { id, params, return_t: _, body } => {
            println!("{}", chunk.len());
            identifiers.insert(*id, IdentifierValue { address: chunk.len() as u16, args: None/* Some(params.iter().map(|(_, v)| *v).collect()) */ });
            instr!(SP);                 // A = SP
            instr!(0b1001110000010000); // D = *A
            instr!(LOCALS);             // A = LOCALS
            instr!(0b1000001100001000); // *A = D
            instr!(0); // A = localsCount ????
            instr!(0b1000000010010000); // D = D + A
            instr!(SP);                 // A = SP
            instr!(0b1000001100001000); // *A = D
            expr(body, chunk, identifiers, sp)?;
        },
        StructDecl { .. } => todo!("Structure declarations are not implemented yet."),
        Block(exprs) => {
            // let sp_now = *sp;
            for e in exprs.iter() {
                expr(e, chunk, identifiers, sp)?;
            }
            // *sp = sp_now;
        }
        Parent(e) => {
            expr(e, chunk, identifiers, sp)?;
        }
        Return(return_expr) => {
            //HACK assuming return expr puts the return value in register A
            expr(return_expr, chunk, identifiers, sp)?;
            instr!(0b1000110000010000); // D = A
            instr!(RETVAL);             // A = RETVAL
            instr!(0b1000001100001000); // *A = D
            instr!(LOCALS);             // A = LOCALS
            instr!(0b1001110000010000); // D = *A
            instr!(SP);                 // A = SP
            instr!(0b1000001100001000); // *A = D
            POP_A!();                   // POP_A
            instr!(SP);
            instr!(0b1000000000000111); // JMP
        },
        End => (),
        Invalid => unimplemented!(),
    }

    Ok(())
}
