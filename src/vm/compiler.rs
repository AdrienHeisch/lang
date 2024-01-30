use super::*;
use crate::ast::{Error, Expr, ExprDef, Identifier};
use crate::ast::{IdentifierTools, Op};
use crate::value::Value;
use std::collections::HashMap;

pub fn compile(statements: &[&Expr]) -> Result<(Chunk, DebugInfo), Error> {
    let mut chunk = Chunk::new();
    let mut identifiers = HashMap::<Identifier, u16>::new();
    let mut sp = SP_INIT; // necessary to keep track of variables on the stack at compile time

    for s in statements {
        expr(s, &mut chunk, &mut identifiers, &mut sp)?;
    }

    if cfg!(lang_print_vm_compiler) && !cfg!(lang_benchmark) {
        println!("========= ASM =========");
        for (offset, instruction) in chunk.iter().enumerate() {
            print!(
                "{:04} {:>11}   {:04X}",
                offset,
                instruction.to_asm(),
                instruction
            );
            println!();
        }
        println!("=======================");
        println!();
    }

    Ok((chunk, DebugInfo { identifiers }))
}

fn expr(e: &Expr, chunk: &mut Chunk, identifiers: &mut HashMap<Identifier, u16>, sp: &mut u16) -> Result<(), Error>{
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
            chunk.push(SP);                 // A = SP
            chunk.push(0b1001110000100000); // A = *A
            chunk.push(0b1000001100001000); // *A = D
            chunk.push(SP);                 // A = SP
            chunk.push(0b1001110111001000); // *A = *A + 1
        };
    }

    macro_rules! POP_D {
        () => {
            chunk.push(SP);                 // A = SP
            chunk.push(0b1001111111001000); // *A = *A - 1
            chunk.push(0b1001110000100000); // A = *A
            chunk.push(0b1001110000010000); // D = *A
        };
    }

    macro_rules! POP_A {
        () => {
            chunk.push(SP);                 // A = SP
            chunk.push(0b1001111111001000); // *A = *A - 1
            chunk.push(0b1001110000100000); // A = *A
            chunk.push(0b1001110000100000); // A = *A
        };
    }
    
    macro_rules! PUSH_STATIC {
        ($address: ident) => {
            chunk.push($address);               // A = ARGS
            chunk.push(0b1000110000010000); // D = A
            PUSH_D!();                      // PUSH_D
        };
    }

    use ExprDef::*;
    match &e.def {
        Const(value) => {
            chunk.push(match value {
                Value::Fn(_, _) => todo!("Functions as constants are not implemented yet."),
                Value::Pointer(p, _) if *p <= MEM_SIZE.try_into().unwrap() => todo!("Pointers are not implemented yet."),
                Value::Pointer(_, _) => panic!("Pointer out of range"),
                Value::Int(i) if *i <= 0x7fff => *i as u16,
                Value::Int(_) => panic!("Int out of range"),
                Value::Char(c) => *c as u16,
                Value::Float(_) => todo!("Floats are not implemented yet."), //TODO two's complement
                Value::Bool(false) => 0,
                Value::Bool(true) => 0xFFFF,
                Value::Void => panic!(),
                Value::Array { .. } => todo!("Arrays are not implemented yet."),
            });
        }
        Id(id) => {
            if let Some(address) = identifiers.get(id) {
                chunk.push(*address);           // A = address
                chunk.push(0b1001110000100000); // A = *A
            } else {
                panic!("Unknown identifier : {}", id.to_string())
            }
        }
        ArrayLit { .. } => todo!("Array literals are not implemented yet."),
        StringLit { .. } => todo!("String literals are not implemented yet."),
        If { cond, then, elze } => {
            let before_if = chunk.len();
            chunk.push(0); // A = tbd
            expr(cond, chunk, identifiers, sp)?;
            expr(then, chunk, identifiers, sp)?;

            let skip_else = chunk.len();
            if elze.is_some() {
                chunk.push(0);
                chunk.push(0b1000000000000111); // JMP
            }

            chunk[before_if] = (chunk.len() - 1) as u16; // A =

            if let Some(elze) = elze {
                expr(elze, chunk, identifiers, sp)?;
                chunk[skip_else] = (chunk.len() - 1) as u16;
            }
        }
        While { cond, body } => {
            let loop_start = chunk.len();
            chunk.push(0);
            expr(cond, chunk, identifiers, sp)?;
            expr(body, chunk, identifiers, sp)?;
            chunk.push((loop_start - 1) as u16);
            chunk.push(0b1000000000000111);     // JMP

            chunk[loop_start] = (chunk.len() - 1) as u16;
        }
        UnOp { op, e } => {
            expr(e, chunk, identifiers, sp)?;   // A = e
            match *op {
                Op::SubOrNeg =>     chunk.push(0b100001101001000),  // D = -A
                Op::Not =>          chunk.push(0b1000001100010000), // D = ~D
                Op::MultOrDeref =>  chunk.push(0b1001110000010000), // D = *A
                Op::Addr =>         chunk.push(0b1000110000010000), // D = A //TODO should be checked
                _ => todo!("This unary operator is not implemented yet."),
            };
        }
        BinOp { op, left, right } => {
            macro_rules! simple_binop {
                () => {
                    expr(left, chunk, identifiers, sp)?;    // A = left
                    chunk.push(0b1000110000010000);         // D = A
                    expr(right, chunk, identifiers, sp)?;   // A = right
                };
            }

            macro_rules! cond_binop {
                () => {
                    chunk.push(0b1000110000010000);         // D = A
                    chunk.push(JMP_ADDRESS);                // A = JMP_ADDRESS
                    chunk.push(0b1000001100001000);         // *A = D
                    expr(left, chunk, identifiers, sp)?;    // A = left
                    chunk.push(0b1000110000010000);         // D = A
                    expr(right, chunk, identifiers, sp)?;   // A = right
                    chunk.push(0b1000010011010000);         // D = D - A
                    chunk.push(JMP_ADDRESS);                // A = JMP_ADDRESS
                    chunk.push(0b1001110000100000);         // A = *A
                };
            }

            match *op {
                Op::Assign => {
                    match &left.def {
                        Id(id) => {
                            let address = if let Some(address_ref) = identifiers.get(id) {
                                *address_ref
                            } else {
                                panic!(
                                    "Unkown identifier : {}",
                                    String::from_utf8(Vec::from(id)).unwrap()
                                );
                            };

                            expr(right, chunk, identifiers, sp)?;
                            if let Const(_) = right.def {
                                chunk.push(0b1000110000010000); // D = A
                            }
                            chunk.push(address);                // A = address
                            chunk.push(0b1000001100001000);     // *A = D
                        }
                        UnOp { op: Op::MultOrDeref, e } => {

                            expr(right, chunk, identifiers, sp)?;
                            if let Const(_) = right.def {
                                chunk.push(0b1000110000010000); // D = A
                            }
                            expr(e, chunk, identifiers, sp)?;   // A = address
                            chunk.push(0b1000001100001000);     // *A = D
                        }
                        _ => {
                            panic!("Can't assign {:?} to {:?}", right, left);
                        }
                    }
                }
                Op::Add => {
                    simple_binop!();
                    chunk.push(0b1000000010010000); // D = D + A
                }
                Op::SubOrNeg => {
                    simple_binop!();
                    chunk.push(0b1000010011010000); // D = D - A
                }
                Op::BoolAnd => {
                    simple_binop!();
                    chunk.push(0b1000000000010000); // D = D & A
                }
                Op::BoolOr => {
                    simple_binop!();
                    chunk.push(0b1000010101010000); // D = D & A
                }
                Op::Equal => {
                    cond_binop!();
                    chunk.push(0b1000001010000101); // D; JNE
                }
                Op::NotEqual => {
                    cond_binop!();
                    chunk.push(0b1000001010000010); // D; JEQ
                }
                Op::Gt => {
                    cond_binop!();
                    chunk.push(0b1000001010000110); // D; JLE
                }
                Op::Gte => {
                    cond_binop!();
                    chunk.push(0b1000001010000100); // D; JLT
                }
                Op::Lt => {
                    cond_binop!();
                    chunk.push(0b1000001010000011); // D; JGE
                }
                Op::Lte => {
                    cond_binop!();
                    chunk.push(0b1000001010000001); // D; JGT
                }
                _ => todo!("This binary operator is not implemented yet."),
            };
        }
        Call { function, args } => {
            chunk.push(ARGS);               // A = ARGS
            chunk.push(0b1000110000010000); // D = A
            PUSH_D!();                      // PUSH_D

            chunk.push(LOCALS);             // A = LOCALS
            chunk.push(0b1000110000010000); // D = A
            PUSH_D!();                      // PUSH_D

            let return_label = chunk.len();
            chunk.push(0);                  // A = RETURN_LABEL
            chunk.push(0b1000110000010000); // D = A
            PUSH_D!();                      // PUSH_D

            chunk.push(SP);                 // A = SP
            chunk.push(0b1001110000010000); // D = *A
            chunk.push(args.len() as u16);  // A = args.len()
            chunk.push(0b1000010011010000); // D = D - A
            chunk.push(3);                  // A = 3
            chunk.push(0b1000010011010000); // D = D - A
            chunk.push(ARGS);               // A = ARGS
            chunk.push(0b1000001100001000); // *A = D

            expr(function, chunk, identifiers, sp)?;
            chunk.push(0b1000000000000111); // JMP
            chunk[return_label] = chunk.len() as u16;

            chunk.push(ARGS);               // A = ARGS
            chunk.push(0b1001110000010000); // D = *A
            chunk.push(TEMP_0);             // A = TEMP_0
            chunk.push(0b1000001100001000); // *A = D

            POP_D!();                       // POP_D
            chunk.push(LOCALS);             // A = LOCALS
            chunk.push(0b1000001100001000); // *A = D

            POP_D!();                       // POP_D
            chunk.push(ARGS);               // A = ARGS
            chunk.push(0b1000001100001000); // *A = D

            chunk.push(TEMP_0);             // A = TEMP_0
            chunk.push(0b1001110000010000); // D = *A
            chunk.push(SP);                 // A = SP
            chunk.push(0b1000001100001000); // *A = D

            chunk.push(RETVAL);             // A = RETVAL
            chunk.push(0b1001110000010000); // D = *A
            PUSH_D!();                      // PUSH_D
        },
        Field(_, _) => todo!("Field access is not implemented yet."),
        VarDecl(.., None) => todo!("Uninitialized variables are not implemented yet."), //TODO uninitialized var ?
        VarDecl(id, _, Some(assign_expr)) => { //TODO type checking ?
            identifiers.insert(*id, *sp);
            expr(assign_expr, chunk, identifiers, sp)?;
            if let Const(_) = assign_expr.def {
                chunk.push(0b1000110000010000); // D = A
            }
            PUSH_D!();
            *sp += 1;
        }
        FnDecl { id, params, return_t, body } => {
            identifiers.insert(*id, chunk.len() as u16);
            chunk.push(SP);                 // A = SP
            chunk.push(0b1001110000010000); // D = *A
            chunk.push(LOCALS);             // A = LOCALS
            chunk.push(0b1000001100001000); // *A = D
            chunk.push(params.len() as u16);// A = params.len()
            chunk.push(0b1000000010010000); // D = D + A
            chunk.push(SP);                 // A = SP
            chunk.push(0b1000001100001000); // *A = D
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
            chunk.push(0b1000110000010000); // D = A
            chunk.push(RETVAL);             // A = RETVAL
            chunk.push(0b1000001100001000); // *A = D
            chunk.push(LOCALS);             // A = LOCALS
            chunk.push(0b1001110000010000); // D = *A
            chunk.push(SP);                 // A = SP
            chunk.push(0b1000001100001000); // *A = D
            POP_A!();                       // POP_A
            chunk.push(0b1000000000000111); // JMP
        },
        End => (),
        Invalid => unimplemented!(),
    }

    Ok(())
}
