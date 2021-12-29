use crate::ast::Op;
use crate::ast::{ Expr, ExprDef, Identifier };
use crate::value::{ Value };
use super::*;
use core::panic;
use std::collections::HashMap;

pub fn compile (statements:&[&Expr]) -> Result<Chunk, ()>
{
    let mut chunk = Chunk::new();
    let mut identifiers = HashMap::<Identifier, u16>::new();
    let mut sp = SP_INIT;

    for s in statements {
        expr(s, &mut chunk, &mut identifiers, &mut sp);
    }

    if cfg!(not(lang_benchmark)) {
        println!("======== ASM ========");
        for (offset, instruction) in chunk.iter().enumerate() {
            println!("{:04} {:>16}", offset, instruction.to_asm());
        }
        println!("=====================");
        println!();
    }

    Ok(chunk)
}

fn expr (e: &Expr, chunk: &mut Chunk, identifiers: &mut HashMap<Identifier, u16>, sp: &mut u16)
{
    use ExprDef::*;
    match &e.def {
        Const(value) => {
            chunk.push(match value
            {
                Value::Int(i) if *i <= 0x7fff => *i as u16,
                Value::Int(_) => panic!("Int out of range"),
                Value::Float(_) => unimplemented!(),
                Value::Bool(_) => unimplemented!(),
                Value::Void => panic!(),
            });
        },
        Id (id) => {
            if let Some(address) = identifiers.get(id) {
                chunk.push(*address);           // A = address
                chunk.push(0b1001110000100000); // A = *A
            } else {
                panic!("Unknown identifier")
            }
        },
        If { cond, then, elze } => {
            let before_if = chunk.len();
            chunk.push(0);                          // A = tbd
            expr(cond, chunk, identifiers, sp);
            expr(then, chunk, identifiers, sp);

            let skip_else = chunk.len();
            if elze.is_some() {
                chunk.push(0);
                chunk.push(0b1000000000000111); // JMP
            }

            chunk[before_if] = (chunk.len() - 1) as u16;

            if let Some(elze) = elze {
                expr(elze, chunk, identifiers, sp);
                chunk[skip_else] = (chunk.len() - 1) as u16;
            }
        },
        While { cond, body } => {
            let loop_start = chunk.len();
            chunk.push(0);
            expr(cond, chunk, identifiers, sp);
            expr(body, chunk, identifiers, sp);
            chunk.push((loop_start - 1) as u16);
            chunk.push(0b1000000000000111); // JMP

            chunk[loop_start] = (chunk.len() - 1) as u16;
        },
        UnOp { op, e } => {
            expr(e, chunk, identifiers, sp);    // A = e
            chunk.push(match *op {
                Op::Sub => 0b1000110011010000,      // D = -A
                _ => unimplemented!()
            });
        },
        BinOp { op, left, right } => {
            macro_rules! math_binop {
                () => {
                    expr(left, chunk, identifiers, sp);     // A = left
                    chunk.push(0b1000110000010000);         // D = A
                    expr(right, chunk, identifiers, sp);    // A = right    
                }
            }

            macro_rules! cond_binop {
                () => {
                    chunk.push(0b1000110000010000);         // D = A
                    chunk.push(JMP_ADDRESS);                // A = JMP_ADDRESS
                    chunk.push(0b1000001100001000);         // *A = D
                    expr(left, chunk, identifiers, sp);     // A = left
                    chunk.push(0b1000110000010000);         // D = A
                    expr(right, chunk, identifiers, sp);    // A = right
                    chunk.push(0b1000010011010000);         // D = D - A
                    chunk.push(JMP_ADDRESS);                // A = JMP_ADDRESS
                    chunk.push(0b1001110000100000);         // A = *A
                }
            }

            match *op {
                Op::Assign => {
                    if let Id(id) = left.def {
                        let address = if let Some(address_ref) = identifiers.get(&id) {
                            *address_ref
                        } else {
                            panic!("Unkown identifier : {}", String::from_utf8(Vec::from(id)).unwrap());
                        };

                        expr(right, chunk, identifiers, sp);
                        if let Const(_) = right.def {
                            chunk.push(0b1000110000010000);     // D = A
                        }
                        chunk.push(address);                    // A = address
                        chunk.push(0b1000001100001000);         // *A = D
                    } else {
                        panic!("Can't assign {:?} to {:?}", right, left);
                    }
                },
                Op::Add => {
                    math_binop!();
                    chunk.push(0b1000000010010000);             // D = D + A
                },
                Op::Sub => {
                    math_binop!();
                    chunk.push(0b1000010011010000);             // D = D - A
                },
                Op::Equal => {
                    cond_binop!();
                    chunk.push(0b1000001010000101);             // D; JNE
                },
                Op::NotEqual => {
                    cond_binop!();
                    chunk.push(0b1000001010000010);             // D; JEQ
                },
                Op::Gt => {
                    cond_binop!();
                    chunk.push(0b1000001010000110);             // D; JLE
                },
                Op::Gte => {
                    cond_binop!();
                    chunk.push(0b1000001010000100);             // D; JLT
                },
                Op::Lt => {
                    cond_binop!();
                    chunk.push(0b1000001010000011);             // D; JGE
                },
                Op::Lte => {
                    cond_binop!();
                    chunk.push(0b1000001010000001);             // D; JGT
                },
                _ => unimplemented!()
            };
        },
        VarDecl(id, assign_expr) => {
            identifiers.insert(*id, *sp);
            expr(assign_expr, chunk, identifiers, sp);
            if let Const(_) = assign_expr.def {
                chunk.push(0b1000110000010000);             // D = A
            }
            chunk.push(SP_ADDRESS);                         // A = SP
            chunk.push(0b1001110000100000);                 // A = *A
            chunk.push(0b1000001100001000);                 // *A = D
            chunk.push(SP_ADDRESS);                         // A = SP
            chunk.push(0b1001110111001000);                 // *A = *A + 1
            *sp += 1;
        },
        Block(exprs) => {
            // let sp_now = *sp;
            for e in exprs.iter() {
                expr(e, chunk, identifiers, sp);
            }
            // *sp = sp_now;
        },
        Parent(e) => {
            expr(e, chunk, identifiers, sp);
        },
        End => (),
        _ => unimplemented!()
    }
}

/* fn assign (e: &Expr, address: u16, chunk: &mut Chunk) {

} */