use crate::ast::Op;
use crate::ast::{ Expr, ExprDef, Identifier };
use crate::value::{ Value };
use super::*;
use core::panic;
use std::collections::HashMap;

pub fn compile (statements:&[&Expr]) -> Result<Chunk, ()>
{
    let mut chunk = Chunk::new();
    let mut offset = 0_usize;
    let mut identifiers = HashMap::<Identifier, u16>::new();
    let mut sp = SP_INIT;

    for s in statements {
        expr(s, &mut chunk, &mut offset, &mut identifiers, &mut sp);
    }

    Ok(chunk)
}

fn expr (e: &Expr, chunk: &mut Chunk, offset: &mut usize, identifiers: &mut HashMap<Identifier, u16>, sp: &mut u16)
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
                chunk.push(0b1111110000100000); // A = *A
            } else {
                panic!("Unknown identifier")
            }
        },
        /* While { cond, body } => {
            let start_offset = *offset;

        }, */
        UnOp { op, e } => {
            expr(e, chunk, offset, identifiers, sp);    // A = e
            chunk.push(match *op {
                Op::Sub => 0b1000110011010000,      // D = -A
                _ => unimplemented!()
            });
        },
        BinOp { op, left, right } => {
            match *op {
                Op::Add => {
                    expr(left, chunk, offset, identifiers, sp);      // A = left
                    chunk.push(0b1110110000010000);             // D = A
                    expr(right, chunk, offset, identifiers, sp);     // A = right
                    chunk.push(0b1000000010010000);             // D = D + A
                },
                Op::Sub => {
                    expr(left, chunk, offset, identifiers, sp);      // A = left
                    chunk.push(0b1110110000010000);             // D = A
                    expr(right, chunk, offset, identifiers, sp);     // A = right
                    chunk.push(0b1000010011010000);             // D = D - A
                },
                Op::Assign => {
                    if let Id(id) = left.def {
                        let address = if let Some(address_ref) = identifiers.get(&id) {
                            *address_ref
                        } else {
                            panic!("Unkown identifier : {}", String::from_utf8(Vec::from(id)).unwrap());
                        };

                        expr(right, chunk, offset, identifiers, sp);
                        if let Const(_) = right.def {
                            chunk.push(0b1110110000010000);     // D = A
                        }
                        chunk.push(address);                    // A = address
                        chunk.push(0b1110001100001000);         // *A = D
                    } else {
                        panic!("Can't assign {:?} to {:?}", right, left);
                    }
                },
                _ => unimplemented!()
            };
        },
        VarDecl(id, assign_expr) => {
            identifiers.insert(*id, *sp);
            expr(assign_expr, chunk, offset, identifiers, sp);
            if let Const(_) = assign_expr.def {
                chunk.push(0b1110110000010000);             // D = A
            }
            chunk.push(0);                                  // A = SP
            chunk.push(0b1111110000100000);                 // A = *A
            chunk.push(0b1110001100001000);                 // *A = D
            chunk.push(0);                                  // A = SP
            chunk.push(0b1111110111001000);                 // *A = *A + 1
            *sp += 1;
        },
        Block(exprs) => {
            let sp_now = *sp;
            for e in exprs.iter() {
                expr(e, chunk, offset, identifiers, sp);
            }
            *sp = sp_now;
        },
        Parent(e) => {
            expr(e, chunk, offset, identifiers, sp);
        },
        End => (),
        _ => unimplemented!()
    }
}

/* fn assign (e: &Expr, address: u16, chunk: &mut Chunk) {

} */