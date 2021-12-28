use crate::vm::ToAsm;

use super::*;
use std::num::Wrapping;

pub fn interpret (chunk: Chunk) {
    let mut a = 0u16;
    let mut d = 0u16;
    let mut memory = [0u16; MEM_SIZE];
    memory[0] = SP_INIT;

    let mut index = 0;
    while index < chunk.len() {
        let instruction = chunk[index];
        if check_bit(instruction, 15) {
            if check_bit(instruction, 5) {
                a = compute(instruction, a, d, memory[a as usize]);
            } else if check_bit(instruction, 4) {
                d = compute(instruction, a, d, memory[a as usize]);
            } else if check_bit(instruction, 3) {
                memory[a as usize] = compute(instruction, a, d, memory[a as usize]);
            } else {
                compute(instruction, a, d, memory[a as usize]);
            }
        } else {
            a = instruction;
        }
        println!("{:>16}   -->   A: {:<3} | D: {:<3} | A*: {:<3}", instruction.to_asm(), a as i16, d as i16, memory[a as usize] as i16);
        index += 1;
    }

    println!("MEMORY: {:?}", memory);
}

fn compute (instruction: Instruction, a: u16, d: u16, a_: u16) -> u16 {
    let mut d_val = Wrapping(d);
    if check_bit(instruction, 11) {
        d_val.0 = 0;
    }
    if check_bit(instruction, 10) {
        d_val.0 = !d_val.0;
    }

    let mut a_val = Wrapping(if check_bit(instruction, 12) { a_ } else { a });
    if check_bit(instruction, 9) {
        a_val.0 = 0;
    }
    if check_bit(instruction, 8) {
        a_val.0 = !a_val.0;
    }

    let mut out = if check_bit(instruction, 7) {
        (a_val + d_val).0
    } else {
        (a_val & d_val).0
    };

    if check_bit(instruction, 6) {
        out = !out;
    }

    out
}