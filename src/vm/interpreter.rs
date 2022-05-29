use super::*;
use std::num::Wrapping;

pub fn interpret (chunk: &Chunk) -> Result<(), ()> {
    if cfg!(lang_print_vm_runtime) {
        print!("BYTECODE: ");
        for s in chunk.iter().map(|el| format!("{:04X}", el)) {
            print!("{} ", s);
        }
        println!();
    }

    let mut a = 0_u16;
    let mut d = 0_u16;
    let mut memory = [0_u16; MEM_SIZE];
    memory[0] = SP_INIT;
    let mut dummy_memory = 0;

    let mut offset = 0;
    while offset < chunk.len() {
        let in_mem = if (a as usize) < memory.len() {
            &mut memory[a as usize]
        } else {
            &mut dummy_memory
        };

        let instruction = chunk[offset];
        if cfg!(lang_print_vm_runtime) {
            print!("{:04} {:>16}   -->   ", offset, instruction.to_asm());
        }
        if check_bit(instruction, 15) {
            let result = compute(instruction, a, d, *in_mem);

            if check_bit(instruction, 5) {
                a = result;
            } else if check_bit(instruction, 4) {
                d = result;
            } else if check_bit(instruction, 3) {
                *in_mem = result;
            }
            
            if (check_bit(instruction, 0) && (result as i16) > 0) || (check_bit(instruction, 1) && result == 0) || (check_bit(instruction, 2) && (result as i16) < 0) {
                offset = a as usize;
            }
        } else {
            a = instruction;
        }
        if cfg!(lang_print_vm_runtime) {
            println!("A: {:<3} | D: {:<3} | A*: {:<3}", a as i16, d as i16, *in_mem as i16);
        }
        offset += 1;
    }
    
    if cfg!(lang_print_vm_runtime) {
        println!("MEMORY: {:?}", memory);
    }

    Ok(())
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