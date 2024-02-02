use super::*;
use std::num::Wrapping;

pub fn interpret (chunk: &Chunk, entrypoint: Address, globals: Vec<Instruction>) -> Result<i32, ()> {
    if cfg!(lang_print_vm_interpreter) && !cfg!(lang_benchmark) {
        /* print!("BYTECODE: ");
        for s in chunk.iter().map(|el| format!("{:04X}", el)) {
            print!("{} ", s);
        }
        println!(); */
        println!("===================================================");
    }

    let mut a = 0_u16;
    let mut d = 0_u16;
    let mut dummy_memory = 0;
    let mut memory: Memory = [0_u16; MEM_SIZE];
    memory[SP as usize] = STACK;

    if globals.len() > (STACK - GLOBALS) as usize {
        panic!("Too may globals");
    }
    for (i, global) in globals.iter().enumerate() {
        memory[GLOBALS as usize + i] = global.code
    }

    let mut counter = 0;
    let mut offset = entrypoint as usize;
    
    while offset < chunk.len() {
        let mut in_mem = if (a as usize) < memory.len() {
            &mut memory[a as usize]
        } else {
            &mut dummy_memory
        };

        let instruction = &chunk[offset];
        let mut did_jump = false;

        if cfg!(lang_print_vm_interpreter) && !cfg!(lang_benchmark) {
            print!("{:04} {:>12}   -->   ", offset, instruction.to_asm());
        }
        if check_bit(&instruction, 15) {
            let result = compute(instruction, a, d, *in_mem);

            if check_bit(&instruction, 5) {
                a = result;
            } else if check_bit(instruction, 4) {
                d = result;
            } else if check_bit(instruction, 3) {
                *in_mem = result;
            }
            
            if (check_bit(instruction, 0) && (result as i16) > 0) || (check_bit(instruction, 1) && result == 0) || (check_bit(instruction, 2) && (result as i16) < 0) {
                if a == 0x7FFF {
                    break;
                }
                offset = a as usize;
                did_jump = true;
            }
        } else {
            a = instruction.code;
        }
        
        in_mem = if (a as usize) < memory.len() {
            &mut memory[a as usize]
        } else {
            &mut dummy_memory
        };
        if cfg!(lang_print_vm_interpreter) && !cfg!(lang_benchmark) {
            println!("A: {:>3} | D: {:>3} | A*: {:>3}   |   SP: {}, ARGS: {}, LOCALS: {}, RETVAL: {}   |   {}, {:?}", a as i16, d as i16, *in_mem as i16, memory[SP as usize], memory[ARGS as usize], memory[LOCALS as usize], memory[RETVAL as usize], format!("{:?}", instruction.debug_info.def).split(['{', '(', '[']).nth(0).unwrap().trim().replace("\"", "").to_string(), instruction.debug_info.pos);
        }

        if !did_jump {
            offset += 1;
        }

        if cfg!(lang_vm_step) {
            std::io::stdin().read_line(&mut String::new()).ok();
        }

        if cfg!(lang_100_instructions_max) {
            counter += 1;
            if counter >= 100 {
                break;
            }
        }

        if memory[SP as usize] < STACK {
            break;
        }
    }
    
    if cfg!(lang_print_vm_interpreter) && !cfg!(lang_benchmark) {
        println!("===================================================");
        println!("\nMEMORY: {:?}\n", memory);
    }

    Ok(memory[RETVAL as usize] as i32) //TODO shouldn't be i32
}

fn compute (instruction: &Instruction, a: u16, d: u16, a_: u16) -> u16 {
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