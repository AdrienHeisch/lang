pub mod compiler;
pub mod interpreter;

const MEM_SIZE: usize = 64;

const SP_ADDRESS: u16 = 0;
const SP_INIT: u16 = 16;

const JMP_ADDRESS: u16 = 1;

pub type Chunk = Vec<Instruction>;
type Instruction = u16; //TODO use i16 ?

fn check_bit (instruction: Instruction, n: u8) -> bool {
    instruction & 1 << n != 0
}

trait ToAsm {
    fn to_asm (&self) -> String;
}

impl ToAsm for Instruction {

    fn to_asm (&self) -> String { //TODO real thing
        match self {
            0b1000000000000111 => format!("JMP"),
            0b1000110000010000 => format!("D = A"),
            0b1000110011010000 => format!("D = -A"),
            0b1000000010010000 => format!("D = D + A"),
            0b1000010011010000 => format!("D = D - A"),
            0b1001110000100000 => format!("A = *A"),
            0b1000001100010000 => format!("*A = A"),
            0b1000001100001000 => format!("*A = D"),
            0b1001110111001000 => format!("*A = *A + 1"),
            0b1000001010000010 => format!("D; JEQ"),
            0b1000001010000101 => format!("D; JNE"),
            0b1000001010000011 => format!("D; JGE"),
            value if !check_bit(*value, 15) => format!("A = {}", *value as i16),
            computation => format!("Unknown computation {:b}", computation)
        }
    }

}