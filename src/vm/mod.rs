pub mod compiler;
pub mod interpreter;

const MEM_SIZE: usize = 32;
const SP_INIT: u16 = 16;

pub type Chunk = Vec<Instruction>;
type Instruction = u16;

fn check_bit (instruction: Instruction, n: u8) -> bool {
    instruction & 1 << n != 0
}

trait ToAsm {
    fn to_asm (&self) -> String;
}

impl ToAsm for Instruction {

    fn to_asm (&self) -> String {
        match self {
            0b1110110000010000 => format!("D = A"),
            0b1000110011010000 => format!("D = -A"),
            0b1000000010010000 => format!("D = D + A"),
            0b1000010011010000 => format!("D = D - A"),
            0b1111110000100000 => format!("A = *A"),
            0b1110001100001000 => format!("*A = D"),
            0b1111110111001000 => format!("*A = *A + 1"),
            value if !check_bit(*value, 15) => format!("A = {}", *value as i16),
            _ => format!("Unknown computation")
        }
    }

}