pub mod compiler;
pub mod interpreter;

const MEM_SIZE: usize = 64;
pub type Memory = [u16; MEM_SIZE];

const SP_ADDRESS: u16 = 0;
const SP_INIT: u16 = 16;

const JMP_ADDRESS: u16 = 1;

pub type Chunk = Vec<Instruction>;
type Instruction = u16; //TODO use i16 ?

fn check_bit(instruction: Instruction, n: u8) -> bool {
    instruction & 1 << n != 0
}

trait InstructionTools {
    fn to_asm(&self) -> String;
}

impl InstructionTools for Instruction {
    fn to_asm(&self) -> String {
        if !check_bit(*self, 15) {
            return format!("A = {}", *self as i16);
        }

        let sm = if (self & 0b1000000000000) == 0 {
            "A"
        } else {
            "*A"
        };

        let opcode = match (self & 0b111111000000) >> 6 {
            _ if self & 0b111 == 0b111 => format!(""),
            0b001010 | 0b001100 => format!("D"),
            0b100010 | 0b110000 => format!("{}", sm),
            0b000000 => format!("D&{}", sm),
            0b010101 => format!("D|{}", sm),
            0b011010 => format!("~D"),
            0b100110 => format!("~{}", sm),
            0b000010 => format!("D+{}", sm),
            0b010011 => format!("D-{}", sm),
            0b000111 => format!("{}-D", sm),
            0b100000 => format!("0"),
            0b100001 => format!("-1"),
            0b111111 => format!("1"),
            0b001111 => format!("-D"),
            0b110011 => format!("-{}", sm),
            0b011111 => format!("D+1"),
            0b110111 => format!("{}+1", sm),
            0b001110 => format!("D-1"),
            0b110010 => format!("{}-1", sm),
            unknown => panic!("{:b}", unknown),
        };

        let target = match (self & 0b111000) >> 3 {
            0b000 => "",
            0b001 => "*A = ",
            0b010 => "D = ",
            0b011 => "D, *A = ",
            0b100 => "A = ",
            0b101 => "A, *A = ",
            0b110 => "A, D = ",
            0b111 => "A, D, *A = ",
            _ => panic!(),
        };

        let cond = match self & 0b111 {
            0b000 => "",
            0b001 => "; JGT",
            0b010 => "; JEQ",
            0b011 => "; JGE",
            0b100 => "; JLT",
            0b101 => "; JNE",
            0b110 => "; JLE",
            0b111 => "JMP",
            _ => panic!(),
        };

        format!("{}{}{}", target, opcode, cond)
    }

    //TODO from_asm ?
}

pub struct DebugInfo {
    pub identifiers: std::collections::HashMap<crate::ast::Identifier, u16>,
}
