use crate::ast::Position;

pub mod compiler;
pub mod interpreter;

pub const MEM_SIZE: usize = 64;
pub type Memory = [u16; MEM_SIZE]; // type MemoryCell = u16 ?

const SP: Address = 0;
const SP_INIT: u16 = 16;
const JMP_ADDRESS: Address = 1;
const ARGS: Address = 2;
const LOCALS: Address = 3;
const RETVAL: Address = 4;
const TEMP_0: Address = 5;

pub type Chunk = Vec<Instruction>;
pub type Address = u16;

#[derive(Clone)]
pub struct Instruction {
    code: u16,
    #[cfg(lang_debug)]
    debug_info: InstructionDebugInfo
}

#[cfg(lang_debug)]
#[derive(Debug, Clone)]
struct InstructionDebugInfo {
    pos: Position,
    def: String
}

fn check_bit(instruction: &Instruction, n: u8) -> bool {
    instruction.code & 1_u16 << n != 0
}

trait InstructionTools { //TODO useless trait ?
    fn to_asm(&self) -> String;
    // fn get_debug_info(&self) -> InstructionDebugInfo;
}

impl InstructionTools for Instruction {
    fn to_asm(&self) -> String {
        if !check_bit(self, 15) {
            return format!("A = {}", self.code);
        }

        let sm = if check_bit(self, 12) {
            "*A"
        } else {
            "A"
        };

        #[allow(clippy::useless_format)]
        let opcode = match (self.code & 0b111111000000) >> 6 {
            _ if self.code & 0b111 == 0b111 => format!(""),
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
            unknown => format!("{:b} (code unknown)", unknown),
        };

        let target = match (self.code & 0b111000) >> 3 {
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

        let cond = match self.code & 0b111 {
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


    /* #[cfg(not(lang_debug))]
    fn get_debug_info(&self) -> InstructionDebugInfo {}

    #[cfg(lang_debug)]
    fn get_debug_info(&self) -> InstructionDebugInfo {
        self.debug_info
    } */

    //TODO from_asm ?
}

impl From<Instruction> for u16 {
    fn from (instruction: Instruction) -> Self {
        instruction.code
    }
}

pub struct DebugInfo {
    pub identifiers: std::collections::HashMap<crate::ast::Identifier, u16>,
}
