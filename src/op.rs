#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op
{
    Not,
    Add,
    Mult,
    Div,
    Sub,
    Mod,
    Equal,
    NotEqual,
    Gt,
    Gte,
    Lt,
    Lte,
    BoolAnd,
    BoolOr,
    Assign
}

impl Op
{

    pub fn from_string (string:&str) -> (Op, bool)
    {
        let mut is_assign = false;
        let op = match string
        {
            "!" =>  Op::Not,
            "==" => Op::Equal,
            "!=" => Op::NotEqual,
            ">" =>  Op::Gt,
            ">=" => Op::Gte,
            "<" =>  Op::Lt,
            "<=" => Op::Lte,
            "&&" => Op::BoolAnd,
            "||" => Op::BoolOr,
            "=" =>  Op::Assign,
            //Check if this is an assign operator other than "="
            op if op.ends_with('=') => {
                is_assign = true;
                Op::from_string(&op[0..(op.len() - 1)]).0
            },
            //All operators below can have an assign version like "+="
            "%" =>  Op::Mod,
            "+" =>  Op::Add,
            "-" =>  Op::Sub,
            "*" =>  Op::Mult,
            "/" =>  Op::Div,
            op => {
                eprintln!("Invalid unop : {}", op);
                panic!();
            }
        };

        (op, is_assign)
    }

    pub fn priority (self) -> u8
    {
        match self
        {
            Op::Not => 0,
            Op::Mod => 1,
            Op::Mult => 2,
            Op::Div => 2,
            Op::Add => 3,
            Op::Sub => 3,
            Op::Equal => 4,
            Op::NotEqual => 4,
            Op::Gt => 4,
            Op::Gte => 4,
            Op::Lt => 4,
            Op::Lte => 4,
            Op::BoolAnd => 5,
            Op::BoolOr => 6,
            Op::Assign => 7
        }
    }

}