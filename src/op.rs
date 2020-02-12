#[derive(Debug, Clone/* , PartialEq */)]
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
        let is_assign = false;
        (match string
        {
            "==" => Op::Equal,
            "!=" => Op::NotEqual,
            ">" =>  Op::Gt,
            ">=" => Op::Gte,
            "<" =>  Op::Lt,
            "<=" => Op::Lte,
            "&&" => Op::BoolAnd,
            "||" => Op::BoolOr,
            "=" =>  Op::Assign,
            op if op.ends_with('=') => {
                is_assign = true;
                Op::from_string(&op[0..(op.len() - 1)]).0
            },
            "+" =>  Op::Add,
            "-" =>  Op::Sub,
            "*" =>  Op::Mult,
            "/" =>  Op::Div,
            "%" =>  Op::Mod,
            op => {
                eprintln!("Invalid unop : {}", op);
                panic!();
            }
        }, is_assign)
    }

    pub fn priority (op:Op) -> u8
    {
        match op
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