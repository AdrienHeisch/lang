use crate::ast::{ Expr, ExprDef };
use crate::langval::{ LangVal };
use crate::memory::Memory;

pub struct Compiler
{
    pub memory: Memory,
    pub chunk: Vec<u8>
}

#[derive(Copy, Clone)]
enum OpCode
{
    Goto,
    Const
}

impl Compiler
{

    pub fn new () -> Self
    {
        Compiler
        {
            memory: Memory::new(),
            chunk: Vec::new()
        }
    }

    pub fn compile (&mut self, exprs:&[&Expr])
    {
        for e in exprs {
            self.expr(e);
        }
    }

    fn expr (&mut self, expr:&Expr)
    {
        use ExprDef::*;
        if let Const(value) = &expr.def {
            self.chunk.push(OpCode::Const.as_byte());

            use std::convert::TryInto;
            let cst = match value
            {
                LangVal::Number(n) if n.fract() == 0. => (*n as u64).try_into().unwrap(),
                LangVal::Number(_) => unimplemented!(),
                LangVal::Bool(b) => (*b) as u8,
                LangVal::Obj(_) => unimplemented!(),
                LangVal::FnPtr(_) => unimplemented!(),
                LangVal::Void => panic!(),
            };

            self.chunk.push(cst);
        }
    }

    pub fn print_bytecode (&self)
    {
        let mut out = String::from("bytecode:\n");
        self.chunk.iter().for_each(|byte| out = format!("{} {:X}", out, byte));
        println!("{}", out);
    }

}

impl OpCode
{
    fn as_byte (&self) -> u8
    {
        *self as u8
    }
}