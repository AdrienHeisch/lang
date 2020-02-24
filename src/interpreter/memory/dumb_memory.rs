use crate::langval::LangVal;
use super::Memory;

pub struct DumbMemory;

#[allow(unused_variables)]
impl Memory for DumbMemory
{

    fn new () -> Self
    {
        Self
    }

    fn get_var (&self, id:&str) -> LangVal
    {
        LangVal::Void
    }

    fn set_var (&mut self, id:&str, value:&LangVal) {}

    fn open_scope (&mut self) {}
    
    fn close_scope (&mut self) {}

    fn print_memory (&self)
    {
        println!("Sorry I'm dumb.");
    }

}