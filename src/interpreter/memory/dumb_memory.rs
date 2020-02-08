use crate::cst::Const;
use super::Memory;

pub struct DumbMemory;

#[allow(unused_variables)]
impl Memory for DumbMemory
{

    fn new () -> Self
    {
        DumbMemory
    }

    fn get_var (&self, id:&str) -> Const
    {
        Const::Void
    }

    fn set_var (&mut self, id:&str, value:&Const) {}

    fn open_scope (&mut self) {}
    
    fn close_scope (&mut self) {}

    fn print_memory (&self)
    {
        println!("Sorry I'm dumb.");
    }

}