// pub mod dyn_memory;
// pub mod table_memory;
pub mod static_memory;

use crate::cst::Const;

pub trait Memory
{
    fn new () -> Self;
    fn get_var (&self, id:&String) -> Const;
    fn set_var (&mut self, id:&String, value:&Const) -> ();
    fn open_scope (&mut self) -> ();
    fn close_scope (&mut self) -> ();
    fn print_memory (&self) -> ();
}