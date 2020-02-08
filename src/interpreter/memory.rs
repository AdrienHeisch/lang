// pub mod dyn_memory;
// pub mod table_memory;
pub mod static_memory;
// pub mod dumb_memory;

use crate::cst::Const;

pub trait Memory
{
    fn new () -> Self;
    fn get_var (&self, id:&str) -> Const;
    fn set_var (&mut self, id:&str, value:&Const) -> ();
    fn open_scope (&mut self) -> ();
    fn close_scope (&mut self) -> ();
    fn print_memory (&self) -> ();
}