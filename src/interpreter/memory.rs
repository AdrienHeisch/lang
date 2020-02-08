// pub mod dyn_memory;
// pub mod table_memory;
pub mod static_memory;

use dynamic::Dynamic;

pub trait Memory
{
    fn new () -> Self;
    fn get_var (&self, id:&str) -> Box<Dynamic>;
    fn set_var (&mut self, id:&str, value:&Dynamic) -> ();
    fn open_scope (&mut self) -> ();
    fn close_scope (&mut self) -> ();
    fn print_memory (&self) -> ();
}