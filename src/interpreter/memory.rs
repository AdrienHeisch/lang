pub mod dyn_memory;
pub mod table_memory;
pub mod static_memory;

use dynamic::Dynamic;

pub trait Memory
{
    fn new () -> Self;
    fn get_var (&self, id:&String) -> Box<Dynamic>;
    fn set_var (&mut self, id:&String, value:&Box<Dynamic>) -> ();
    fn print_memory (&self) -> ();
}