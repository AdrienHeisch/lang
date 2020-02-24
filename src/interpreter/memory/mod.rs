pub mod vec_memory;
pub mod hashmap_memory;
pub mod table_memory;
pub mod static_memory;
pub mod dumb_memory;

use crate::langval::LangVal;

pub trait Memory
{
    fn new () -> Self;
    fn get_var (&self, id:&str) -> LangVal;
    fn set_var (&mut self, id:&str, value:&LangVal) -> ();
    fn open_scope (&mut self) -> ();
    fn close_scope (&mut self) -> ();
    fn print_memory (&self) -> ();
}

#[derive(Clone, Debug)]
enum VarType //TODO replace with enum discriminants ?
{
    Number,
    Str,
    Bool,
    // Void
}