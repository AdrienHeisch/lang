// pub mod vec_memory;
// pub mod hashmap_memory;
// pub mod table_memory;
pub mod static_memory;
// pub mod dumb_memory;

use crate::langval::LangVal;
use crate::ast::Identifier;

pub trait Memory
{
    fn new () -> Self;
    fn get_var (&self, id:&Identifier) -> LangVal;
    fn set_var (&mut self, id:&Identifier, value:&LangVal) -> ();
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