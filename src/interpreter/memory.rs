use crate::{
    value::{Type, Value},
    memory::{Pointer as RawPointer, Memory as RawMemory}
};

use std::{convert::TryInto};

pub struct Memory(RawMemory);

#[derive(Debug, Copy, Clone, Default, PartialEq)]
pub struct Pointer
{
    pub t: Type,
    raw: RawPointer
}

impl Memory {

    pub fn new () -> Self {
        Self(RawMemory::new())
    }

    pub fn get_var (&self, ptr:Pointer) -> Value
    {
        match ptr.t
        {
            Type::Int => Value::Int(i32::from_ne_bytes(self.0.access(ptr.raw).try_into().unwrap())),
            Type::Float => Value::Float(f32::from_ne_bytes(self.0.access(ptr.raw).try_into().unwrap())),
            Type::Bool  => Value::Bool(self.0.access(ptr.raw)[0] == 1),
            Type::Void  => Value::Void,
        }
    }
    

    pub fn set_var (&mut self, ptr:Pointer, value:&Value) -> Pointer //TODO remove return
    {
        use Value::*;
        match value
        {
            Int(i) => {
                self.0.access_mut(ptr.raw).copy_from_slice(&(*i).to_ne_bytes());
            },
            Float(f) => {
                self.0.access_mut(ptr.raw).copy_from_slice(&(*f).to_ne_bytes());
            },
            Bool(b) => {
                self.0.access_mut(ptr.raw)[0] = if *b { 1u8 } else { 0u8 };
            },
            Void => panic!() //DESIGN set var to Void ?
        }
        ptr
    }

    pub fn free_ptr (&mut self, ptr:Pointer)
    {
        self.0.free(ptr.raw);
    }

    pub fn make_pointer_for_type (&mut self, t:Type) -> Pointer
    {
        if let Type::Void = t {
            panic!("Tried to create pointer for void"); //DESIGN make pointer for Void ?
        } else {
            Pointer {
                t,
                raw: self.0.alloc(t.get_size())
            }
        }
    }

    pub fn print_ram (&self)
    {
        self.0.print_ram();
    }

}