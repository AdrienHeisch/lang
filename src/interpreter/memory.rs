use crate::{
    memory::{RawMemory, RawPointer},
    value::{Type, Value},
};

use std::convert::TryInto;

// pub struct Memory(RawMemory);

#[derive(Debug, Copy, Clone, Default, PartialEq)]
pub struct Pointer {
    pub t: Type,
    raw: RawPointer,
}

pub trait Memory {
    fn new() -> Self;
    fn get_var(&self, ptr: Pointer) -> Value;
    fn set_var(&mut self, ptr: Pointer, value: &Value) -> Pointer;
    fn free_ptr(&mut self, ptr: Pointer);
    fn make_pointer_for_type(&mut self, t: Type) -> Pointer;
    fn print_ram(&self);
}

impl Memory for RawMemory {
    fn new() -> Self {
        Self::new()
    }

    fn get_var(&self, ptr: Pointer) -> Value {
        match ptr.t {
            Type::Int => Value::Int(i32::from_ne_bytes(self.access(ptr.raw).try_into().unwrap())),
            Type::Float => {
                Value::Float(f32::from_ne_bytes(self.access(ptr.raw).try_into().unwrap()))
            }
            Type::Bool => Value::Bool(self.access(ptr.raw)[0] == 1),
            Type::Fn => panic!(),
            Type::Void => Value::Void,
        }
    }

    fn set_var(&mut self, ptr: Pointer, value: &Value) -> Pointer //TODO remove return
    {
        use Value::*;
        match value {
            Int(i) => {
                self.access_mut(ptr.raw)
                    .copy_from_slice(&(*i).to_ne_bytes());
            }
            Float(f) => {
                self.access_mut(ptr.raw)
                    .copy_from_slice(&(*f).to_ne_bytes());
            }
            Bool(b) => {
                self.access_mut(ptr.raw)[0] = if *b { 1u8 } else { 0u8 };
            }
            Void => panic!(), //DESIGN set var to Void ?
        }
        ptr
    }

    fn free_ptr(&mut self, ptr: Pointer) {
        self.free(ptr.raw);
    }

    fn make_pointer_for_type(&mut self, t: Type) -> Pointer {
        if let Type::Void = t {
            panic!("Tried to create pointer for void"); //DESIGN make pointer for Void ?
        } else {
            Pointer {
                t,
                raw: self.alloc(t.get_size()),
            }
        }
    }

    fn print_ram(&self) {
        self.print_ram();
    }
}
