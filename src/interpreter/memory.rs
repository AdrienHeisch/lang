use crate::{
    memory::{Address, RawMemory},
    value::{Type, Value},
};

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Variable {
    pub t: Type,
    pub raw: Address,
} //TODO refactor without type ?

pub trait Memory {
    fn new() -> Self;
    fn get_var(&self, var: &Variable) -> Value;
    fn set_var(&mut self, var: &Variable, value: &Value);
    fn free_ptr(&mut self, var: &Variable);
    fn make_pointer_for_type(&mut self, t: &Type) -> Variable;
    fn print_ram(&self);
}

impl Memory for RawMemory {
    fn new() -> Self {
        Self::new()
    }

    fn get_var(&self, var: &Variable) -> Value {
        match var.t {
            Type::Pointer(ref t) => Value::Pointer(
                u32::from_ne_bytes(self.access(var.raw).try_into().unwrap()),
                t.clone(),
            ),
            Type::Array { len, ref t } => {
                let raw = self.access(var.raw);
                let addr = u32::from_ne_bytes(raw[0..4].try_into().unwrap());
                let len_ = u32::from_ne_bytes(raw[4..8].try_into().unwrap());
                if len != len_ {
                    panic!("Unmatched array lengths: ");
                }
                Value::Array {
                    addr,
                    len,
                    t: t.clone(),
                }
            },
            Type::Int => Value::Int(i32::from_ne_bytes(self.access(var.raw).try_into().unwrap())),
            Type::Char => Value::Char(self.access(var.raw)[0] as char),
            Type::Float => {
                Value::Float(f32::from_ne_bytes(self.access(var.raw).try_into().unwrap()))
            }
            Type::Bool => Value::Bool(self.access(var.raw)[0] == 1),
            Type::Fn(_, _) => panic!(),
            Type::Void => Value::Void,
        }
    }

    fn set_var(&mut self, var: &Variable, value: &Value) {
        use Value::*;
        match value {
            Pointer(p, _) => {
                self.access_mut(var.raw)
                    .copy_from_slice(&(*p).to_ne_bytes());
            }
            Array { addr, len, .. } => {
                self.access_mut(var.raw)
                    .copy_from_slice(&[addr.to_ne_bytes(), len.to_ne_bytes()].concat());
            }
            Int(i) => {
                self.access_mut(var.raw).copy_from_slice(&i.to_ne_bytes());
            }
            Char(c) => {
                self.access_mut(var.raw)[0] = *c as u8;
            }
            Float(f) => {
                self.access_mut(var.raw).copy_from_slice(&f.to_ne_bytes());
            }
            Bool(b) => {
                self.access_mut(var.raw)[0] = if *b { 1u8 } else { 0u8 };
            }
            Void => panic!(), //DESIGN set var to Void ?
        }
    }

    fn free_ptr(&mut self, ptr: &Variable) {
        self.free(ptr.raw);
    }

    fn make_pointer_for_type(&mut self, t: &Type) -> Variable {
        if let Type::Void = t {
            panic!("Tried to create pointer for void"); //DESIGN make pointer for Void ?
        } else {
            Variable {
                t: t.clone(),
                raw: self.alloc(t.get_size()),
            }
        }
    }

    fn print_ram(&self) {
        self.print_ram();
    }
}
