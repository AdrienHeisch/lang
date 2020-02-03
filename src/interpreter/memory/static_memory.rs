use super::Memory;
use dynamic::Dynamic;
use std::{
    any::TypeId,
    collections::HashMap
};

const MEMORY_SIZE_IN_BYTES:usize = 32;

#[derive(Debug)]
pub struct StaticMemory
{
    ram:Vec<u8>, //TODO any value ?
    allocation_map:[bool; MEMORY_SIZE_IN_BYTES],
    vars:HashMap<String, Variable>
}

#[derive(Clone, Copy, Debug)]
struct Variable
{
    t: TypeId,
    ptr:Pointer
}

#[derive(Clone, Copy, Debug)]
struct Pointer
{
    pos:usize,
    len:usize
}

// #region MACROS
macro_rules! invalid_type_error {
    ($id:expr) => {
        eprintln!("Invalid type id : {:?}", $id);
        eprintln!("Valid type ids would be : {:?}", vec!(TypeId::of::<i32>(), TypeId::of::<f32>()));
        panic!();
    };
}

macro_rules! get_dyn_size {
    ($bx:expr) => {
        match $bx.id()
        {
            //☺
            t if t == TypeId::of::<f32>() => std::mem::size_of::<f32>(),
            t if t == TypeId::of::<String>() => $bx.downcast_ref::<String>().unwrap().as_bytes().len(),
            t => {
                invalid_type_error!(t);
            }
        }
    };
}

/* macro_rules! call_on_dyn_box {
    ($bx:expr, $f:ident) => {
        match $bx.id()
        {
            //☺
            t if t == TypeId::of::<f32>() => *$f::<f32>(*$bx.downcast_ref::<f32>().unwrap()),
            t => {
                invalid_type_error!(t);
            }
        }
    };
    ($bx:expr, $f:ident, $T2:ty) => {
        match $bx.id()
        {
            //☺
            t if t == TypeId::of::<f32>() => &$f::<f32, $T2>(*$bx.downcast_ref::<f32>().unwrap()),
            t => {
                invalid_type_error!(t);
            }
        }
    };
}

macro_rules! clone_dynamic_box {
    ($bx:ident) => {
        match $bx.id()
        {
            //☺
            t if t == TypeId::of::<f32>() => Dynamic::new(*$bx.downcast_ref::<f32>().unwrap()),
            t => {
                invalid_type_error!(t);
            }
        }
    };
} */

macro_rules! dyn_box_to_string {
    ($bx:expr) => {
        match $bx.id()
        {
            //☺
            t if t == TypeId::of::<f32>() => $bx.downcast_ref::<f32>().unwrap().to_string(),
            t if t == TypeId::of::<String>() => $bx.downcast_ref::<String>().unwrap().clone(),
            t => {
                invalid_type_error!(t);
            }
        }
    };
}
// #endregion

impl Memory for StaticMemory
{

    fn new () -> StaticMemory
    {
        StaticMemory
        {
            ram: {
                let mut vec = Vec::with_capacity(MEMORY_SIZE_IN_BYTES);
                vec.resize(MEMORY_SIZE_IN_BYTES, 0);
                vec
            },
            allocation_map: [false; MEMORY_SIZE_IN_BYTES],
            vars: HashMap::new()
        }
    }

    fn get_var (&self, id:&String) -> Box<Dynamic>
    {
        let var = if let Some(var) = self.vars.get(id) {
            *var
        } else {
            eprintln!("Unknown identifier : {}", id);
            panic!()
        };

        match var.t
        {
            //☺
            t if t == TypeId::of::<f32>() => unsafe {
                Dynamic::new(f32::from_ne_bytes(std::mem::transmute([self.ram[var.ptr.pos], self.ram[var.ptr.pos + 1], self.ram[var.ptr.pos + 2], self.ram[var.ptr.pos + 3]])))
            },
            t if t == TypeId::of::<String>() => {
                Dynamic::new(String::from_utf8(Vec::from(self.access(var.ptr))).ok().unwrap())
            },
            t => {
                invalid_type_error!(t);
            }
        }
    }

    fn set_var (&mut self, id:&String, value:&Box<Dynamic>) -> ()
    {
        let var = if let Some(var) = self.vars.get(id) {
            *var
        } else {
            let var = Variable
            {
                t: value.id(),
                ptr: self.alloc(get_dyn_size!(value))
            };
            self.vars.insert(id.clone(), var);
            var
        };
        
        match value.id()
        {
            //☺
            t if t == TypeId::of::<f32>() => {
                self.access_mut(var.ptr).copy_from_slice(&value.downcast_ref::<f32>().unwrap().to_ne_bytes()[0..4])
            },
            t if t == TypeId::of::<String>() => {
                self.access_mut(var.ptr).copy_from_slice(value.downcast_ref::<String>().unwrap().as_bytes()) //FIXME crash if string have different lengths
            },
            t => {
                invalid_type_error!(t);
            }
        }
    }

    #[allow(dead_code)]
    fn print_memory (&self) -> ()
    {
        let mut mem_str = String::default();
        for (id, _) in &self.vars
        {
            mem_str = format!("{}{} => {}, ", mem_str, id, dyn_box_to_string!(*(&self.get_var(id))));
        }
        println!("{}", mem_str);
        println!("raw: {:?}", self.ram);
        println!("map: {:?}", self.allocation_map);
    }
}

impl StaticMemory
{

    fn alloc (&mut self, len:usize) -> Pointer
    {
        #[allow(unused_assignments)]
        let mut ptr_option = None;
        let mut pos = 0;
        loop
        {
            while self.allocation_map[pos] {
                pos += 1;
            }
            let mut is_valid = true;
            for b in &self.allocation_map[pos..(pos + len)] {
                if *b { is_valid = false; break; }
            }
            if is_valid {
                ptr_option = Some(Pointer { pos, len });
                break;
            }
        }

        if let Some(ptr) = ptr_option
        {
            for i in pos..(pos + len) {
                self.allocation_map[i] = true;
            }
            ptr
        } 
        else
        {
            eprintln!("OUT OF MEMORY !");
            panic!()
        }
    }

    fn access (&self, ptr:Pointer) -> &[u8]
    {
        &self.ram[ptr.pos..(ptr.pos + ptr.len)]
    }

    fn access_mut (&mut self, ptr:Pointer) -> &mut [u8]
    {
        &mut self.ram[ptr.pos..(ptr.pos + ptr.len)]
    }

}