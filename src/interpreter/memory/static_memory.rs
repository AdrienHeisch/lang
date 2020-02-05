use crate::{get_dyn_size, invalid_type_error, dyn_box_to_string}; //MACROS
use super::Memory;
use dynamic::Dynamic;
use std::{
    any::TypeId,
    collections::HashMap
};

//IN BYTES
const MEMORY_SIZE:usize = 32;

//TODO look at C stack heap implementations (stack growing from the end)
#[derive(Debug)]
pub struct StaticMemory
{
    ram:[u8; MEMORY_SIZE],
    allocation_map:[bool; MEMORY_SIZE],
    scopes:Vec<Scope>
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

type Scope = HashMap<String, Variable>;

impl Memory for StaticMemory
{

    fn new () -> StaticMemory
    {
        StaticMemory
        {
            ram: [u8::default(); MEMORY_SIZE],
            allocation_map: [false; MEMORY_SIZE],
            scopes: Vec::new()
        }
    }

    fn get_var (&self, id:&String) -> Box<Dynamic>
    {
        let mut var_opt = None;
        for scope in self.scopes.iter().rev()
        {
            var_opt = scope.get(id);
            if let Some(_) = var_opt {
                break;
            }
        }

        let var = if let Some(var) = var_opt {
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
        let mut var_opt = None;
        for scope in self.scopes.iter().rev()
        {
            var_opt = scope.get(id);
            if let Some(_) = var_opt {
                break;
            }
        }

        let mut var = if let Some(var) = var_opt {
            *var
        } else {
            let var = Variable
            {
                t: value.id(),
                ptr: self.alloc(get_dyn_size!(value))
            };
            self.scopes.last_mut().unwrap().insert(id.clone(), var);
            var
        };
        
        match value.id()
        {
            //☺
            t if t == TypeId::of::<f32>() => {
                self.access_mut(var.ptr).copy_from_slice(&value.downcast_ref::<f32>().unwrap().to_ne_bytes()[0..4])
            },
            t if t == TypeId::of::<String>() => {
                let bytes = value.downcast_ref::<String>().unwrap().as_bytes();
                let len = bytes.len();
                if len != var.ptr.len {
                    self.realloc(&mut var, len);
                    self.scopes.last_mut().unwrap().insert(id.clone(), var);
                }
                self.access_mut(var.ptr).copy_from_slice(bytes)
            },
            t => {
                invalid_type_error!(t);
            }
        }
    }

    fn open_scope (&mut self) -> ()
    {
        self.scopes.push(Scope::new());
    }

    fn close_scope (&mut self) -> ()
    {
        let scope = if let Some(scope) = self.scopes.pop() {
            scope
        } else {
            eprintln!("There is no scope to close.");
            panic!();
        };

        for (_, var) in scope
        {
            self.free(var.ptr);
        }
    }

    #[allow(dead_code)]
    fn print_memory (&self) -> ()
    {
        // self.vars.iter().map(|(k, v)| format!("{} => {:?}", k, v.ptr)).for_each(|s| print!("{}, ", s));
        // println!();
        let mut mem_str = String::default();
        for scope in self.scopes.iter().rev()
        {
            for (id, address) in scope
            {
                mem_str = format!("{}{} => {:?} => {}\n", mem_str, id, address.ptr, dyn_box_to_string!(*(&self.get_var(&id))));
            }
            mem_str = format!("{}----------\n", mem_str);
        }
        print!("{}", mem_str);
        println!("raw: {:?}", self.ram);
        // println!("map: {:?}", self.allocation_map);
    }
}

impl StaticMemory
{

    fn alloc (&mut self, len:usize) -> Pointer
    {
        macro_rules! out_of_mem_error {
            () => {
                eprintln!("OUT OF MEMORY !");
                panic!();
            };
        }

        #[allow(unused_assignments)]
        let mut ptr_option = None;
        let mut pos = 0;
        loop
        {
            while self.allocation_map[pos] {
                pos += 1;
                if pos + len > MEMORY_SIZE{
                    out_of_mem_error!();
                }
            }
            let mut is_valid = true;
            let mut next_pos_found = false;
            for i in pos..(pos + len)
            {
                if self.allocation_map[i] {
                    is_valid = false;
                } else if !is_valid
                {
                    pos = i;
                    next_pos_found = true;
                    break;
                }
            }
            if is_valid {
                ptr_option = Some(Pointer { pos, len });
                break;
            }
            if !next_pos_found
            {
                pos += len;
            }
        }

        if let Some(ptr) = ptr_option
        {
            for i in pos..(pos + len) {
                self.allocation_map[i] = true;
            }
            ptr
        } 
        else {
            out_of_mem_error!();
        }
    }

    fn free (&mut self, ptr:Pointer) -> () //TODO shift everything to the right so there is always free memory on the left ?
    {
        for i in (ptr.pos)..(ptr.pos + ptr.len) {
            self.ram[i] = u8::default();
            self.allocation_map[i] = false;
        }
    }

    fn realloc (&mut self, var:&mut Variable, new_len:usize) -> ()
    {
        self.free(var.ptr);
        var.ptr = self.alloc(new_len);
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