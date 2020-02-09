use crate::cst::Const;
use super::Memory;
use std::{
    collections::HashMap
};

//IN BYTES
const MEMORY_SIZE:usize = 32;

//RESEARCH look at C stack heap implementations (stack growing from the end)
#[derive(Debug)]
pub struct StaticMemory
{
    ram:[u8; MEMORY_SIZE],
    allocation_map:[bool; MEMORY_SIZE],
    scopes:Vec<HashMap<String, Variable>>
}

#[derive(Clone, Debug)]
struct Variable
{
    t: VarType,
    ptr:Pointer
}

#[derive(Clone, Debug)]
enum VarType
{
    Number,
    Str,
    Bool,
    // Void
}

#[derive(Clone, Debug)]
struct Pointer
{
    pos:usize,
    len:usize
}

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

    fn get_var (&self, id:&str) -> Const
    {
        let var = if let Some(var) = self.get_var_from_ident(id) {
            var
        } else {
            eprintln!("Unknown identifier : {}", id);
            panic!()
        };

        match var.t
        {
            //☺
            VarType::Number => {
                Const::Number(f32::from_ne_bytes([self.ram[var.ptr.pos], self.ram[var.ptr.pos + 1], self.ram[var.ptr.pos + 2], self.ram[var.ptr.pos + 3]]))
            },
            VarType::Str => Const::Str(String::from_utf8(Vec::from(self.access(&var.ptr))).ok().unwrap()),
            VarType::Bool => Const::Bool(self.access(&var.ptr)[0] == 1),
            // VarType::Void => Const::Void,
            /* t => {
                eprintln!("Invalid type id: {:?}", t);
                panic!();
            } */
        }
    }

    fn set_var (&mut self, id:&str, value:&Const)
    {
        let mut var = if let Some(var) = self.get_var_from_ident(id) {
            var.clone()
        } else {
            let var = match value
            {
                Const::Number(_) => Variable {
                    t: VarType::Number,
                    ptr: self.alloc(std::mem::size_of::<f32>())
                },
                Const::Str(_) => Variable {
                    t: VarType::Str,
                    ptr: self.alloc(std::mem::size_of::<String>()) //TODO lazy allocation with alloc(0) ?
                },
                Const::Bool(_) => Variable {
                    t: VarType::Bool,
                    ptr: self.alloc(std::mem::size_of::<bool>())
                },
                Const::Void => panic!() //TODO ?
            };
            self.scopes.last_mut().unwrap().insert(id.to_string(), var.clone());
            var
        };
        
        match value
        {
            //☺
            Const::Number(f) => {
                self.access_mut(&var.ptr).copy_from_slice(&(*f).to_ne_bytes()[0..4])
            },
            Const::Str(s) => {
                let bytes = s.as_bytes();
                let len = bytes.len();
                if len != var.ptr.len {
                    self.realloc(&mut var, len);
                    self.scopes.last_mut().unwrap().insert(id.to_string(), var.clone());
                }
                self.access_mut(&var.ptr).copy_from_slice(bytes)
            },
            Const::Bool(b) => {
                self.access_mut(&var.ptr)[0] = if *b {
                    1u8
                } else {
                    0u8
                };
            },
            Const::Void => panic!() //TODO ?
        }
    }

    fn open_scope (&mut self)
    {
        self.scopes.push(HashMap::new());
    }

    fn close_scope (&mut self)
    {
        let scope = if let Some(scope) = self.scopes.pop() {
            scope
        } else {
            eprintln!("There is no scope to close.");
            panic!();
        };

        for (_, var) in scope
        {
            self.free(&var.ptr);
        }
    }

    #[allow(dead_code)]
    fn print_memory (&self)
    {
        // self.vars.iter().map(|(k, v)| format!("{} => {:?}", k, v.ptr)).for_each(|s| print!("{}, ", s));
        // println!();
        let mut mem_str = String::default();
        for scope in self.scopes.iter().rev()
        {
            for (id, address) in scope
            {
                mem_str = format!("{}{} => {:?} => {:?}\n", mem_str, id, address.ptr, self.get_var(id));
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

    fn get_var_from_ident (&self, id:&str) -> Option<&Variable>
    {
        let mut var_opt = None;
        for scope in self.scopes.iter().rev()
        {
            var_opt = scope.get(id);
            if var_opt.is_some() {
                break;
            }
        }
        var_opt
    }

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
            let init_pos = pos;
            for i in init_pos..(init_pos + len)
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

    fn free (&mut self, ptr:&Pointer) //RESEARCH shift everything to the right so there is always free memory on the left ?
    {
        for i in (ptr.pos)..(ptr.pos + ptr.len) {
            self.ram[i] = u8::default();
            self.allocation_map[i] = false;
        }
    }

    fn realloc (&mut self, var:&mut Variable, new_len:usize)
    {
        self.free(&var.ptr);
        var.ptr = self.alloc(new_len);
    }

    fn access (&self, ptr:&Pointer) -> &[u8]
    {
        &self.ram[ptr.pos..(ptr.pos + ptr.len)]
    }

    fn access_mut (&mut self, ptr:&Pointer) -> &mut [u8]
    {
        &mut self.ram[ptr.pos..(ptr.pos + ptr.len)]
    }

}