use crate::langval::LangVal;
use crate::ast::Identifier;
use super::{ Memory, VarType };
use std::collections::HashMap;

//IN BYTES
const MEMORY_SIZE:usize = 32;

//RESEARCH look at C stack heap implementations (stack growing from the end)
#[derive(Debug)]
pub struct StaticMemory
{
    ram: [u8; MEMORY_SIZE],
    allocation_map: [bool; MEMORY_SIZE],
    scopes: Vec<HashMap<Identifier, Variable>>
}

#[derive(Clone, Debug)]
struct Variable
{
    t: VarType,
    ptr: Pointer
}

#[derive(Clone, Debug)]
struct Pointer
{
    pos:usize,
    len:usize
}

impl Memory for StaticMemory
{

    fn new () -> Self
    {
        Self
        {
            ram: [u8::default(); MEMORY_SIZE],
            allocation_map: [false; MEMORY_SIZE],
            scopes: vec!(HashMap::new())
        }
    }

    fn get_var (&self, id:&Identifier) -> LangVal
    {
        let (var, _) = if let Some(var) = self.get_var_from_ident(id) {
            var
        } else {
            eprintln!("Unknown identifier : {}", std::str::from_utf8(id).ok().unwrap());
            panic!()
        };

        match var.t
        {
            VarType::Number => {
                LangVal::Number(f32::from_ne_bytes([self.ram[var.ptr.pos], self.ram[var.ptr.pos + 1], self.ram[var.ptr.pos + 2], self.ram[var.ptr.pos + 3]]))
            },
            VarType::Str => LangVal::Str(String::from_utf8(Vec::from(self.access(&var.ptr))).ok().unwrap()),
            VarType::Bool => LangVal::Bool(self.access(&var.ptr)[0] == 1),
            // VarType::Void => LangVal::Void,
        }
    }

    fn set_var (&mut self, id:&Identifier, value:&LangVal)
    {
        let (mut var, scope_index) = if let Some((var, scope_index)) = self.get_var_from_ident(id) {
            (var.clone(), scope_index)
        } else {
            let var = match value
            {
                LangVal::Number(_) => Variable {
                    t: VarType::Number,
                    ptr: self.alloc(std::mem::size_of::<f32>())
                },
                LangVal::Str(s) => Variable {
                    t: VarType::Str,
                    ptr: self.alloc(s.len())
                },
                LangVal::Bool(_) => Variable {
                    t: VarType::Bool,
                    ptr: self.alloc(std::mem::size_of::<bool>())
                },
                LangVal::Void => panic!() //TODO ?
            };
            self.scopes.last_mut().unwrap().insert(*id, var.clone());
            (var, self.scopes.len() - 1)
        };
        
        match value
        {
            LangVal::Number(f) => {
                self.access_mut(&var.ptr).copy_from_slice(&(*f).to_ne_bytes()[0..4])
            },
            LangVal::Str(s) => {
                let bytes = s.as_bytes();
                let len = bytes.len();
                if len != var.ptr.len {
                    self.realloc(&mut var, len);
                    self.scopes[scope_index].insert(*id, var.clone());
                }
                self.access_mut(&var.ptr).copy_from_slice(bytes)
            },
            LangVal::Bool(b) => {
                self.access_mut(&var.ptr)[0] = if *b { 1u8 } else { 0u8 };
            },
            LangVal::Void => panic!() //TODO ?
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
                mem_str = format!("{}{:?} => {:?} => {:?}\n", mem_str, id, address.ptr, self.get_var(id));
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

    fn get_var_from_ident (&self, id:&Identifier) -> Option<(&Variable, usize)>
    {
        let mut var_opt = None;
        for (i, scope) in self.scopes.iter().enumerate().rev()
        {
            if let Some(var) = scope.get(id)
            {
                var_opt = Some((var, i));
                break;
            }
        }
        var_opt
    }

    fn alloc (&mut self, len:usize) -> Pointer
    {
        if len > MEMORY_SIZE {
            eprintln!("Tried to allocate more bytes than the memory can contain : {} / {}", len, MEMORY_SIZE);
            panic!();
        }

        let ptr;
        let mut pos = 0;
        loop
        {
            while self.allocation_map[pos] {
                if pos + len > MEMORY_SIZE - 1 {
                    eprintln!("----------");
                    eprintln!("OUT OF MEMORY !");
                    eprintln!("Tried to allocate {} bytes at index {}", len, pos);
                    eprintln!("----------");
                    self.print_memory();
                    eprintln!("----------");
                    panic!();
                }
                pos += 1;
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
                ptr = Pointer { pos, len };
                break;
            }
            if !next_pos_found
            {
                pos += len;
            }
        }

        for i in pos..(pos + len) {
            self.allocation_map[i] = true;
        }
        ptr
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