use crate::{invalid_type_error, dyn_box_to_string, clone_dynamic_box}; //MACROS
use super::Memory;
use dynamic::Dynamic;
use std::collections::HashMap;

const MEMORY_SIZE:usize = 8; //TODO remove this, use a Vec<Dynamic>

//array of pointers to dynamic values on heap
#[derive(Debug)]
pub struct DynamicMemory
{
    ram:[Box<Dynamic>;MEMORY_SIZE],
    vars:HashMap<String, usize>
}

impl Memory for DynamicMemory
{

    fn new () -> DynamicMemory
    {
        DynamicMemory
        {
            ram: [DynamicMemory::def_value(), DynamicMemory::def_value(), DynamicMemory::def_value(), DynamicMemory::def_value(), DynamicMemory::def_value(), DynamicMemory::def_value(), DynamicMemory::def_value(), DynamicMemory::def_value()],
            vars: HashMap::new()
        }
    }

    fn get_var (&self, id:&String) -> Box<Dynamic>
    {
        let address = if let Some(address) = self.vars.get(id) {
            *address
        } else {
            eprintln!("Invalid identifier : {}", id);
            panic!()
        };

        let bx = &self.ram[address];
        clone_dynamic_box!(bx)
    }

    fn set_var (&mut self, id:&String, value:&Box<Dynamic>) -> ()
    {
        let address = if let Some(_address) = self.vars.get(id) {
            *_address
        } else {
            let _address = self.get_free_address();
            self.vars.insert(id.clone(), _address);
            _address
        };
        self.ram[address] = clone_dynamic_box!(value);
    }

    #[allow(dead_code)]
    fn print_memory (&self) -> ()
    {
        // println!("vars:   {:?}", self.vars);
        let mut mem_str = String::default();
        for (id, _) in &self.vars
        {
            mem_str = format!("{}{} => {}, ", mem_str, id, dyn_box_to_string!(*(&self.get_var(id))));
        }
        println!("{}", mem_str);
    }
}

impl DynamicMemory
{
    fn def_value () -> Box<Dynamic> { Dynamic::new(0) }

    fn get_free_address (&self) -> usize
    {
        let mut addresses = Vec::with_capacity(self.vars.len());
        addresses.resize(addresses.capacity(), 0);
        
        let mut i = 0;

        for (_, v) in &self.vars {
            addresses[i] = *v;
            i += 1;
        }

        for index in 0..self.ram.len() {
            if !addresses.contains(&index) {
                return index;
            }
        }

        eprintln!("OUT OF MEMORY !");
        panic!();
    }
}