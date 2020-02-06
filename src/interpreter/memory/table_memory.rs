use crate::{invalid_type_error, dyn_box_to_string}; //MACROS
use super::Memory;
use dynamic::Dynamic;
use std::{
    any::TypeId,
    collections::HashMap
};

//DEPRECATED FOR NOW
//TODO update

#[derive(Debug)]
pub struct TableMemory
{
    f32_table:Vec<f32>,
    string_table:Vec<String>,
    vars:HashMap<String, Address>
}

#[derive(Debug)]
struct Address
{
    t: TypeId,
    index:usize
}

impl Memory for TableMemory
{

    fn new () -> TableMemory
    {
        TableMemory
        {
            f32_table: Vec::new(),
            string_table: Vec::new(),
            vars: HashMap::new()
        }
    }

    fn get_var (&self, id:&String) -> Box<Dynamic>
    {
        let address = if let Some(address) = self.vars.get(id) {
            address
        } else {
            eprintln!("Invalid identifier : {}", id);
            panic!()
        };

        match address.t
        {
            //☺
            t if t == TypeId::of::<f32>() =>    Dynamic::new(self.f32_table[address.index]),
            t if t == TypeId::of::<String>() => Dynamic::new(self.string_table[address.index].clone()),
            t => {
                invalid_type_error!(t);
            }
        }
    }

    fn set_var (&mut self, id:&String, value:&Box<Dynamic>) -> ()
    {
        if let Some(address) = self.vars.get(id) {
            if address.t != value.id() {
                eprintln!("Wrong type"); //TODO better error
                panic!();
            }
            match address.t
            {
                //☺
                t if t == TypeId::of::<f32>() =>    self.f32_table[address.index] = *value.downcast_ref::<f32>().unwrap(),
                t if t == TypeId::of::<String>() => self.string_table[address.index] = value.downcast_ref::<String>().unwrap().clone(),
                t => {
                    invalid_type_error!(t);
                }
            };
        } else {
            self.vars.insert(id.clone(), Address
            {
                t: value.id(),
                index: match value.id()
                {
                    //☺
                    t if t == TypeId::of::<f32>() => {
                        self.f32_table.push(*value.downcast_ref::<f32>().unwrap());
                        self.f32_table.len() - 1
                    },
                    t if t == TypeId::of::<String>() => {
                        self.string_table.push(value.downcast_ref::<String>().unwrap().clone());
                        self.string_table.len() - 1
                    },
                    t => {
                        invalid_type_error!(t);
                    }
                }
            });
        }
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