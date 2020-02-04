use super::Memory;
use dynamic::Dynamic;
use std::{
    any::TypeId,
    collections::HashMap
};

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

// #region MACROS
macro_rules! invalid_type_error {
    ($id:expr) => {
        eprintln!("Invalid type id : {:?}", $id);
        eprintln!("Valid type ids would be : {:?}", vec!(TypeId::of::<i32>(), TypeId::of::<f32>()));
        panic!();
    };
}

/* macro_rules! clone_dynamic_box {
    ($bx:ident) => {
        match $bx.id()
        {
            t if t == TypeId::of::<i32>() => Dynamic::new(*$bx.downcast_ref::<i32>().unwrap()),
            t if t == TypeId::of::<f32>() => Dynamic::new(*$bx.downcast_ref::<f32>().unwrap()),
            t => {
                invalid_type_error!(t);
            }
        }
    };
}
 */
macro_rules! dyn_box_to_string {
    ($bx:expr) => {
        match $bx.id()
        {
            t if t == TypeId::of::<f32>() => $bx.downcast_ref::<f32>().unwrap().to_string(),
            t if t == TypeId::of::<String>() => $bx.downcast_ref::<String>().unwrap().clone(),
            t => {
                invalid_type_error!(t);
            }
        }
    };
}
// #endregion

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