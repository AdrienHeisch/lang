use super::Memory;
use crate::cst::Const;
use std::collections::HashMap;

#[derive(Debug)]
pub struct HashMapMemory
{
    scopes:Vec<Scope>
}

type Scope = HashMap<String, Const>;

impl Memory for HashMapMemory
{

    fn new () -> Self
    {
        Self
        {
            scopes: Vec::new()
        }
    }

    fn get_var (&self, id:&str) -> Const
    {
        for scope in self.scopes.iter().rev()
        {
            if let Some(value) = scope.get(id) {
                return value.clone();
            }
        }

        eprintln!("Unknown identifier : {}", id);
        panic!();
    }

    fn set_var (&mut self, id:&str, value:&Const)
    {
        for scope in self.scopes.iter_mut().rev()
        {
            if scope.get(id).is_some() {
                scope.insert(String::from(id), value.clone());
                return;
            }
        }

        self.scopes.last_mut().unwrap().insert(String::from(id), value.clone());
    }
    
    fn open_scope (&mut self)
    {
        self.scopes.push(HashMap::new());
    }
    
    fn close_scope (&mut self)
    {
        if self.scopes.pop().is_none() {
            eprintln!("There is no scope to close.");
            panic!();
        };
    }

    #[allow(dead_code)]
    fn print_memory (&self)
    {
        // println!("vars:   {:?}", self.vars);
        let mut mem_str = String::default();
        for scope in self.scopes.iter().rev() {
            for id in scope.keys() {
                mem_str = format!("{}{} => {}, ", mem_str, id, self.get_var(id));
            }
        }
        println!("{}", mem_str);
    }
}