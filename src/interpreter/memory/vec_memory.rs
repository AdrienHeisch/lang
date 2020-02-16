use super::Memory;
use crate::cst::Const;
use std::collections::HashMap;

#[derive(Debug)]
pub struct VecMemory
{
    vec:Vec<Const>,
    scopes:Vec<HashMap<String, usize>>
}

impl Memory for VecMemory
{

    fn new () -> Self
    {
        Self
        {
            vec: Vec::new(),
            scopes: Vec::new()
        }
    }

    fn get_var (&self, id:&str) -> Const
    {
        let index = if let Some(index) = self.get_var_from_ident(id) {
            index
        } else {
            eprintln!("Unknown identifier : {}", id);
            panic!()
        };

        self.vec[*index].clone()
    }

    fn set_var (&mut self, id:&str, value:&Const)
    {
        let index = if let Some(index) = self.get_var_from_ident(id) {
            *index
        } else {
            self.vec.push(value.clone());
            self.scopes.last_mut().unwrap().insert(String::from(id), self.vec.len() - 1);
            return;
        };

        self.vec[index] = value.clone();
    }
    
    fn open_scope (&mut self)
    {
        self.scopes.push(HashMap::new());
    }
    
    
    fn close_scope (&mut self) -> bool
    {
        let scope = if let Some(scope) = self.scopes.pop() {
            scope
        } else {
            eprintln!("There is no scope to close.");
            panic!();
        };

        //TODO shift all remaining pointers ?
        /* for (_, var) in scope
        {
            match var.t
            {
                VarType::Number => Const::Number(self.f32_table.remove(var.index)),
                VarType::Str => Const::Str(self.str_table.remove(var.index)),
                VarType::Bool => Const::Bool(self.bool_table.remove(var.index)),
            };
        } */

        self.scopes.is_empty()
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

impl VecMemory
{

    fn get_var_from_ident (&self, id:&str) -> Option<&usize>
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

}