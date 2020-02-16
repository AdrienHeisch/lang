use super::{ Memory, VarType };
use crate::cst::Const;
use std::collections::HashMap;

#[derive(Debug)]
pub struct TableMemory
{
    f32_table:Vec<f32>,
    str_table:Vec<String>,
    bool_table:Vec<bool>,
    scopes:Vec<HashMap<String, Variable>>
}

#[derive(Clone, Debug)]
struct Variable
{
    t: VarType,
    index:usize
}

impl Memory for TableMemory
{

    fn new () -> TableMemory
    {
        TableMemory
        {
            f32_table: Vec::new(),
            str_table: Vec::new(),
            bool_table: Vec::new(),
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
            VarType::Number => Const::Number(self.f32_table[var.index]),
            VarType::Str => Const::Str(self.str_table[var.index].clone()),
            VarType::Bool => Const::Bool(self.bool_table[var.index]),
        }
    }

    fn set_var (&mut self, id:&str, value:&Const)
    {
        let var = if let Some(var) = self.get_var_from_ident(id) {
            var.clone()
        } else {
                let var = match value
                {
                    Const::Number(f) => Variable {
                        t: VarType::Number,
                        index: {
                            self.f32_table.push(*f);
                            self.f32_table.len() - 1
                        }
                    },
                    Const::Str(s) => Variable {
                        t: VarType::Str,
                        index: {
                            self.str_table.push(s.clone());
                            self.str_table.len() - 1
                        }
                    },
                    Const::Bool(b) => Variable {
                        t: VarType::Bool,
                        index: {
                            self.bool_table.push(*b);
                            self.bool_table.len() - 1
                        }
                    },
                    Const::Void => panic!() //TODO ?
                };
                self.scopes.last_mut().unwrap().insert(String::from(id), var);
                return;
        };

        match (value, &var.t) {
            (Const::Number(f), VarType::Number) => self.f32_table[var.index] = *f,
            (Const::Str(s), VarType::Str) => self.str_table[var.index] = s.clone(),
            (Const::Bool(b), VarType::Bool) => self.bool_table[var.index] = *b,
            (_, _) => panic!() //TODO ?
        }
    }
    
    fn open_scope (&mut self)
    {
        self.scopes.push(HashMap::new());
    }
    
    
    fn close_scope (&mut self) -> bool
    {
        let _ = if let Some(scope) = self.scopes.pop() {
            scope
        } else {
            eprintln!("There is no scope to close.");
            panic!();
        };

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

impl TableMemory
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

}