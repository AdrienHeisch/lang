use crate::parser::Expr;
use crate::lexer::Const;
use std::collections::VecDeque;
use std::collections::HashMap;

const MEMORY_SIZE:usize = 32;

#[derive(Debug)]
struct Memory
{
    ram:[i32;MEMORY_SIZE], //TODO any value ?
    vars:HashMap<String, usize>
}

pub fn interpret (exprs:VecDeque<Expr>)
{
    let mut mem = Memory::new();
    
    for e in exprs
    {
        expr(&mut mem, &e);
    }

    println!("{:?}", mem.vars);
    println!("{:?}", mem.ram);
}

fn expr (mem:&mut Memory, e:&Expr) -> i32
{
    let e = e.clone();
    match e
    {
        Expr::Const(cst) => {
            match cst
            {
                Const::Int(i) => i,
                // _ => 0
            }
        },
        Expr::Id(id) => mem.ram[mem.get_var_address(id)] ,
        Expr::Var(id, e) => {
            // if (variables.exists(id)) throw 'There is already a variable called $id.';
            let address = mem.get_free_address();
            mem.vars.insert(id, address);
            let v = match *e
            {
                Expr::Invalid => 0,
                e => expr(mem, &e)
            };
            mem.ram[address] = v;
            v
        },
        Expr::BinOp(op, e1, e2) => operation(mem, op, &*e1, &*e2),
        // Expr::EParent(e): expr(e),
        Expr::Invalid => {
            eprintln!("Invalid expression.");
            0
        }
    }
}

fn operation (mem:&mut Memory, op:String, e1:&Expr, e2:&Expr) -> i32
{
    match &op[..]
    {
        "+" => expr(mem, e1) + expr(mem, e2),
        "-" => expr(mem, e1) - expr(mem, e2),
        "*" => expr(mem, e1) * expr(mem, e2),
        "/" => expr(mem, e1) / expr(mem, e2),
        "=" => assign(mem, e1, e2),
        _ => {
            let last_char = if let Some(c) = op.chars().nth(op.len() - 1) { c } else { ' ' };
            if op.len() > 1 && last_char == '=' {
                let value = operation(mem, op[0..op.len() - 1].to_string(), e1, e2);
                assign(mem, e1, &Expr::Const(Const::Int(value)))
            } else {
                eprintln!("Invalid operator : {}", op);
                0
            }
        }
    }
}

fn assign (mem:&mut Memory, to:&Expr, from:&Expr) -> i32
{
    let value = expr(mem, from);
    match to
    {
        Expr::Id(id) => mem.ram[mem.get_var_address(id.clone())] = value,
        _ => eprintln!("Can't assign {} to {:?}", value, to)
    }
    value
}

impl Memory
{
    fn new () -> Memory
    {
        Memory
        {
            ram: [0; MEMORY_SIZE],
            vars: HashMap::new()
        }
    }
    
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

    fn get_var_address (&self, id:String) -> usize
    {
        if let Some(address) = self.vars.get(&id) {
            *address
        } else {
            eprintln!("Invalid identifier : {}", id);
            0_usize
        }
    }
}