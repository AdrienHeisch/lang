use crate::parser::Expr;
use crate::lexer::Const;
use std::collections::VecDeque;
use std::collections::HashMap;
use dynamic::Dynamic;
use std::any::TypeId;

macro_rules! copy {
    ($b:expr, $f:ident, $($param:expr),+) => {
        match $b.id()
        {
            t if t == TypeId::of::<i32>() => $f::<i32>($($param),+),
            t if t == TypeId::of::<f32>() => $f::<f32>($($param),+),
            _ => {
                eprintln!("Invalid type");
                panic!();
            }
        }
    };
}

macro_rules! dyn_box_to_string {
    ($bx:expr) => {
        match $bx.id()
        {
            t if t == TypeId::of::<i32>() => $bx.downcast_ref::<i32>().unwrap().to_string(),
            t if t == TypeId::of::<f32>() => $bx.downcast_ref::<f32>().unwrap().to_string(),
            _ => {
                eprintln!("Invalid type");
                panic!();
            }
        }
    };
}

const MEMORY_SIZE:usize = 8;

trait ValueType : Copy + 'static {}
impl ValueType for i32 {}
impl ValueType for f32 {}

#[derive(Debug)]
struct Memory
{
    ram:[Box<Dynamic>;MEMORY_SIZE], //TODO any value ?
    vars:HashMap<String, usize>
}

impl Memory
{
    fn new () -> Memory
    {
        Memory
        {
            ram: [Memory::def_value(), Memory::def_value(), Memory::def_value(), Memory::def_value(), Memory::def_value(), Memory::def_value(), Memory::def_value(), Memory::def_value()],
            vars: HashMap::new()
        }
    }

    fn def_value () -> Box<Dynamic> { Dynamic::new(0) }

    fn get_var (&self, id:&String) -> Box<Dynamic>
    {
        let bx = &self.ram[self.get_var_address(id)];
        match bx.id()
        {
            t if t == TypeId::of::<i32>() => Dynamic::new(*bx.downcast_ref::<i32>().unwrap()),
            t if t == TypeId::of::<f32>() => Dynamic::new(*bx.downcast_ref::<f32>().unwrap()),
            _ => {
                eprintln!("Invalid type");
                panic!();
            }
        }
    }

    fn set_var<T:ValueType> (&mut self, id:&String, value:T) -> ()
    {
        // *(&mut self.ram[self.get_var_address(id)]).as_mut().downcast_mut::<T>().unwrap() = value;
        self.ram[self.get_var_address(id)] = Dynamic::new(value);
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

    fn get_var_address (&self, id:&String) -> usize
    {
        if let Some(address) = self.vars.get(id) {
            *address
        } else {
            eprintln!("Invalid identifier : {}", id);
            panic!()
        }
    }

    #[allow(dead_code)]
    fn print_memory (&self) -> ()
    {
        // println!("vars:   {:?}", self.vars);
        let mut mem_str = String::default();
        for (id, _) in &self.vars
        {
            mem_str = format!("{}{} => {}, ", mem_str, id, dyn_box_to_string!(*(&self.ram[self.get_var_address(id)])));
        }
        println!("{}", mem_str);
    }
}

pub fn interpret (exprs:VecDeque<Expr>)
{
    let mut mem = Memory::new();
    
    for e in exprs
    {
        expr(&mut mem, &e);
    }

    mem.print_memory();
}

fn expr (mem:&mut Memory, e:&Expr) -> Box<Dynamic>
{
    let e = e.clone();
    match e
    {
        Expr::Const(cst) => {
            match cst
            {
                Const::Number(n) => Dynamic::new(n),
            }
        },
        Expr::Id(id) => mem.get_var(&id),
        Expr::Var(id, assign_expr) => {
            if mem.vars.contains_key(&id) 
            {
                eprintln!("There is already a variable called {}", id);
                panic!();
            }
            let address = mem.get_free_address();
            mem.vars.insert(id.clone(), address);
            let v = copy!(expr(mem, &*assign_expr), assign, mem, &Expr::Id(id), &*assign_expr);
            v
            /* match expr(mem, &*assign_expr).id()
            {
                t if t == TypeId::of::<i32>() => assign::<i32>(mem, &Expr::Id(id), &*assign_expr),
                t if t == TypeId::of::<f32>() => assign::<f32>(mem, &Expr::Id(id), &*assign_expr),
                _ => {
                    eprintln!("Invalid type");
                    panic!();
                }
            } */
        },
        Expr::BinOp(op, e1, e2) => operation(mem, &op, &*e1, &*e2),
        // Expr::EParent(e): expr(e),
        /* Expr::Invalid */e => {
            eprintln!("Invalid expression : {:?}", e);
            panic!();
        },
    }
}

fn operation (mem:&mut Memory, op:&str, e1:&Expr, e2:&Expr) -> Box<Dynamic> //TODO FIX OPERATIONS PRECEDENCE (* and +)
{
    let expr1 = expr(mem, e1);
    let expr2 = expr(mem, e2);
    match (expr1, expr2)
    {
        (expr1, expr2) if expr1.is::<f32>() && expr2.is::<f32>() => { //TODO match types instead of using guards (Dynamic.id)
            let e1_val = *expr1.downcast_ref::<f32>().unwrap();
            let e2_val = *expr2.downcast_ref::<f32>().unwrap();
            match &op[..]
            {
                "+" => Dynamic::new(e1_val + e2_val),
                "-" => Dynamic::new(e1_val - e2_val),
                "*" => Dynamic::new(e1_val * e2_val),
                "/" => Dynamic::new(e1_val / e2_val),
                "=" => assign::<f32>(mem, e1, e2),
                _ => {
                    let last_char = if let Some(c) = op.chars().nth(op.len() - 1) { c } else { ' ' };
                    if op.len() > 1 && last_char == '=' {
                        let value:f32 = *operation(mem, &op[0..op.len() - 1], e1, e2).downcast_ref().unwrap();
                        assign::<f32>(mem, e1, &Expr::Const(Const::Number(value)))
                    } else {
                        eprintln!("Invalid operator : {}", op);
                        panic!()
                    }
                }
            }
        },
        _ => {
            eprintln!("Invalid operation !! : {}", op);
            panic!()
        }
    }
}

fn assign<T:ValueType> (mem:&mut Memory, to:&Expr, from:&Expr) -> Box<Dynamic>
{
    let value = expr(mem, from);
    match to
    {
        Expr::Id(id) => {
            mem.set_var::<T>(id, *(&value).as_ref().downcast_ref::<T>().unwrap());
        },
        _ => {
            eprintln!("Can't assign {:?} to {:?}", from, to);
            panic!()
        }
    }
    value
}