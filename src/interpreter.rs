use crate::parser::Expr;
use crate::lexer::Const;
use std::collections::VecDeque;
use std::collections::HashMap;
use /* crate::interpreter:: */dynamic::Dynamic;

// mod dynamic;

// macro_rules! types_equal {
//     ($T:ident, $V:ident) => {
//         TypeId::of::<$T>() == TypeId::of::<$V>()
//     };
// }

const MEMORY_SIZE:usize = 8;

// type Value<'l> = &'l Dynamic;
trait ValueType : Copy + 'static {}
impl ValueType for i32 {}
impl ValueType for f32 {}

#[derive(Debug)]
struct Memory
{
    ram:[Box<Dynamic>;MEMORY_SIZE], //TODO any value ?
    vars:HashMap<String, usize>
}

impl<'l> Memory
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

    fn get_var<T:ValueType> (&self, id:String) -> Box<Dynamic>
    {
        Dynamic::new(*(&self.ram[self.get_var_address(id)]).as_ref().downcast_ref::<T>().unwrap())
    }

    fn set_var<T:ValueType> (&mut self, id:String, value:T) -> ()
    {
        *(&mut self.ram[self.get_var_address(id)]).as_mut().downcast_mut::<T>().unwrap() = value;
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
            panic!()
        }
    }

    fn print_memory (&self) -> ()
    {
        for (id, _) in &self.vars {
            let v = *(&self.ram[self.get_var_address(id.clone())]).as_ref().downcast_ref::<i32>().unwrap();
            println!("{} = {}", id, v);
        }
    }
}

pub fn interpret (exprs:VecDeque<Expr>)
{
    let mut mem = Memory::new();
    
    for e in exprs
    {
        expr(&mut mem, &e);
    }

    println!("vars:   {:?}", mem.vars);
    // println!("mem:    {:?}", mem.ram);
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
                Const::Int(i) =>        Dynamic::new(i),
                Const::Float(f) =>      Dynamic::new(f)
            }
        },
        Expr::Id(id) => mem.get_var::<i32>(id),
        Expr::Var(id, assign_expr) => {
            if mem.vars.contains_key(&id) 
            {
                eprintln!("There is already a variable called {}", id);
                panic!();
            }
            let address = mem.get_free_address();
            mem.vars.insert(id.clone(), address);
            // let v = match *e
            // {
            //     Expr::Invalid => panic!(),
            //     e => expr(mem, &e)
            // }
            use std::any::TypeId;
            #[allow(unused_variables)]
            match expr(mem, &*assign_expr).id()
            {
                t if t == TypeId::of::<i32>() => assign::<i32>(mem, &Expr::Id(id), &*assign_expr),
                t if t == TypeId::of::<f32>() => assign::<f32>(mem, &Expr::Id(id), &*assign_expr),
                _ => {
                    eprintln!("Invalid type");
                    panic!();
                }
            }
            
        },
        Expr::BinOp(op, e1, e2) => operation(mem, op, &*e1, &*e2),
        // Expr::EParent(e): expr(e),
        /* Expr::Invalid */e => {
            eprintln!("Invalid expression : {:?}", e);
            panic!();
        },
    }
}

fn operation (mem:&mut Memory, op:String, e1:&Expr, e2:&Expr) -> Box<Dynamic>
{
    let expr1 = expr(mem, e1);
    let expr2 = expr(mem, e2);
    match (expr1, expr2)
    {
        (expr1, expr2) if expr1.is::<i32>() && expr2.is::<i32>() => { //TODO match types instead of using guards (Dynamic.id)
            let e1_val = *expr1.downcast_ref::<i32>().unwrap();
            let e2_val = *expr2.downcast_ref::<i32>().unwrap();
            match &op[..]
            {
                "+" => Dynamic::new(e1_val + e2_val),
                "-" => Dynamic::new(e1_val - e2_val),
                "*" => Dynamic::new(e1_val * e2_val),
                "/" => Dynamic::new(e1_val / e2_val),
                // "=" => assign::<i32>(mem, e1, e2),
                _ => {
                    let last_char = if let Some(c) = op.chars().nth(op.len() - 1) { c } else { ' ' };
                    if op.len() > 1 && last_char == '=' {
                        let value:i32 = *operation(mem, op[0..op.len() - 1].to_string(), e1, e2).downcast_ref().unwrap();
                        assign::<i32>(mem, e1, &Expr::Const(Const::Int(value)))
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
            mem.set_var::<T>(id.clone(), *(&value).as_ref().downcast_ref::<T>().unwrap());
        },
        _ => {
            eprintln!("Can't assign {:?} to {:?}", from, to);
            panic!()
        }
    }
    value
}

// fn unwrap_value<T> (value:ValueAny) -> T where T:ValueType
// {
//     unsafe { *std::mem::transmute::<ValueAny, Value<T>>(value) }
//     // if let Some(i) = value.downcast_ref::<T>() {
//     //     *i
//     // } else {
//     //     panic!()
//     // }
// }