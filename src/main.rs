mod cst;
mod lexer;
mod parser;
mod interpreter;
mod utils;

// mod macros;

use std::time::{ Instant, Duration };

//TODO find a name and push to github ?
//TODO use Result instead of panic! on error
//TODO add tests
//TODO use enums instead of typeids OR use custom types
fn main ()
{
    let path = "./code.lang";
    let program:String;

    match std::fs::read_to_string(path.to_string())
    {
        Ok(p) => program = p,
        Err(_) => {
            eprintln!("Program not found at {}", path);
            panic!();
        }
    }

    #[cfg(benchmark)]
    measure_n_times(&program, 6_000_000);

    #[cfg(not(benchmark))]
    eval(&program);
}

fn eval (program:&str)
{
    let tokens = lexer::lex(program);
    let block = parser::parse(&tokens);
    interpreter::interpret(block);
}

#[allow(dead_code)]
fn measure_n_times (program:&str, n:usize)
{
    let mut durations = (Duration::new(0, 0), Duration::new(0, 0), Duration::new(0, 0));
    for _ in 0..n
    {
        let measure = measure_once(program);
        durations.0 += measure.0;
        durations.1 += measure.1;
        durations.2 += measure.2;
    }

    //empirical correction
    // durations.0 -= Duration::new(0, 50000000);
    // durations.1 -= Duration::new(0, 50000000);
    // durations.2 -= Duration::new(0, 50000000);

    println!("For {} iterations :", n);
    println!("Lexing :  {}ms", durations.0.as_millis());
    println!("Parsing : {}ms", durations.1.as_millis());
    println!("Interp :  {}ms", durations.2.as_millis());
    println!("Total :   {}ms", durations.0.as_millis() + durations.1.as_millis() + durations.2.as_millis());
}

#[allow(unused_variables)]
fn measure_once (program:&str) -> (Duration, Duration, Duration)
{
    let now = Instant::now();
    let tokens = lexer::lex(program);
    let lex_time = now.elapsed();
    
    let now = Instant::now();
    let exprs = parser::parse(&tokens);
    let parse_time = now.elapsed();

    let now = Instant::now();
    interpreter::interpret(exprs);
    let interp_time = now.elapsed();

    (lex_time, parse_time, interp_time)
}