mod lexer;
mod parser;
mod interpreter;
use std::time::{Instant, Duration};

fn main () //TODO make lexer, parser and interpreter objects with state
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

    // measure_n_times(program.as_ref(), 1000000);

    let tokens = lexer::lex(program.as_str()); 
    let exprs = parser::parse(&tokens);
    interpreter::interpret(exprs);
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
    durations.0 -= Duration::new(0, 50000000);
    durations.1 -= Duration::new(0, 50000000);
    durations.2 -= Duration::new(0, 50000000);

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
    let tokens = lexer::lex(program.as_ref());
    let lex_time = now.elapsed();
    
    let now = Instant::now();
    let exprs = parser::parse(&tokens);
    let parse_time = now.elapsed();

    let now = Instant::now();
    interpreter::interpret(exprs);
    let interp_time = now.elapsed();

    (lex_time, parse_time, interp_time)
}