mod lexer;
mod parser;
mod interpreter;

use std::time::{ Instant, Duration };

//TODO add tests
//TODO use enums instead of typeids ?
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

    // #[cfg(features = "benchmark")]
    // measure_n_times(&program, 1000000);

    // #[cfg(not(features = "benchmark"))]
    eval(&program);
}

fn eval (program:&str)
{
    let tokens = lexer::lex(program);
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