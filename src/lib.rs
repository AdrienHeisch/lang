mod cst;
mod expr;
mod lexer;
mod interpreter;
mod op;
mod parser;
mod utils;

use typed_arena::Arena;

pub fn eval (program:&str)
{
    let tokens = lexer::lex(program);
    let expr_arena = Arena::new();
    let block = parser::parse(&expr_arena, &tokens);
    interpreter::interpret(block);
}

// #[cfg(test)]
pub mod test_exports
{
    pub use crate::lexer::lex;
    pub use crate::parser::parse;
    pub use crate::interpreter::interpret;
}

#[cfg(test)]
#[allow(dead_code)]
pub mod test
{
    use crate::{
        interpreter,
        lexer,
        parser
    };
    use std::time::{ Instant, Duration };
    use typed_arena::Arena;

    #[test]
    fn benchmark () -> Result<(), std::io::Error>
    {
        let n = 6_000_000;
        let durations = measure_n_times(&std::fs::read_to_string("./code.lang")?, n);
        
        std::fs::write("./benchmark.txt", format!("{}{}{}{}{}",
            format!("For {} iterations :\n", n),
            format!("Lexing :  {}ms\n", durations.0.as_millis()),
            format!("Parsing : {}ms\n", durations.1.as_millis()),
            format!("Interp :  {}ms\n", durations.2.as_millis()),
            format!("Total :   {}ms\n", durations.0.as_millis() + durations.1.as_millis() + durations.2.as_millis())
        ))?;

        Ok(())
    }

    fn measure_n_times (program:&str, n:usize) -> (Duration, Duration, Duration)
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

        durations
    }

    fn measure_once (program:&str) -> (Duration, Duration, Duration)
    {
        let now = Instant::now();
        let tokens = lexer::lex(program);
        let lex_time = now.elapsed();
        
        let expr_arena = Arena::new();
        let now = Instant::now();
        let block = parser::parse(&expr_arena, &tokens);
        let parse_time = now.elapsed();

        let now = Instant::now();
        interpreter::interpret(block);
        let interp_time = now.elapsed();

        (lex_time, parse_time, interp_time)
    }

}