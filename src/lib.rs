mod ast;
mod interpreter;
mod utils;

pub fn eval (program:&str)
{
    let ast = ast::Ast::from_str(program);
    interpreter::interpret(ast.get_top_level());
}

pub const BENCHMARK_ITERATIONS:u32 = 6_000_000;

/* // #[cfg(test)]
pub mod test_exports
{
    pub use crate::lexer::lex;
    pub use crate::parser::parse;
    pub use crate::interpreter::interpret;
} */

#[allow(dead_code)]
pub mod benchmarks
{
    use crate::{ ast, interpreter };
    use crate::BENCHMARK_ITERATIONS as ITERATIONS;
    use std::time::{ Instant };

    /* pub fn benchmark () -> Result<(), std::io::Error>
    {
        let durations = measure_n_times(&std::fs::read_to_string("./code.lang")?, ITERATIONS);
        
        let text = format!("{}{}{}{}{}",
            format!("For {} iterations :\n", ITERATIONS),
            format!("Lexing :  {}ms\n", durations.0.as_millis()),
            format!("Parsing : {}ms\n", durations.1.as_millis()),
            format!("Interp :  {}ms\n", durations.2.as_millis()),
            format!("Total :   {}ms\n", durations.0.as_millis() + durations.1.as_millis() + durations.2.as_millis())
        );

        std::fs::write("./benchmark.txt", text)?;

        Ok(())
    }

    pub fn measure_n_times (program:&str, n:u32) -> (Duration, Duration, Duration)
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
        let block = parser::parse(&expr_arena, tokens);
        let parse_time = now.elapsed();

        let now = Instant::now();
        interpreter::interpret(block);
        let interp_time = now.elapsed();

        (lex_time, parse_time, interp_time)
    } */

    pub fn benchmark_lexer  () { crate::ast::benchmarks::benchmark_lexer()  }
    pub fn benchmark_parser () { crate::ast::benchmarks::benchmark_parser() }

    pub fn benchmark_interpreter ()
    {
        let ast = ast::Ast::from_str(&std::fs::read_to_string("./code.lang").unwrap());
        let expr = ast.get_top_level();
        let now = Instant::now();
        for _ in 0..ITERATIONS { interpreter::interpret(expr); }
        println!("Interp: {}ms", now.elapsed().as_millis());
    }

}