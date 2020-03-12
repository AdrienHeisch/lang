mod ast;
mod env;
mod interpreter;
mod langval;
mod memory;
mod utils;
// mod vm;

pub fn eval (program:&str) -> Result<(), ()>
{
    let mut interp = interpreter::Interpreter::new();
    let (ast, parse_errors) = ast::Ast::from_str(program);

    let error_free = parse_errors.is_empty();
    
    if !error_free {
        for error in parse_errors {
            println!("{}", error);
        }
    }

    if cfg!(lang_ignore_parse_errors) || error_free {
        if let Err(error) = interp.interpret(&ast.get_top_level()) {
            println!("{}", error);
        } else {
            interp.print_locals();
        }
        Ok(())
    } else {
        Err(())
    }
}

#[allow(dead_code)]
pub mod benchmarks
{
    use crate::{ ast, interpreter };
    use std::time::{ Instant, Duration };

    pub const ITERATIONS:u32 = 1_000_000;

    pub fn benchmark_lexer  () { crate::ast::benchmarks::benchmark_lexer()  }
    pub fn benchmark_parser () { crate::ast::benchmarks::benchmark_parser() }

    pub fn benchmark_interpreter () //TODO move to interpreter ?
    {
        let program = std::fs::read_to_string("./code.lang").unwrap();
        let (ast, errors) = ast::Ast::from_str(&program);

        if !errors.is_empty() {
            println!("Interp: couldn't proceed to benchmark due to parsing errors.");
            return;
        }

        let expr = ast.get_top_level();
        let mut interp = interpreter::Interpreter::new();
        if interp.interpret(&expr).is_err() {
            println!("Interp: couldn't proceed to benchmark due to runtime errors.");
            return;
        }

        let mut duration = Duration::new(0, 0);
        for _ in 0..ITERATIONS
        {
            let mut interp = interpreter::Interpreter::new();
            let now = Instant::now();
            
            #[allow(unused_must_use)]
            { interp.interpret(&expr); }

            duration += now.elapsed();
        }
        println!("Interp: {}ms", duration.as_millis());
    }

}