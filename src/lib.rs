mod ast;
mod env;
mod interpreter;
mod langval;
mod memory;
mod utils;
// mod vm;

pub fn eval (program:&str)
{
    let mut interp = interpreter::Interpreter::new();
    let ast = ast::Ast::from_str(program);
    interp.interpret(&ast.get_top_level());
    // use interpreter::memory::Memory;
    interp.print_locals();
}

#[allow(dead_code)]
pub mod benchmarks
{
    use crate::{ ast, interpreter };
    use std::time::{ Instant, Duration };

    pub const ITERATIONS:u32 = 6_000_000;

    pub fn benchmark_lexer  () { crate::ast::benchmarks::benchmark_lexer()  }
    pub fn benchmark_parser () { crate::ast::benchmarks::benchmark_parser() }

    pub fn benchmark_interpreter ()
    {
        let ast = ast::Ast::from_str(&std::fs::read_to_string("./code.lang").unwrap());
        let expr = ast.get_top_level();
        let mut duration = Duration::new(0, 0);
        for _ in 0..ITERATIONS
        {
            let mut interp = interpreter::Interpreter::new();
            let now = Instant::now();
            interp.interpret(&expr);
            duration += now.elapsed();
        }
        println!("Interp: {}ms", duration.as_millis());
    }

}