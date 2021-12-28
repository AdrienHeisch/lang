#![allow(dead_code)]

mod ast;
mod env;
mod interpreter;
mod value;
mod memory;
mod utils;
mod vm;

pub fn eval (program:&str)
{
    let (ast, ast_errors) = ast::Ast::from_str(program);

    for error in &ast_errors {
        println!("{} -> {}", error.pos.get_full(program), error.msg);
    }

    let mut interp = interpreter::Interpreter::new();
    if cfg!(lang_ignore_parse_errors) || ast_errors.is_empty() {
        if let Err(interp_error) = interp.interpret(ast.get_top_level()) {
            println!("{} -> {}", interp_error.pos.get_full(program), interp_error.msg);
        } else if cfg!(lang_interp_print_locals) {
            interp.print_locals();
        }
    }
}

pub fn compile (program:&str)
{
    let (ast, ast_errors) = ast::Ast::from_str(program);

    for error in &ast_errors {
        println!("{} -> {}", error.pos.get_full(program), error.msg);
    }

    if cfg!(lang_ignore_parse_errors) || ast_errors.is_empty() {
        let chunk = vm::compiler::compile(ast.get_top_level());
        print!("BYTECODE: ");
        for s in chunk.iter().map(|el| format!("{:04X}", el)) {
            print!("{} ", s);
        }
        println!();
        vm::interpreter::interpret(chunk);
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
        if interp.interpret(expr).is_err() {
            println!("Interp: couldn't proceed to benchmark due to runtime errors.");
            return;
        }

        let mut duration = Duration::new(0, 0);
        for _ in 0..ITERATIONS
        {
            let mut interp = interpreter::Interpreter::new();
            let now = Instant::now();
            
            #[allow(unused_must_use)]
            { interp.interpret(expr); }

            duration += now.elapsed();
        }
        println!("Interp: {}ms", duration.as_millis());
    }

}