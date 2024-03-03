#[cfg(test)]
mod tests;

mod ast;
mod env;
mod interpreter;
mod memory;
mod utils;
mod value;
mod vm;

use ast::Ast;
use interpreter::Interpreter;
use value::Value;
use vm::{Address, Chunk, Instruction};

//TODO multiple files
//TODO linker
//TODO main() function parameters
//TODO use real C compiler tests
//TODO hunt unwrap / clone / panic!

pub fn build_ast<'a>(program: &str) -> Result<Ast<'a>, String> {
    match Ast::from_str(program) {
        Ok(ast) => Ok(ast),
        Err(errors) => {
            let mut str = String::new();
            errors.iter().for_each(|error| {
                str += &format!("{} -> {}\n", error.pos.get_full(program), error.msg)
            });
            Err(str)
        }
    }
}

pub fn walk_ast(ast: &Ast) -> Result<i32, String> {
    let mut interpreter = Interpreter::new();
    let result = interpreter.run(&ast.top_level);

    if cfg!(not(lang_benchmark)) {
        println!("Program stdout :");
        println!("{}", interpreter.stdout);
    }

    match result {
        Ok(Value::Int(i)) => Ok(i),
        Ok(value) => Err(format!(
            "Invalid return code : {}",
            value
        )),
        Err(error) => Err(format!(
            "{} -> {}",
            error.pos.get_full(&ast.source),
            error.msg
        )),
    }
}

pub fn compile_ast(ast: &Ast) -> Result<(Chunk, Address, Vec<Instruction>), String> {
    match vm::compiler::compile(&ast.top_level) {
        Ok((chunk, Some(entrypoint), globals, _)) => Ok((chunk, entrypoint, globals)),
        Ok((_, None, _, _)) => Err("No entrypoint.".to_string()),
        Err(error) => Err(format!(
            "{} -> {}",
            error.pos.get_full(&ast.source),
            error.msg
        )),
    }
}

pub fn run_bytecode(
    chunk: &Chunk,
    entrypoint: Address,
    globals: Vec<Instruction>,
) -> Result<i32, String> {
    match vm::interpreter::interpret(chunk, entrypoint, globals) {
        Ok(i) => Ok(i),
        Err(_) => Err("Error handling unimplemented.".to_string()),
    }
}
