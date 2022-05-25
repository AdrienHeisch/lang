#![allow(dead_code)]

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
use vm::{ Chunk, DebugInfo };

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

pub fn walk_ast(ast: &Ast) -> Result<(), String> {
    let mut interpreter = Interpreter::new();
    match interpreter.run(&ast.top_level) {
        Ok(()) => {
            if cfg!(lang_print_interp_locals) {
                interpreter.print_locals();
            }
            Ok(())
        }
        Err(error) => Err(format!(
            "{} -> {}",
            error.pos.get_full(&ast.source),
            error.msg
        )),
    }
}

pub fn compile_ast(ast: &Ast) -> Result<Chunk, String> {
    if let Ok((chunk, _)) = vm::compiler::compile(&ast.top_level) {
        Ok(chunk)
    } else {
        Err(format!("Error handling unimplemented."))
    }
}

pub fn run_bytecode(chunk: &Chunk) -> Result<(), String> {
    if let Ok(()) = vm::interpreter::interpret(chunk) {
        Ok(())
    } else {
        Err(format!("Error handling unimplemented."))
    }
}
