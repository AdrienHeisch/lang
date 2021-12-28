#![allow(dead_code)]

mod ast;
mod env;
mod interpreter;
mod value;
mod memory;
mod utils;
mod vm;

use ast::Ast;
use interpreter::Interpreter;
use vm::Chunk;

pub fn build_ast<'a> (program: &str) -> Result<Ast<'a>, String> {
    match Ast::from_str(program) {
        Ok(ast) => Ok(ast),
        Err(errors) => {
            let mut str = String::new();
            errors.iter().for_each(|error| str += &format!("{} -> {}\n", error.pos.get_full(program), error.msg));
            Err(str)
        }
    }
}

pub fn walk_ast (ast: &Ast) -> Result<(), String> {
    let mut interp = Interpreter::new();
    match interp.interpret(&ast.top_level) {
        Ok(_) => {
            if cfg!(lang_print_interp_locals) {
                interp.print_locals();
            }
            Ok(())
        }
        Err(error) => Err(format!("{} -> {}", error.pos.get_full(&ast.source), error.msg))
    }
}

pub fn compile_bytecode (ast: &Ast) -> Result<Chunk, String> {
    match vm::compiler::compile(&ast.top_level) {
        Ok(chunk) => Ok(chunk),
        Err(_) => Err(format!("Error handling unimplemented."))
    }
}

pub fn run_bytecode (chunk: &Chunk) -> Result<(), String> {
    match vm::interpreter::interpret(chunk) {
        Ok(_) => Ok(()),
        Err(_) => Err(format!("Error handling unimplemented."))
    }
}