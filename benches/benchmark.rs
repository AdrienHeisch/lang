#![allow(dead_code)]
extern crate lang;

use criterion::{criterion_group, criterion_main, Criterion};

fn build_ast(c: &mut Criterion) {
    let program = std::fs::read_to_string("./code.lang").unwrap();
    c.bench_function("build_ast", |b| {
        b.iter(|| lang::build_ast(&program).unwrap())
    });
}

fn walk_ast(c: &mut Criterion) {
    let program = std::fs::read_to_string("./code.lang").unwrap();
    let ast = lang::build_ast(&program).unwrap();
    c.bench_function("walk_ast", |b| b.iter(|| lang::walk_ast(&ast).unwrap()));
}

fn compile_bytecode(c: &mut Criterion) {
    let program = std::fs::read_to_string("./code.lang").unwrap();
    let ast = lang::build_ast(&program).unwrap();
    c.bench_function("compile_ast", |b| {
        b.iter(|| lang::compile_ast(&ast).unwrap())
    });
}

fn run_bytecode(c: &mut Criterion) {
    let program = std::fs::read_to_string("./code.lang").unwrap();
    let ast = lang::build_ast(&program).unwrap();
    let bytecode = lang::compile_ast(&ast).unwrap();
    c.bench_function("run_bytecode", |b| {
        b.iter(|| lang::run_bytecode(&bytecode).unwrap())
    });
}

criterion_group!(all, /* build_ast, */walk_ast, /*compile_bytecode,  */run_bytecode);
criterion_main!(all);
