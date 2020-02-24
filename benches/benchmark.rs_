extern crate lang;

use criterion::{criterion_group, criterion_main, Criterion};

// pub fn criterion_benchmark(c: &mut Criterion) {
//     c.bench_function("fib 20", |b| b.iter(|| lang::test_exports::lex("a")));
// }

fn benchmark_lexer (c: &mut Criterion)
{
    let program = std::fs::read_to_string("./code.lang").unwrap();
    c.bench_function("lexer", |b| b.iter(|| lang::test_exports::lex(&program)));
}

fn benchmark_parser (c: &mut Criterion)
{
    let program = std::fs::read_to_string("./code.lang").unwrap();
    let tokens = lang::test_exports::lex(&program);
    let expr_arena = typed_arena::Arena::new();
    c.bench_function("parser", |b| b.iter(|| lang::test_exports::parse(&expr_arena, &tokens)));
}

fn benchmark_interpreter (c: &mut Criterion)
{
    let program = std::fs::read_to_string("./code.lang").unwrap();
    let tokens = lang::test_exports::lex(&program);
    let expr_arena = typed_arena::Arena::new();
    let block = lang::test_exports::parse(&expr_arena, &tokens);
    c.bench_function("interpreter", |b| b.iter(|| lang::test_exports::interpret(block)));
}

criterion_group!(benches, benchmark_lexer, benchmark_parser, benchmark_interpreter);
criterion_main!(benches);