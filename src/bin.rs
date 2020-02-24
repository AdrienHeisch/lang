use lang::eval;

//TODO use a logging framework
//TODO find a name and push to github ?
//TODO use Result instead of panic! on error
//TODO use enums instead of typeids OR use custom types
fn main () -> Result<(), std::io::Error>
{
    // let args:Vec<String> = std::env::args().collect();
    // let path = args[1];
    eval_file("./code.lang")
}

fn eval_file (path:&str) -> Result<(), std::io::Error>
{
    // lang::benchmarks::benchmark_lexer();
    // lang::benchmarks::benchmark_parser();
    // lang::benchmarks::benchmark_interpreter();
    eval(&std::fs::read_to_string(path)?);
    Ok(())
}