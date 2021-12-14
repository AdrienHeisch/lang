//TODO PROFILING
//TODO use a logging framework
//TODO find a name and push to github ?
//TODO use Result instead of panic! on error
//TODO custom types ?
//TODO rework modules cyclic dependencies
fn main () -> Result<(), std::io::Error>
{
    //TODO console arguments
    // let args:Vec<String> = std::env::args().collect();
    // let path = args[1];
    
    if cfg!(lang_benchmark)
    {
        lang::benchmarks::benchmark_lexer();
        lang::benchmarks::benchmark_parser();
        lang::benchmarks::benchmark_interpreter();
    } else {
        eval_file("./code.lang")?;
    }

    Ok(())
}

fn eval_file (path:&str) -> Result<(), std::io::Error>
{
    lang::eval(&std::fs::read_to_string(path)?);
    // lang::compile(&std::fs::read_to_string(path)?);
    Ok(())
}