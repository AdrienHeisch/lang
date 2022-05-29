//TODO PROFILING
//TODO use a logging framework
//TODO find a name and push to github ?
//TODO use Result instead of panic! on error
//TODO custom types ?
//TODO rework modules cyclic dependencies
fn main() -> Result<(), std::io::Error> {
    //TODO console arguments
    // let args:Vec<String> = std::env::args().collect();
    // let path = args[1];

    let program = std::fs::read_to_string("./code.lang")?;

    if let Err(error) = run_lang(&program) {
        println!("{}", error);
    }

    Ok(())
}

fn run_lang(program: &str) -> Result<(), String> {
    let ast = lang::build_ast(program)?;
    if cfg!(lang_use_vm) {
        let bytecode = lang::compile_ast(&ast)?;
        lang::run_bytecode(&bytecode)?;
    } else {
        lang::walk_ast(&ast)?;
    }
    Ok(())
}
