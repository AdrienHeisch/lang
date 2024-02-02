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

    match run_lang(&program) {
        Ok(val) => println!("\nProgram finished with exit code {}", val),
        Err(error) => println!("{}", error),
    }

    Ok(())
}

fn run_lang(program: &str) -> Result<i32, String> {
    let ast = lang::build_ast(program)?;
    if cfg!(lang_use_vm) {
        let (chunk, entrypoint, globals) = lang::compile_ast(&ast)?;
        if !cfg!(lang_use_vm_compile_only) {
            lang::run_bytecode(&chunk, entrypoint, globals)
        } else {
            Ok(0)
        }
    } else {
        lang::walk_ast(&ast)
    }
}
