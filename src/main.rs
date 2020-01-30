mod lexer;
mod parser;
mod interpreter;

fn main ()
{
    let tokens = lexer::lex(&String::from("./code.lang"));
    let exprs = parser::parse(&tokens);
    interpreter::interpret(exprs);
}