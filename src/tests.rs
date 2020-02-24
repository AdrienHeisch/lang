use crate::ast::Ast;
use crate::ast::Const;
use crate::interpreter;
use crate::interpreter::memory::Memory;

#[test]
fn assign ()
{
    let program = "\
        var f = 4.5;\n\
        var b = true;\n\
        var s = \"hi\";\n\
    ";
    let ast = Ast::from_str(program);
    let mem = interpreter::interpret(ast.get_top_level());
    assert_eq!(mem.get_var("f"), Const::Number(4.5));
    assert_eq!(mem.get_var("b"), Const::Bool(true));
    assert_eq!(mem.get_var("s"), Const::Str(String::from("hi")));
}