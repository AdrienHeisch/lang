macro_rules! test_assert_eq {
    ($name:ident, $program:literal, $id:literal, $value:expr) => {
        #[test]
        fn $name() {
            use crate::interpreter::Interpreter;
            #[allow(unused_imports)]
            use crate::value::{Type, Value};

            let ast = match crate::build_ast(&format!("int main () {{ {} return 0; }}", $program)) {
                Ok(ast) => ast,
                Err(error) => panic!("PARSER ERROR : {}", error),
            };

            if cfg!(lang_test_interpreter) {
                let mut interpreter = Interpreter::new();
                match interpreter.run(&ast.top_level) {
                    Ok(_) => {
                        assert_eq!(interpreter.get_var_by_name($id).unwrap(), $value)
                    }
                    Err(error) => panic!(
                        "INTERPRETER ERROR : {} -> {}",
                        error.pos.get_full(&ast.source),
                        error.msg
                    ),
                }
            }

            if cfg!(lang_test_vm_interpreter) || cfg!(lang_test_vm_compiler) {
                let bytecode = match crate::compile_ast(&ast) {
                    Ok(bytecode) => bytecode,
                    Err(error) => panic!("COMPILER ERROR : {}", error),
                };

                if cfg!(lang_test_vm_interpreter) {
                    match crate::run_bytecode(&bytecode) {
                        Ok(_) => {
                            todo!() // assert_eq!(interpreter.get_var_by_name($id).unwrap(), $value)
                        }
                        Err(error) => panic!("VM INTERPRETER ERROR : {}", error),
                    }
                }
            }
        }
    };
}

macro_rules! test_should_panic {
    ($name:ident, $program:literal) => {
        #[test]
        fn $name() {
            use crate::interpreter::Interpreter;

            let ast = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                crate::build_ast(&format!("int main () {{ {} return 0; }}", $program))
            })) {
                Ok(result) => match result {
                    Ok(ast) => {
                        if cfg!(lang_test_parser_only) {
                            panic!("Test code didn't panic.")
                        } else {
                            ast
                        }
                    }
                    Err(error) => {
                        println!("Parser error as expected : {}", error);
                        return;
                    }
                },
                Err(error) => {
                    println!("Parser panic as expected : {:?}", error);
                    return;
                }
            };

            if cfg!(lang_test_interpreter) {
                let mut interpreter = Interpreter::new();
                match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    interpreter.run(&ast.top_level)
                })) {
                    Ok(result) => match result {
                        Ok(_) => panic!("Test code didn't panic."),
                        Err(error) => println!("Interpreter error as expected : {}", error),
                    },
                    Err(_) => println!("Interpreter panic as expected."),
                }
            }

            if cfg!(lang_test_vm_interpreter) || cfg!(lang_test_vm_compiler) {
                let bytecode = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    crate::compile_ast(&ast)
                })) {
                    Ok(result) => match result {
                        Ok(bytecode) => bytecode,
                        Err(error) => {
                            println!("Compiler error as expected : {}", error);
                            return;
                        }
                    },
                    Err(error) => {
                        println!("Compiler panic as expected : {:?}", error);
                        return;
                    }
                };

                if cfg!(lang_test_vm_interpreter) {
                    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        crate::run_bytecode(&bytecode)
                    })) {
                        Ok(result) => match result {
                            Ok(_) => panic!("Test code didn't panic."),
                            Err(error) => println!("VM interpreter error as expected : {}", error),
                        },
                        Err(_) => println!("VM interpreter panic as expected."),
                    }
                }
            }
        }
    };
}

test_should_panic!(assign_void, "int i = void;");

test_should_panic!(assign_wrong_type, "int i = true;");

test_assert_eq!(assign_true, "bool b = true;", "b", Value::Bool(true));

test_assert_eq!(assign_false, "bool b = false;", "b", Value::Bool(false));

test_assert_eq!(
    reassign,
    "bool b = false; b = true;",
    "b",
    Value::Bool(true)
);

test_should_panic!(reassign_wrong_type, "bool b = false; b = 1;");

test_assert_eq!(
    assign_defined,
    "bool b = true; bool c = b;",
    "c",
    Value::Bool(true)
);

test_should_panic!(assign_undefined, "bool b = c;");

test_assert_eq!(assign_int, "int i = 6;", "i", Value::Int(6));

test_assert_eq!(assign_int_neg, "int i = -6;", "i", Value::Int(-6));

test_assert_eq!(add_int, "int i = 2 + 4;", "i", Value::Int(6));

test_assert_eq!(sub_int, "int i = 11 - 5;", "i", Value::Int(6));

test_assert_eq!(mul_int, "int i = 3 * 2;", "i", Value::Int(6));

test_assert_eq!(div_int, "int i = 12 / 2;", "i", Value::Int(6));

test_assert_eq!(mod_int, "int i = 42 % 9;", "i", Value::Int(6));

test_assert_eq!(assign_float, "float f = 4.5;", "f", Value::Float(4.5));

test_assert_eq!(assign_float_neg, "float f = -4.5;", "f", Value::Float(-4.5));

test_assert_eq!(add_float, "float f = 2.7 + 1.8;", "f", Value::Float(4.5));

test_assert_eq!(sub_float, "float f = 8.2 - 3.7;", "f", Value::Float(4.5));

test_assert_eq!(mul_float, "float f = 9.0 * 0.5;", "f", Value::Float(4.5));

test_assert_eq!(div_float, "float f = 11.25 / 2.5;", "f", Value::Float(4.5));

test_assert_eq!(mod_float, "float f = 40.5 % 9.0;", "f", Value::Float(4.5));

test_assert_eq!(unop_precedence, "int i = -1 + 5;", "i", Value::Int(4));

test_assert_eq!(parenthesis, "bool b = (true);", "b", Value::Bool(true));

test_assert_eq!(
    condition,
    "bool b = false; if true { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    not,
    "bool b = false; if !false { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    and_0,
    "bool b = true; if false && false { b = false; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    and_1,
    "bool b = true; if false && true { b = false; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    and_2,
    "bool b = false; if true && true { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    or_0,
    "bool b = true; if false || false { b = false; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    or_1,
    "bool b = true; if false || true { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    or_2,
    "bool b = true; if true || true { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    eq_0,
    "bool b = false; if 0 == 0 { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    eq_1,
    "bool b = true; if 0 == 1 { b = false; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    gt_0,
    "bool b = false; int c = 1; if c > 0 { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    gt_1,
    "bool b = true; int c = 0; if c > 0 { b = false; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    gt_2,
    "bool b = true; int c = 0; if c > 1 { b = false; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    gte_0,
    "bool b = false; int c = 1; if c >= 0 { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    gte_1,
    "bool b = false; int c = 0; if c >= 0 { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    gte_2,
    "bool b = true; int c = 0; if c >= 1 { b = false; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    lt_0,
    "bool b = true; int c = 1; if c < 0 { b = false; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    lt_1,
    "bool b = true; int c = 0; if c < 0 { b = false; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    lt_2,
    "bool b = false; int c = 0; if c < 1 { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    lte_0,
    "bool b = true; int c = 1; if c <= 0 { b = false; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    lte_1,
    "bool b = false; int c = 0; if c <= 0 { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    lte_2,
    "bool b = false; int c = 0; if c <= 1 { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    scope_0,
    "bool b = true; { int c = 0; }",
    "b",
    Value::Bool(true)
);

test_should_panic!(scope_1, "{ int a = 0; } int b = a;");

test_should_panic!(block_value, "int a = { 5; };");

test_should_panic!(assign_expr_value, "int a = { int b = 1; };");

test_assert_eq!(
    loop_,
    "int i = 1; int c = 0; while c < 5 { i = i + 2; c = c + 1; }",
    "i",
    Value::Int(11)
);

test_should_panic!(type_as_identifier, "int int = 1;");

test_assert_eq!(
    pointer_create,
    "int i = 6; int j = 7; int *p = &j;",
    "p",
    Value::Pointer(4, Box::new(Type::Int))
);

test_assert_eq!(
    pointer_deref,
    "int i = 6; int j = 7; int *p = &j; int k = *p;",
    "k",
    Value::Int(7)
);

test_assert_eq!(
    pointer_assign,
    "int i = 6; int j = 7; int *p = &j; *p = 9; int k = *p;",
    "k",
    Value::Int(9)
);

test_assert_eq!(
    pointer_double_create,
    "int i = 6; int j = 7; int *p = &j; int **pp = &p;",
    "pp",
    Value::Pointer(8, Box::new(Type::Pointer(Box::new(Type::Int))))
);

test_assert_eq!(
    pointer_double_deref,
    "int i = 6; int j = 7; int *p = &j; int **pp = &p; int k = **pp;",
    "k",
    Value::Int(7)
);

//TODO fixed by disabling parser errors (works in interpreter)
test_assert_eq!(
    pointer_double_assign,
    "int i = 6; int j = 7; int *p = &j; int **pp = &p; **pp = 9; int k = **pp;",
    "k",
    Value::Int(9)
);

test_assert_eq!(
    pointer_double_assign_ptr,
    "int i = 6; int j = 7; int *q = &i; int **pp = &q; int *p = &j; pp = &p; int k = **pp;",
    "k",
    Value::Int(7)
);

test_assert_eq!(
    function,
    "int add(int a, int b) { return a + b; } int i = add(2, 4);",
    "i",
    Value::Int(6)
);

test_assert_eq!(
    function_header,
    "int add(int a, int b); int i = add(2, 4); int add(int a, int b) { return a + b; }",
    "i",
    Value::Int(6)
);

test_should_panic!(
    function_invalid_arg,
    "int add(int a, int b) { a + b; } int i = add(true, 4);"
);

test_should_panic!(
    function_invalid_arg_number,
    "int add(int a, int b) { a + b; } int i = add(2, 4, 6);"
);

test_should_panic!(
    function_invalid_type,
    "int add(int a, int b) { a + b; } bool i = add(2, 4);"
);

test_should_panic!(
    function_header_mismatch,
    "int add(int a, int b); int i = add(2, 4); int add(int a, int b, int c) { return a + b; }"
);

test_assert_eq!(
    array,
    "int a[3] = {12, 53, 84}; int k = a[5 - 4] + 7;",
    "k",
    Value::Int(60)
);

test_assert_eq!(array_uninit, "int a[3]; int k = a[1];", "k", Value::Int(0));

test_assert_eq!(
    array_assign,
    "int a[3]; a[1] = 60; int k = a[1];",
    "k",
    Value::Int(60)
);

test_assert_eq!(
    array_auto_length,
    "int a[] = {24, 60, 183}; int k = a[1];",
    "k",
    Value::Int(60)
);

test_should_panic!(array_invalid_length, "int[3] a = {12, 53};");

test_should_panic!(array_invalid_type, "bool a[3] = {12, 53, 84};");

test_should_panic!(array_invalid_literal, "int a[3] = [12, 53, 84];");

test_assert_eq!(
    string,
    "char s[5] = \"Hello\"; char c = s[3];",
    "c",
    Value::Char('l')
);

#[test]
fn test_anything() -> Result<(), std::io::Error> {
    let files: Vec<_> = std::fs::read_dir("./tests/")?
        .filter_map(|el| match el {
            Ok(dir_entry) if dir_entry.path().is_file() => Some(dir_entry),
            _ => None,
        })
        .collect();

    let files_iter_l: std::slice::Iter<std::fs::DirEntry> = files.iter();
    let files_iter_r: std::slice::Iter<std::fs::DirEntry> = files.iter();
    let tests: Vec<_> = (files_iter_l.filter(|el| {
        if let Some(s) = el.path().extension() {
            let s = s.to_str().unwrap();
            s == "c"
        } else {
            false
        }
    }))
    .zip(files_iter_r.filter(|el| {
        if let Some(s) = el.path().extension() {
            let s = s.to_str().unwrap();
            s == "expected"
        } else {
            false
        }
    }))
    .collect();

    for (test, expectation) in tests {
        print!("{:?}", test.file_name());
        let test = std::fs::read_to_string(test.path())?;
        let expectation = std::fs::read_to_string(expectation.path())?;

        match {
            let program: &str = &test;
            let ast = match crate::build_ast(program) {
                Ok(it) => it,
                Err(err) => panic!("{err}"),
            };
            {
                let ast = &ast;
                let mut interpreter = crate::interpreter::Interpreter::new();
                if let Err(err) = interpreter.run(&ast.top_level) {
                    Err(err)
                } else {
                    Ok(interpreter.stdout)
                }
            }
        } {
            Ok(stdout) => {
                println!("   PASSED");
                assert_eq!(expectation, stdout)
            }
            Err(error) => {
                println!("   FAILED");
                panic!("{error}")
            }
        }
    }

    Ok(())
}