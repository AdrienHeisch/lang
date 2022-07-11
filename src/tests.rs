macro_rules! test_assert_eq {
    ($name:ident, $program:literal, $id:literal, $value:expr) => {
        #[test]
        fn $name() {
            use crate::interpreter::Interpreter;
            #[allow(unused_imports)]
            use crate::value::{Type, Value};

            let ast = match crate::build_ast($program) {
                Ok(ast) => ast,
                Err(error) => panic!("PARSER ERROR : {}", error),
            };

            if cfg!(lang_test_interpreter) {
                let mut interpreter = Interpreter::new();
                match interpreter.run(&ast.top_level) {
                    Ok(()) => {
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

                if cfg!(lang_test_vm_compiler) {
                    match crate::run_bytecode(&bytecode) {
                        Ok(()) => {
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
                crate::build_ast($program)
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
                        Ok(()) => panic!("Test code didn't panic."),
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

                if cfg!(lang_test_vm_compiler) {
                    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        crate::run_bytecode(&bytecode)
                    })) {
                        Ok(result) => match result {
                            Ok(()) => panic!("Test code didn't panic."),
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

// test_assert_eq!(assign_float, "float f = 4.5;", "f", Value::Float(4.5));

// test_assert_eq!(assign_float_neg, "float f = -4.5;", "f", Value::Float(-4.5));

test_assert_eq!(add_int, "int i = 2 + 4;", "i", Value::Int(6));

// test_assert_eq!(add_float, "float f = 2.7 + 1.8;", "f", Value::Float(4.5));

test_assert_eq!(sub_int, "int i = 11 - 5;", "i", Value::Int(6));

// test_assert_eq!(sub_float, "float f = 8.2 - 3.7;", "f", Value::Float(4.5));

test_assert_eq!(mul_int, "int i = 3 * 2;", "i", Value::Int(6));

// test_assert_eq!(mul_float, "float f = 9.0 * 0.5;", "f", Value::Float(4.5));

test_assert_eq!(div_int, "int i = 12 / 2;", "i", Value::Int(6));

// test_assert_eq!(div_float, "float f = 11.25 / 2.5;", "f", Value::Float(4.5));

test_assert_eq!(mod_int, "int i = 42 % 9;", "i", Value::Int(6));

// test_assert_eq!(mod_float, "float f = 40.5 % 9.0;", "f", Value::Float(4.5));

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

test_assert_eq!(
    block_value,
    "int a = { int b = 1; b; };",
    "a",
    Value::Int(1)
);

test_should_panic!(assign_expr_value, "int a = { int b = 1; };");

test_assert_eq!(
    loop_,
    "int i = 1; int c = 0; while c < 5 { i = i * 2; c = c + 1; }",
    "i",
    Value::Int(32)
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
    "int i = 6; int j = 7; int *p = &j; *p = 8; int k = *p;",
    "k",
    Value::Int(8)
);

test_assert_eq!(
    function,
    "int add(int a, int b) { return a + b; } int i = add(2, 4);",
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
