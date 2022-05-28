macro_rules! test_assert_eq {
    ($name:ident, $program:literal, $id:literal, $value:expr) => {
        #[test]
        fn $name() {
            use crate::interpreter::Interpreter;
            use crate::value::Value;

            let ast = match crate::build_ast($program) {
                Ok(ast) => ast,
                Err(error) => panic!("PARSER ERROR : {}", error)
            };
            let mut interpreter = Interpreter::new();
            match interpreter.run(&ast.top_level) {
                Ok(()) => {
                    assert_eq!(interpreter.get_var_by_name($id).unwrap(), $value)
                }
                Err(error) => panic!("INTERPRETER ERROR : {} -> {}", error.pos.get_full(&ast.source), error.msg)
            }
            /* let bytecode = match crate::compile_ast(&ast) {
                Ok(bytecode) => bytecode,
                Err(error) => panic!("COMPILER ERROR : {}", error)
            };
            match crate::run_bytecode(&bytecode) {
                Ok(()) => {
                    assert_eq!(interpreter.get_var_by_name($id).unwrap(), $value)
                }
                Err(error) => panic!("INTERPRETER ERROR : {} -> {}", error.pos.get_full(&ast.source), error.msg)
            } */
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
                    Ok(ast) => ast,
                    Err(_) => {
                        println!("ERROR IS GOOD");
                        return;
                    }
                },
                Err(_) => {
                    println!("PANIC IS GOOD");
                    return;
                }
            };
            let mut interpreter = Interpreter::new();
            match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                interpreter.run(&ast.top_level)
            })) {
                Ok(result) => match result {
                    Ok(()) => panic!("This code should panic"),
                    Err(_) => println!("ERROR IS GOOD"),
                },
                Err(_) => println!("PANIC IS GOOD"),
            }
        }
    };
}

test_assert_eq!(assign_int, "let i = 6;", "i", Value::Int(6));

test_assert_eq!(assign_int_neg, "let i = -6;", "i", Value::Int(-6));

test_assert_eq!(assign_float, "let f = 4.5;", "f", Value::Float(4.5));

test_assert_eq!(assign_float_neg, "let f = -4.5;", "f", Value::Float(-4.5));

test_assert_eq!(assign_true, "let b = true;", "b", Value::Bool(true));

test_assert_eq!(assign_false, "let b = false;", "b", Value::Bool(false));

test_assert_eq!(add_int, "let i = 2 + 4;", "i", Value::Int(6));

test_assert_eq!(add_float, "let f = 2.7 + 1.8;", "f", Value::Float(4.5));

test_assert_eq!(sub_int, "let i = 11 - 5;", "i", Value::Int(6));

test_assert_eq!(sub_float, "let f = 8.2 - 3.7;", "f", Value::Float(4.5));

test_assert_eq!(mul_int, "let i = 3 * 2;", "i", Value::Int(6));

test_assert_eq!(mul_float, "let f = 9.0 * 0.5;", "f", Value::Float(4.5));

test_assert_eq!(div_int, "let i = 12 / 2;", "i", Value::Int(6));

test_assert_eq!(div_float, "let f = 11.25 / 2.5;", "f", Value::Float(4.5));

test_assert_eq!(mod_int, "let i = 42 % 9;", "i", Value::Int(6));

test_assert_eq!(mod_float, "let f = 40.5 % 9.0;", "f", Value::Float(4.5));

test_assert_eq!(parenthesis, "let b = (true);", "b", Value::Bool(true));

test_assert_eq!(reassign, "let b = false; b = true;", "b", Value::Bool(true));

test_assert_eq!(assign_defined, "let b = true; let c = b;", "c", Value::Bool(true));

test_should_panic!(assign_undefined, "let b = c;");

test_assert_eq!(
    condition,
    "let b = false; if true { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    not,
    "let b = false; if !false { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    and_0,
    "let b = true; if false && false { b = false; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    and_1,
    "let b = true; if false && true { b = false; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    and_2,
    "let b = false; if true && true { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    or_0,
    "let b = true; if false || false { b = false; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    or_1,
    "let b = true; if false || true { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    or_2,
    "let b = true; if true || true { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    eq_0,
    "let b = false; if 0 == 0 { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    eq_1,
    "let b = true; if 0 == 1 { b = false; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    gt_0,
    "let b = false; let c = 1; if c > 0 { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    gt_1,
    "let b = true; let c = 0; if c > 0 { b = false; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    gt_2,
    "let b = true; let c = 0; if c > 1 { b = false; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    gte_0,
    "let b = false; let c = 1; if c >= 0 { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    gte_1,
    "let b = false; let c = 0; if c >= 0 { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    gte_2,
    "let b = true; let c = 0; if c >= 1 { b = false; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    lt_0,
    "let b = true; let c = 1; if c < 0 { b = false; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    lt_1,
    "let b = true; let c = 0; if c < 0 { b = false; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    lt_2,
    "let b = false; let c = 0; if c < 1 { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    lte_0,
    "let b = true; let c = 1; if c <= 0 { b = false; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    lte_1,
    "let b = false; let c = 0; if c <= 0 { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    lte_2,
    "let b = false; let c = 0; if c <= 1 { b = true; }",
    "b",
    Value::Bool(true)
);

test_assert_eq!(
    scope_0,
    "let b = true; { let c = 0; }",
    "b",
    Value::Bool(true)
);

test_should_panic!(scope_1, "{ let a = 0; } let b = a;");

test_assert_eq!(block_value, "let a = { let b = 1; };", "a", Value::Int(1));

test_assert_eq!(
    loop_,
    "let i = 1; let c = 0; while c < 5 { i = i * 2; c = c + 1; }",
    "i",
    Value::Int(32)
);

test_assert_eq!(
    function,
    "fn add(a, b) { a + b; } let i = add(2, 4);",
    "i",
    Value::Int(6)
);
