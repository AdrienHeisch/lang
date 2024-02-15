#![allow(clippy::mut_from_ref)]

use super::{
    Delimiter, Error, Expr, ExprDef, Identifier, IdentifierTools, Op, Position, Token, TokenDef,
};
use crate::{
    ast::WithPosition,
    env::{Context, Environment, Local},
    value::{type_id_pattern, Type, Value},
};
use std::collections::VecDeque;
use typed_arena::Arena;

type TkIter<'t> = std::iter::Peekable<std::collections::vec_deque::Iter<'t, Token>>;

#[allow(dead_code)]
#[derive(Default)]
struct StaticMem {
    values: Vec<Option<Value>>,
    len: u32,
}

// ----- PARSING -----

pub fn parse<'e>(
    arena: &'e Arena<Expr<'e>>,
    tokens: &VecDeque<Token>,
) -> Result<Vec<&'e Expr<'e>>, Vec<Error>> {
    let mut tokens_iter = tokens.iter().peekable();
    let mut env = Environment::new(Context::TopLevel);
    let mut errors = Vec::new();

    let mut top_level = Vec::new();
    let mut statik: StaticMem = Default::default(); //TODO remove this

    loop {
        top_level.push(parse_statement(
            arena,
            &mut tokens_iter,
            &mut env,
            &mut statik,
            &mut errors,
        ));
        if tokens_iter.peek().is_none() {
            break;
        }
    }

    sort_functions_first(&mut top_level);

    if errors.is_empty() || cfg!(lang_ignore_parse_errors) {
        Ok(top_level)
    } else {
        Err(errors)
    }
}

fn parse_statement<'e>(
    arena: &'e Arena<Expr<'e>>,
    tokens: &mut TkIter<'_>,
    env: &mut Environment,
    statik: &mut StaticMem,
    errors: &mut Vec<Error>,
) -> &'e Expr<'e> {
    let expr = parse_expr(arena, tokens, env, statik, errors);

    if let Context::TopLevel = env.context {
        match expr.def {
            ExprDef::FnDecl { .. } | ExprDef::VarDecl(_, _, _) => (),
            ExprDef::End => (),
            _ if !cfg!(lang_allow_no_function) => push_error(
                errors,
                format!("Unexpected non-declaration statement : {:?}", expr.def),
                expr.pos,
            ),
            _ => (),
        }
    }

    if peek(tokens).def == TokenDef::Semicolon {
        next(tokens);
    } else {
        match expr.def {
            ExprDef::End => (),
            _ if expr.is_block() => (),
            _ => {
                let tk = peek(tokens);
                push_error(
                    errors,
                    format!("Expected Semicolon, got : {:?}", tk.def),
                    tk.pos,
                )
            }
        }
    }

    expr
}

fn parse_expr<'e>(
    arena: &'e Arena<Expr<'e>>,
    tokens: &mut TkIter<'_>,
    env: &mut Environment,
    statik: &mut StaticMem,
    errors: &mut Vec<Error>,
) -> &'e mut Expr<'e> {
    let tk = next(tokens);
    match &tk.def {
        TokenDef::Const(value) => parse_expr_next(
            arena,
            tokens,
            env,
            statik,
            errors,
            arena.alloc(Expr {
                def: ExprDef::Const(value.clone()),
                pos: tk.pos,
            }),
        ),
        TokenDef::Id(_) => parse_structure(arena, tokens, env, statik, errors, tk),
        TokenDef::StringLit(chars) => parse_expr_next(
            arena,
            tokens,
            env,
            statik,
            errors,
            arena.alloc(Expr {
                def: ExprDef::StringLit(chars.clone()),
                pos: tk.pos,
            }),
        ),
        TokenDef::Op(op) => {
            let n_errs = errors.len();
            let e = parse_expr(arena, tokens, env, statik, errors);
            let expr = match e.def {
                ExprDef::BinOp { ref mut left, .. } => {
                    while errors.len() > n_errs {
                        //HACK error vulnerability here : expr is not type checked
                        errors.pop();
                    }
                    *left = arena.alloc(Expr {
                        def: ExprDef::UnOp { op: *op, e: left },
                        pos: tk.pos + e.pos,
                    });
                    e
                }
                _ => arena.alloc(Expr {
                    def: ExprDef::UnOp { op: *op, e },
                    pos: tk.pos + e.pos,
                }),
            };
            if let Err(err) = eval_type(expr, env, errors) {
                push_error(errors, err.msg, err.pos);
            }
            expr
        }
        TokenDef::If => {
            let cond = parse_expr(arena, tokens, env, statik, errors);
            let then = parse_expr(arena, tokens, env, statik, errors);
            let mut pos = tk.pos + cond.pos + then.pos;

            let elze = match peek(tokens).def {
                TokenDef::Id(next_id) if &next_id == b"else\0\0\0\0" => {
                    next(tokens);
                    Some(parse_expr(arena, tokens, env, statik, errors) as &Expr)
                }
                _ => None,
            };
            if let Some(elze) = elze {
                pos += elze.pos
            };

            arena.alloc(Expr {
                def: ExprDef::If { cond, then, elze },
                pos,
            })
        }
        TokenDef::While => {
            let cond = parse_expr(arena, tokens, env, statik, errors);
            let body = parse_expr(arena, tokens, env, statik, errors);
            arena.alloc(Expr {
                def: ExprDef::While { cond, body },
                pos: tk.pos + cond.pos + body.pos,
            })
        }
        TokenDef::Return => {
            if let Context::TopLevel = env.context {
                push_error(errors, "Can't return from top level.".to_string(), tk.pos);
            }
            let e = parse_expr(arena, tokens, env, statik, errors);
            arena.alloc(Expr {
                def: ExprDef::Return(e),
                pos: tk.pos + e.pos,
            })
        }
        /* b"struct\0\0" => unimplemented!(), {
        let mut pos = pos;
        let tk = next(tokens);
        let id = match tk.def {
        TokenDef::Id(id) => id,
        _ => {
        push_error(
        errors,
        format!("Expected identifier, got : {:?}", tk.def),
        tk.pos,
        );
        Default::default()
        }
        };

        let tk = peek(tokens);
        let fields = match tk.def {
        TokenDef::DelimOpen(Delimiter::Br) => {
        next(tokens);
        let (fields, tk_delim_close) = make_ident_list(tokens, env, errors, tk);
        pos += tk_delim_close.pos;
        fields.into_boxed_slice()
        }
        _ => {
        push_error(errors, format!("Expected {{, got : {:?}", tk.def), tk.pos);
        Default::default()
        }
        };

        if let Context::TopLevel = env.context {
        push_error(errors, "Can't declare a struct here.".to_owned(), pos);
        make_invalid(arena, pos)
        } else {
        arena.alloc(Expr {
        def: ExprDef::StructDecl { id, fields },
        pos,
        })
        }
        } */
        TokenDef::DelimOpen(Delimiter::Pr) => {
            let e = parse_expr(arena, tokens, env, statik, errors);
            let tk = next(tokens);
            if let TokenDef::DelimClose(Delimiter::Pr) = tk.def {
                parse_expr_next(
                    arena,
                    tokens,
                    env,
                    statik,
                    errors,
                    arena.alloc(Expr {
                        def: ExprDef::Parent(e),
                        pos: tk.pos,
                    }),
                )
            } else {
                push_error(
                    errors,
                    format!("Unclosed delimiter : {:?}", Delimiter::Pr),
                    tk.pos,
                );
                make_invalid(arena, tk.pos)
            }
        }
        TokenDef::DelimOpen(Delimiter::Br) => {
            env.open_scope();

            let mut statements: Vec<&Expr> = Vec::new();
            while {
                let peek = peek(tokens);
                match peek.def {
                    TokenDef::DelimClose(Delimiter::Br) => false,
                    TokenDef::Eof => {
                        push_error(errors, "Unclosed delimiter {.".to_owned(), tk.pos);
                        false
                    }
                    _ => true,
                }
            } {
                statements.push(parse_statement(arena, tokens, env, statik, errors));
            }
            next(tokens);

            env.close_scope();

            sort_functions_first(&mut statements);
            arena.alloc(Expr {
                def: ExprDef::Block(statements.into_boxed_slice()),
                pos: tk.pos,
            })
        }
        TokenDef::Eof => arena.alloc(Expr {
            def: ExprDef::End,
            pos: tk.pos,
        }),
        _ => {
            push_error(errors, format!("Unexpected token : {:?}", tk.def), tk.pos);
            make_invalid(arena, tk.pos)
        }
    }
}

fn parse_expr_next<'e>(
    arena: &'e Arena<Expr<'e>>,
    tokens: &mut TkIter<'_>,
    env: &mut Environment,
    statik: &mut StaticMem,
    errors: &mut Vec<Error>,
    e: &'e mut Expr<'e>,
) -> &'e mut Expr<'e> {
    let tk = peek(tokens);
    match tk.def {
        TokenDef::Op(op) => {
            next(tokens);
            let e_ = parse_expr(arena, tokens, env, statik, errors);
            #[allow(clippy::let_and_return)]
            let expr = make_binop(arena, op, e, e_);
            //TODO type checking should be done after parsing
            //HACK error vulnerability here : expr is not type checked
            /* if let Err(err) = eval_type(expr, env, errors) {
                push_error(errors, err.msg, err.pos);
            } */
            expr
        }
        TokenDef::DelimOpen(Delimiter::SqBr) => {
            next(tokens);
            let e_ = parse_expr(arena, tokens, env, statik, errors);
            if let TokenDef::DelimClose(Delimiter::SqBr) = peek(tokens).def {
                next(tokens);
            } else {
                push_error(errors, "Unclosed delimiter [.".to_owned(), tk.pos);
            }
            let expr = parse_expr_next(
                arena,
                tokens,
                env,
                statik,
                errors,
                make_binop(
                    arena,
                    Op::Index,
                    e,
                    arena.alloc(Expr {
                        def: ExprDef::Parent(e_),
                        pos: e_.pos,
                    }),
                ),
            );
            if let Err(err) = eval_type(expr, env, errors) {
                push_error(errors, err.msg, err.pos);
            }
            expr
        }
        TokenDef::DelimOpen(Delimiter::Pr) => {
            next(tokens);
            let (expr_list, tk_delim_close) =
                make_expr_list(arena, tokens, env, statik, errors, tk);
            let expr = arena.alloc(Expr {
                def: ExprDef::Call {
                    function: e,
                    args: expr_list.into_boxed_slice(),
                },

                pos: e.pos + tk_delim_close.pos,
            });
            if let Err(err) = eval_type(expr, env, errors) {
                push_error(errors, err.msg, err.pos);
            }
            expr
        }
        TokenDef::Dot => {
            next(tokens);
            let expr = arena.alloc(Expr {
                def: ExprDef::Field(e, parse_expr(arena, tokens, env, statik, errors)),
                pos: tk.pos,
            });
            if let Err(err) = eval_type(expr, env, errors) {
                push_error(errors, err.msg, err.pos);
            }
            expr
        }
        _ => e,
    }
}

fn parse_structure<'e>(
    arena: &'e Arena<Expr<'e>>,
    tokens: &mut TkIter<'_>,
    env: &mut Environment,
    statik: &mut StaticMem,
    errors: &mut Vec<Error>,
    tk_identifier: &Token,
) -> &'e mut Expr<'e> {
    let keyword = if let TokenDef::Id(id) = tk_identifier.def {
        id
    } else {
        return make_invalid(arena, tk_identifier.pos);
    };
    let pos = tk_identifier.pos;

    match &keyword {
        type_id_pattern!() => {
            //TODO should basic types be dedicated tokens ?
            //TODO pointer of pointer
            let mut t = Type::from_identifier(&keyword);

            if let TokenDef::DelimOpen(Delimiter::Pr) = peek(tokens).def {
                next(tokens);
                if let TokenDef::Op(Op::MultOrDeref) = peek(tokens).def {
                    next(tokens);
                } else {
                    panic!();
                }
                t = Type::Fn(Box::new([]), Box::new(t));
            } else {
                while let TokenDef::Op(Op::MultOrDeref) = peek(tokens).def {
                    next(tokens);
                    t = Type::Pointer(Box::new(t))
                }
            }

            let mut tk = next(tokens);
            match tk.def {
                TokenDef::Id(id) if matches!(&id, type_id_pattern!()) => {
                    push_error(errors, format!("Invalid identifier : {:?}", tk.def), tk.pos);
                    make_invalid(arena, pos)
                }
                TokenDef::Id(id) => {
                    let mut array_len_tdb = false;
                    if let Type::Fn(ref mut args, _) = t {
                        if let TokenDef::DelimClose(Delimiter::Pr) = peek(tokens).def {
                            next(tokens);
                        } else {
                            panic!();
                        }

                        tk = next(tokens);
                        if let TokenDef::DelimOpen(Delimiter::Pr) = tk.def {
                        } else {
                            panic!();
                        }

                        let mut args_vec = Vec::new();
                        let (arg_types, _) = make_types_list(tokens, errors, tk);
                        for arg_t in arg_types.into_iter() {
                            args_vec.push(arg_t);
                        }
                        *args = args_vec.into_boxed_slice();
                    } else if let TokenDef::DelimOpen(Delimiter::SqBr) = peek(tokens).def {
                        next(tokens);
                        tk = next(tokens);
                        let len = if let TokenDef::Const(Value::Int(len)) = tk.def {
                            tk = next(tokens);
                            len
                        } else {
                            array_len_tdb = true;
                            1
                        };
                        if len > 0 {
                            t = Type::Array {
                                len: len as u32,
                                t: Box::new(t),
                            };
                            if let TokenDef::DelimClose(Delimiter::SqBr) = tk.def {
                            } else {
                                push_error(errors, format!("Expected ], got {:?}", tk.def), tk.pos);
                                return make_invalid(arena, pos);
                            }
                        } else {
                            push_error(
                                errors,
                                "Array length must be superior to 0".to_string(),
                                tk.pos,
                            );
                            return make_invalid(arena, pos);
                        }
                    }

                    //TODO functions can't return an array

                    let pos = pos + tk.pos; //TODO clarify this
                    let tk = peek(tokens);
                    match tk.def {
                        // VARIABLE
                        TokenDef::Semicolon => {
                            if array_len_tdb {
                                if let Type::Array { .. } = t {
                                    push_error(errors, "Array needs a length".to_string(), tk.pos);
                                    return make_invalid(arena, pos);
                                }
                            }
                            declare(env, errors, &t, &id);
                            arena.alloc(Expr {
                                def: ExprDef::VarDecl(id, t, None),
                                pos,
                            })
                        }
                        TokenDef::Op(Op::Assign) => {
                            next(tokens);

                            let value = if let Type::Array { t: t_arr, len } = &mut t {
                                let tk = peek(tokens);
                                if let TokenDef::DelimOpen(Delimiter::Br) = &tk.def {
                                    next(tokens);
                                    let (items, tk_close) =
                                        make_expr_list(arena, tokens, env, statik, errors, tk);
                                    if array_len_tdb {
                                        *len = items.len() as u32;
                                    }

                                    let pos =
                                        Position(tk.pos.0, tk_close.pos.0 + tk.pos.1 - tk.pos.0);

                                    let t = eval_type(items[0], env, errors);
                                    if let Err(err) = t {
                                        push_error(errors, err.msg, err.pos);
                                        return make_invalid(arena, pos);
                                    }

                                    let t = t.unwrap();
                                    for item in &items[1..] {
                                        match eval_type(item, env, errors) {
                                            Ok(t_) => {
                                                if t != t_ {
                                                    push_error(
                                                        errors,
                                                        "Array items type mismatch".to_string(),
                                                        pos,
                                                    );
                                                }
                                            }
                                            Err(err) => {
                                                push_error(errors, err.msg, err.pos);
                                            }
                                        }
                                    }

                                    parse_expr_next(
                                        arena,
                                        tokens,
                                        env,
                                        statik,
                                        errors,
                                        arena.alloc(Expr {
                                            def: ExprDef::ArrayLit {
                                                items: items.into_boxed_slice(),
                                                t: Box::new(t),
                                            },
                                            pos: tk.pos,
                                        }),
                                    )
                                } else if **t_arr == Type::Char {
                                    let expr = parse_expr(arena, tokens, env, statik, errors);
                                    if let ExprDef::StringLit(chars) = &expr.def {
                                        if array_len_tdb {
                                            *len = chars.len() as u32;
                                        }
                                        expr
                                    } else {
                                        push_error(
                                            errors,
                                            "Expected string literal".to_string(),
                                            pos,
                                        );
                                        arena.alloc(Expr {
                                            def: ExprDef::VarDecl(id, t.clone(), None),
                                            pos,
                                        })
                                    }
                                } else {
                                    push_error(errors, "Expected array literal".to_string(), pos);
                                    declare(env, errors, &t, &id);
                                    arena.alloc(Expr {
                                        def: ExprDef::VarDecl(id, t.clone(), None),
                                        pos,
                                    })
                                }
                            } else {
                                parse_expr(arena, tokens, env, statik, errors)
                            };
                            let t = match eval_type(value, env, errors) {
                                Ok(t_) => {
                                    if t != t_ {
                                        push_error(
                                            errors,
                                            format!(
                                                "Mismatched types, expected {:?}, got {:?}",
                                                t, t_
                                            ),
                                            pos + value.pos,
                                        );
                                    }
                                    declare(env, errors, &t, &id);
                                    t.clone()
                                }
                                Err(err) => {
                                    push_error(errors, err.msg, err.pos);
                                    declare(env, errors, &Type::Void, &id);
                                    Type::Void
                                }
                            };
                            arena.alloc(Expr {
                                def: ExprDef::VarDecl(id, t, Some(value)),
                                pos: pos + value.pos,
                            })
                        }
                        // FUNCTION
                        TokenDef::DelimOpen(Delimiter::Pr) => {
                            next(tokens);

                            //TODO allow unnamed parameters in function headers
                            let (params, end_tk) = make_args_list(tokens, errors, tk);

                            let arity = params.len() as u8;
                            if arity > u8::max_value() {
                                push_error(errors, "Too many parameters !".to_owned(), end_tk.pos);
                            }

                            let prev_context = env.context;
                            env.context = Context::Function;
                            let body: Option<&WithPosition<ExprDef>> = {
                                let tk = next(tokens);
                                match &tk.def {
                                    //TODO could this be a function (merge code with block in parse_expr)
                                    TokenDef::DelimOpen(Delimiter::Br) => {
                                        env.open_scope();
                                        for param in params.iter() {
                                            declare(env, errors, &param.0, &param.1);
                                        }

                                        let mut statements: Vec<&Expr> = Vec::new();
                                        while {
                                            let peek = peek(tokens);
                                            match peek.def {
                                                TokenDef::DelimClose(Delimiter::Br) => false,
                                                TokenDef::Eof => {
                                                    push_error(
                                                        errors,
                                                        "Unclosed delimiter {.".to_owned(),
                                                        tk.pos,
                                                    );
                                                    false
                                                }
                                                _ => true,
                                            }
                                        } {
                                            let statement =
                                                parse_statement(arena, tokens, env, statik, errors);

                                            if let Err(error) =
                                                look_for_return_in(statement, env, errors, &t)
                                            {
                                                push_error(errors, error.msg, error.pos);
                                            }

                                            statements.push(statement);
                                        }
                                        next(tokens);

                                        env.close_scope();

                                        sort_functions_first(&mut statements);
                                        Some(arena.alloc(Expr {
                                            def: ExprDef::Block(statements.into_boxed_slice()),
                                            pos: tk.pos,
                                        }))
                                    }
                                    TokenDef::Semicolon => None,
                                    _ => {
                                        push_error(
                                            errors,
                                            format!("Unexpected token : {:?}", tk.def),
                                            tk.pos,
                                        );
                                        None
                                    }
                                }
                            };
                            env.context = prev_context;

                            declare(
                                env,
                                errors,
                                &Type::Fn(
                                    params.iter().map(|param| param.0.clone()).collect(),
                                    Box::new(t.clone()),
                                ),
                                &id,
                            );

                            if let Some(body) = body {
                                arena.alloc(Expr {
                                    def: ExprDef::FnDecl {
                                        id,
                                        params: params.into_boxed_slice(),
                                        return_t: t,
                                        body,
                                    },
                                    pos: pos + body.pos,
                                })
                            } else {
                                //TODO create function header expression
                                //TODO LINKER
                                parse_expr(arena, tokens, env, statik, errors)
                            }
                        }
                        _ => {
                            push_error(errors, format!("Unexpected token : {:?}", tk.def), tk.pos);
                            make_invalid(arena, pos)
                        }
                    }
                }
                _ => {
                    push_error(
                        errors,
                        format!("Expected identifier, got : {:?}", tk.def),
                        tk.pos,
                    );
                    make_invalid(arena, pos)
                }
            }
        }
        id => {
            if env.get_from_id(id).is_none() {
                push_error(
                    errors,
                    format!("Undeclared variable : {}", id.to_string()),
                    tk_identifier.pos,
                );
                // println!("At {} -> Unknown identifier : {}", tk_identifier.pos, id_str); //DESIGN maybe there should only be closures (no non-capturing local functions)
            };
            parse_expr_next(
                arena,
                tokens,
                env,
                statik,
                errors,
                arena.alloc(Expr {
                    def: ExprDef::Id(*id),
                    pos,
                }),
            )
        }
    }
}

fn make_binop<'e>(
    arena: &'e Arena<Expr<'e>>,
    op: Op,
    left: &'e Expr<'e>,
    right: &'e Expr<'e>,
) -> &'e mut Expr<'e> {
    match right.def {
        ExprDef::BinOp {
            op: op_,
            left: left_,
            right: right_,
        } if op.precedence() <= op_.precedence() => arena.alloc(Expr {
            def: ExprDef::BinOp {
                op: op_,
                left: make_binop(arena, op, left, left_),
                right: right_,
            },
            pos: left.pos + right.pos,
        }),
        _ => arena.alloc(Expr {
            def: ExprDef::BinOp { op, left, right },
            pos: left.pos + right.pos,
        }),
    }
}

fn make_args_list<'t>(
    tokens: &mut TkIter<'t>,
    errors: &mut Vec<Error>,
    tk_delim_open: &Token,
) -> (Vec<(Type, Identifier)>, &'t Token) {
    let mut list = Vec::new();

    let delimiter = if let TokenDef::DelimOpen(delimiter) = tk_delim_open.def {
        delimiter
    } else {
        panic!("Only a TokenDef::DelimOpen should be passed here.");
    };

    if peek(tokens).def == TokenDef::DelimClose(delimiter) {
        return (list, next(tokens));
    }

    loop {
        let tk_0 = next(tokens);
        let t = match tk_0.def {
            TokenDef::Id(id) if matches!(&id, type_id_pattern!()) => {
                // let id = id;
                Type::from_identifier(&id)
            }
            _ => {
                push_error(
                    errors,
                    if let TokenDef::Id(id) = tk_0.def {
                        format!(
                            "Expected type identifier, got identifier : {}",
                            id.to_string()
                        )
                    } else {
                        format!("Expected type identifier, got : {:?}", tk_0.def)
                    },
                    tk_0.pos,
                );
                Type::Void
            }
        };

        let tk_1 = next(tokens);
        let id = if let TokenDef::Id(id) = tk_1.def {
            id
        } else {
            push_error(
                errors,
                format!("Expected identifier, got : {:?}", tk_1.def),
                tk_1.pos,
            );
            Default::default()
        };

        list.push((t, id));

        let tk_2 = peek(tokens);
        match tk_2.def {
            TokenDef::Comma => {
                next(tokens);
            }
            TokenDef::DelimClose(delimiter_) if delimiter_ == delimiter => {
                break;
            }
            TokenDef::Eof => {
                push_error(
                    errors,
                    format!("Unclosed delimiter : {}", delimiter.to_str(false)),
                    tk_delim_open.pos,
                );
                break;
            }
            _ => {
                push_error(
                    errors,
                    format!(
                        "Expected , or {}, got {:?}",
                        delimiter.to_str(true),
                        tk_2.def
                    ),
                    tk_2.pos,
                );
                break;
            }
        }
    }

    (list, next(tokens))
}

fn make_types_list<'t>(
    tokens: &mut TkIter<'t>,
    errors: &mut Vec<Error>,
    tk_delim_open: &Token,
) -> (Vec<Type>, &'t Token) {
    let mut list = Vec::new();

    let delimiter = if let TokenDef::DelimOpen(delimiter) = tk_delim_open.def {
        delimiter
    } else {
        panic!("Only a TokenDef::DelimOpen should be passed here.");
    };

    if peek(tokens).def == TokenDef::DelimClose(delimiter) {
        return (list, next(tokens));
    }

    loop {
        let tk_0 = next(tokens);
        let t = match tk_0.def {
            TokenDef::Id(id) if matches!(&id, type_id_pattern!()) => {
                // let id = id;
                Type::from_identifier(&id)
            }
            _ => {
                push_error(
                    errors,
                    if let TokenDef::Id(id) = tk_0.def {
                        format!(
                            "Expected type identifier, got identifier : {}",
                            id.to_string()
                        )
                    } else {
                        format!("Expected type identifier, got : {:?}", tk_0.def)
                    },
                    tk_0.pos,
                );
                Type::Void
            }
        };

        list.push(t);

        let tk_2 = peek(tokens);
        match tk_2.def {
            TokenDef::Comma => {
                next(tokens);
            }
            TokenDef::DelimClose(delimiter_) if delimiter_ == delimiter => {
                break;
            }
            TokenDef::Eof => {
                push_error(
                    errors,
                    format!("Unclosed delimiter : {}", delimiter.to_str(false)),
                    tk_delim_open.pos,
                );
                break;
            }
            _ => {
                push_error(
                    errors,
                    format!(
                        "Expected , or {}, got {:?}",
                        delimiter.to_str(true),
                        tk_2.def
                    ),
                    tk_2.pos,
                );
                break;
            }
        }
    }

    (list, next(tokens))
}

fn make_expr_list<'e, 't>(
    arena: &'e Arena<Expr<'e>>,
    tokens: &mut TkIter<'t>,
    env: &mut Environment,
    statik: &mut StaticMem,
    errors: &mut Vec<Error>,
    tk_delim_open: &Token,
) -> (Vec<&'e Expr<'e>>, &'t Token) {
    let mut list = Vec::new();

    let delimiter = if let TokenDef::DelimOpen(delimiter) = tk_delim_open.def {
        delimiter
    } else {
        panic!("Only a TokenDef::DelimOpen should be passed here.");
    };

    if peek(tokens).def == TokenDef::DelimClose(delimiter) {
        return (list, next(tokens));
    }

    loop {
        list.push(parse_expr(arena, tokens, env, statik, errors));

        let tk = peek(tokens);
        match tk.def {
            TokenDef::Comma => {
                next(tokens);
            }
            TokenDef::DelimClose(delimiter_) if delimiter_ == delimiter => {
                break;
            }
            TokenDef::Eof => {
                push_error(
                    errors,
                    format!("Unclosed delimiter : {}", delimiter.to_str(false)),
                    tk_delim_open.pos,
                );
                break;
            }
            _ => {
                push_error(
                    errors,
                    format!("Expected , or {}, got {:?}", delimiter.to_str(true), tk.def),
                    tk.pos,
                );
                break;
            }
        }
    }

    (list, next(tokens))
}

// ----- TOKEN ITERATOR -----

fn peek<'t>(tokens: &mut TkIter<'t>) -> &'t Token {
    if let Some(item) = tokens.peek() {
        item
    } else {
        &Token {
            def: TokenDef::Eof,
            pos: Position(0, 0),
        }
    }
}

fn next<'t>(tokens: &mut TkIter<'t>) -> &'t Token {
    if let Some(item) = tokens.next() {
        item
    } else {
        panic!("Unexpected end of file.");
    }
}

// ----- SCOPES -----

//TODO better error handling here
fn declare(env: &mut Environment, errors: &mut Vec<Error>, t: &Type, id: &Identifier) {
    if let Context::TopLevel = env.context {
        if let Some(local) = env.globals.iter().find(|item| item.id == *id) {
            let Local { id: id_, t: t_, .. } = local;
            if id_ == id {
                match t_ {
                    Type::Fn(_, _) if t_ == t => (),
                    Type::Fn(_, _) => push_error(
                        errors,
                        "Mismatched function signatures".to_string(),
                        Position::zero(),
                    ), //TODO position
                    _ => push_error(
                        errors,
                        format!("Redefinition of global variable {}", id.to_string()),
                        Position::zero(),
                    ), //TODO position
                }
            }
        }

        env.globals.push(Local {
            id: *id,
            t: t.clone(),
            depth: 0,
        });
    } else {
        let n_locals = env.locals_count;

        for local in env.locals.iter() {
            let Local {
                id: id_,
                t: t_,
                depth,
            } = local;
            if id_ == id && depth >= &env.scope_depth {
                match t_ {
                    Type::Fn(_, _) if t_ == t => (),
                    Type::Fn(_, _) => push_error(
                        errors,
                        "Mismatched function signatures".to_string(),
                        Position::zero(),
                    ), //TODO position
                    _ => push_error(
                        errors,
                        format!("Redefinition of local variable {}", id.to_string()),
                        Position::zero(),
                    ), //TODO position
                }
            }
        }

        env.locals[n_locals as usize] = Local {
            id: *id,
            t: t.clone(),
            depth: env.scope_depth,
        };
        if let Some(n_locals) = n_locals.checked_add(1) {
            env.locals_count = n_locals;
        } else {
            // push_error("Too many locals.", tk.pos);
            panic!("Too many locals.");
        }
    }
}

// ----- ERROR HANDLING -----

fn push_error(errors: &mut Vec<Error>, msg: String, pos: Position) {
    //TODO turn into a macro for better call stacks ?
    let error = Error { msg, pos };
    //TODO replace all !cfg!(_) with cfg!(not(_))
    if cfg!(lang_panic_on_error) && !cfg!(lang_ignore_parse_errors) {
        panic!("{}", error);
    } else {
        errors.push(error);
    }
}

fn make_invalid<'e>(arena: &'e Arena<Expr<'e>>, pos: Position) -> &'e mut Expr<'e> {
    arena.alloc(Expr {
        def: ExprDef::Invalid,
        pos,
    })
}

// ----- UTILITY -----

//TODO LINKER
fn sort_functions_first(statements: &mut [&Expr]) {
    statements.sort_by(|e1, e2| {
        use std::cmp::Ordering;
        match (&e1.def, &e2.def) {
            (ExprDef::FnDecl { .. }, ExprDef::FnDecl { .. }) => Ordering::Equal,
            (ExprDef::FnDecl { .. }, _) => Ordering::Less,
            (_, ExprDef::FnDecl { .. }) => Ordering::Greater,
            (_, _) => Ordering::Equal,
        }
    });
}

// ----- TYPING -----

//TODO should type checking be performed after parsing ?
// TODO could this be replaced by an interpreter instance ?
fn eval_type(expr: &Expr, env: &mut Environment, errors: &mut Vec<Error>) -> Result<Type, Error> {
    macro_rules! unwrap_or_return {
        ($e:expr) => {
            match $e {
                Ok(t) => t,
                err @ Err(_) => return err,
            }
        };
    }

    Ok(match &expr.def {
        ExprDef::Const(value) => value.as_type(),
        ExprDef::Id(id) => {
            if let Some(local) = env.get_from_id(id) {
                local.t
            } else {
                return Err(Error {
                    msg: format!("Undefined variable: {}", id.to_string()),
                    pos: expr.pos,
                });
            }
        }
        ExprDef::ArrayLit { items, t } => Type::Array {
            len: items.len() as u32,
            t: t.clone(),
        },
        ExprDef::StringLit(chars) => Type::Array {
            len: chars.len() as u32,
            t: Box::new(Type::Char),
        },
        ExprDef::If { .. } => Type::Void,
        ExprDef::While { .. } => Type::Void,
        ExprDef::Field(_, _) => todo!(),
        ExprDef::UnOp { op, e } => {
            use {Op::*, Type::*};
            let t = unwrap_or_return!(eval_type(e, env, errors));
            if let Addr = op {
                Type::Pointer(Box::new(t))
            } else {
                match t {
                    Pointer(ptr_t) => match op {
                        MultOrDeref => *ptr_t,
                        _ => {
                            return Err(Error {
                                msg: format!(
                                    "Invalid operation: {} {:?}",
                                    op.to_string(),
                                    Pointer(ptr_t)
                                ),
                                pos: expr.pos,
                            })
                        }
                    },
                    Int => match op {
                        SubOrNeg => Int,
                        _ => {
                            return Err(Error {
                                msg: format!("Invalid operation: {} {:?}", op.to_string(), Int),
                                pos: expr.pos,
                            })
                        }
                    },
                    Float => match op {
                        SubOrNeg => Float,
                        _ => {
                            return Err(Error {
                                msg: format!("Invalid operation: {} {:?}", op.to_string(), Float),
                                pos: expr.pos,
                            })
                        }
                    },
                    Bool => match op {
                        Not => Bool,
                        _ => {
                            return Err(Error {
                                msg: format!("Invalid operation: {} {:?}", op.to_string(), Bool),
                                pos: expr.pos,
                            })
                        }
                    },
                    t => match op {
                        Addr => Type::Pointer(Box::new(t)),
                        _ => {
                            return Err(Error {
                                msg: format!("Invalid operation: {} {:?}", op.to_string(), t),
                                pos: expr.pos,
                            })
                        }
                    },
                }
            }
        }
        ExprDef::BinOp { op, left, right } => {
            let t_right = unwrap_or_return!(eval_type(right, env, errors));

            if *op == Op::Assign {
                match left.def {
                    ExprDef::UnOp {
                        op: Op::MultOrDeref,
                        e,
                    } => {
                        if let Pointer(t) = eval_type(e, env, errors)? {
                            if t_right == *t {
                                return Ok(Type::Void);
                            }
                        }
                    }
                    ExprDef::BinOp {
                        op: Op::Index,
                        left,
                        right,
                    } => {
                        if let Array { .. } = eval_type(left, env, errors)? {
                            if let Int = eval_type(right, env, errors)? {
                                return Ok(Type::Void);
                            } else {
                                panic!()
                            }
                        } else {
                            panic!()
                        }
                    }
                    _ => (),
                }
            }

            let t_left = unwrap_or_return!(eval_type(left, env, errors));
            use {Op::*, Type::*};
            match (t_left, t_right) {
                (Pointer(_), Pointer(_)) => match op {
                    Assign => Type::Void,
                    _ => {
                        return Err(Error {
                            msg: format!(
                                "Invalid operation : {:?} {} {:?}",
                                Int,
                                op.to_string(),
                                Int
                            ),
                            pos: expr.pos,
                        })
                    }
                },
                (Pointer(ptr_t), t) if *ptr_t == t => match op {
                    Assign => Type::Void,
                    _ => {
                        return Err(Error {
                            msg: format!(
                                "Invalid operation : {:?} {} {:?}",
                                Int,
                                op.to_string(),
                                Int
                            ),
                            pos: expr.pos,
                        })
                    }
                },
                (ref t_left @ Array { ref t, .. }, Int) => match op {
                    Index => *t.clone(),
                    _ => {
                        return Err(Error {
                            msg: format!(
                                "Invalid operation : {:?} {} {:?}",
                                t_left.clone(),
                                op.to_string(),
                                Int
                            ),
                            pos: expr.pos,
                        })
                    }
                },
                (Int, Int) => match op {
                    Equal | NotEqual | Gt | Gte | Lt | Lte => Bool,
                    Add | SubOrNeg | MultOrDeref | Div | Mod => Int,
                    Assign | AddAssign | SubAssign | MultAssign | DivAssign | ModAssign => {
                        Type::Void
                    }
                    _ => {
                        return Err(Error {
                            msg: format!(
                                "Invalid operation : {:?} {} {:?}",
                                Int,
                                op.to_string(),
                                Int
                            ),
                            pos: expr.pos,
                        })
                    }
                },
                (Float, Float) => match op {
                    Equal | NotEqual | Gt | Gte | Lt | Lte => Bool,
                    Add | SubOrNeg | MultOrDeref | Div | Mod => Float,
                    Assign | AddAssign | SubAssign | MultAssign | DivAssign | ModAssign => {
                        Type::Void
                    }
                    _ => {
                        return Err(Error {
                            msg: format!(
                                "Invalid operation : {:?} {} {:?}",
                                Float,
                                op.to_string(),
                                Float
                            ),
                            pos: expr.pos,
                        })
                    }
                },
                (Bool, Bool) => match op {
                    Equal | NotEqual | BoolAnd | BoolOr => Bool,
                    Assign => Type::Void,
                    _ => {
                        return Err(Error {
                            msg: format!(
                                "Invalid operation : {:?} {} {:?}",
                                Bool,
                                op.to_string(),
                                Bool
                            ),
                            pos: expr.pos,
                        })
                    }
                },
                (t_left, t_right) if op.is_assign() => {
                    return Err(Error {
                        msg: format!("Invalid assignment : {:?} to {:?}", t_right, t_left),
                        pos: expr.pos,
                    })
                }
                (t_left, t_right) => {
                    return Err(Error {
                        msg: format!(
                            "Invalid operation : {:?} {} {:?}",
                            t_left,
                            op.to_string(),
                            t_right
                        ),
                        pos: expr.pos,
                    })
                }
            }
        }
        ExprDef::Call { function, args } => match &function.def {
            ExprDef::Id(id) => {
                if let Some(local) = env.get_from_id(id) {
                    if let Type::Fn(params_t, return_t) = local.t {
                        //TODO function pointer ?
                        if params_t.len() != args.len() {
                            return Err(Error {
                                msg: format!(
                                    "Expected {} arguments, found {}",
                                    params_t.len(),
                                    args.len()
                                ),
                                pos: expr.pos,
                            });
                        }

                        for (param_t, arg) in params_t.iter().zip(args.iter()) {
                            let arg_t = unwrap_or_return!(eval_type(arg, env, errors));
                            if param_t != &arg_t {
                                return Err(Error {
                                    msg: format!(
                                        "Invalid argument, expected {}, got {}",
                                        param_t, arg_t
                                    ),
                                    pos: arg.pos,
                                });
                            }
                        }

                        *return_t
                    } else {
                        return Err(Error {
                            msg: format!("Expected function, got {:?}", local.t),
                            pos: expr.pos,
                        });
                    }
                } else {
                    //TODO functions as values
                    return Err(Error {
                        msg: format!("Undefined variable : {}", id.to_string()),
                        pos: expr.pos,
                    });
                }
            }
            e => {
                return Err(Error {
                    msg: format!("Expected function, found {:?}", e),
                    pos: expr.pos,
                });
            }
        },
        ExprDef::VarDecl(_, _, _) => Type::Void,
        ExprDef::FnDecl { .. } => Type::Void,
        ExprDef::StructDecl { .. } => Type::Void,
        ExprDef::Block(exprs) => {
            env.open_scope();
            for e in exprs.iter() {
                match &e.def {
                    ExprDef::VarDecl(id, t, _) => declare(env, errors, t, id),
                    ExprDef::FnDecl {
                        id,
                        params,
                        return_t,
                        ..
                    } => declare(
                        env,
                        errors,
                        &Type::Fn(
                            params.iter().map(|param| param.0.clone()).collect(),
                            Box::new(return_t.clone()),
                        ),
                        id,
                    ),
                    _ => (),
                }
                unwrap_or_return!(eval_type(e, env, errors)); //TODO is the inner type checking on e needed ?
            }
            env.close_scope();
            Type::Void
        }
        ExprDef::Parent(e) => {
            unwrap_or_return!(eval_type(e, env, errors))
        }
        ExprDef::Return(_) => Type::Void,
        ExprDef::Invalid => Type::Void,
        ExprDef::End => Type::Void,
    })
}

fn look_for_return_in(
    e: &WithPosition<ExprDef>,
    env: &mut Environment,
    errors: &mut Vec<Error>,
    return_type: &Type,
) -> Result<(), Error> {
    macro_rules! unwrap_or_return {
        ($e:expr) => {
            match $e {
                Ok(t) => t,
                err @ Err(_) => return err,
            }
        };
    }

    use ExprDef::*;
    match &e.def {
        If { cond, then, elze } => {
            unwrap_or_return!(look_for_return_in(cond, env, errors, return_type));
            unwrap_or_return!(look_for_return_in(then, env, errors, return_type));
            if let Some(elze) = elze {
                unwrap_or_return!(look_for_return_in(elze, env, errors, return_type));
            }
        }
        While { cond, body } => {
            unwrap_or_return!(look_for_return_in(cond, env, errors, return_type));
            unwrap_or_return!(look_for_return_in(body, env, errors, return_type));
        }
        Field(_, _) => unimplemented!(),
        UnOp { e, .. } => {
            unwrap_or_return!(look_for_return_in(e, env, errors, return_type));
        }
        BinOp { left, right, .. } => {
            unwrap_or_return!(look_for_return_in(left, env, errors, return_type));
            unwrap_or_return!(look_for_return_in(right, env, errors, return_type));
        }
        Call { function, args } => {
            unwrap_or_return!(look_for_return_in(function, env, errors, return_type));
            for arg in args.iter() {
                unwrap_or_return!(look_for_return_in(arg, env, errors, return_type));
            }
        }
        StructDecl { .. } => unimplemented!(),
        Block(exprs) => {
            for e in exprs.iter() {
                unwrap_or_return!(look_for_return_in(e, env, errors, return_type));
            }
        }
        Return(return_expr) => {
            let t = match eval_type(return_expr, env, errors) {
                Ok(t) => t,
                Err(error) => return Err(error),
            };
            if &t != return_type {
                return Err(Error {
                    msg: format!(
                        "Return type mismatch : expected {:?}, found {:?}",
                        return_type, t
                    ),
                    pos: e.pos,
                });
            }
        }
        Parent(e) => {
            unwrap_or_return!(look_for_return_in(e, env, errors, return_type));
        }
        _ => (),
    };
    Ok(())
}
