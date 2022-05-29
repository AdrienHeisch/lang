use super::{
    Delimiter, Error, Expr, ExprDef, Identifier, IdentifierTools, Op, Position, Token, TokenDef,
};
use crate::{
    ast::WithPosition,
    env::{Context, Environment, Local},
    value::{type_id_pattern, Type},
};
use std::collections::VecDeque;
use typed_arena::Arena;

type TkIter<'t> = std::iter::Peekable<std::collections::vec_deque::Iter<'t, Token>>;

// ----- PARSING -----

pub fn parse<'e, 't>(
    arena: &'e Arena<Expr<'e>>,
    tokens: &'t VecDeque<Token>,
) -> Result<Vec<&'e Expr<'e>>, Vec<Error>> {
    let mut tokens_iter = tokens.iter().peekable();
    let mut env = Environment::new(Context::TopLevel);
    let mut errors = Vec::new();

    let mut statements = Vec::new();

    loop {
        statements.push(parse_statement(
            arena,
            &mut tokens_iter,
            &mut env,
            &mut errors,
        ));
        if tokens_iter.peek().is_none() {
            break;
        }
    }

    sort_functions_first(&mut statements);

    if errors.is_empty() || cfg!(lang_ignore_parse_errors) {
        Ok(statements)
    } else {
        Err(errors)
    }
}

fn parse_statement<'e, 't>(
    arena: &'e Arena<Expr<'e>>,
    tokens: &mut TkIter<'t>,
    env: &mut Environment,
    errors: &mut Vec<Error>,
) -> &'e Expr<'e> {
    let expr = parse_expr(arena, tokens, env, errors);

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

fn parse_expr<'e, 't>(
    arena: &'e Arena<Expr<'e>>,
    tokens: &mut TkIter<'t>,
    env: &mut Environment,
    errors: &mut Vec<Error>,
) -> &'e Expr<'e> {
    let tk = next(tokens);
    match &tk.def {
        TokenDef::Op(op) => {
            let e = parse_expr(arena, tokens, env, errors);
            arena.alloc(Expr {
                def: ExprDef::UnOp { op: *op, e },
                pos: tk.pos + e.pos,
            })
        }
        TokenDef::Const(value) => parse_expr_next(
            arena,
            tokens,
            env,
            errors,
            arena.alloc(Expr {
                def: ExprDef::Const(value.clone()),
                pos: tk.pos,
            }),
        ),
        TokenDef::Id(_) => parse_structure(arena, tokens, env, errors, tk),
        TokenDef::DelimOpen(Delimiter::Pr) => {
            let e = parse_expr(arena, tokens, env, errors);
            let tk = next(tokens);
            match tk.def {
                TokenDef::DelimClose(Delimiter::Pr) => parse_expr_next(
                    arena,
                    tokens,
                    env,
                    errors,
                    arena.alloc(Expr {
                        def: ExprDef::Parent(e),
                        pos: tk.pos,
                    }),
                ),
                _ => {
                    push_error(
                        errors,
                        format!("Unclosed delimiter : {:?}", Delimiter::Pr),
                        tk.pos,
                    );
                    make_invalid(arena, tk.pos)
                }
            }
        }
        //DESIGN should blocks return last expression only if there is no semicolon like rust ?
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
                statements.push(parse_statement(arena, tokens, env, errors));
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

//TODO assert previous expression
fn parse_expr_next<'e, 't>(
    arena: &'e Arena<Expr<'e>>,
    tokens: &mut TkIter<'t>,
    env: &mut Environment,
    errors: &mut Vec<Error>,
    e: &'e Expr<'e>,
) -> &'e Expr<'e> {
    let tk = peek(tokens);
    match tk.def {
        TokenDef::Op(op) => {
            next(tokens);
            let e_ = parse_expr(arena, tokens, env, errors);
            let binop = make_binop(arena, op, e, e_);
            if let Err(err) = eval_type(binop, env) {
                push_error(errors, err.msg, err.pos);
            }
            binop
        }
        TokenDef::DelimOpen(Delimiter::Pr) => {
            next(tokens);
            let (expr_list, tk_delim_close) = make_expr_list(arena, tokens, env, errors, tk);
            arena.alloc(Expr {
                def: ExprDef::Call {
                    function: e,
                    args: expr_list.into_boxed_slice(),
                },

                pos: e.pos + tk_delim_close.pos,
            })
        }
        TokenDef::Dot => {
            next(tokens);
            arena.alloc(Expr {
                def: ExprDef::Field(e, parse_expr(arena, tokens, env, errors)),
                pos: tk.pos,
            })
        }
        _ => e,
    }
}

fn parse_structure<'e, 't>(
    arena: &'e Arena<Expr<'e>>,
    tokens: &mut TkIter<'t>,
    env: &mut Environment,
    errors: &mut Vec<Error>,
    tk_identifier: &Token,
) -> &'e Expr<'e> {
    let keyword = if let TokenDef::Id(id) = tk_identifier.def {
        id
    } else {
        return make_invalid(arena, tk_identifier.pos);
    };
    let pos = tk_identifier.pos;

    match &keyword {
        type_id_pattern!() => {
            let t = Type::from_identifier(&keyword);
            let tk = next(tokens);
            if let TokenDef::Id(id) = tk.def {
                let pos = pos + tk.pos;
                let tk = peek(tokens);
                match tk.def {
                    // VARIABLE
                    TokenDef::Op(Op::Assign) => {
                        next(tokens);
                        let value = parse_expr(arena, tokens, env, errors);
                        let t = match eval_type(value, env) {
                            Ok(t_) => {
                                if t != t_ {
                                    push_error(
                                        errors,
                                        format!("Mismatched types, expected {:?}, got {:?}", t, t_),
                                        pos + value.pos,
                                    );
                                }
                                declare_local(env, &id, &t);
                                t.clone()
                            }
                            Err(err) => {
                                push_error(errors, err.msg, err.pos);
                                declare_local(env, &id, &Type::Void);
                                Type::Void
                            }
                        };
                        arena.alloc(Expr {
                            def: ExprDef::VarDecl(id, t, value),
                            pos: pos + value.pos,
                        })
                    }
                    // FUNCTION
                    TokenDef::DelimOpen(Delimiter::Pr) => {
                        next(tokens);

                        let mut local_env = env.clone(); //TODO is this needed ?
                        local_env.context = Context::Function;

                        let (params, end_tk) = make_ident_list(tokens, errors, tk);
                        for param in params.iter() {
                            declare_local(&mut local_env, &param.0, &param.1);
                        }

                        let arity = params.len() as u8;
                        if arity > u8::max_value() {
                            push_error(errors, "Too many parameters !".to_owned(), end_tk.pos);
                        }

                        let body = {
                            let tk = next(tokens);
                            match &tk.def {
                                TokenDef::DelimOpen(Delimiter::Br) => {
                                    local_env.open_scope();

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
                                            parse_statement(arena, tokens, &mut local_env, errors);

                                        if let Err(error) =
                                            look_for_return_in(statement, &mut local_env, &t)
                                        {
                                            push_error(errors, error.msg, error.pos);
                                        }

                                        statements.push(statement);
                                    }
                                    next(tokens);

                                    local_env.close_scope();

                                    sort_functions_first(&mut statements);
                                    arena.alloc(Expr {
                                        def: ExprDef::Block(statements.into_boxed_slice()),
                                        pos: tk.pos,
                                    })
                                }
                                _ => {
                                    push_error(
                                        errors,
                                        format!("Unexpected token : {:?}", tk.def),
                                        tk.pos,
                                    );
                                    make_invalid(arena, tk.pos)
                                }
                            }
                        };

                        //TODO recursion ?
                        declare_local(
                            env,
                            &id,
                            &Type::Fn(
                                params.iter().map(|param| param.1.clone()).collect(),
                                Box::new(t.clone()),
                            ),
                        );
                        arena.alloc(Expr {
                            def: ExprDef::FnDecl {
                                id,
                                params: params.into_boxed_slice(),
                                return_t: t,
                                body,
                            },
                            pos: pos + body.pos,
                        })
                    }
                    _ => {
                        //TODO uninitialized var
                        push_error(
                            errors,
                            format!("Expected assign operator, got : {:?}", tk.def),
                            tk.pos,
                        );
                        make_invalid(arena, pos)
                    }
                }
            } else {
                push_error(
                    errors,
                    format!("Expected identifier, got : {:?}", tk.def),
                    tk.pos,
                );
                make_invalid(arena, pos)
            }
        }
        b"if\0\0\0\0\0\0" => {
            let cond = parse_expr(arena, tokens, env, errors);
            let then = parse_expr(arena, tokens, env, errors);
            let mut pos = pos + cond.pos + then.pos;

            let elze = match peek(tokens).def {
                TokenDef::Id(next_id) if &next_id == b"else\0\0\0\0" => {
                    next(tokens);
                    Some(parse_expr(arena, tokens, env, errors))
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
        b"while\0\0\0" => {
            let cond = parse_expr(arena, tokens, env, errors);
            let body = parse_expr(arena, tokens, env, errors);
            arena.alloc(Expr {
                def: ExprDef::While { cond, body },
                pos: pos + cond.pos + body.pos,
            })
        }
        b"return\0\0" => {
            if let Context::TopLevel = env.context {
                push_error(
                    errors,
                    format!("Can't return from top level."),
                    tk_identifier.pos,
                );
            }
            let e = parse_expr(arena, tokens, env, errors);
            arena.alloc(Expr {
                def: ExprDef::Return(e),
                pos: pos + e.pos,
            })
        }
        b"struct\0\0" => unimplemented!(), /* {
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
        id => {
            if let None = env.get_from_id(id) {
                push_error(
                    errors,
                    format!("Unknown identifier : {}", id.to_string()),
                    tk_identifier.pos,
                ); //FIXME throws an error if function is used before being declared
                   // println!("At {} -> Unknown identifier : {}", tk_identifier.pos, id_str); //DESIGN maybe there should only be closures (no non-capturing local functions)
            };
            parse_expr_next(
                arena,
                tokens,
                env,
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
) -> &'e Expr<'e> {
    match right.def {
        ExprDef::BinOp {
            op: op_,
            left: left_,
            right: right_,
        } if op.priority() <= op_.priority() => arena.alloc(Expr {
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

fn make_ident_list<'t>(
    tokens: &mut TkIter<'t>,
    errors: &mut Vec<Error>,
    tk_delim_open: &Token,
) -> (Vec<(Identifier, Type)>, &'t Token) {
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
                let id = id;
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
                Default::default()
            }
        };

        let tk_1 = next(tokens);
        let id = if let TokenDef::Id(id) = tk_1.def {
            let id = id;
            id
        } else {
            push_error(
                errors,
                format!("Expected identifier, got : {:?}", tk_1.def),
                tk_1.pos,
            );
            Default::default()
        };

        list.push((id, t));

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
        list.push(parse_expr(arena, tokens, env, errors));

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

fn declare_local(env: &mut Environment, id: &Identifier, t: &Type) {
    let n_locals = env.locals_count;
    env.locals[n_locals as usize] = Local {
        id: *id,
        t: t.clone(),
        depth: env.scope_depth,
    };
    if let Some(n_locals) = n_locals.checked_add(1) {
        env.locals_count = n_locals;
    } else {
        // push_error("Too many locals.", tk.pos);
        panic!("Too many locals."); //TODO better error handling here
    }
}

// ----- ERROR HANDLING -----

fn push_error(errors: &mut Vec<Error>, msg: String, pos: Position) {
    let error = Error { msg, pos };
    if cfg!(lang_panic_on_error) && !cfg!(lang_ignore_parse_errors) {
        panic!("{}", error);
    } else {
        errors.push(error);
    }
}

fn make_invalid<'e>(arena: &'e Arena<Expr<'e>>, pos: Position) -> &'e Expr<'e> {
    arena.alloc(Expr {
        def: ExprDef::Invalid,
        pos,
    })
}

// ----- UTILITY -----

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

fn eval_type(expr: &Expr, env: &Environment) -> Result<Type, Error> {
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
        ExprDef::If { then, elze, .. } => {
            let then_type = unwrap_or_return!(eval_type(then, env));
            match elze {
                Some(elze) => {
                    let elze_type = unwrap_or_return!(eval_type(elze, env));
                    if then_type == elze_type {
                        then_type
                    } else {
                        return Err(Error {
                            msg: format!(
                                "If statement has mismatched types : {:?} / {:?}",
                                then_type, elze_type
                            ),
                            pos: expr.pos,
                        });
                    }
                }
                _ => then_type,
            }
        }
        ExprDef::While { .. } => Type::Void,
        ExprDef::Field(_, _) => todo!(),
        ExprDef::UnOp { op, e } => {
            use {Op::*, Type::*};
            match unwrap_or_return!(eval_type(e, env)) {
                Int => match op {
                    Sub => Int,
                    _ => {
                        return Err(Error {
                            msg: format!("Invalid operation: {} {:?}", op.to_string(), Int),
                            pos: expr.pos,
                        })
                    }
                },
                Float => match op {
                    Sub => Float,
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
                t => {
                    return Err(Error {
                        msg: format!("Invalid operation: {} {:?}", op.to_string(), t),
                        pos: expr.pos,
                    })
                }
            }
        }
        ExprDef::BinOp { op, left, right } => {
            use {Op::*, Type::*};
            match (
                unwrap_or_return!(eval_type(left, env)),
                unwrap_or_return!(eval_type(right, env)),
            ) {
                (Int, Int) => match op {
                    Equal | NotEqual | Gt | Gte | Lt | Lte => Bool,
                    Add | Sub | Mult | Div | Mod => Int,
                    Assign | AddAssign | SubAssign | MultAssign | DivAssign | ModAssign => Void,
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
                    Add | Sub | Mult | Div | Mod => Float,
                    Assign | AddAssign | SubAssign | MultAssign | DivAssign | ModAssign => Void,
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
                    Assign => Void,
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
                if let Some(local) = env.get_from_id(&id) {
                    if let Type::Fn(params_t, return_t) = local.t {
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
                            let arg_t = unwrap_or_return!(eval_type(arg, env));
                            if param_t != &arg_t {
                                return Err(Error {
                                    msg: format!(
                                        "Invalid argument, expected {:?}, found {:?}",
                                        param_t, arg_t
                                    ),
                                    pos: arg.pos,
                                });
                            }
                        }

                        *return_t.clone()
                    } else {
                        return Err(Error {
                            msg: format!("Expected function, found {:?}", local.t),
                            pos: expr.pos,
                        });
                    }
                } else {
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
            let mut local_env = env.clone(); //TODO is this needed ?
            local_env.open_scope();
            let mut return_type = Type::Void;
            for e in exprs.iter() {
                match &e.def {
                    ExprDef::VarDecl(id, t, _) => declare_local(&mut local_env, &id, &t),
                    ExprDef::FnDecl {
                        id,
                        params,
                        return_t,
                        ..
                    } => declare_local(
                        &mut local_env,
                        &id,
                        &Type::Fn(
                            params.iter().map(|param| param.1.clone()).collect(),
                            Box::new(return_t.clone()),
                        ),
                    ),
                    _ => (),
                }
                return_type = unwrap_or_return!(eval_type(e, &local_env));
            }
            local_env.close_scope();
            return_type
        }
        ExprDef::Parent(e) => {
            unwrap_or_return!(eval_type(e, env))
        }
        ExprDef::Return(_) => Type::Void,
        ExprDef::Invalid => Type::Void,
        ExprDef::End => Type::Void,
    })
}

fn look_for_return_in(
    e: &WithPosition<ExprDef>,
    env: &mut Environment,
    return_type: &Type,
) -> Result<(), Error> {
    macro_rules! unwrap_or_return_ {
        ($e:expr) => {
            match $e {
                Ok(t) => t,
                err @ Err(_) => return err,
            }
        };
    }

    use ExprDef::*;
    Ok(match &e.def {
        If { cond, then, elze } => {
            unwrap_or_return_!(look_for_return_in(cond, env, return_type));
            unwrap_or_return_!(look_for_return_in(then, env, return_type));
            if let Some(elze) = elze {
                unwrap_or_return_!(look_for_return_in(elze, env, return_type));
            }
        }
        While { cond, body } => {
            unwrap_or_return_!(look_for_return_in(cond, env, return_type));
            unwrap_or_return_!(look_for_return_in(body, env, return_type));
        }
        Field(_, _) => unimplemented!(),
        UnOp { e, .. } => {
            unwrap_or_return_!(look_for_return_in(e, env, return_type));
        }
        BinOp { left, right, .. } => {
            unwrap_or_return_!(look_for_return_in(left, env, return_type));
            unwrap_or_return_!(look_for_return_in(right, env, return_type));
        }
        Call { function, args } => {
            unwrap_or_return_!(look_for_return_in(function, env, return_type));
            for arg in args.iter() {
                unwrap_or_return_!(look_for_return_in(arg, env, return_type));
            }
        }
        VarDecl(id, t, _) => declare_local(env, &id, &t),
        FnDecl {
            id,
            params,
            return_t,
            ..
        } => declare_local(
            env,
            &id,
            &Type::Fn(
                params.iter().map(|param| param.1.clone()).collect(),
                Box::new(return_t.clone()),
            ),
        ),
        StructDecl { .. } => unimplemented!(),
        Block(exprs) => {
            for e in exprs.iter() {
                unwrap_or_return_!(look_for_return_in(e, env, return_type));
            }
        }
        Return(return_expr) => {
            let t = match eval_type(return_expr, env) {
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
            unwrap_or_return_!(look_for_return_in(e, env, return_type));
        }
        _ => (),
    })
}