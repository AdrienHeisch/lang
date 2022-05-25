use super::{
    Delimiter, Error, Expr, ExprDef, Identifier, IdentifierTools, Op, Position, Token, TokenDef,
};
use crate::{
    env::{Context, Environment},
    value::Type,
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
    let mut globals = vec![Identifier::make("print"), Identifier::make("printmem")];

    let mut statements = Vec::new();

    loop {
        statements.push(parse_statement(
            arena,
            &mut tokens_iter,
            &mut env,
            &mut errors,
            &mut globals,
        ));
        if tokens_iter.peek().is_none() {
            break;
        }
    }

    sort_functions_first(&mut statements);

    if errors.is_empty() {
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
    globals: &mut Vec<Identifier>,
) -> &'e Expr<'e> {
    let expr = parse_expr(arena, tokens, env, errors, globals);

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
    globals: &mut Vec<Identifier>,
) -> &'e Expr<'e> {
    let tk = next(tokens);
    match &tk.def {
        TokenDef::Op(op) => {
            let e = parse_expr(arena, tokens, env, errors, globals);
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
            globals,
            arena.alloc(Expr {
                def: ExprDef::Const(value.clone()),
                pos: tk.pos,
            }),
        ),
        TokenDef::Id(_) => parse_structure(arena, tokens, env, errors, globals, tk),
        TokenDef::DelimOpen(Delimiter::Pr) => {
            let e = parse_expr(arena, tokens, env, errors, globals);
            let tk = next(tokens);
            match tk.def {
                TokenDef::DelimClose(Delimiter::Pr) => parse_expr_next(
                    arena,
                    tokens,
                    env,
                    errors,
                    globals,
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
                statements.push(parse_statement(arena, tokens, env, errors, globals));
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
    globals: &mut Vec<Identifier>,
    e: &'e Expr<'e>,
) -> &'e Expr<'e> {
    let tk = peek(tokens);
    match tk.def {
        TokenDef::Op(op) => {
            next(tokens);
            let expr = parse_expr(arena, tokens, env, errors, globals);
            make_binop(arena, op, e, expr)
        }
        TokenDef::DelimOpen(Delimiter::Pr) => {
            next(tokens);
            let (expr_list, tk_delim_close) =
                make_expr_list(arena, tokens, env, errors, globals, tk);
            arena.alloc(Expr {
                def: ExprDef::Call {
                    id: e,
                    args: expr_list.into_boxed_slice(),
                },

                pos: e.pos + tk_delim_close.pos,
            })
        }
        TokenDef::Dot => {
            next(tokens);
            arena.alloc(Expr {
                def: ExprDef::Field(e, parse_expr(arena, tokens, env, errors, globals)),
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
    globals: &mut Vec<Identifier>,
    tk_identifier: &Token,
) -> &'e Expr<'e> {
    let id = if let TokenDef::Id(id) = tk_identifier.def {
        id
    } else {
        return make_invalid(arena, tk_identifier.pos);
    };
    let pos = tk_identifier.pos;

    match &id {
        b"let\0\0\0\0\0" => {
            //TODO type checking (type_ @ (b"int\0\0\0\0\0" | b"float\0\0\0" | b"bool\0\0\0\0") => {)
            let tk = next(tokens);
            match tk.def {
                TokenDef::Id(id) => {
                    let pos = pos + tk.pos;
                    let tk = peek(tokens);
                    let value = match tk.def {
                        TokenDef::Op(Op::Assign) => {
                            next(tokens);
                            parse_expr(arena, tokens, env, errors, globals)
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
                    };
                    declare_local(env, &id);
                    arena.alloc(Expr {
                        def: ExprDef::VarDecl(id, value),
                        pos: pos + value.pos,
                    })
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
        b"fn\0\0\0\0\0\0" => {
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

            let mut local_env = *env;

            let tk = peek(tokens);
            let (params, end_tk) = match tk.def {
                TokenDef::DelimOpen(Delimiter::Pr) => {
                    next(tokens);
                    let (params, end_tk) = make_ident_list(tokens, &mut local_env, errors, tk);
                    (params.into_boxed_slice(), end_tk)
                }
                _ => {
                    push_error(errors, format!("Expected (, got : {:?}", tk.def), tk.pos);
                    (Default::default(), tk)
                }
            };

            let arity = params.len() as u8;
            if arity > u8::max_value() {
                push_error(errors, "Too many parameters !".to_owned(), end_tk.pos);
            }

            let body = parse_expr(arena, tokens, &mut local_env, errors, globals);

            //TODO recursion ?
            declare_local(env, &id);
            arena.alloc(Expr {
                def: ExprDef::FnDecl { id, params, body },
                pos: pos + body.pos,
            })
        }
        b"if\0\0\0\0\0\0" => {
            let cond = parse_expr(arena, tokens, env, errors, globals);
            let then = parse_expr(arena, tokens, env, errors, globals);
            let mut pos = pos + cond.pos + then.pos;

            let elze = match peek(tokens).def {
                TokenDef::Id(next_id) if &next_id == b"else\0\0\0\0" => {
                    next(tokens);
                    Some(parse_expr(arena, tokens, env, errors, globals))
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
            let cond = parse_expr(arena, tokens, env, errors, globals);
            let body = parse_expr(arena, tokens, env, errors, globals);
            arena.alloc(Expr {
                def: ExprDef::While { cond, body },
                pos: pos + cond.pos + body.pos,
            })
        }
        b"return\0\0" => {
            if let Context::TopLevel = env.get_context() {
                push_error(
                    errors,
                    format!("Can't return from top level."),
                    tk_identifier.pos,
                );
            }
            let e = parse_expr(arena, tokens, env, errors, globals);
            arena.alloc(Expr {
                def: ExprDef::Return(e),
                pos: pos + e.pos,
            })
        }
        b"struct\0\0" => {
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

            if let Context::TopLevel = env.get_context() {
                push_error(errors, "Can't declare a struct here.".to_owned(), pos);
                make_invalid(arena, pos)
            } else {
                arena.alloc(Expr {
                    def: ExprDef::StructDecl { id, fields },
                    pos,
                })
            }
        }
        id => {
            if !check_id_exists(env, globals, id) {
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
                globals,
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
    env: &mut Environment,
    errors: &mut Vec<Error>,
    tk_delim_open: &Token,
) -> (Vec<Identifier>, &'t Token) {
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
        let tk_ = peek(tokens);
        if let TokenDef::Id(id) = tk_.def {
            next(tokens);
            let id = id;
            declare_local(env, &id);
            list.push(id);
        } else {
            push_error(
                errors,
                format!("Expected identifier, got : {:?}", tk_.def),
                tk_.pos,
            );
            list.push(Default::default());
        };

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

fn make_expr_list<'e, 't>(
    arena: &'e Arena<Expr<'e>>,
    tokens: &mut TkIter<'t>,
    env: &mut Environment,
    errors: &mut Vec<Error>,
    globals: &mut Vec<Identifier>,
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
        list.push(parse_expr(arena, tokens, env, errors, globals));

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

// #region make use macros ?
/* fn make_ident_list<'e, 't> (arena: &'e Arena<Expr<'e>>, tokens: &mut TkIter<'t>, env: &mut Environment, errors: &mut Vec<Error>, globals: &mut Vec<Identifier>, tk_delim_open:&Token) -> (Vec<Identifier>, &'t Token)
{
    __make_list(tokens, errors, tk_delim_open, |list:&mut Vec<Identifier>| {
        let tk = peek(tokens);
        if let TokenDef::Id(id) = tk.def {
            next(tokens);
            let id = id;
            declare_local(arena, tokens, env, errors, globals, &id, true);
            list.push(id);
        } else {
            push_error(errors, format!("Expected identifier, got : {:?}", tk.def), tk.pos);
            list.push(Default::default());
        };
    })
}

fn make_expr_list<'e, 't> (arena: &'e Arena<Expr<'e>>, tokens: &mut TkIter<'t>, env: &mut Environment, errors: &mut Vec<Error>, globals: &mut Vec<Identifier>, tk_delim_open:&Token) -> (Vec<&'e Expr<'e>>, &'t Token)
{
    __make_list(tokens, errors, tk_delim_open, |list:&mut Vec<&'e Expr<'e>>| {
        list.push(parse_expr(arena, tokens, env, errors, globals));
    })
}

#[doc(hidden)]
fn __make_list<'e, 't, T> (tokens: &mut TkIter<'t>, errors: &mut Vec<Error>, tk_delim_open:&Token, add_item:impl Fn(&mut Vec<T>)) -> (Vec<T>, &'t Token)
{
    let mut list = Vec::new();

    let delimiter = if let TokenDef::DelimOpen(delimiter) = tk_delim_open.def {
        delimiter
    } else {
        panic!("Only a TokenDef::DelimOpen should be passed here.");
    };

    if peek(tokens).def == TokenDef::DelimClose(delimiter) {
        return (list, next(tokens));
    }

    loop
    {
        add_item(&mut list);

        let tk = peek(tokens);
        match tk.def
        {
            TokenDef::Comma => {
                next(tokens);
            },
            TokenDef::DelimClose(delimiter_) if delimiter_ == delimiter => {
                break;
            },
            TokenDef::Eof => {
                push_error(errors, format!("Unclosed delimiter : {}", delimiter.to_str(false)), tk_delim_open.pos);
                break;
            },
            _ => {
                push_error(errors, format!("Expected , or {}, got {:?}", delimiter.to_str(true), tk.def), tk.pos);
                break;
            }
        }
    }

    (list, next(tokens))
} */
// #endregion

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

fn declare_local(env: &mut Environment, id: &Identifier) {
    let n_locals = env.locals_count;
    env.locals[n_locals as usize] = (*id, env.scope_depth);
    if let Some(n_locals) = n_locals.checked_add(1) {
        env.locals_count = n_locals;
    } else {
        // push_error("Too many locals.", tk.pos);
        panic!("Too many locals."); //TODO better error handling here
    }
}

fn check_id_exists(env: &Environment, globals: &[Identifier], id: &Identifier) -> bool {
    if globals.contains(id) {
        return true;
    }

    for (id_, _) in env.locals.iter().take(env.locals_count.into()).rev() {
        if id == id_ {
            return true;
        }
    }

    false
}

// ----- ERROR HANDLING -----

fn push_error(errors: &mut Vec<Error>, msg: String, pos: Position) {
    let error = Error { msg, pos };
    if cfg!(lang_panic_on_error) {
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

fn eval_type(expr: &Expr, env: &Environment) -> Type {
    match &expr.def {
        ExprDef::Const(value) => value.as_type(),
        ExprDef::Id(id) => {
            // check_id_exists(env, globals, id)
            todo!()
        },
        ExprDef::If {
            cond: _,
            then,
            elze,
        } => {
            let then_type = eval_type(then, env);
            if elze.is_some() && then_type == eval_type(elze.unwrap(), env) {
                then_type
            } else {
                panic!()
            }
        }
        ExprDef::While { cond: _, body: _ } => Type::Void,
        ExprDef::Field(_, _) => todo!(),
        ExprDef::UnOp { op, e } => match eval_type(e, env) {
            Type::Int => match op {
                Op::Sub => Type::Int,
                _ => panic!(),
            },
            Type::Float => match op {
                Op::Sub => Type::Float,
                _ => panic!(),
            },
            Type::Bool => match op {
                Op::Not => Type::Bool,
                _ => panic!(),
            },
            _ => panic!(),
        },
        ExprDef::BinOp { op, left, right } => {
            use {Op::*, Type::*};
            match (eval_type(left, env), eval_type(right, env)) {
                (Int, Int) => match op {
                    Equal | NotEqual | Gt | Gte | Lt | Lte => Bool,
                    Add | Sub | Mult | Div | Mod => Int,
                    Assign | AddAssign | SubAssign | MultAssign | DivAssign | ModAssign => Void, //TODO should not be void ?
                    _ => panic!(),
                },
                (Float, Float) => match op {
                    Equal | NotEqual | Gt | Gte | Lt | Lte => Bool,
                    Add | Sub | Mult | Div | Mod => Float,
                    Assign | AddAssign | SubAssign | MultAssign | DivAssign | ModAssign => Void, //TODO should not be void ?
                    _ => panic!(),
                },
                (Bool, Bool) => match op {
                    Equal | NotEqual | BoolAnd | BoolOr => Bool,
                    Assign => Void, //TODO should not be void ?
                    _ => panic!(),
                },
                (_, _) => panic!(),
            }
        }
        ExprDef::Call { id, args } => todo!(),
        ExprDef::VarDecl(_, _) => Type::Void,
        ExprDef::FnDecl { id, params, body } => Type::Void,
        ExprDef::StructDecl { id, fields } => Type::Void,
        ExprDef::Block(exprs) => {
            if !exprs.is_empty() {
                eval_type(exprs.last().unwrap(), env)
            } else {
                Type::Void
            }
        }
        ExprDef::Parent(e) => eval_type(e, env),
        ExprDef::Return(_) => Type::Void,
        ExprDef::Invalid => Type::Void,
        ExprDef::End => Type::Void,
    }
}

/* #[allow(dead_code)]
pub fn benchmark ()
{
    use crate::benchmarks::ITERATIONS;
    use super::lexer;
    use std::time::{ Instant, Duration };

    let program = std::fs::read_to_string("./code.lang").unwrap();
    let (tokens, errors) = lexer::lex(&program);
    if !errors.is_empty() {
        println!("Parsing: couldn't proceed to benchmark due to lexing errors.");
        return;
    }
    let mut duration = Duration::new(0, 0);
    for _ in 0..ITERATIONS
    {
        let arena = typed_arena::Arena::new();
        let now = Instant::now();
        parse(&arena, &tokens);
        duration += now.elapsed();
    }
    println!("Parsing: {}ms", duration.as_millis());
} */
