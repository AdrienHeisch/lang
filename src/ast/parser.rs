use super::{ Expr, ExprDef, Op, Token, TokenDef, Position, Delimiter, Identifier, IdentifierTools, Error };
use crate::env::{ Environment, Context };
use std::collections::{ VecDeque };
use std::cell::{ RefCell };
use typed_arena::Arena;

pub fn parse<'e, 't> (arena:&'e Arena<Expr<'e>>, tokens:&'t VecDeque<Token>) -> (Vec<&'e Expr<'e>>, Vec<Error>)
{
    Parser::new(arena, tokens).parse()
}

struct Parser<'e, 't> //TODO any way to avoid all these refcells ?
{
    arena: &'e Arena<Expr<'e>>,
    tokens: RefCell<TkIter<'t>>,
    env: RefCell<Environment>,
    errors: RefCell<Vec<Error>>,
    globals: Vec<Identifier>
}

type TkIter<'t> = std::iter::Peekable<std::collections::vec_deque::Iter<'t, Token>>;

impl<'e, 't> Parser<'e, 't>
{

    fn new (arena:&'e Arena<Expr<'e>>, tokens:&'t VecDeque<Token>) -> Self
    {
        Parser {
            arena,
            tokens: RefCell::new(tokens.iter().peekable()),
            env: RefCell::new(Environment::new(Context::TopLevel)),
            errors: RefCell::new(Vec::new()),
            globals: vec!(Identifier::make("print"), Identifier::make("printmem"))
        }
    }

    fn push_error (&self, msg:String, pos:Position)
    {
        let error = Error { msg, pos };
        if cfg!(lang_panic_on_error) {
            panic!("{}", error);
        } else {
            self.errors.borrow_mut().push(error);
        }
    }

    fn make_invalid (&self, pos:Position) -> &'e Expr<'e>
    {
        self.arena.alloc(Expr { def: ExprDef::Invalid, pos })
    }

    // ----- PARSING -----

    pub fn parse (&mut self) -> (Vec<&'e Expr<'e>>, Vec<Error>)
    {
        let mut statements = Vec::new();

        loop
        {
            statements.push(self.parse_statement());
            if self.tokens.borrow_mut().peek().is_none() { break; }
        }

        sort_functions_first(&mut statements);

        let errors = RefCell::new(Vec::new());
        self.errors.swap(&errors);

        (statements, errors.into_inner())
    }

    fn parse_statement (&self) -> &'e Expr<'e>
    {
        let expr = self.parse_expr();

        if self.peek().def == TokenDef::Semicolon {
            self.next();
        } else {
            match expr.def
            {
                _ if expr.is_block() => (),
                ExprDef::End => (),
                _ => {
                    let tk = self.peek();
                    self.push_error(format!("Expected Semicolon, got : {:?}", tk.def), tk.pos)
                }
            }
        }

        expr
    }

    fn parse_expr (&self) -> &'e Expr<'e>
    {
        let tk = self.next();
        match &tk.def
        {
            TokenDef::Op(op) => {
                let e = self.parse_expr();
                self.arena.alloc(Expr { def: ExprDef::UnOp(*op, e), pos: tk.pos + e.pos })
            },
            TokenDef::Const(value) => self.parse_expr_next(self.arena.alloc(Expr { def: ExprDef::Const(value.clone()), pos: tk.pos })),
            TokenDef::Id(_) => {
                self.parse_structure(tk)
            },
            TokenDef::DelimOpen(Delimiter::Pr) => {
                let tk = self.next();
                match tk.def
                {
                    TokenDef::DelimClose(Delimiter::Pr) => {
                        let e = self.parse_expr();
                        self.parse_expr_next(self.arena.alloc(Expr { def: ExprDef::Parent(e), pos: tk.pos }))
                    },
                    _ => {
                        self.push_error(format!("Unclosed delimiter : {:?}", Delimiter::Pr), tk.pos);
                        self.make_invalid(tk.pos)
                    }
                }
            },
            //DESIGN should blocks return last expression only if there is no semicolon like rust ?
            TokenDef::DelimOpen(Delimiter::Br) => {
                self.open_scope();

                let mut statements:Vec<&Expr> = Vec::new();
                while {
                    let peek = self.peek();
                    match peek.def
                    {
                        TokenDef::DelimClose(Delimiter::Br) => false,
                        TokenDef::Eof => {
                            self.push_error("Unclosed delimiter {.".to_owned(), tk.pos);
                            false
                        },
                        _ => true
                    }
                } {
                    statements.push(self.parse_statement());
                }
                self.next();

                self.close_scope();
                
                sort_functions_first(&mut statements);
                self.arena.alloc(Expr { def: ExprDef::Block(statements.into_boxed_slice()), pos: tk.pos })
            },
            TokenDef::Eof => self.arena.alloc(Expr { def: ExprDef::End, pos: tk.pos }),
            _ => {
                self.push_error(format!("Unexpected token : {:?}", tk.def), tk.pos);
                self.make_invalid(tk.pos)
            }
        }
    }

    //TODO assert previous expression
    fn parse_expr_next (&self, e:&'e Expr<'e>) -> &'e Expr<'e>
    {
        let tk = self.peek();
        match tk.def
        {
            TokenDef::Op(op) => {
                self.next();
                self.make_binop(op, e, self.parse_expr())
            },
            TokenDef::DelimOpen(Delimiter::Pr) => {
                self.next();
                let (expr_list, tk_delim_close) = self.make_expr_list(tk);
                self.arena.alloc(Expr {
                    def: ExprDef::Call { id: e, args: expr_list.into_boxed_slice() },
                    
                    pos: e.pos + tk_delim_close.pos
                })
            },
            TokenDef::Dot => {
                self.next();
                self.arena.alloc(Expr { def: ExprDef::Field(e, self.parse_expr()), pos: tk.pos })
            }
            _ => e
        }
    }

    fn parse_structure (&self, tk_identifier:&Token) -> &'e Expr<'e>
    {
        let id = if let TokenDef::Id(id) = tk_identifier.def {
            id
        } else {
            return self.make_invalid(tk_identifier.pos);
        };
        let pos = tk_identifier.pos;

        match &id
        {
            b"if\0\0\0\0\0\0" => {
                let cond = self.parse_expr();
                let then = self.parse_expr();
                let mut pos = pos + cond.pos + then.pos;

                let elze = match self.peek().def
                {
                    TokenDef::Id(next_id) if &next_id == b"else\0\0\0\0" => {
                        self.next();
                        Some(self.parse_expr())
                    },
                    _ => None
                };
                if let Some(elze) = elze { pos += elze.pos };

                self.arena.alloc(Expr { def: ExprDef::If { cond, then, elze }, pos })
            },
            b"while\0\0\0" => {
                let cond = self.parse_expr();
                let body = self.parse_expr();
                self.arena.alloc(Expr { def: ExprDef::While { cond, body }, pos: pos + cond.pos + body.pos })
            },
            b"var\0\0\0\0\0" => { //TODO explicit types ?
                let tk = self.next();
                match tk.def
                {
                    TokenDef::Id(id) => {
                        let pos = pos + tk.pos;
                        let tk = self.peek();
                        let value = match tk.def
                        {
                            TokenDef::Op(Op::Assign) => {
                                self.next();
                                self.parse_expr()
                            },
                            _ => { //TODO uninitialized var
                                self.push_error(format!("Expected assign operator, got : {:?}", tk.def), tk.pos);
                                self.make_invalid(pos)
                            }
                        };
                        self.declare_local(&id, true); //TODO uninitialized var
                        self.arena.alloc(Expr { def: ExprDef::Var(id, value), pos: pos + value.pos })
                    }
                    _ => {
                        self.push_error(format!("Expected identifier, got : {:?}", tk.def), tk.pos);
                        self.make_invalid(pos)
                    }
                }
            },
            b"return\0\0" => {
                let e = self.parse_expr();
                self.arena.alloc(Expr { def: ExprDef::Return(e), pos: pos + e.pos })
            },
            b"fn\0\0\0\0\0\0" => { //TODO return statement ?
                let tk = self.next();
                let id = match tk.def {
                    TokenDef::Id(id) => id,
                    _ => {
                        self.push_error(format!("Expected identifier, got : {:?}", tk.def), tk.pos);
                        Default::default()
                    }
                };

                let prev_env = RefCell::new(Environment::new(Context::Function));
                self.env.swap(&prev_env);

                let tk = self.peek();
                let (params, end_tk) = match tk.def
                {
                    TokenDef::DelimOpen(Delimiter::Pr) => {
                        self.next();
                        let (params, end_tk) = self.make_ident_list(tk);
                        (params.into_boxed_slice(), end_tk)
                    },
                    _ => {
                        self.push_error(format!("Expected (, got : {:?}", tk.def), tk.pos);
                        (Default::default(), tk)
                    }
                };
                
                let arity = params.len() as u8;
                if arity > u8::max_value() { self.push_error("Too many parameters !".to_owned(), end_tk.pos); }

                let body = self.parse_expr();

                self.env.swap(&prev_env);

                //TODO recursion ?
                self.declare_local(&id, true); //TODO uninitialized var
                self.arena.alloc(Expr { def: ExprDef::FnDecl { id, params, body }, pos: pos + body.pos })
            },
            b"struct\0\0" => {
                let mut pos = pos;
                let tk = self.next();
                let id = match tk.def {
                    TokenDef::Id(id) => id,
                    _ => {
                        self.push_error(format!("Expected identifier, got : {:?}", tk.def), tk.pos);
                        Default::default()
                    }
                };

                let tk = self.peek();
                let fields = match tk.def
                {
                    TokenDef::DelimOpen(Delimiter::Br) => {
                        self.next();
                        let (fields, tk_delim_close) = self.make_ident_list(tk);
                        pos += tk_delim_close.pos;
                        fields.into_boxed_slice()
                    },
                    _ => {
                        self.push_error(format!("Expected {{, got : {:?}", tk.def), tk.pos);
                        Default::default()
                    }
                };

                if let Context::TopLevel = self.env.borrow().get_context() {
                    self.push_error("Can't declare a struct here.".to_owned(), pos);
                    self.make_invalid(pos)
                } else {
                    self.arena.alloc(Expr { def: ExprDef::StructDecl { id, fields }, pos })
                }
            },
            id => {
                if !self.check_id_exists(id) {
                    self.push_error(format!("Unknown identifier : {}", std::str::from_utf8(id).unwrap()), tk_identifier.pos); //FIXME throws an error if function is used before being declared
                    // println!("At {} -> Unknown identifier : {}", tk_identifier.pos, id_str); //DESIGN maybe there should only be closures (no non-capturing local functions)
                };
                self.parse_expr_next(self.arena.alloc(Expr { def: ExprDef::Id(*id), pos }))
            }
        }
    }

    fn make_binop (&self, op:Op, left:&'e Expr<'e>, right:&'e Expr<'e>) -> &'e Expr<'e>
    {
        match right.def
        {
            ExprDef::BinOp { op:op_, left:left_, right:right_ } if op.priority() <= op_.priority() =>
                    self.arena.alloc(Expr { def: ExprDef::BinOp { op:op_, left:self.make_binop(op, left, left_), right:right_ }, pos: left.pos + right.pos }),
            _ =>    self.arena.alloc(Expr { def: ExprDef::BinOp { op, left, right }, pos: left.pos + right.pos })
        }
    }

    fn make_ident_list (&self, tk_delim_open:&Token) -> (Vec<Identifier>, &Token)
    {
        self.__make_list(tk_delim_open, |list:&mut Vec<Identifier>| {
            let tk = self.peek();
            if let TokenDef::Id(id) = tk.def {
                self.next();
                let id = id;
                self.declare_local(&id, true);
                list.push(id);
            } else {
                self.push_error(format!("Expected identifier, got : {:?}", tk.def), tk.pos);
                list.push(Default::default());
            };
        })
    }

    fn make_expr_list (&self, tk_delim_open:&Token) -> (Vec<&'e Expr<'e>>, &Token)
    {
        self.__make_list(tk_delim_open, |list:&mut Vec<&'e Expr<'e>>| {
            list.push(self.parse_expr());
        })
    }

    #[doc(hidden)]
    fn __make_list<T> (&self, tk_delim_open:&Token, add_item:impl Fn(&mut Vec<T>)) -> (Vec<T>, &Token)
    {
        let mut list = Vec::new();
        
        let delimiter = if let TokenDef::DelimOpen(delimiter) = tk_delim_open.def {
            delimiter
        } else {
            panic!("Only a TokenDef::DelimOpen should be passed here.");
        };

        if self.peek().def == TokenDef::DelimClose(delimiter) {
            return (list, self.next());
        }

        loop
        {
            add_item(&mut list);

            let tk = self.peek();
            match tk.def
            {
                TokenDef::Comma => {
                    self.next();
                },
                TokenDef::DelimClose(delimiter_) if delimiter_ == delimiter => {
                    break;
                },
                TokenDef::Eof => {
                    self.push_error(format!("Unclosed delimiter : {}", delimiter.to_str(false)), tk_delim_open.pos);
                    break;
                },
                _ => {
                    self.push_error(format!("Expected , or {}, got {:?}", delimiter.to_str(true), tk.def), tk.pos);
                    break;
                }
            }
        }
        
        (list, self.next())
    }

    // ----- TOKEN ITERATOR -----

    fn peek (&self) -> &Token
    {
        if let Some(item) = self.tokens.borrow_mut().peek() {
            item
        } else {
            &Token { def: TokenDef::Eof, pos: Position(0, 0) }
        }
    }

    fn next (&self) -> &Token
    {
        if let Some(item) = self.tokens.borrow_mut().next() {
            item
        } else {
            panic!("Unexpected end of file.");
        }
    }

    // ----- SCOPES -----

    fn open_scope (&self)
    {
        self.env.borrow_mut().open_scope();
    }

    fn close_scope (&self)
    {
        self.env.borrow_mut().close_scope();
    }

    fn declare_local (&self, id:&Identifier, init:bool)
    {
        let n_locals = self.env.borrow().locals_count;
        self.env.borrow_mut().locals[n_locals as usize] = (*id, if init { 1 } else { 0 });
        if let Some(n_locals) = n_locals.checked_add(1) {
            self.env.borrow_mut().locals_count = n_locals;
        } else {
            // self.push_error("Too many locals.", tk.pos);
            panic!("Too many locals."); //TODO better error handling here
        }
    }

    fn check_id_exists (&self, id:&Identifier) -> bool
    {
        if self.globals.contains(id) {
            return true;
        }
        
        let env = self.env.borrow();
        for (id_, _) in env.locals.iter().take(env.locals_count.into()).rev() {
            if id == id_ {
                return true;
            }
        }
        
        false
    }

}

// ----- UTILITY -----

fn sort_functions_first (statements:&mut [&Expr])
{
    statements.sort_by(|e1, e2| {
        use std::cmp::Ordering;
        match (&e1.def, &e2.def) {
            (ExprDef::FnDecl {..}, ExprDef::FnDecl {..})    => Ordering::Equal,
            (ExprDef::FnDecl {..}, _)     => Ordering::Less,
            (_, ExprDef::FnDecl {..})     => Ordering::Greater,
            (_, _)                      => Ordering::Equal,
        }
        
    });
}

#[allow(dead_code)]
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
        let mut parser = Parser::new(&arena, &tokens);
        let now = Instant::now();
        parser.parse();
        duration += now.elapsed();
    }
    println!("Parsing: {}ms", duration.as_millis());
}