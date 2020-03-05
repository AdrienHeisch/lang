use super::{ Expr, Op, Token, Delimiter, Identifier, make_identifier };
use crate::env::Environment;
use std::collections::{ VecDeque };
use std::cell::{ RefCell };
use typed_arena::Arena;

pub fn parse<'a> (arena:&'a Arena<Expr<'a>>, tokens:&VecDeque<Token>) -> Vec<&'a Expr<'a>>
{
    Parser::new(arena, tokens).parse()
}

struct Parser<'a, 'b>
{
    arena: &'a Arena<Expr<'a>>,
    tokens: RefCell<TkIter<'b>>,
    env: RefCell<Environment>, //TODO refactor with environments (see interpreter)
    globals: Vec<Identifier>
}

impl<'a, 'b> Parser<'a, 'b>
{

    fn new (arena:&'a Arena<Expr<'a>>, tokens:&'b VecDeque<Token<'b>>) -> Self
    {
        Parser {
            arena,
            tokens: RefCell::new(tokens.iter().peekable()),
            env: RefCell::new(Environment::new()),
            globals: vec!(super::make_identifier("print"))
        }
    }

    // ----- PARSING -----

    //DESIGN should blocks return last expression only if there is no semicolon like rust ?
    pub fn parse (&mut self) -> Vec<&'a Expr<'a>>
    {
        let mut statements = Vec::new();

        loop
        {
            statements.push(self.parse_statement());
            if self.tokens.borrow_mut().peek().is_none() { break; }
        }

        sort_functions_first(&mut statements);

        statements
    }

    fn parse_statement (&self) -> &'a Expr<'a>
    {
        let expr = self.parse_expr();

        if self.peek() == &Token::Semicolon {
            self.next();
        } else {
            match &expr
            {
                e if e.is_block() => (),
                Expr::End => (),
                _ => {
                    eprintln!("{:?}", expr);
                    panic!("Expected Semicolon, got : {:?}", self.peek());
                }
            }
        }

        expr
    }

    fn parse_expr (&self) -> &'a Expr<'a>
    {
        match self.next()
        {
            Token::Op(op, _) => self.arena.alloc(Expr::UnOp(*op, self.parse_expr())),
            Token::Const(value) => self.parse_expr_next(self.arena.alloc(Expr::Const(value.clone()))),
            Token::Id(id) => {
                self.parse_structure(id)
            },
            Token::DelimOpen(Delimiter::Pr) => {
                match self.next()
                {
                    Token::DelimClose(Delimiter::Pr) => {
                        let e = self.parse_expr();
                        self.parse_expr_next(self.arena.alloc(Expr::Parent(e)))
                    },
                    _ => {
                        panic!("Unclosed delimiter : {:?}", Delimiter::Pr);
                    }
                }
            },
            Token::DelimOpen(Delimiter::Br) => {
                self.open_scope();

                let mut statements:Vec<&Expr> = Vec::new();
                while *self.peek() != Token::DelimClose(Delimiter::Br) { //FIXME infinite loop on unclosed bracket
                    statements.push(self.parse_statement());
                }
                self.next();

                self.close_scope();
                
                sort_functions_first(&mut statements);
                self.arena.alloc(Expr::Block(statements.into_boxed_slice()))
            },
            Token::Eof => self.arena.alloc(Expr::End),
            tk => panic!("Unexpected token : {:?}", tk)
        }
    }

    //TODO assert previous expression
    fn parse_expr_next (&self, e:&'a Expr<'a>) -> &'a Expr<'a>
    {
        match self.peek()
        {
            Token::Op(op, is_assign) => {
                self.next();
                self.make_binop(*op, *is_assign, e, self.parse_expr())
            },
            Token::DelimOpen(Delimiter::Pr) => {
                self.next();
                self.arena.alloc(Expr::Call { name: e, args: self.make_expr_list(Delimiter::Pr).into_boxed_slice() })
            },
            Token::Dot => {
                self.next();
                self.arena.alloc(Expr::Field(e, self.parse_expr()))
            }
            _ => e
        }
    }

    fn parse_structure (&self, id:&str) -> &'a Expr<'a>
    {
        match id
        {
            "if" => {
                let cond = self.parse_expr();
                let then = self.parse_expr();
                let elze = match self.peek()
                {
                    Token::Id("else") => {
                        self.next();
                        Some(self.parse_expr())
                    },
                    _ => None
                };
                self.arena.alloc(Expr::If { cond, then, elze })
            },
            "while" => {
                let cond = self.parse_expr();
                let body = self.parse_expr();
                self.arena.alloc(Expr::While { cond, body })
            },
            "var" => { //TODO implicit types ?
                match self.next()
                {
                    Token::Id(id) => {
                        let id = make_identifier(id);
                        let value = match self.peek()
                        {
                            Token::Op(Op::Assign, _) => {
                                self.next();
                                self.parse_expr()
                            },
                            tk => panic!("Expected assign operator, got : {:?}", tk) //TODO uninitialized var
                        };
                        self.declare_local(&id, true); //TODO uninitialized var
                        self.arena.alloc(Expr::Var(id, value))
                    }
                    tk => panic!("Expected identifier, got : {:?}", tk)
                }
            },
            "fn" => {
                let id = match self.next() {
                    Token::Id(id) => make_identifier(id),
                    tk => panic!("Expected identifier, got : {:?}", tk)
                };

                let params = match self.peek()
                {
                    Token::DelimOpen(Delimiter::Pr) => { //TODO create function environment here
                        self.next();
                        self.make_params_list(Delimiter::Pr).into_boxed_slice()
                    },
                    tk => {
                        panic!("Expected open parenthese, got : {:?}", tk);
                    }
                };
                
                let arity = params.len() as u8;
                if arity > u8::max_value() { panic!("Too many arguments !"); }

                let body = self.parse_expr();

                self.declare_local(&id, true); //TODO uninitialized var
                self.arena.alloc(Expr::FnDecl { id, params, body })
            },
            /* "struct" => {
                let name = match self.next() {
                    Token::Id(s) => *s,
                    tk => panic!("Expected identifier, got : {:?}", tk)
                };
                
            }, */
            id_str => {
                let id = make_identifier(id_str);
                if !self.check_id_exists(&id) {
                    panic!("Unknown identifier : {}", id_str);
                };
                self.parse_expr_next(self.arena.alloc(Expr::Id(id)))
            }
        }
    }

    fn make_binop (&self, op:Op, is_assign:bool, left:&'a Expr<'a>, right:&'a Expr<'a>) -> &'a Expr<'a>
    {
        let priority = if is_assign {
            std::u8::MAX
        } else {
            op.priority()
        };

        match right
        {
            Expr::BinOp { op:op_, is_assign:is_assign_, left:left_, right:right_ } if priority <= op_.priority() =>
                    self.arena.alloc(Expr::BinOp { op:*op_, is_assign:*is_assign_, left:self.make_binop(op, is_assign, left, left_), right:right_ }),
            _ =>    self.arena.alloc(Expr::BinOp { op, is_assign, left, right })
        }
    }

    //TODO use a predicate in make_expr_list insead of a specific method
    fn make_params_list (&self, delimiter:Delimiter) -> Vec<Identifier>
    {
        let mut params_list = Vec::new();
        if *self.peek() == Token::DelimClose(delimiter) {
            self.next();
            return params_list;
        }

        loop
        {
            let tk = self.next();
            if let Token::Id(id) = tk { //TODO remove this (function environment)
                let id = make_identifier(id);
                self.declare_local(&id, true);
                params_list.push(id);
            } else {
                panic!("Expected identifier, got : {:?}", tk);
            };

            match self.next()
            {
                Token::Comma => continue,
                Token::DelimClose(c) if *c == delimiter => break,
                Token::Eof => panic!("Unclosed delimiter : {:?}", delimiter),
                tk => {
                    eprintln!("Expr list: {:?}", params_list);
                    panic!("Expected Comma or DelimClose('{:?}'), got  {:?}", delimiter, tk);
                }
            }
        }
        
        params_list
    }

    fn make_expr_list (&self, delimiter:Delimiter) -> Vec<&'a Expr<'a>>
    {
        let mut expr_list = Vec::new();
        if *self.peek() == Token::DelimClose(delimiter) {
            self.next();
            return expr_list;
        }

        loop
        {
            expr_list.push(self.parse_expr());
            match self.peek()
            {
                Token::Comma => { self.next(); },
                Token::DelimClose(c) if *c == delimiter => {
                    self.next();
                    break;
                },
                Token::Eof => panic!("Unclosed delimiter : {:?}", delimiter),
                tk => {
                    eprintln!("Expr list: {:?}", expr_list);
                    panic!("Expected Comma or DelimClose('{:?}'), got  {:?}", delimiter, tk);
                }
            }
        }
        
        expr_list
    }

    // ----- TOKEN ITERATOR -----

    fn peek (&self) -> &Token
    {
        if let Some(item) = self.tokens.borrow_mut().peek() {
            item
        } else {
            &Token::Eof
        }
    }

    fn next (&self) -> &Token
    {
        if let Some(item) = self.tokens.borrow_mut().next() {
            item
        } else {
            &Token::Eof
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
        self.env.borrow_mut().locals[n_locals as usize] = (*id, init.into());
        if let Some(n_locals) = n_locals.checked_add(1) {
            self.env.borrow_mut().locals_count = n_locals;
        } else {
            panic!("Too many locals.");
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
        match (e1, e2) {
            (Expr::FnDecl {..}, Expr::FnDecl {..})    => Ordering::Equal,
            (Expr::FnDecl {..}, _)     => Ordering::Less,
            (_, Expr::FnDecl {..})     => Ordering::Greater,
            (_, _)                      => Ordering::Equal,
        }
        
    });
}

type TkIter<'l> = std::iter::Peekable<std::collections::vec_deque::Iter<'l, Token<'l>>>;

#[allow(dead_code)]
pub fn benchmark ()
{
    use crate::benchmarks::ITERATIONS;
    use super::lexer;
    use std::time::{ Instant, Duration };

    let program = std::fs::read_to_string("./code.lang").unwrap();
    let tokens = lexer::lex(&program);
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