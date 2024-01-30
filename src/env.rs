use crate::{
    ast::Identifier,
    value::Type,
};

#[derive(Clone)]
pub struct Environment {
    pub globals: Vec<Local>,
    pub locals: [Local; 256], //TODO max locals ? (u8?)
    pub locals_count: u8,
    pub scope_depth: u8,
    pub context: Context,
}

//TODO change name to be more general than "Local" (or create "Globals" ?)
#[derive(Clone, Default)]
pub struct Local {
    pub id: Identifier,
    pub t: Type,
    pub depth: u8,
}

#[derive(Clone, Copy)]
pub enum Context {
    TopLevel,
    Function,
}

impl Environment {
    pub fn new(context: Context) -> Self {
        Environment {
            globals: Vec::new(),
            locals: [(); 256].map(|_| Local::default()),
            locals_count: 0,
            scope_depth: 0,
            context,
        }
    }

    pub fn get_from_id(&self, id: &Identifier) -> Option<Local> {
        match id {
            b"print\0\0\0" => Some(Local {
                id: *id,
                t: Type::Fn(
                    Box::new([Type::Int]),
                    Box::new(Type::Void),
                ),
                depth: 0,
            }),
            b"printmem" => Some(Local {
                id: *id,
                t: Type::Fn(Box::new([]), Box::new(Type::Void),),
                depth: 0,
            }),
            id => {
                for local in self.locals.iter().take(self.locals_count.into()).rev() {
                    if *id == local.id {
                        return Some(local.clone());
                    }
                }

                for global in self.globals.iter() {
                    if *id == global.id {
                        return Some(global.clone());
                    }
                }

                None
            }
        }
    }

    pub fn open_scope(&mut self) {
        if let Some(n) = self.scope_depth.checked_add(1) {
            self.scope_depth = n;
        } else {
            panic!("Too many locals.");
        }
    }

    pub fn close_scope(&mut self) -> u8 {
        if let Some(n) = self.scope_depth.checked_sub(1) {
            self.scope_depth = n;
        } else {
            panic!("No scope to close.");
        }

        let n_locals = self.locals_count;
        for i in (0..self.locals_count as usize).rev() {
            let Local { depth, .. } = self.locals[i];
            if depth > self.scope_depth {
                self.locals_count -= 1;
            } else {
                break;
            }
        }
        n_locals - self.locals_count
    }

    #[allow(dead_code)]
    pub fn clear(&mut self) {
        self.scope_depth = 0;
        self.locals_count = 0;
    }
}
