use crate::{ast::Identifier, value::Type};

pub struct EnvError {
    pub msg: String,
}

#[derive(Clone, Debug)]
pub struct Environment {
    //TODO make it generic to contain crate::vm::compiler::IdentifierValue
    pub globals: Vec<Local>,
    pub locals: [Local; 256], //TODO max locals ? (u8?)
    pub locals_count: u8,     //TODO usize
    pub scope_depth: u8,      //TODO usize ?
    pub context: Context,
}

//TODO change name to be more general than "Local" (or create "Globals" ?)
#[derive(Clone, Default, Debug)]
pub struct Local {
    pub id: Identifier,
    pub t: Type,
    pub depth: u8,
}

#[derive(Clone, Copy, Debug)]
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
                t: Type::Fn(Box::new([Type::Int]), Box::new(Type::Void)),
                depth: 0,
            }),
            b"printmem" => Some(Local {
                id: *id,
                t: Type::Fn(Box::new([]), Box::new(Type::Void)),
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

    pub fn open_scope(&mut self) -> Result<(), EnvError> {
        if let Some(n) = self.scope_depth.checked_add(1) {
            self.scope_depth = n;
            Ok(())
        } else {
            Err(EnvError {
                msg: "Too many locals.".to_string(),
            })
        }
    }

    pub fn close_scope(&mut self) -> Result<u8, EnvError> {
        if let Some(n) = self.scope_depth.checked_sub(1) {
            self.scope_depth = n;
        } else {
            return Err(EnvError {
                msg: "No scope to close.".to_string(),
            });
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
        Ok(n_locals - self.locals_count)
    }

    #[allow(dead_code)]
    pub fn clear(&mut self) {
        self.scope_depth = 0;
        self.locals_count = 0;
    }
}
