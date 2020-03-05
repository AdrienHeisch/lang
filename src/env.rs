use crate::ast::Identifier;

pub struct Environment
{
    pub locals: [(Identifier, u8); 256], //TODO max locals ? (u8?)
    pub locals_count: u8,
    pub scope_depth: u8,
    // pub parent: Option<Box<Environment>>
}

impl Environment
{

    pub fn new (/* parent:Option<Box<Environment>> */) -> Self
    {
        Environment
        {
            locals: [(Default::default(), 0); 256],
            locals_count: 0,
            scope_depth: 0,
            // parent
        }
    }
    
    pub fn open_scope (&mut self)
    {
        if let Some(n) = self.scope_depth.checked_add(1) {
            self.scope_depth = n;
        } else {
            panic!("Too many locals.");
        }
    }

    pub fn close_scope (&mut self) -> u8
    {
        if let Some(n) = self.scope_depth.checked_sub(1) {
            self.scope_depth = n;
        } else {
            panic!("No scope to close.");
        }

        let n_locals = self.locals_count;
        for i in (0..self.locals_count as usize).rev()
        {
            let (_, depth) = self.locals[i];
            if depth > self.scope_depth {
                self.locals[i] = Default::default();
                self.locals_count -= 1;
            } else {
                break;
            }
        }
        n_locals - self.locals_count
    }

}