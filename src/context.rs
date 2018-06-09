use std::collections::LinkedList;

use ast::Var;

#[derive(Debug)]
pub struct Context {
    pub vars: LinkedList<Var>,
}

impl Context {
    pub fn new() -> Self {
        Self { vars: LinkedList::new() }
    }

    pub fn push_var(&mut self, v: Var) {
        self.vars.push_front(v);
    }

    pub fn get_vars(&self) -> &LinkedList<Var> {
        &self.vars
    }

    pub fn get_vars_len(&self) -> usize {
        self.vars.len()
    }
}
