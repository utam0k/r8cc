use std::collections::LinkedList;

use Var;
use Str as AstStr;

#[derive(Debug)]
pub struct Context {
    pub vars: LinkedList<Var>,
    pub strings: LinkedList<AstStr>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            vars: LinkedList::new(),
            strings: LinkedList::new(),
        }
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

    pub fn push_string(&mut self, s: AstStr) {
        self.strings.push_front(s);
    }

    pub fn get_strings(&self) -> &LinkedList<AstStr> {
        &self.strings
    }

    pub fn get_strings_len(&self) -> usize {
        self.strings.len()
    }
}
