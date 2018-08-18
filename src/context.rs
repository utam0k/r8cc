use std::collections::LinkedList;

use Var;

#[derive(Debug)]
pub struct Context {
    pub vars: LinkedList<Var>,
    pub strings: LinkedList<(String, usize)>,
    pub sid: usize,
}

impl Context {
    pub fn new() -> Self {
        Self {
            vars: LinkedList::new(),
            strings: LinkedList::new(),
            sid: 0,
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

    pub fn push_string(&mut self, s: String) {
        self.strings.push_front((s, self.sid));
        self.sid += 1;
    }

    pub fn get_strings(&self) -> &LinkedList<(String, usize)> {
        &self.strings
    }

    pub fn get_strings_len(&self) -> usize {
        self.strings.len()
    }
}
