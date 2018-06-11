pub mod stream;
pub mod ast;
pub mod context;

#[macro_use]
extern crate lazy_static;

use std::sync::Mutex;
use std::marker::Send;
use std::io::stdin;

unsafe impl Send for context::Context {}
unsafe impl Send for stream::Stream {}

lazy_static! {
    pub static ref CONTEXT: Mutex<context::Context> = {
        Mutex::new(context::Context::new())
    };

    static ref INPUT: Mutex<stream::Stream> = {
        let mut input = String::new();
        stdin().read_line(&mut input).unwrap();

        // remove `\n`
        input.pop();
        Mutex::new(stream::Stream::from(input))
    };
}

mod r8cc_io {
    use INPUT;

    pub fn getc() -> Option<char> {
        INPUT.lock().unwrap().next()
    }

    pub fn ungetc() {
        INPUT.lock().unwrap().prev()
    }

    pub fn skip_space() {
        INPUT.lock().unwrap().skip_space()
    }
}
