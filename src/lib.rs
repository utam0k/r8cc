pub mod stream;
pub mod ast;
pub mod context;

#[macro_use]
extern crate lazy_static;

use std::io;
use std::sync::Mutex;
use std::marker::Send;

unsafe impl Send for context::Context {}
unsafe impl Send for stream::Stream {}

lazy_static! {
    pub static ref CONTEXT: Mutex<context::Context> = {
        Mutex::new(context::Context::new())
    };

    pub static ref INPUT: Mutex<stream::Stream> = {
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        // remove `\n`
        input.pop();
        Mutex::new(stream::Stream::from(input))
    };
}
