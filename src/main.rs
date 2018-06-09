extern crate r8cc;

use std::io;
use std::env;

use r8cc::ast;

fn main() -> io::Result<()> {
    let wantast = env::args().len() > 1;
    if !wantast {
        print!(".text\n\t");
        print!(".global mymain\n");
        print!("mymain:\n\t");
    }

    loop {
        if let Some(ast) = ast::read_expr() {
            if wantast {
                ast.print_ast();
            } else {
                ast.emit_expr();
            }
        } else {
            break;
        };
    }

    if !wantast {
        print!("ret\n");
    }

    Ok(())
}
