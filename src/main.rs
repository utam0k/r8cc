extern crate r8cc;

use std::io;
use std::env;

use r8cc::ast;

fn main() -> io::Result<()> {
    let wantast = env::args().len() > 1;
    let mut exprs: Vec<ast::Ast> = Vec::new();
    loop {
        if let Some(t) = ast::read_expr() {
            exprs.push(t);
        } else {
            break;
        }
    }
    if !wantast {
        ast::emit_data_section();
        print!(".text\n\t");
        print!(".global mymain\n");
        print!("mymain:\n\t");
    }

    for ast in exprs {
        if wantast {
            ast.print_ast();
        } else {
            ast.emit_expr();
        }
    }

    if !wantast {
        print!("ret\n");
    }

    Ok(())
}
