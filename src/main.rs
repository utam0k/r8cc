extern crate r8cc;

use std::io;
use std::env;

fn main() -> io::Result<()> {
    let wantast = env::args().len() > 1;
    let mut exprs: Vec<r8cc::Ast> = Vec::new();
    loop {
        if let Some(t) = r8cc::read_decl_or_stmt() {
            exprs.push(t);
        } else {
            break;
        }
    }
    if !wantast {
        r8cc::emit_data_section();
        print!(".text\n\t");
        print!(".global mymain\n");
        print!("mymain:\n\t");
        print!("push %rbp\n\t");
        print!("mov %rsp, %rbp\n\t");

        let mut vars = r8cc::CONTEXT.lock().unwrap().get_vars().clone();

        if !vars.is_empty() {
            print!("sub ${}, %rsp\n\t", vars.pop_front().unwrap().vpos * 8);
        }
    }

    for ast in exprs {
        if wantast {
            print!("{}", ast.to_string());
        } else {
            ast.emit_expr();
        }
    }

    if !wantast {
        print!("leave\n\t");
        print!("ret\n");
    }

    Ok(())
}
