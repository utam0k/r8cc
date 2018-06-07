extern crate r8cc;

use std::io;
use std::env;

use r8cc::stream::Stream;
use r8cc::ast;

fn compile(ast: ast::Ast) {
    if ast.kind == ast::AstKind::AstStr {
        ast::emit_string(Box::new(ast));
    } else {
        println!(
            "\t.text
            .global intfn
            intfn:"
        );
        ast::emit_intexpr(Box::new(ast));
        println!("ret");
    }
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

    // remove `\n`
    input.pop();

    let stream = Stream::from(input.clone());
    let (ast, _) = ast::Ast::read_expr(stream);

    let args = env::args();

    if args.len() > 1 {
        ast::print_ast(&Box::new(ast));
    } else {
        compile(ast);
    }

    Ok(())
}
