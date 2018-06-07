use stream::Stream;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AstKind {
    AstOpPlus,
    AstOpMinus,
    AstInt,
    AstStr,
}

#[derive(Clone, Debug)]
pub struct Ast {
    pub kind: AstKind,
    ival: Option<u32>,
    sval: Option<String>,
    left: Option<Box<Ast>>,
    right: Option<Box<Ast>>,
}

impl Ast {
    pub fn make_ast_op(kind: AstKind, left: Box<Ast>, right: Box<Ast>) -> Self {
        Self {
            kind: kind,
            ival: None,
            sval: None,
            left: Some(left),
            right: Some(right),
        }
    }

    pub fn make_ast_int(val: u32) -> Self {
        Self {
            kind: AstKind::AstInt,
            ival: Some(val),
            sval: None,
            left: None,
            right: None,
        }
    }

    pub fn make_ast_str(val: String) -> Self {
        Self {
            kind: AstKind::AstStr,
            ival: None,
            sval: Some(val),
            left: None,
            right: None,
        }
    }

    pub fn read_number(mut n: u32, mut stream: Stream) -> (Ast, Stream) {
        loop {
            if let Some(c) = stream.next() {
                if c.is_whitespace() {
                    break;
                } else if !c.is_ascii_digit() {
                    return (Self::make_ast_int(n), stream.prev());
                }
                n = n * 10 + c.to_digit(10).unwrap();
            } else {
                return (Self::make_ast_int(n), stream);
            }
        }

        return (Self::make_ast_int(n), stream);
    }

    pub fn read_string(stream: Stream) -> (Ast, Stream) {
        let mut res = String::new();
        if stream.clone().last() != Some('"') {
            panic!("Unterminated string")
        }

        let mut n = 1;
        for c in stream.clone() {
            n += 1;
            if c == '"' {
                break;
            }
            res.push(c);
        }

        return (Self::make_ast_str(res), stream.jump(n));
    }

    pub fn read_expr2(mut stream: Stream, left: Self) -> (Ast, Stream) {
        let op: AstKind;
        stream.skip_space();
        match stream.next() {
            Some(c) => {
                op = match c {
                    '+' => AstKind::AstOpPlus,
                    '-' => AstKind::AstOpMinus,
                    _ => {
                        panic!("Operator expected, but got {:?}", c);
                    }
                };
                stream.skip_space();
                let (right, stream) = Self::read_prim(stream);
                return Self::read_expr2(
                    stream,
                    Self::make_ast_op(op, Box::new(left), Box::new(right)),
                );
            }
            None => return (left, stream),
        }
    }

    pub fn read_prim(mut stream: Stream) -> (Ast, Stream) {
        let c = stream.next().unwrap();
        if c.is_ascii_digit() {
            let n = c.to_digit(10).unwrap();
            return Self::read_number(n, stream);
        } else if c == '"' {
            return Self::read_string(stream);
        }
        panic!("Don't know how to handle {:?}", c)
    }

    pub fn read_expr(stream: Stream) -> (Ast, Stream) {
        let (stream, left) = Self::read_prim(stream);
        return Self::read_expr2(left, stream);
    }
}

pub fn print_quote(q: String) {
    for c in q.chars() {
        if c == '"' || c == '\\' {
            print!("\\");
        }
        print!("{}", c);
    }
}

pub fn emit_string(ast: Box<Ast>) {
    print!(
        "\t.data
        .mydata:
        \t.string \""
    );
    print_quote(ast.sval.unwrap());
    println!(
        "\"\n\t.text
        \t.global stringfn
        stringfn:
        \tlea .mydata(%rip), %rax
        \tret",
    );
}

pub fn ensure_intexpr(ast: Box<Ast>) {
    use self::AstKind::*;
    if ast.kind != AstOpPlus && ast.kind != AstOpMinus && ast.kind != AstInt {
        panic!("integer or binary operator expected");
    }
}

pub fn emit_intexpr(ast: Box<Ast>) {
    ensure_intexpr(ast.clone());
    if ast.clone().kind == AstKind::AstInt {
        println!("\tmov ${}, %eax", ast.ival.unwrap());
    } else {
        emit_binop(ast.clone());
    }
}

pub fn emit_binop(ast: Box<Ast>) {
    let op: &str;
    op = match ast.kind {
        AstKind::AstOpPlus => "add",
        AstKind::AstOpMinus => "sub",
        _ => panic!("invalid operand"),
    };
    emit_intexpr(ast.clone().left.unwrap());
    println!("\tmov %eax, %ebx");
    emit_intexpr(ast.clone().right.unwrap());
    println!("{} %ebx, %eax", op);
}

pub fn print_ast(ast: &Box<Ast>) {
    use self::AstKind::*;
    match ast.kind {
        AstOpPlus => {
            print!("(+ ");
            print_ast(&ast.clone().left.unwrap());
            print!(" ");
            print_ast(&ast.clone().right.unwrap());
            print!(")");
        }
        AstOpMinus => {
            print!("(- ");
            print_ast(&ast.clone().left.unwrap());
            print!(" ");
            print_ast(&ast.clone().right.unwrap());
            print!(")");
        }
        AstInt => print!("{}", ast.ival.unwrap()),
        AstStr => print_quote(ast.clone().sval.unwrap()),
    }
}
