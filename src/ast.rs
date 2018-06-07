use stream::Stream;

#[derive(Clone, Debug)]
pub enum AstKind {
    AstOpPlus(Box<Ast>, Box<Ast>),
    AstOpMinus(Box<Ast>, Box<Ast>),
    AstInt(u32),
    AstStr(String),
}

#[derive(Clone, Debug)]
pub struct Ast {
    pub kind: AstKind,
}

impl Ast {
    fn new(kind: AstKind) -> Self {
        Self { kind: kind }
    }

    pub fn read_expr(stream: Stream) -> (Ast, Stream) {
        let (stream, left) = Self::read_prim(stream);
        return Self::read_expr2(left, stream);
    }

    fn read_number(mut n: u32, mut stream: Stream) -> (Ast, Stream) {
        loop {
            if let Some(c) = stream.next() {
                if c.is_whitespace() {
                    break;
                } else if !c.is_ascii_digit() {
                    return (Self::new(AstKind::AstInt(n)), stream.prev());
                }
                n = n * 10 + c.to_digit(10).unwrap();
            } else {
                return (Self::new(AstKind::AstInt(n)), stream);
            }
        }

        return (Self::new(AstKind::AstInt(n)), stream);
    }

    fn read_string(mut stream: Stream) -> (Ast, Stream) {
        let mut res = String::new();
        if stream.end() != Some(&'"') {
            panic!("Unterminated string")
        }

        loop {
            if let Some(c) = stream.next() {
                if c == '"' {
                    break;
                }
                res.push(c);
            } else {
                break;
            }
        }

        return (Self::new(AstKind::AstStr(res)), stream);
    }

    fn read_expr2(mut stream: Stream, left: Self) -> (Ast, Stream) {
        let op: AstKind;
        stream.skip_space();
        match stream.next() {
            Some(c) => {
                stream.skip_space();
                let (right, stream) = Self::read_prim(stream);
                op = match c {
                    '+' => AstKind::AstOpPlus(Box::new(left), Box::new(right)),
                    '-' => AstKind::AstOpMinus(Box::new(left), Box::new(right)),
                    _ => {
                        panic!("Operator expected, but got {:?}", c);
                    }
                };
                return Self::read_expr2(stream, Self::new(op));
            }
            None => return (left, stream),
        }
    }

    fn read_prim(mut stream: Stream) -> (Ast, Stream) {
        let c = stream.next().unwrap();
        if c.is_ascii_digit() {
            let n = c.to_digit(10).unwrap();
            return Self::read_number(n, stream);
        } else if c == '"' {
            return Self::read_string(stream);
        }
        panic!("Don't know how to handle {:?}", c)
    }
}

fn print_quote(q: String) {
    for c in q.chars() {
        if c == '"' || c == '\\' {
            print!("\\");
        }
        print!("{}", c);
    }
}

pub fn emit_string(val: String) {
    print!("\t.data\n");
    print!(".mydata:\n\t");
    print!(".string \"");
    print_quote(val);
    print!("\"\n\t");
    print!(".text\n\t");
    print!(".global stringfn\n");
    print!("stringfn:\n\t");
    print!("lea .mydata(%rip), %rax\n\t");
    print!("ret\n");
}

pub fn emit_intexpr(ast: &Box<Ast>) {
    match ast.kind {
        AstKind::AstInt(val) => print!("mov ${}, %eax\n\t", val),
        AstKind::AstOpPlus(_, _) |
        AstKind::AstOpMinus(_, _) => emit_binop(&ast),
        _ => panic!("integer or binary operator expected"),
    }
}

fn emit_binop(ast: &Box<Ast>) {
    let op: &str;
    op = match ast.kind {
        AstKind::AstOpPlus(_, _) => "add",
        AstKind::AstOpMinus(_, _) => "sub",
        _ => panic!("invalid operand"),
    };
    match ast.kind {
        AstKind::AstOpPlus(ref left, ref right) => {
            emit_intexpr(&left);
            print!("mov %eax, %ebx\n\t");
            emit_intexpr(&right);
        }
        AstKind::AstOpMinus(ref left, ref right) => {
            emit_intexpr(&left);
            print!("mov %eax, %ebx\n\t");
            emit_intexpr(&right);
        }
        _ => panic!("invalid operand"),
    }
    print!("{} %ebx, %eax\n\t", op);
}

pub fn print_ast(ast: &Box<Ast>) {
    use self::AstKind::*;
    match ast.kind {
        AstOpPlus(ref left, ref right) => {
            print!("(+ ");
            print_ast(&left);
            print!(" ");
            print_ast(&right);
            print!(")");
        }
        AstOpMinus(ref left, ref right) => {
            print!("(- ");
            print_ast(&left);
            print!(" ");
            print_ast(&right);
            print!(")");
        }
        AstInt(val) => print!("{}", val),
        AstStr(ref val) => print_quote(val.to_string()),
    }
}
