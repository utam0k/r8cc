use CONTEXT;
use INPUT;

#[derive(Clone, Debug)]
pub struct Var {
    name: String,
    pos: usize,
}


impl Var {
    pub fn new(name: String) -> Self {
        let ret = Self {
            name: name,
            pos: CONTEXT.lock().unwrap().get_vars_len() + 1,
        };
        CONTEXT.lock().unwrap().push_var(ret.clone());
        ret
    }
}

fn find_var(name: &String) -> Option<Var> {
    let vars = CONTEXT.lock().unwrap().get_vars().clone();

    for v in vars {
        if &v.name == name {
            return Some(v.clone());
        }
    }
    None
}

#[derive(Clone, Debug)]
pub enum AstKind {
    AstOp(char, Box<Ast>, Box<Ast>),
    AstInt(u32),
    AstSym(Var),
}

#[derive(Clone, Debug)]
pub struct Ast {
    pub kind: AstKind,
}

impl Ast {
    fn new(kind: AstKind) -> Self {
        Self { kind: kind }
    }

    pub fn emit_expr(&self) {
        match self.kind {
            AstKind::AstInt(ival) => print!("mov ${}, %eax\n\t", ival),
            AstKind::AstSym(ref var) => print!("mov -{}(%rbp), %eax\n\t", var.pos * 4),
            _ => self.emit_binop(),
        }
    }

    fn emit_binop(&self) {
        let op: &str;
        op = match self.kind {
            AstKind::AstOp(kind, ref left, ref right) => {
                match kind {
                    '=' => {
                        right.emit_expr();
                        match left.kind {
                            AstKind::AstSym(ref var) => {
                                print!("mov %eax, -{}(%rbp)\n\t", var.pos * 4)
                            }
                            _ => panic!("invalid operand"),
                        }
                        return;
                    }
                    '+' => "add",
                    '-' => "sub",
                    '*' => "imul",
                    '/' => "",
                    _ => panic!("invalid operand"),
                }
            }
            _ => panic!("invalid operand"),
        };

        match self.kind {
            AstKind::AstOp(kind, ref left, ref right) => {
                left.emit_expr();
                print!("push %rax\n\t");
                right.emit_expr();
                if kind == '/' {
                    print!("mov %eax, %ebx\n\t");
                    print!("pop %rax\n\t");
                    print!("mov $0, %edx\n\t");
                    print!("idiv %ebx\n\t");
                } else {
                    print!("pop %rbx\n\t");
                    print!("{} %ebx, %eax\n\t", op);
                }
            }
            _ => panic!("invalid operand"),
        }
    }

    pub fn print_ast(&self) {
        use self::AstKind::*;
        match self.kind {
            AstOp(kind, ref left, ref right) => {
                print!("({} ", kind);
                left.print_ast();
                print!(" ");
                right.print_ast();
                print!(")");
            }
            AstInt(val) => print!("{}", val),
            AstSym(ref var) => print!("{}", var.name),
        }
    }
}

pub fn read_expr() -> Option<Ast> {
    if let Some(r) = read_expr2(0) {
        INPUT.lock().unwrap().skip_space();
        if let Some(c) = INPUT.lock().unwrap().next() {
            if c != ';' {
                panic!("Unterminated expression");
            }
            return Some(r);
        } else {
            return None;
        }
    }
    None
}


fn read_expr2(prec: i8) -> Option<Ast> {
    let mut ast: Ast;
    let mut op: AstKind;

    INPUT.lock().unwrap().skip_space();
    if let Some(ret_val) = read_prim() {;
        ast = ret_val;
    } else {
        return None;
    }

    loop {
        INPUT.lock().unwrap().skip_space();
        let next_char = INPUT.lock().unwrap().next();
        match next_char {
            Some(c) => {
                let prec2 = priority(c);
                if prec2 < 0 || prec2 < prec {
                    INPUT.lock().unwrap().prev();
                    return Some(ast);
                }

                INPUT.lock().unwrap().skip_space();
                if let Some(right) = read_expr2(prec2 + 1) {
                    op = AstKind::AstOp(c, Box::new(ast.clone()), Box::new(right));
                    ast = Ast::new(op);
                } else {
                    panic!("Unterminated expression");
                }
            }
            None => {
                return Some(ast);
            }
        }
    }
}

fn read_number(mut n: u32) -> Ast {
    loop {
        let next_char: Option<char>;
        {
            next_char = INPUT.lock().unwrap().next();
        }
        if let Some(c) = next_char {
            if c.is_whitespace() {
                break;
            } else if !c.is_ascii_digit() {
                INPUT.lock().unwrap().prev();
                return Ast::new(AstKind::AstInt(n));
            }
            n = n * 10 + c.to_digit(10).unwrap();
        } else {
            return Ast::new(AstKind::AstInt(n));
        }
    }

    Ast::new(AstKind::AstInt(n))
}

fn read_symbol(c: char) -> Ast {
    let mut buf = String::new();
    buf.push(c);

    loop {
        INPUT.lock().unwrap().skip_space();
        let next_char = INPUT.lock().unwrap().next();
        if let Some(c) = next_char {
            if !c.is_alphabetic() {
                INPUT.lock().unwrap().prev();
                break;
            }
            buf.push(c);
        }
    }

    let v: Var;
    if let Some(v2) = find_var(&buf) {
        v = v2.clone();
    } else {
        v = Var::new(buf.clone());
    };

    Ast::new(AstKind::AstSym(v))
}


fn read_prim() -> Option<Ast> {
    let next_char: Option<char>;
    {
        next_char = INPUT.lock().unwrap().next();
    }
    if let Some(c) = next_char {
        if c.is_ascii_digit() {
            let n = c.to_digit(10).unwrap();
            return Some(read_number(n));
        } else if c.is_alphabetic() {
            return Some(read_symbol(c));
        }
    }
    None
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

fn priority(c: char) -> i8 {
    match c {
        '=' => 1,
        '+' | '-' => 2,
        '*' | '/' => 3,
        _ => -1,
    }
}
