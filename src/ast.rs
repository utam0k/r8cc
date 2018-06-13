use CONTEXT;
use r8cc_io::{getc, ungetc, skip_space};

const REGS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
const MAX_ARGS: usize = 6;

#[derive(Clone, Debug)]
pub struct Var {
    name: String,
    pos: usize,
}

impl Var {
    pub fn new(name: String) -> Self {
        Self {
            name: name,
            pos: CONTEXT.lock().unwrap().get_vars_len() + 1,
        }
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
pub struct FuncCall {
    pub fname: String,
    pub args: Vec<Ast>,
    pub nargs: usize,
}

impl FuncCall {
    pub fn new(fname: String, args: Vec<Ast>) -> Self {
        Self {
            fname: fname,
            nargs: args.len(),
            args: args,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Str {
    sval: String,
    sid: usize,
}

impl Str {
    pub fn new(sval: String) -> Self {
        Self {
            sval: sval,
            sid: CONTEXT.lock().unwrap().get_strings_len() + 1,
        }
    }
}

#[derive(Clone, Debug)]
pub enum AstKind {
    AstOp(char, Box<Ast>, Box<Ast>),
    AstInt(u32),
    AstChar(char),
    AstStr(Str),
    AstVar(Var),
    AstFuncCall(FuncCall),
}

#[derive(Clone, Debug)]
pub struct Ast {
    pub kind: AstKind,
}

impl Ast {
    fn new(kind: AstKind) -> Self {
        match kind {
            AstKind::AstStr(ref str_val) => CONTEXT.lock().unwrap().push_string(str_val.clone()),
            AstKind::AstVar(ref var) => CONTEXT.lock().unwrap().push_var(var.clone()),
            _ => (),
        }
        Self { kind: kind }
    }

    pub fn emit_expr(&self) {
        match self.kind {
            AstKind::AstInt(ival) => print!("mov ${}, %eax\n\t", ival),
            AstKind::AstChar(c) => print!("mov ${}, %eax\n\t", c as i8),
            AstKind::AstVar(ref var) => print!("mov -{}(%rbp), %eax\n\t", var.pos * 4),
            AstKind::AstStr(ref ast_str) => print!("lea .s{}(%rip), %rax\n\t", ast_str.sid),
            AstKind::AstFuncCall(ref func_call) => {
                let args_len = func_call.args.len();

                for i in 1..args_len {
                    print!("push %{}\n\t", REGS[i]);
                }
                for arg in &func_call.args {
                    arg.emit_expr();
                    print!("push %rax\n\t");
                }
                for i in (0..args_len).rev() {
                    print!("pop %{}\n\t", REGS[i]);
                }
                print!("mov $0, %eax\n\t");
                print!("call {}\n\t", func_call.fname);
                for i in (1..args_len).rev() {
                    print!("pop %{}\n\t", REGS[i]);
                }
            }
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
                            AstKind::AstVar(ref var) => {
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
            AstChar(c) => print!("'{}'", c),
            AstVar(ref var) => print!("{}", var.name),
            AstStr(ref ast_str) => {
                print!("\"");
                print_quote(&ast_str.sval);
                print!("\"");
            }
            AstFuncCall(ref func_call) => {
                print!("{}(", func_call.fname);
                for i in 0..func_call.args.len() {
                    func_call.args[i].print_ast();
                    if i + 1 < func_call.args.len() {
                        print!(",");
                    }
                }
                print!(")");
            }
        }
    }
}

pub fn read_expr() -> Option<Ast> {
    if let Some(r) = read_expr2(0) {
        skip_space();
        if let Some(c) = getc() {
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

    skip_space();
    if let Some(ret_val) = read_prim() {;
        ast = ret_val;
    } else {
        return None;
    }

    loop {
        skip_space();
        let next_char = getc();
        match next_char {
            Some(c) => {
                let prec2 = priority(c);
                if prec2 < 0 || prec2 < prec {
                    ungetc();
                    return Some(ast);
                }

                skip_space();
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
            next_char = getc();
        }
        if let Some(c) = next_char {
            if c.is_whitespace() {
                break;
            } else if !c.is_ascii_digit() {
                ungetc();
                return Ast::new(AstKind::AstInt(n));
            }
            n = n * 10 + c.to_digit(10).unwrap();
        } else {
            return Ast::new(AstKind::AstInt(n));
        }
    }

    Ast::new(AstKind::AstInt(n))
}

fn read_string() -> Ast {
    let mut buffer = String::new();
    let mut next_char: Option<char>;

    loop {
        next_char = getc();
        if let Some(c) = next_char {
            if c == '"' {
                break;
            } else if c == '\\' {
                next_char = getc();
                if let Some(c2) = next_char {
                    buffer.push(c2);
                } else {
                    panic!("Unterminated \\");
                }
            }
            buffer.push(c);
        } else {
            panic!("Unterminated string");
        }
    }

    Ast::new(AstKind::AstStr(Str::new(buffer)))
}

fn read_char() -> Ast {
    let next_char: Option<char> = getc();
    match next_char {
        Some(c) => {
            if c == '\\' {
                if getc().is_none() {
                    panic!("Unterminated \\");
                }
            }
            if let Some(c2) = getc() {
                if c2 != '\'' {
                    panic!("Malformed char constant");
                }
            } else {
                panic!("Unterminated \\");
            }
            return Ast::new(AstKind::AstChar(c));
        }
        None => panic!("Unterminated string"),
    }
}

fn read_prim() -> Option<Ast> {
    let next_char: Option<char> = getc();
    if let Some(c) = next_char {
        if c.is_ascii_digit() {
            let n = c.to_digit(10).unwrap();
            return Some(read_number(n));
        } else if c == '"' {
            return Some(read_string());
        } else if c == '\'' {
            return Some(read_char());
        } else if c.is_alphabetic() {
            return Some(read_ident_or_func(c));
        }
        panic!("Don't know how to handle '{}'", c);
    }
    None
}


fn read_ident(c: char) -> String {
    let mut buf = String::new();
    buf.push(c);

    loop {
        skip_space();
        let next_char = getc();
        if let Some(c) = next_char {
            if !c.is_alphabetic() && !c.is_ascii_digit() {
                ungetc();
                break;
            }
            buf.push(c);
        }
    }
    buf
}

fn read_func_args(fname: String) -> Ast {
    let mut args: Vec<Ast> = Vec::new();
    let mut c: char;

    for i in 0..MAX_ARGS {
        skip_space();
        c = getc().unwrap();
        if c == ')' {
            break;
        }
        ungetc();
        args.push(read_expr2(0).unwrap());
        c = getc().unwrap();
        match c {
            ')' => break,
            ',' => skip_space(),
            _ => panic!("Unexpected character: '{}'", c),
        };
        if i == MAX_ARGS {
            panic!("Too many arguments: {}", fname);
        }
    }

    Ast::new(AstKind::AstFuncCall(FuncCall::new(fname, args)))
}

fn read_ident_or_func(c: char) -> Ast {
    let name: String = read_ident(c);
    skip_space();
    let c2: char = getc().unwrap();

    if c2 == '(' {
        return read_func_args(name);
    }
    ungetc();
    let v: Var;
    if let Some(v2) = find_var(&name) {
        v = v2.clone();
    } else {
        v = Var::new(name);
    };

    Ast::new(AstKind::AstVar(v))
}

fn print_quote(q: &String) {
    for c in q.chars() {
        if c == '"' || c == '\\' {
            print!("\\");
        }
        print!("{}", c);
    }
}

pub fn emit_data_section() {
    let strings = CONTEXT.lock().unwrap().get_strings().clone();
    if strings.is_empty() {
        return;
    }
    print!("\t.data\n");
    for p in strings {
        print!(".s{}:\n\t", p.sid);
        print!(".string \"");
        print_quote(&p.sval);
        print!("\"\n");
    }
    print!("\t");
}

fn priority(c: char) -> i8 {
    match c {
        '=' => 1,
        '+' | '-' => 2,
        '*' | '/' => 3,
        _ => -1,
    }
}
