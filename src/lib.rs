pub mod context;
pub mod lex;
pub mod stream;

#[macro_use]
extern crate lazy_static;

use std::io::stdin;
use std::marker::Send;
use std::sync::Mutex;

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

    pub fn get_nonspace() -> Option<char> {
        INPUT.lock().unwrap().get_nonspace()
    }
}

const REGS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
const MAX_ARGS: usize = 6;

#[derive(Clone, Debug)]
pub struct Var {
    name: String,
    pub vpos: usize,
    ctype: Ctype,
}

impl Var {
    pub fn new(ctype: Ctype, name: String) -> Self {
        Self {
            name: name,
            vpos: CONTEXT.lock().unwrap().get_vars_len() + 1,
            ctype: ctype,
        }
    }
}

fn find_var(name: &String) -> Option<Var> {
    let vars = CONTEXT.lock().unwrap().get_vars().to_owned();

    for v in vars {
        if &v.name == name {
            return Some(v);
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

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Ctype {
    Void,
    Int(i32),
    Char(char),
    Array(Vec<Ctype>, usize),
    Ptr(Box<Ctype>),
}

impl Ctype {
    pub fn as_string(&self) -> String {
        use Ctype::*;
        match self {
            Void => "void".into(),
            Int(_) => "int".into(),
            Char(_) => "char".into(),
            Array(ptrs, _) => {
                let mut s = String::new();
                for p in ptrs {
                    s = s + &p.as_string();
                }
                s + "[]"
            }
            Ptr(ctype) => format!("{}*", ctype.as_string()),
        }
    }

    pub fn ctype_size(&self) -> usize {
        1 << self.ctype_shift()
    }

    pub fn ctype_shift(&self) -> usize {
        use Ctype::*;
        match self {
            Char(_) => 0,
            Int(_) => 2,
            _ => 3,
        }
    }
}

#[derive(Clone, Debug)]
pub enum AstKind {
    Binop(char, Ctype, Box<Ast>, Box<Ast>),
    Uop(Box<AstKind>, Ctype, Box<Ast>),
    Literal(Ctype),
    Var(Var),
    FuncCall(FuncCall),
    Decl(Box<Ast>, Box<Ast>),
    Addr(Box<Ast>, Ctype),
    Deref(Box<Ast>, Ctype),
}

macro_rules! matches(
    ($e:expr, $p:pat) => (
        match $e {
            $p => true,
            _ => false
        }
    )
);

#[derive(Clone, Debug)]
pub struct Ast {
    pub kind: AstKind,
}

/// 1. Call read_expr() for each ';'.
/// 2. Call emit_data_section().
/// 3. Call emit_expr() or print_ast().
impl Ast {
    fn new(kind: AstKind) -> Self {
        match kind {
            AstKind::Literal(ref ctype) => match ctype {
                Ctype::Array(c_chars, _) => {
                    let mut s = String::new();
                    for c_char in c_chars {
                        match c_char {
                            Ctype::Char(c) => s.push(c.clone()),
                            _ => unreachable!(),
                        }
                    }
                    CONTEXT.lock().unwrap().push_string(s);
                }
                _ => (),
            },
            AstKind::Var(ref var) => CONTEXT.lock().unwrap().push_var(var.clone()),
            _ => (),
        }
        Self { kind: kind }
    }

    fn make_binop(kind: char, ctype: Ctype, left: Ast, right: Ast) -> Self {
        Self::new(AstKind::Binop(kind, ctype, Box::new(left), Box::new(right)))
    }

    fn make_int(val: i32) -> Self {
        Self::new(AstKind::Literal(Ctype::Int(val)))
    }

    fn make_char(c: char) -> Self {
        Self::new(AstKind::Literal(Ctype::Char(c)))
    }

    fn make_string(sval: String) -> Self {
        let sid = CONTEXT.lock().unwrap().get_strings_len();
        let mut v = vec![];
        for c in sval.chars() {
            v.push(Ctype::Char(c));
        }
        Self::new(AstKind::Literal(Ctype::Array(v, sid)))
    }

    pub fn to_string(&self) -> String {
        self.to_string_int()
    }

    pub fn emit_expr(&self) {
        use self::AstKind::*;
        use self::Ctype::*;

        match self.kind {
            Literal(ref ctype) => match ctype {
                Int(ival) => print!("mov ${}, %rax\n\t", ival),
                Char(c) => print!("mov ${}, %rax\n\t", c.to_owned() as i8),
                Array(_, sid) => print!("lea .s{}(%rip), %rax\n\t", sid),
                _ => panic!("internal error"),
            },
            Var(ref var) => match var.ctype.ctype_size() {
                1 => {
                    print!("mov $0, %eax\n\t");
                    print!("mov -{}(%rbp), %al\n\t", var.vpos * 8);
                }
                4 => print!("mov -{}(%rbp), %eax\n\t", var.vpos * 8),
                8 => print!("mov -{}(%rbp), %rax\n\t", var.vpos * 8),
                _ => panic!("internal error"),
            },
            FuncCall(ref func_call) => {
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
            Decl(ref decl_var, ref decl_init) => decl_var.emit_assign(decl_init),
            Addr(ref operand, _) => {
                assert!(matches!(operand.kind, AstKind::Var(_)));
                match operand.kind {
                    AstKind::Var(ref var) => print!("lea -{}(%rbp), %rax\n\t", var.vpos * 8),
                    _ => (),
                }
            }
            Deref(ref operand, ref p_ctype) => {
                assert!(matches!(operand.get_ctype(), Some(Ctype::Ptr(_))));
                let mut reg;
                match p_ctype {
                    Ctype::Ptr(ctype) => {
                        reg = match ctype.ctype_size() {
                            1 => "%bl",
                            4 => "%ebx",
                            8 => "%rbx",
                            _ => panic!("internal error"),
                        };
                    }
                    _ => unreachable!(),
                }
                operand.emit_expr();
                print!("mov $0, %ebx\n\t");
                print!("mov (%rax), {}\n\t", reg);
                print!("mov %rbx, %rax\n\t");
            }
            _ => self.emit_binop(),
        }
    }

    fn emit_assign(&self, value: &Box<Self>) {
        value.emit_expr();
        match self.kind {
            AstKind::Var(ref var) => print!("mov %rax, -{}(%rbp)\n\t", var.vpos * 8),
            _ => panic!("invalid operand"),
        }
    }

    fn emit_binop(&self) {
        let op: &str;
        if let Some(ctype) = self.get_ctype() {
            if matches!(ctype, Ctype::Ptr(_)) {
                match self.kind {
                    AstKind::Binop(_, _, ref left, ref right) => {
                        emit_pointer_arith(self, left, right);
                        return;
                    }
                    _ => unreachable!(),
                }
            }
        }
        op = match self.kind {
            AstKind::Binop(kind, _, ref left, ref right) => match kind {
                '=' => {
                    left.emit_assign(right);
                    return;
                }
                '+' => "add",
                '-' => "sub",
                '*' => "imul",
                '/' => "",
                _ => panic!("invalid operand"),
            },
            _ => panic!("invalid operand"),
        };

        match self.kind {
            AstKind::Binop(kind, _, ref left, ref right) => {
                left.emit_expr();
                print!("push %rax\n\t");
                right.emit_expr();
                if kind == '/' {
                    print!("mov %rax, %rbx\n\t");
                    print!("pop %rax\n\t");
                    print!("mov $0, %edx\n\t");
                    print!("idiv %rbx\n\t");
                } else {
                    print!("pop %rbx\n\t");
                    print!("{} %rbx, %rax\n\t", op);
                }
            }
            _ => panic!("invalid operand"),
        }
    }

    pub fn to_string_int(&self) -> String {
        use self::AstKind::*;
        let mut buf = String::new();
        match self.kind {
            Binop(kind, _, ref left, ref right) => {
                buf.push_str(&format!(
                    "({} {} {})",
                    kind,
                    left.to_string_int(),
                    right.to_string_int()
                ));
            }
            Literal(ref ctype) => match ctype {
                Ctype::Int(val)  => buf.push_str(&format!("{}", val)),
                Ctype::Char(c) => buf.push_str(&format!("'{}'", c)),
                Ctype::Array(ctype_chars, _) => {
                    let mut s = String::new();
                    for ctype_char in ctype_chars {
                        match ctype_char {
                            Ctype::Char(c) => s.push(c.clone()),
                            _ => unreachable!(),
                        }
                    }
                    buf.push_str(&format!("\"{}\"", quote(&s)));
                }
                _ => panic!("literal expected"),
            },
            Var(ref var) => buf.push_str(&var.name),
            FuncCall(ref func_call) => {
                buf.push_str(&format!("{}(", func_call.fname));
                for i in 0..func_call.args.len() {
                    buf.push_str(&func_call.args[i].to_string());
                    if i + 1 < func_call.args.len() {
                        buf.push(',');
                    }
                }
                buf.push(')');
            }
            Decl(ref decl_var, ref decl_init) => {
                let (ctype, vname) = match decl_var.kind {
                    AstKind::Var(ref var) => (&var.ctype, &var.name),
                    _ => panic!("variable expected"),
                };

                buf.push_str(&format!(
                    "(decl {} {} {})",
                    ctype.as_string(),
                    vname,
                    decl_init.to_string()
                ));
            }
            Addr(ref operand, _) => {
                buf.push_str(&format!("(& {})", operand.to_string()));
            }
            Deref(ref operand, _) => {
                buf.push_str(&format!("(* {})", operand.to_string()));
            }
            _ => panic!("No implement"),
        };

        buf
    }

    pub fn ensure_lvalue(&self) -> bool {
        match self.kind {
            AstKind::Var(_) => true,
            _ => panic!("variable expected"),
        }
    }

    pub fn get_ctype(&self) -> Option<&Ctype> {
        use AstKind::*;
        match self.kind {
            Binop(_, ref ctype, ..) | Literal(ref ctype) => Some(ctype),
            Var(ref var) => Some(&var.ctype),
            Addr(_, ref ctype) => Some(&ctype),
            Deref(_, ref ctype) => Some(&ctype),
            _ => None,
        }
    }
}

fn result_type_int(op: char, a_type: &Ctype, b_type: &Ctype) -> Ctype {
    use Ctype::*;
    let types;
    if a_type > b_type {
        types = (b_type, a_type);
    } else {
        types = (a_type, b_type);
    }

    if matches!(b_type, Ctype::Ptr(_)) {
        if op != '+' && op != '-' {
            error(types, op);
        }
        if !matches!(a_type, Ctype::Ptr(_)) {
            return b_type.clone();
        }
    }

    match types.0 {
        Void => error(types, op),
        Int(a) => return Int(a.clone()),
        Char(a) => return Int(a.clone() as i32),
        Array(_, _) => return result_type_int(op, &Ptr(Box::new(types.0.clone())), &types.1),
        Ptr(a_ptr) => match types.1 {
            Ptr(b_ptr) => return Ptr(Box::new(result_type_int(op, a_ptr, b_ptr))),
            _ => panic!("Internal Error"),
        },
    }

    unreachable!();

    fn error((a, b): (&Ctype, &Ctype), op: char) {
        panic!("incompatible operands: {:?} and {:?} for {}", a, b, op);
    }
}

fn result_type(op: char, a: &Ast, b: &Ast) -> Ctype {
    result_type_int(op, a.get_ctype().unwrap(), b.get_ctype().unwrap())
}

fn read_expr(prec: i8) -> Option<Ast> {
    let mut ast: Ast;

    if let Some(ret_val) = read_unary_expr() {
        ast = ret_val;
    } else {
        return None; // Reach ';'.
    }

    loop {
        let tok = lex::read_token().unwrap();
        match tok {
            lex::Token::TtypePunct(punct) => {
                let prec2 = priority(punct);
                // Not =, +, -, * and / || priority of arithmetic operation
                if prec2 < 0 || prec2 < prec {
                    tok.unget_token();
                    return Some(ast);
                }

                // Exmple: 1+2+3;
                // Ast {
                //     kind: Binop('+',
                //                 Ast { kind: Binop('+',
                //                                   Ast { kind: AstInt(1) },
                //                                   Ast { kind: AstInt(2) })
                //                     },
                //                 Ast { kind: AstInt(3) })
                // }
                if tok.is_punct('=') {
                    ast.ensure_lvalue();
                }
                if let Some(rest) = read_expr(prec2 + if is_right_assoc(punct) { 0 } else { 1 }) {
                    let ctype = result_type(punct, &ast, &rest);
                    ast = Ast::make_binop(punct, ctype, ast, rest);
                } else {
                    panic!("Unterminated expression");
                }
            }
            _ => {
                tok.unget_token();
                return Some(ast);
            }
        }
    }
}

fn read_prim() -> Option<Ast> {
    use lex::Token::*;
    if let Some(token) = lex::read_token() {
        return Some(match token {
            TtypeIdent(sval) => read_ident_or_func(sval),
            TtypeInt(ival) => Ast::make_int(ival as i32),
            TtypeChar(c) => Ast::make_char(c),
            TtypeString(sval) => Ast::make_string(sval),
            TtypePunct(punct) => panic!("unexpected character: '{}'", punct),
        });
    } else {
        return None;
    }
}
fn read_func_args(fname: String) -> Ast {
    let mut args: Vec<Ast> = Vec::new();
    let mut tok: lex::Token;

    for i in 0..MAX_ARGS {
        tok = lex::read_token().unwrap();
        if tok.is_punct(')') {
            break;
        }
        tok.unget_token();
        args.push(read_expr(0).unwrap());
        tok = lex::read_token().unwrap();
        if tok.is_punct(')') {
            break;
        } else if !tok.is_punct(',') {
            panic!("Unexpected token: '{}'", tok.as_string());
        }

        if i == MAX_ARGS {
            panic!("Too many arguments: {}", fname);
        }
    }

    Ast::new(AstKind::FuncCall(FuncCall::new(fname, args)))
}

fn read_unary_expr() -> Option<Ast> {
    let tok = lex::read_token().unwrap();
    if tok.is_punct('&') {
        let operand = read_unary_expr().unwrap();
        operand.ensure_lvalue();
        let ctype = operand.get_ctype().unwrap().clone();
        return Some(Ast::new(AstKind::Addr(
            Box::new(operand),
            Ctype::Ptr(Box::new(ctype)),
        )));
    } else if tok.is_punct('*') {
        let operand = read_unary_expr().unwrap();
        let ctype = operand.get_ctype().cloned();
        if let Some(Ctype::Ptr(ptr)) = ctype {
            return Some(Ast::new(AstKind::Deref(Box::new(operand), Ctype::Ptr(ptr))));
        } else {
            panic!("pointer type expected, but got {}", operand.to_string());
        }
    }

    tok.unget_token();
    read_prim()
}

fn read_ident_or_func(name: String) -> Ast {
    let tok = lex::read_token().unwrap();
    if tok.is_punct('(') {
        return read_func_args(name);
    }
    tok.unget_token();
    if let Some(v) = find_var(&name) {
        Ast::new(AstKind::Var(v))
    } else {
        panic!("Undefined varaible: {}", name);
    }
}

fn read_decl() -> Ast {
    let mut ctype = lex::read_token().unwrap().get_ctype().unwrap();
    let mut tok;
    loop {
        tok = lex::read_token().unwrap();
        if !tok.is_punct('*') {
            break;
        }
        ctype = Ctype::Ptr(Box::new(ctype));
    }

    match tok {
        lex::Token::TtypeIdent(sval) => {
            let var = Ast::new(AstKind::Var(Var::new(ctype, sval)));
            lex::expect('=');
            let init = read_expr(0).unwrap();
            Ast::new(AstKind::Decl(Box::new(var), Box::new(init)))
        }
        _ => panic!("Identifier expected, but got {}", tok.as_string()),
    }
}

pub fn read_decl_or_stmt() -> Option<Ast> {
    if let Some(tok) = lex::peek_token() {
        let r = if tok.is_type_keyword() {
            Some(read_decl())
        } else {
            read_expr(0)
        };
        if let Some(tok) = lex::read_token() {
            if !tok.is_punct(';') {
                panic!("Unterminated expression: {}", tok.as_string());
            }
        };
        r
    } else {
        None
    }
}

fn quote(q: &String) -> String {
    let mut buf = String::new();
    for c in q.chars() {
        if c == '"' || c == '\\' {
            buf.push_str("\\");
        }
        buf.push(c);
    }
    buf
}

pub fn emit_data_section() {
    let con = CONTEXT.lock().unwrap();
    let strings = con.get_strings();
    if strings.is_empty() {
        return;
    }
    print!("\t.data\n");
    for (p, sid) in strings {
        print!(".s{}:\n\t", sid);
        print!(".string \"{}\"\n", quote(&p));
    }
    print!("\t");
}

fn emit_pointer_arith(_op: &Ast, left: &Ast, right: &Ast) {
    assert!(matches!(left.get_ctype(), Some(Ctype::Ptr(_))));
    left.emit_expr();
    print!("push %rax\n\t");
    right.emit_expr();
    let shift = left.get_ctype().unwrap().ctype_shift();
    if shift > 0 {
        print!("sal $%{}, %rax\n\t", shift);
    }
    print!("mov %rax, %rbx\n\t");
    print!("pop %rax\n\t");
    print!("add %rbx, %rax\n\t");
}

fn is_right_assoc(op: char) -> bool {
    op == '='
}

fn priority(c: char) -> i8 {
    match c {
        '=' => 1,
        '+' | '-' => 2,
        '*' | '/' => 3,
        _ => -1,
    }
}
