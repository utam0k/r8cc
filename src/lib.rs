pub mod stream;
pub mod lex;
pub mod context;

#[macro_use]
extern crate lazy_static;

use std::sync::Mutex;
use std::marker::Send;
use std::io::stdin;
use std::mem;

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

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Ctype {
    Void,
    Int(Option<i32>),
    Char(Option<char>),
    Str(Option<Str>),
    Ptr(Box<Ctype>),
}

impl Ctype {
    pub fn as_string(&self) -> String{
        use Ctype::*;
        match self {
            Void => "void".into(),
            Int(_) => "int".into(),
            Char(_) => "char".into(),
            Str(_) => "string".into(),
            Ptr(ctype) => format!("{}*", ctype.as_string()),
        }
    }
}

#[derive(Clone, Debug)]
pub enum AstKind {
    AstBinop(char, Ctype, Box<Ast>, Box<Ast>),
    AstUop(Box<AstKind>, Ctype, Box<Ast>),
    AstLiteral(Ctype),
    AstVar(Var),
    AstFuncCall(FuncCall),
    AstDecl(Box<Ast>, Box<Ast>),
    AstAddr(Box<Ast>, Ctype),
    AstDeref(Box<Ast>, Ctype),
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
            AstKind::AstLiteral(ref ctype) => 
                match ctype {
                    Ctype::Str(str_val) => CONTEXT.lock().unwrap().push_string(str_val.clone().unwrap()),
                    _ => ()
                }
            AstKind::AstVar(ref var) => CONTEXT.lock().unwrap().push_var(var.clone()),
            _ => (),
        }
        Self { kind: kind }
    }

    fn make_binop(kind: char, ctype: Ctype, left: Ast, right: Ast) -> Self {
        Self::new(AstKind::AstBinop(kind, ctype, Box::new(left), Box::new(right)))
    }

    fn make_int(val: i32) -> Self {
        Self::new(AstKind::AstLiteral(Ctype::Int(Some(val))))
    }

    fn make_char(c: char) -> Self {
        Self::new(AstKind::AstLiteral(Ctype::Char(Some(c))))
    }

    fn make_string(sval: String) -> Self {
        Self::new(AstKind::AstLiteral(Ctype::Str(Some(Str::new(sval)))))
    }

    pub fn to_string(&self) -> String {
        self.to_string_int()
    }

    pub fn emit_expr(&self) {
        use self::AstKind::*;
        use self::Ctype::*;

        match self.kind {
            AstLiteral(ref ctype) => {
                match ctype {
                    Int(Some(ival))=> print!("mov ${}, %rax\n\t", ival),
                    Char(Some(c))=> print!("mov ${}, %rax\n\t", c.to_owned() as i8),
                    Str(Some(ast_str)) => print!("lea .s{}(%rip), %rax\n\t", ast_str.sid),
                    _ => panic!("internal error"),
                }
            }
            AstVar(ref var) => print!("mov -{}(%rbp), %rax\n\t", var.vpos * 8),
            AstFuncCall(ref func_call) => {
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
                print!("mov $0, %rax\n\t");
                print!("call {}\n\t", func_call.fname);
                for i in (1..args_len).rev() {
                    print!("pop %{}\n\t", REGS[i]);
                }
            }
           AstDecl(ref decl_var, ref decl_init) => decl_var.emit_assign(decl_init),
           AstAddr(ref operand, _) => {
               assert!(matches!(operand.kind, AstKind::AstVar(_)));
               match operand.kind {
                   AstKind::AstVar(ref var)=> print!("lea -{}(%rbp), %rax\n\t", var.vpos * 8),
                   _ => (),
               }
           }
           AstDeref(ref operand, _) => {
               assert!(matches!(operand.get_ctype(), Some(Ctype::Ptr(_))));
               operand.emit_expr();
               print!("mov (%rax), %rax\n\t");
           }
           _ => self.emit_binop(),
        }
    }

    fn emit_assign(&self, value: &Box<Self>) {
        value.emit_expr();
        match self.kind {
            AstKind::AstVar(ref var) => print!("mov %rax, -{}(%rbp)\n\t", var.vpos * 8),
            _ => panic!("invalid operand"),
        }
    }

    fn emit_binop(&self) {
        let op: &str;
        op = match self.kind {
            AstKind::AstBinop(kind, _, ref left, ref right) => {
                match kind {
                    '=' => {
                        left.emit_assign(right);
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
            AstKind::AstBinop(kind, _, ref left, ref right) => {
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
            AstBinop(kind, _, ref left, ref right) => {
                buf.push_str(&format!(
                    "({} {} {})",
                    kind,
                    left.to_string_int(),
                    right.to_string_int()
                ));
            }
            AstLiteral(ref ctype) => match ctype {
                Ctype::Int(val) => buf.push_str(&format!("{}", val.unwrap())),
                Ctype::Char(c) => buf.push_str(&format!("'{}'", c.unwrap())),
                Ctype::Str(ast_str) => {
                    buf.push_str(&format!("\"{}\"", quote(&ast_str.clone().unwrap().sval)));
                }
                _ => panic!("literal expected"),
            }
            AstVar(ref var) => buf.push_str(&var.name),
            AstFuncCall(ref func_call) => {
                buf.push_str(&format!("{}(", func_call.fname));
                for i in 0..func_call.args.len() {
                    buf.push_str(&func_call.args[i].to_string());
                    if i + 1 < func_call.args.len() {
                        buf.push(',');
                    }
                }
                buf.push(')');
            }
            AstDecl(ref decl_var, ref decl_init) => {
                let (ctype, vname) = match decl_var.kind {
                    AstKind::AstVar(ref var) => (&var.ctype, &var.name),
                    _ => panic!("variable expected"),
                };

                buf.push_str(&format!(
                    "(decl {} {} {})",
                    ctype.as_string(),
                    vname,
                    decl_init.to_string()
                ));
            }
            AstAddr(ref operand, _) => {
                buf.push_str(&format!("(& {})", operand.to_string()));
            }
            AstDeref(ref operand, _) => {
                buf.push_str(&format!("(* {})", operand.to_string()));
            }
            _ => panic!("No implement"),
        };
        return buf;
    }

    pub fn ensure_lvalue(&self) -> bool {
        match self.kind {
            AstKind::AstVar(_) => true,
            _ => panic!("variable expected"),
        }
    }

    pub fn get_ctype(&self) -> Option<&Ctype> {
        use AstKind::*;
        match self.kind {
            AstBinop(_, ref ctype, ..) |
            AstLiteral(ref ctype) => Some(ctype),
            AstVar(ref var) => Some(&var.ctype),
            AstAddr(_, ref ctype) => Some(&ctype),
            AstDeref(_, ref ctype) => Some(&ctype),
            _ => None,
        }
    }
}

fn result_type_int(op: char, mut a_type: Ctype, mut b_type: Ctype) -> Ctype {
    use Ctype::*;
    let mut swapped = false;
    if a_type > b_type {
        swapped = true;
        mem::swap(&mut a_type, &mut b_type);
    }

    match a_type {
        Void => {
            error(a_type, b_type, op, swapped);
        }
        Int(_) => {
            match b_type {
                Int(_) => return Int(None),
                Char(_) => return Int(None),
                Str(_) => error(a_type, b_type, op, swapped),
                _ => panic!("Internal Error"),
            }
        }
        Char(_) => {
            match b_type {
                Char(_) => return Int(None),
                Str(_) => error(a_type, b_type, op, swapped),
                _ => panic!("Internal Error"),
            }
        }
        Str(_) => error(a_type, b_type, op, swapped),
        Ptr(a_ptr) => {
            match b_type {
                Ptr(b_ptr) => return Ptr(Box::new(result_type_int(op, *a_ptr.clone(), *b_ptr.clone()))),
                _ => panic!("Internal Error"),
            }
        }
    }

    return Int(None);

    fn error(mut a: Ctype, mut b: Ctype, op: char, swapped: bool) {
        if swapped {
            mem::swap(&mut a, &mut b);
        }
        panic!("incompatible operands: {:?} and {:?} for {}", a, b, op);
    }

}

fn result_type(op: char, a: Ast, b: Ast) -> Ctype {
    return result_type_int(op, a.get_ctype().unwrap().clone(), b.get_ctype().unwrap().clone());
}

fn read_expr(prec: i8) -> Option<Ast> {
    let mut ast: Ast;

    if let Some(ret_val) = read_unary_expr() {;
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
                //     kind: AstBinop('+',
                //                 Ast { kind: AstBinop('+',
                //                                   Ast { kind: AstInt(1) },
                //                                   Ast { kind: AstInt(2) })
                //                     },
                //                 Ast { kind: AstInt(3) })
                // }
                if tok.is_punct(&'=') {
                    ast.ensure_lvalue();
                }
                if let Some(rest) = read_expr(prec2 + if is_right_assoc(punct) { 0 } else { 1 }) {
                    // NN?
                    let ctype = result_type(punct, ast.clone(), rest.clone());
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
        if tok.is_punct(&')') {
            break;
        }
        tok.unget_token();
        args.push(read_expr(0).unwrap());
        tok = lex::read_token().unwrap();
        if tok.is_punct(&')') {
            break;
        } else if !tok.is_punct(&',') {
            panic!("Unexpected token: '{}'", tok.as_string());
        }


        if i == MAX_ARGS {
            panic!("Too many arguments: {}", fname);
        }
    }

    Ast::new(AstKind::AstFuncCall(FuncCall::new(fname, args)))
}

fn read_unary_expr() -> Option<Ast> {
    let tok = lex::read_token().unwrap();
    if tok.is_punct(&'&') {
        let operand = read_unary_expr().unwrap();
        operand.ensure_lvalue();
        return Some(Ast::new(AstKind::AstAddr(Box::new(operand.clone()),Ctype::Ptr(Box::new(operand.get_ctype().unwrap().clone())))));
    }
    if tok.is_punct(&'*') {
        let operand = read_unary_expr().unwrap();
        let ctype = operand.get_ctype();
        if let Some(Ctype::Ptr(ptr)) = ctype {
            return Some(Ast::new(AstKind::AstDeref(Box::new(operand.clone()), Ctype::Ptr(ptr.clone()))));
        } else {
            panic!("pointer type expected, but got {}", operand.to_string());
        }
    }

    tok.unget_token();
    read_prim()
}

fn read_ident_or_func(name: String) -> Ast {
    let tok = lex::read_token().unwrap();
    if tok.is_punct(&'(') {
        return read_func_args(name);
    }
    tok.unget_token();
    if let Some(v) = find_var(&name) {
        Ast::new(AstKind::AstVar(v))
    } else {
        panic!("Undefined varaible: {}", name);
    }
}

fn read_decl() -> Ast {
    let mut ctype = lex::read_token().unwrap().get_ctype().unwrap();
    let mut tok;
    loop {
        tok = lex::read_token().unwrap();
        if !tok.is_punct(&'*') {
            break
        }
        ctype = Ctype::Ptr(Box::new(ctype));
    }

    match tok {
        lex::Token::TtypeIdent(ref sval) => {
            let var = Ast::new(AstKind::AstVar(Var::new(ctype, sval.clone())));
            lex::expect('=');
            let init = read_expr(0).unwrap();
            Ast::new(AstKind::AstDecl(Box::new(var), Box::new(init)))
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
            if !tok.is_punct(&';') {
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
    return buf;
}

pub fn emit_data_section() {
    let strings = CONTEXT.lock().unwrap().get_strings().clone();
    if strings.is_empty() {
        return;
    }
    print!("\t.data\n");
    for p in strings {
        print!(".s{}:\n\t", p.sid);
        print!(".string \"{}\"\n", quote(&p.sval));
    }
    print!("\t");
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
