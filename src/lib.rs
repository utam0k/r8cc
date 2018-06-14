pub mod stream;
pub mod lex;
pub mod context;

#[macro_use]
extern crate lazy_static;

use std::sync::Mutex;
use std::marker::Send;
use std::io::stdin;

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

    pub fn skip_space() {
        INPUT.lock().unwrap().skip_space()
    }
}

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

/// 1. Call read_expr() for each ';'.
/// 2. Call emit_data_section().
/// 3. Call emit_expr() or print_ast().
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
        let tok = lex::read_token().unwrap();
        if !tok.is_punct(&';') {
            panic!("Unterminated expression: {}", tok.as_string());
        }
        return Some(r);
    }
    None
}


fn read_expr2(prec: i8) -> Option<Ast> {
    let mut ast: Ast;
    let mut op: AstKind;

    if let Some(ret_val) = read_prim() {;
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
                //     kind: AstOp('+',
                //                 Ast { kind: AstOp('+',
                //                                   Ast { kind: AstInt(1) },
                //                                   Ast { kind: AstInt(2) })
                //                     },
                //                 Ast { kind: AstInt(3) })
                // }
                if let Some(right) = read_expr2(prec2 + 1) {
                    op = AstKind::AstOp(punct, Box::new(ast.clone()), Box::new(right));
                    ast = Ast::new(op);
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
    use AstKind::*;
    if let Some(token) = lex::read_token() {
        return Some(match token {
            TtypeIdent(sval) => read_ident_or_func(sval),
            TtypeInt(ival) => Ast::new(AstInt(ival as u32)),
            TtypeChar(c) => Ast::new(AstChar(c)),
            TtypeString(sval) => Ast::new(AstStr(Str::new(sval))),
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
        args.push(read_expr2(0).unwrap());
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

fn read_ident_or_func(name: String) -> Ast {
    let tok = lex::read_token().unwrap();
    if tok.is_punct(&'(') {
        return read_func_args(name);
    }
    tok.unget_token();
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
