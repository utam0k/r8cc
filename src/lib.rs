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

    pub fn get_nonspace() -> Option<char> {
        INPUT.lock().unwrap().get_nonspace()
    }
}

const REGS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
const MAX_ARGS: usize = 6;

#[derive(Clone, Debug)]
pub struct Var {
    name: String,
    pos: usize,
    ctype: Ctype,
}

impl Var {
    pub fn new(ctype: Ctype, name: String) -> Self {
        Self {
            name: name,
            pos: CONTEXT.lock().unwrap().get_vars_len() + 1,
            ctype: ctype,
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
pub enum Ctype {
    Void,
    Int,
    Char,
    Str,
}

impl Ctype {
    pub fn as_string(&self) -> &str {
        use Ctype::*;
        match self {
            Void => "void",
            Int => "int",
            Char => "char",
            Str => "string",
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
    AstDecl(Box<Ast>, Box<Ast>),
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
            AstKind::AstDecl(ref decl_var, ref decl_init) => decl_var.emit_assign(decl_init),
            _ => self.emit_binop(),
        }
    }

    fn emit_assign(&self, value: &Box<Self>) {
        value.emit_expr();
        match self.kind {
            AstKind::AstVar(ref var) => print!("mov %eax, -{}(%rbp)\n\t", var.pos * 4),
            _ => panic!("invalid operand"),
        }
    }

    fn emit_binop(&self) {
        let op: &str;
        op = match self.kind {
            AstKind::AstOp(kind, ref left, ref right) => {
                match kind {
                    '=' => {
                        left.emit_assign(right);
                        // right.emit_expr();
                        // match left.kind {
                        //     AstKind::AstVar(ref var) => {
                        //         print!("mov %eax, -{}(%rbp)\n\t", var.pos * 4)
                        //     }
                        //     _ => panic!("invalid operand"),
                        // }
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
            AstDecl(ref decl_var, ref decl_init) => {
                let (ctype, vname) = match decl_var.kind {
                    AstKind::AstVar(ref var) => (&var.ctype, &var.name),
                    _ => panic!("variable expected"),
                };

                print!("(decl {} {} ", ctype.as_string(), vname);
                decl_init.print_ast();
                print!(")");
            }
        }
    }

    pub fn ensure_lvalue(&self) -> bool {
        match self.kind {
            AstKind::AstVar(_) => true,
            _ => panic!("variable expected"),
        }
    }
}

fn read_expr(prec: i8) -> Option<Ast> {
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
                if let Some(right) = read_expr(prec2 + 1) {
                    if tok.is_punct(&'=') {
                        ast.ensure_lvalue();
                    }
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
    let ctype = lex::read_token().unwrap().get_ctype().unwrap();
    let name = lex::read_token().unwrap();
    match name {
        lex::Token::TtypeIdent(ref sval) => {
            let var = Ast::new(AstKind::AstVar(Var::new(ctype, sval.clone())));
            lex::expect('=');
            let init = read_expr(0).unwrap();
            Ast::new(AstKind::AstDecl(Box::new(var), Box::new(init)))
        }
        _ => panic!("Identifier expected, but got {}", name.as_string()),
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
