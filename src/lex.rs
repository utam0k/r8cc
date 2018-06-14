use r8cc_io::{getc, ungetc, skip_space};

static mut UNGOTTEN: Option<Token> = None;

#[derive(Clone, Debug)]
pub enum Token {
    TtypeIdent(String),
    TtypePunct(char),
    TtypeInt(i32),
    TtypeChar(char),
    TtypeString(String),
}

impl Token {
    pub fn as_string(&self) -> String {
        match self {
            Token::TtypeIdent(ref sval) |
            Token::TtypeString(ref sval) => sval.clone(),
            Token::TtypePunct(c) |
            Token::TtypeChar(c) => c.to_string(),
            Token::TtypeInt(ival) => ival.to_string(),
        }
    }

    pub fn is_punct(&self, c: &char) -> bool {
        match self {
            Token::TtypePunct(punct) if punct == c => true,
            _ => false,
        }
    }

    pub fn unget_token(&self) {
        unsafe {
            if !UNGOTTEN.is_none() {
                panic!("Push back buffer is already full");
            }
            UNGOTTEN = Some(self.clone())
        }
    }
}

fn read_number(c: char) -> Token {
    let mut n = c.to_digit(10).unwrap();
    loop {
        let next_char: Option<char>;
        next_char = getc();
        if let Some(c) = next_char {
            if !c.is_ascii_digit() {
                ungetc();
                return Token::TtypeInt(n as i32);
            }
            n = n * 10 + c.to_digit(10).unwrap();
        }
    }
}

fn read_char() -> Token {
    let next_char: Option<char> = getc();
    match next_char {
        Some(c) => {
            if c == '\\' && getc().is_none() {
                panic!("Unterminated \\");
            }
            if let Some(c2) = getc() {
                if c2 != '\'' {
                    panic!("Malformed char constant");
                }
            } else {
                panic!("Unterminated \\");
            }
            Token::TtypeChar(c)
        }
        None => panic!("Unterminated string"),
    }
}

fn read_string() -> Token {
    let mut s = String::new();
    let mut next_char: Option<char>;

    loop {
        next_char = getc();
        if let Some(c) = next_char {
            if c == '"' {
                break;
            } else if c == '\\' {
                next_char = getc();
                if let Some(c2) = next_char {
                    s.push(c2);
                } else {
                    panic!("Unterminated \\");
                }
            }
            s.push(c);
        } else {
            panic!("Unterminated string");
        }
    }

    Token::TtypeString(s)
}

fn read_ident(c: char) -> Token {
    let mut s = String::new();
    s.push(c);

    loop {
        let next_char = getc();
        if let Some(c) = next_char {
            if c.is_alphabetic() || c.is_ascii_digit() {
                s.push(c);
            } else {
                ungetc();
                return Token::TtypeIdent(s);
            }
        }
    }
}

fn read_token_int() -> Option<Token> {
    skip_space();
    if let Some(c) = getc() {
        let res = match c {
            '0'...'9' => read_number(c),
            '"' => read_string(),
            '\'' => read_char(),
            'a'...'z' | 'A'...'Z' | '_' => read_ident(c),
            '/' | '=' | '*' | '+' | '-' | '(' | ')' | ',' | ';' => Token::TtypePunct(c),
            _ => panic!("Unexpected character: '{}'", c),
        };
        return Some(res);
    } else {
        return None;
    }
}

pub fn read_token() -> Option<Token> {
    unsafe {
        if !UNGOTTEN.is_none() {
            let tok = UNGOTTEN.clone();
            UNGOTTEN = None;
            return tok;
        }
    }
    read_token_int()
}
