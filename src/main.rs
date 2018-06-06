use std::io;
use std::iter::FromIterator;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Stream {
    data: Vec<char>,
    pos: usize,
}

impl FromIterator<char> for Stream {
    fn from_iter<T: IntoIterator<Item = char>>(t: T) -> Self {
        Self {
            data: t.into_iter().collect(),
            pos: 0,
        }
    }
}

impl From<String> for Stream {
    fn from(s: String) -> Self {
        Self::from_iter(s.chars())
    }
}

impl Iterator for Stream {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        if self.pos >= self.data.len() {
            return None;
        }
        self.pos += 1;
        Some(self.data[self.pos - 1])
    }
}

impl Stream {
    pub fn prev(&self) -> Self {
        self.jump(self.pos - 1)
    }

    pub fn jump(&self, pos: usize) -> Self {
        Self {
            data: self.data.clone(),
            pos: pos,
        }
    }

    pub fn skip_space(&mut self) {
        for c in self.data[self.pos..].iter() {
            self.pos += 1;
            if c.is_whitespace() {
                continue;
            }
            self.pos -= 1;
            return;
        }
    }
}


fn read_number(mut n: u32, mut stream: Stream) -> (u32, Stream) {
    loop {
        if let Some(c) = stream.next() {
            if c.is_whitespace() {
                break;
            } else if !c.is_ascii_digit() {
                return (n, stream.prev());
            }
            n = n * 10 + c.to_digit(10).unwrap();
        } else {
            return (n, stream);
        }
    }

    return (n, stream);
}

fn compile_expr2(mut stream: Stream) {
    let mut op: &str;
    stream.skip_space();
    loop {
        match stream.next() {
            Some(c) => {
                op = match c {
                    '+' => "add",
                    '-' => "sub", 
                    _ => {
                        panic!("Operator expected, but got {:?}", c);
                    }
                }
            }
            None => break,
        }
        stream.skip_space();
        let c = stream.next().unwrap();
        if !c.is_ascii_digit() {
            eprintln!("Number expected, but got {:?}", c);
        }
        println!(
            "\t{} ${}, %rax",
            op,
            read_number(c.to_digit(10).unwrap(), stream.clone()).0
        );
    }

    println!("\tret")
}

fn compile_expr(n: u32, stream: Stream) {
    let (n, stream) = read_number(n, stream);
    println!(
        "\t.text
         \t.global intfn
         intfn:
         \tmov ${}, %rax",
        n
    );
    compile_expr2(stream);
}

fn compile_string(stream: Stream) {
    let mut res = String::new();
    if stream.clone().last() != Some('"') {
        panic!("Unterminated string")
    }

    for c in stream {
        if c == '"' {
            break;
        }
        res.push(c);
    }
    println!(
        "\t.data
        .mydata:
        \t.string {:?}
        \t.text
        \t.global stringfn
        stringfn:
        \tlea .mydata(%rip), %rax
        \tret",
        res
    );
}

fn compile() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

    // remove `\n`
    input.pop();

    let mut stream = Stream::from(input.clone());


    let c = stream.next().unwrap();

    if c.is_ascii_digit() {
        let n = c.to_digit(10).unwrap();
        compile_expr(n, stream);
    } else if c == '"' {
        compile_string(stream);
    }

    Ok(())
}

fn main() -> io::Result<()> {
    compile()?;
    Ok(())
}
