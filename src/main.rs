use std::io;
use std::str::Chars;

fn compile_number(mut n: u32, chars: Chars) {
    for c in chars.skip(1) {
        if c.is_whitespace() {
            break;
        }
        if !c.is_ascii_digit() {
            eprintln!("Invalid character in number: '{}'", c)
        }
        n = n * 10 + c.to_digit(10).unwrap();
    }
    println!(
        "\t.text
         \t.global intfn
         intfn:
         \tmov ${}, %rax
         \tret\n",
        n
    );
}

fn compile_string(chars: Chars) {
    let mut res = String::new();
    if chars.clone().last() != Some('"') {
        panic!("Unterminated string")
    }

    for c in chars.skip(1) {
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

    let c = input.chars().next().unwrap();

    if c.is_ascii_digit() {
        let n = c.to_digit(10).unwrap();
        compile_number(n, input.chars());
    }
    if c == '"' {
        compile_string(input.chars());
    }

    Ok(())
}

fn main() -> io::Result<()> {
    compile()?;
    Ok(())
}
