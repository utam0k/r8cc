use std::io;

fn main() -> io::Result<()> {
    let mut val = String::new();
    io::stdin().read_line(&mut val)?;

    // remove `\n`
    val.pop();

    println!(
        "\t.text
         \t.global mymain
         mymain:
         \tmov ${}, %eax
         \tret\n",
        val.parse::<i32>().unwrap()
    );

    Ok(())
}
