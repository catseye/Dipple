fn main() {
    let mut x: u64 = 0;
    loop {
        io::println(fmt!("%?", x));
        io::stdout().flush();
        x += 1;
    }
}
