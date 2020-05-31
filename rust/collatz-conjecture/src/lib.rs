pub fn collatz(n: u64) -> Option<u64> {
    let mut steps = 0;
    let mut number = n;
    loop {
        match number {
            0 => return None,
            1 => return Some(steps),
            n if n % 2 == 0 => number /= 2,
            _ => number = number * 3 + 1,
        }
        steps += 1;
    }
}
