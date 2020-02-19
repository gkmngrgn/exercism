pub fn collatz(n: u64) -> Option<u64> {
    match n {
        0 => None,
        _ => {
            let mut steps = 0;
            let mut number = n;
            while number != 1 {
                number = match number % 2 {
                    0 => number / 2,
                    _ => number * 3 + 1,
                };
                steps += 1;
            }
            Some(steps)
        }
    }
}
