pub fn factors(n: u64) -> Vec<u64> {
    let mut factors: Vec<u64> = Vec::new();
    let mut number = n;
    let mut divider = 2;

    while number != 1 {
        if number % divider != 0 {
            divider += 1;
            continue;
        }

        factors.push(divider);
        number /= divider;
    }

    factors
}
