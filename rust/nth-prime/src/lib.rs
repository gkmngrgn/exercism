fn is_prime(value: u32) -> bool {
    if value == 2 || value == 3 {
        return true;
    }

    if value % 2 == 0 || value % 3 == 0 {
        return false;
    }

    let mut divisor = 6;
    while divisor * divisor - 2 * divisor + 1 <= value {
        if value % (divisor - 1) == 0 || value % (divisor + 1) == 0 {
            return false;
        }
        divisor += 6;
    }

    return true;
}

pub fn nth(n: u32) -> u32 {
    let mut nth = 0;
    let mut nth_prime = 2;

    while nth < n {
        nth_prime += 1;
        if is_prime(nth_prime) {
            nth += 1;
        }
    }

    return nth_prime;
}
