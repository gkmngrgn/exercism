const MAX_S: u32 = 64;

pub fn square(s: u32) -> u64 {
    if s == 0 || s > MAX_S {
        panic!("Square must be between 1 and 64")
    }
    u64::pow(2, s - 1)
}

pub fn total() -> u64 {
    let square_of_max_s = square(MAX_S);
    square_of_max_s + (square_of_max_s - 1)
}
