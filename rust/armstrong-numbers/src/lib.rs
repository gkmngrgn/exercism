use std::convert::TryInto;

pub fn is_armstrong_number(num: u32) -> bool {
    let digits: Vec<_> = num
        .to_string()
        .chars()
        .map(|d| d.to_digit(10).unwrap())
        .collect();
    let digit_length: u32 = digits.len().try_into().unwrap();
    num == digits.iter().map(|d| d.pow(digit_length)).sum()
}
