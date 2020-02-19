use std::fmt::Display;

pub trait Luhn: Display {
    fn valid_luhn(&self) -> bool;
}

impl<T: Display> Luhn for T {
    fn valid_luhn(&self) -> bool {
        let code = self.to_string().replace(" ", "");
        let digits: Vec<u32> = code.chars().filter_map(|c| c.to_digit(10)).collect();
        if code.len() != digits.len() || digits.len() <= 1 {
            return false;
        }
        let sum: u32 = digits
            .iter()
            .rev()
            .enumerate()
            .map(|(i, &d)| match d {
                n if i % 2 == 0 => n,
                n if n < 5 => n * 2,
                n => n * 2 - 9,
            })
            .sum();
        sum % 10 == 0
    }
}
