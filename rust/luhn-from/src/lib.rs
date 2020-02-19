use std::fmt::Display;

pub struct Luhn {
    digits: Vec<u32>,
}

impl Luhn {
    pub fn is_valid(&self) -> bool {
        if self.digits.len() <= 1 {
            return false;
        }
        let sum: u32 = self
            .digits
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

impl<T: Display> From<T> for Luhn {
    fn from(input: T) -> Self {
        let code = input.to_string().replace(" ", "");
        let digits: Vec<u32> = if code.chars().filter(|c| !c.is_digit(10)).count() == 0 {
            code.chars().filter_map(|c| c.to_digit(10)).collect()
        } else {
            vec![]
        };
        Self { digits: digits }
    }
}
