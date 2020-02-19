use num_bigint::{BigInt, Sign};
use std::cmp::{max, min, Ordering};
use std::ops::{Add, Mul, Sub};
use std::str::FromStr;

#[derive(Debug)]
pub struct Decimal {
    value: String,
    sign: Sign,
    decimal_length: usize,
}

impl Decimal {
    fn new(value: String, sign: Sign, decimal_length: usize) -> Self {
        Self {
            value: value,
            sign: sign,
            decimal_length: decimal_length,
        }
    }

    pub fn try_from(input: &str) -> Option<Decimal> {
        let value = if input.contains('.') {
            input.trim_matches('0').trim_end_matches('.')
        } else {
            input
        }
        .chars()
        .filter(|c| c.is_digit(10))
        .collect::<String>();
        let sign = match input.chars().nth(0) {
            Some('-') => Sign::Minus,
            _ => Sign::Plus,
        };
        let decimal_length = match input.find('.') {
            None => 0,
            _ => input
                .chars()
                .rev()
                .skip_while(|&c| c == '0')
                .take_while(|&c| c != '.')
                .count(),
        };
        Some(Decimal::new(value, sign, decimal_length))
    }

    fn try_from_int(input: BigInt, decimal_length: usize) -> Decimal {
        let mut input = format!("{}", input);
        while input.len() < decimal_length {
            input.insert(0, '0');
        }
        input.insert(input.len() - decimal_length, '.');
        Decimal::try_from(&input).unwrap()
    }

    fn normalize(&self, len: usize) -> BigInt {
        let sign = match self.sign {
            Sign::Minus => '-',
            _ => '+',
        };
        let zeros = "0".repeat(len - self.decimal_length);
        let value = format!("{}{:0<1}{}", sign, self.value, zeros);
        BigInt::from_str(&value).unwrap()
    }
}

impl PartialEq for Decimal {
    fn eq(&self, other: &Decimal) -> bool {
        let len = max(self.decimal_length, other.decimal_length);
        self.normalize(len) == other.normalize(len)
    }
}

impl PartialOrd for Decimal {
    fn partial_cmp(&self, other: &Decimal) -> Option<Ordering> {
        let len = max(self.decimal_length, other.decimal_length);
        self.normalize(len).partial_cmp(&other.normalize(len))
    }
}

impl Add for Decimal {
    type Output = Decimal;

    fn add(self, other: Decimal) -> Self::Output {
        let len = max(self.decimal_length, other.decimal_length);
        Decimal::try_from_int(self.normalize(len) + other.normalize(len), len)
    }
}

impl Sub for Decimal {
    type Output = Decimal;

    fn sub(self, other: Decimal) -> Self::Output {
        let len = max(self.decimal_length, other.decimal_length);
        Decimal::try_from_int(self.normalize(len) - other.normalize(len), len)
    }
}

impl Mul for Decimal {
    type Output = Decimal;

    fn mul(self, other: Decimal) -> Self::Output {
        let len = max(self.decimal_length, other.decimal_length);
        let mut multipled = self.normalize(len) * other.normalize(len);
        for _ in 0..len - min(self.decimal_length, other.decimal_length) {
            multipled /= 10;
        }
        Decimal::try_from_int(multipled, len)
    }
}
