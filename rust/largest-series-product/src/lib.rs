use std::cmp::max;

#[derive(Debug, PartialEq)]
pub enum Error {
    SpanTooLong,
    InvalidDigit(char),
}

pub fn lsp(string_digits: &str, span: usize) -> Result<u64, Error> {
    if span == 0 {
        return Ok(1);
    } else if string_digits.len() < span {
        return Err(Error::SpanTooLong);
    } else if let Some(c) = string_digits.chars().skip_while(|d| d.is_digit(10)).next() {
        return Err(Error::InvalidDigit(c));
    }
    Ok(string_digits
        .chars()
        .filter_map(|d| d.to_digit(10))
        .collect::<Vec<u32>>()
        .windows(span)
        .fold(0, |product, serie| {
            max(product, serie.iter().map(|&d| d as u64).product())
        }))
}
