use regex::Regex;

pub fn is_valid_isbn(isbn: &str) -> bool {
    let re = Regex::new(r"^[0-9-]{9,}?[0-9X]{1}$").unwrap();
    if !re.is_match(isbn) {
        return false;
    }
    let digits = isbn.chars().filter(|c| c.is_digit(10) || c == &'X');
    let total: usize = digits
        .clone()
        .enumerate()
        .map(|(i, c)| c.to_digit(10).unwrap_or(10) as usize * (i + 1))
        .sum();
    total % 11 == 0 && digits.count() == 10
}
