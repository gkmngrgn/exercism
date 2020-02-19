pub fn series(digits: &str, len: usize) -> Vec<String> {
    let mut series = Vec::new();
    for index in 0..digits.len() + 1 - len {
        series.push(digits[index..index + len].to_string());
    }
    series
}
