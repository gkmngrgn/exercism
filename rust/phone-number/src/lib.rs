pub fn number(user_number: &str) -> Option<String> {
    let normalized: String = user_number.chars().filter(|c| c.is_digit(10)).collect();
    match normalized.len() {
        11 => {
            if normalized.chars().nth(0).unwrap() != '1' {
                None
            } else {
                get_number_checked(normalized.chars().skip(1).collect())
            }
        }
        10 => get_number_checked(normalized),
        _ => None,
    }
}

fn get_number_checked(number: String) -> Option<String> {
    let invalid_digits = ['0', '1'];
    for pos in &[0, 3] {
        if invalid_digits.contains(&number.chars().nth(*pos).unwrap()) {
            return None;
        }
    }
    Some(number)
}
