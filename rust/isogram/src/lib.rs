pub fn check(candidate: &str) -> bool {
    let letters: String = candidate
        .to_lowercase()
        .chars()
        .filter(|c| c.is_alphabetic())
        .collect();
    let mut chars: Vec<char> = letters.chars().collect();
    chars.sort();
    chars.dedup();
    letters.len() == chars.len()
}
