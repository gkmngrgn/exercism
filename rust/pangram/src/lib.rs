use std::collections::HashSet;

const LETTER_COUNT: usize = 26;

pub fn is_pangram(sentence: &str) -> bool {
    sentence
        .to_ascii_lowercase()
        .chars()
        .filter(|c| c.is_alphabetic() && c.is_ascii())
        .collect::<HashSet<char>>()
        .len()
        == LETTER_COUNT
}
