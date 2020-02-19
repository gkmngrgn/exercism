use std::collections::HashSet;

pub fn anagrams_for<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    let sorted_word = sort_text(word);
    possible_anagrams
        .iter()
        .filter(|w| w.to_lowercase() != word.to_lowercase())
        .filter(|w| sort_text(w) == sorted_word)
        .map(|&w| w)
        .collect()
}

fn sort_text(text: &str) -> String {
    let mut sorted_text: Vec<char> = text.to_lowercase().chars().collect();
    sorted_text.sort();
    sorted_text.iter().collect()
}
