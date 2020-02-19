use regex::Regex;
use std::collections::HashMap;

pub fn word_count(words: &str) -> HashMap<String, u32> {
    let re = Regex::new(r"([a-z0-9']+)").unwrap();
    let mut stats: HashMap<String, u32> = HashMap::new();
    for item in re.find_iter(&words.to_lowercase()) {
        let word = item.as_str().trim_matches(|c| c == '\'').to_string();
        stats.insert(word.clone(), stats.get(&word).unwrap_or(&0) + 1);
    }
    stats
}
