use std::collections::HashSet;

pub fn score(word: &str) -> u64 {
    let score_map = "|AEIOULNRST|DG|BCMP|FHVWY|K|||JX||QZ";
    let word_upper = word
        .chars()
        .filter(|&c| c.is_ascii())
        .collect::<String>()
        .to_uppercase();
    word_upper
        .chars()
        .collect::<HashSet<char>>()
        .iter()
        .fold(0, |sum, &c| {
            sum + word_upper.matches(c).count()
                * score_map
                    .chars()
                    .take_while(|&i| i != c)
                    .filter(|&i| i == '|')
                    .count()
        }) as u64
}
