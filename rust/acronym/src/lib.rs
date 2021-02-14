const IGNORES: [char; 3] = [' ', '-', '_'];

pub fn abbreviate(phrase: &str) -> String {
    let acronym: String = format!(" {}", phrase)
        .chars()
        .collect::<Vec<char>>()
        .windows(2)
        .filter(|p| {
            p[1].is_alphabetic() && IGNORES.iter().any(|x| x == &p[0])
                || (p[1].is_uppercase() && p[0].is_lowercase())
        })
        .map(|p| p[1])
        .collect();
    acronym.to_uppercase()
}
