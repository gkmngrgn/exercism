const A_LEN: i8 = 26; // alphabet length

pub fn rotate(input: &str, key: i8) -> String {
    let rotated_c = |a, c| ((c - a + key) % A_LEN + A_LEN) % A_LEN + a;
    input
        .chars()
        .map(|c| match c {
            'a'..='z' => rotated_c('a' as i8, c as i8) as u8 as char,
            'A'..='Z' => rotated_c('A' as i8, c as i8) as u8 as char,
            _ => c,
        })
        .collect()
}
