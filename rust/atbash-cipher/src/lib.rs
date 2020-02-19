const Z: u8 = 'z' as u8;
const A: u8 = 'a' as u8;

fn translate(text: &str) -> String {
    String::from_utf8(
        text.chars()
            .filter(|c| c.is_alphabetic() || c.is_digit(10))
            .map(|c| match c as u8 {
                b if b >= A && b <= Z => Z - b + A,
                b => b,
            })
            .collect(),
    )
    .unwrap()
}

pub fn encode(plain: &str) -> String {
    translate(&plain.to_ascii_lowercase())
        .chars()
        .enumerate()
        .map(|(i, c)| {
            if (i + 1) % 5 == 0 {
                format!("{} ", c)
            } else {
                c.to_string()
            }
        })
        .collect::<String>()
        .trim_end()
        .to_string()
}

pub fn decode(cipher: &str) -> String {
    translate(cipher)
}
