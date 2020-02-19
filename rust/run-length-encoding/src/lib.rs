use std::cmp::max;

pub fn encode(source: &str) -> String {
    let mut remain = String::from(source);
    let mut encoded = String::new();
    while remain.len() > 0 {
        let letter = remain.chars().next().unwrap();
        let remain_new = remain.trim_start_matches(letter);
        match remain.len() - remain_new.len() {
            1 => encoded.push(letter),
            c => encoded.push_str(&format!("{}{}", c, letter)),
        };
        remain = remain_new.to_string();
    }
    encoded
}

pub fn decode(source: &str) -> String {
    let mut decoded = String::new();
    let mut count = 0;
    for index in 0..source.len() {
        let current = source.chars().skip(index).next().unwrap();
        if current.is_digit(10) {
            count = count * 10 + current.to_digit(10).unwrap();
            continue;
        }
        decoded.push_str(&(0..max(1, count)).map(|_| current).collect::<String>());
        count = 0;
    }
    decoded
}
