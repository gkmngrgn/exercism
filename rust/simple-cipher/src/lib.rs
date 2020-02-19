use rand::Rng;

const A: u8 = 'a' as u8;
const Z: u8 = 'z' as u8;
const A_LEN: u8 = Z - A + 1; // alphabet length

enum Direction {
    Positive,
    Negative,
}

pub fn encode(key: &str, s: &str) -> Option<String> {
    shift(key, s, Direction::Positive)
}

pub fn decode(key: &str, s: &str) -> Option<String> {
    shift(key, s, Direction::Negative)
}

pub fn encode_random(s: &str) -> (String, String) {
    let key: String = (0..=100)
        .map(|_| rand::thread_rng().gen_range(A, Z + 1) as char)
        .collect();
    (key.clone(), shift(&key, s, Direction::Positive).unwrap())
}

fn shift(key: &str, s: &str, direction: Direction) -> Option<String> {
    if key == "" || key.as_bytes().iter().filter(|&b| b < &A || b > &Z).count() > 0 {
        return None;
    }
    let k = |i| key.as_bytes().iter().nth(i % key.len()).unwrap() - A;
    Some(
        s.chars()
            .enumerate()
            .map(|(i, c)| {
                let d = match direction {
                    Direction::Positive => A_LEN + k(i),
                    Direction::Negative => A_LEN - k(i),
                };
                ((d + c as u8 - A) % A_LEN + A) as char
            })
            .collect(),
    )
}
