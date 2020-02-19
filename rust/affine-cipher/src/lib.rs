use std::char;

const M: i32 = 26; // alphabet len, typical denoting in affince cipher

#[derive(Debug, Eq, PartialEq)]
pub enum AffineCipherError {
    NotCoprime(i32),
}

pub fn encode(plaintext: &str, a: i32, b: i32) -> Result<String, AffineCipherError> {
    if gcd(a) > 1 {
        return Err(AffineCipherError::NotCoprime(a));
    }
    Ok(plaintext
        .to_ascii_lowercase()
        .chars()
        .filter(|c| c.is_alphabetic() || c.is_digit(10))
        .map(|c| {
            let x = c as i32 - 97;
            if x < 0 {
                c
            } else {
                char::from_u32(((a * x + b) % M) as u32 + 97).unwrap()
            }
        })
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
        .to_string())
}

pub fn decode(ciphertext: &str, a: i32, b: i32) -> Result<String, AffineCipherError> {
    if gcd(a) > 1 {
        return Err(AffineCipherError::NotCoprime(a));
    }
    let mmi_a = mmi(a);
    Ok(ciphertext
        .chars()
        .filter(|c| c.is_alphabetic() || c.is_digit(10))
        .map(|c| {
            let y = c as i32 - 97;
            if y < 0 {
                c
            } else {
                let index = (mmi_a * (y - b) % M + M) % M;
                char::from_u32(index as u32 + 97).unwrap()
            }
        })
        .collect())
}

fn mmi(a: i32) -> i32 {
    for i in 0..M {
        if (a * i) % M == 1 {
            return i;
        }
    }
    1
}

fn gcd(mut a: i32) -> i32 {
    let mut m = M;
    while m != 0 {
        let t = m;
        m = a % m;
        a = t;
    }
    a
}
