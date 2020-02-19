pub fn translate(input: &str) -> String {
    input
        .split_whitespace()
        .map(|word| {
            let output = if is_vowel(word.chars().nth(0).unwrap())
                || word.starts_with("xr")
                || word.starts_with("yt")
            {
                word.to_string()
            } else if let Some(index) = word.find("qu") {
                shift_consonants(word, index + 2)
            } else {
                match word.find(is_vowel) {
                    Some(index) => shift_consonants(word, index),
                    None => {
                        let index_y = word.find("y").unwrap_or(0);
                        if index_y > 0 && !is_vowel(word.chars().nth(index_y - 1).unwrap()) {
                            shift_consonants(word, index_y)
                        } else {
                            word.to_string()
                        }
                    }
                }
            };
            format!("{}ay", output)
        })
        .collect::<Vec<String>>()
        .join(" ")
}

fn is_vowel(letter: char) -> bool {
    match letter {
        'a' | 'e' | 'i' | 'o' | 'u' => true,
        _ => false,
    }
}

fn shift_consonants(input: &str, index: usize) -> String {
    format!(
        "{}{}",
        input.chars().skip(index).collect::<String>(),
        input.chars().take(index).collect::<String>()
    )
}
