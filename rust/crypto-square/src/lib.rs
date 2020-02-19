pub fn encrypt(input: &str) -> String {
    let normalized: String = input
        .to_ascii_lowercase()
        .chars()
        .filter(|c| c.is_alphanumeric())
        .collect();
    let normalized_length = normalized.len();
    let (width, height) = get_edges(normalized_length);
    let mut output = String::new();
    for h in 0..height {
        output.push(' ');
        for w in 0..width {
            output.push(normalized.chars().nth(h + w * height).unwrap_or(' '));
        }
    }
    output.trim_start().to_string()
}

fn get_edges(length: usize) -> (usize, usize) {
    let (mut r, mut c) = (0, 0);
    while r * c < length {
        if r == c {
            c += 1;
        } else {
            r += 1;
        }
    }
    (r, c)
}
